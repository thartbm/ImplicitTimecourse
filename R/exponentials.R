library('parallel')
source('R/utilities.R')
# advanced pre-processing -----


bootstrapExponentialLearning <- function(condition, type, iterations=1000, clust=NULL, depvar='reachdeviation_deg') {
  
  # load data:
  info <- groupInfo()
  idx <- which(info$condition == condition)
  exp <- info$exp[idx]
  df <- read.csv(sprintf('data/exp%d/%s_%s.csv', exp, condition, type))
  
  # clean data a bit:
  df <- removeOutliers(df, depvar=depvar)
  df <- baseline(df, depvar=depvar)

  df$depvar <- df[,depvar]
  if (depvar %in% c('reachdeviation_deg', 'aimingdeviation_deg')) {
    df$depvar <- -1 * df$depvar
  }
  df <- df[which(df$phase == 'rotation' & df$trialno < 77),]

  participants <- unique(df$participant)
  BSparticipants <- sample(participants, size=iterations*length(participants), replace=TRUE)
  BSparticipants <- matrix(BSparticipants, nrow=iterations)
  
  # data <- list()
  # for (participant in participants) {
  #   data[[participant]] <- df[which(df$participant == participant),]
  # }
  
  if (is.null(clust)) {
    ncores   <- parallel::detectCores()
    clust <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  }
  
  prepend <- c()
  if (type %in% c('nocursors')) {
    prepend <- c(0)
  }
  
  # print(str(df))
  # print(BSparticipants[1,])
  
  a <- parApply(cl = clust,
                X = BSparticipants,
                MARGIN = 1,
                FUN = fitExp,
                df = df,
                prepend = prepend,
                mode = mode)
  
  outdf <- as.data.frame(t(a))
  
  outfilename <- sprintf('data/exp%d/%s_%s_exp-fits.csv',exp,condition,type)

  write.csv(outdf, outfilename, row.names=FALSE)
  
}

fitExp <- function(participants, df, prepend=c(), mode='learning') {
  
  dfs <- NA
  for (participant in participants) {
    pdf <- df[which(df$participant == participant),]
    if (is.data.frame(dfs)) {
      dfs <- rbind(dfs, pdf)
    } else {
      dfs <- pdf
    }
  }
  
  # if (length(participants) > 1) {
  #   id <- 'all'
  # } else {
  #   id <- participants[1]
  # }
  
  agdf <- aggregate(depvar ~ trialno, data=dfs, FUN=mean, na.rm=TRUE)
  
  signal <- c(prepend, agdf$depvar)
  
  # if (savetempfig) {
  #   if ('type' %in% names(df)) {type=df$type[1]} else {type <- 'undefined'}
  #   if ('condition' %in% names(df)) {condition=df$condition[1]} else {condition <- 'undefined'}
  #   
  #   dir.create(sprintf('temp/%s/',condition), showWarnings = FALSE)
  #   dir.create(sprintf('temp/%s/%s/',condition, type), showWarnings = FALSE)
  #   
  #   pdf(file=sprintf('temp/%s/%s/%s_%s.pdf',condition,type,id,mode))
  #   plot(signal,type='l')
  #   dev.off()
  # }

  # if (mode=='washout') {
  #   setN0 <- signal[1]
  #   gridpoints=5
  # } else {
  #   setN0 <- NULL
  #   gridpoints=11
  # }
  
  pars <- Reach::exponentialFit(signal, timepoints=length(signal), mode=mode, gridpoints=11, gridfits=10)
  
  return(pars)
  
}

bootstrapAllLearningExpFits <- function(iterations=5000, conditions=NULL) {
  
  ncores   <- parallel::detectCores()
  clust <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  
  info <- groupInfo()
  
  types <- c('reaches', 'nocursors')
  
  if (is.null(conditions)) {
    conditions <- info$condition
  }
  
  for (condition in conditions) {
    for (type in types) {
      cat(sprintf('exponential fit (%d iterations) for: %s %s\n',iterations,condition,type))
      bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust)
    }
  }
  
  # bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust)
  
  stopCluster(clust)
  
}


bootstrapAllAimingExpFits <- function(iterations=5000) {
  
  ncores   <- parallel::detectCores()
  clust <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  
  condition = 'aiming'
  type = 'aiming'
  cat(sprintf('exponential fit (%d iterations) for: %s %s\n',iterations,condition,type))
  bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust, depvar='aimingdeviation_deg')

  # bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust)
  
  stopCluster(clust)
  
}

# washouts ------


bootstrapAllWashoutExpFits <- function(iterations=5000) {
  
  # cat('setting up cluster...\n')
  ncores <- parallel::detectCores()
  clust  <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))

  info       <- groupInfo()
  conditions <- info$condition[which(info$exp == 1)]

  for (condition in conditions) {
    for (type in c('reaches','nocursors')) {

      cat(sprintf('exponential fit (%d iterations) for: %s %s\n',iterations,condition,type))
      bootstrapExponentialWashout(condition, type, iterations=iterations, clust=clust, depvar='reachdeviation_deg')

    }
  }

  # cat('stopping cluster...\n')
  stopCluster(clust)

}

bootstrapExponentialWashout <- function(condition, type, iterations=5000, clust=NULL, depvar='reachdeviation_deg') {
  
  # cat('loading data...\n')
  # load data:
  info <- groupInfo()
  idx <- which(info$condition == condition)
  exp <- info$exp[idx]
  df <- read.csv(sprintf('data/exp%d/%s_%s.csv', exp, condition, type))
  
  # cat('cleaning data...\n')
  # clean data a bit:
  df <- removeOutliers(df, depvar=depvar)
  df <- baseline(df, depvar=depvar)
  
  # cat('get washout data only...\n')
  df <- getWashout(df)
  
  # cat('sign of dependent variable...\n')
  df$depvar <- df[,depvar]
  if (depvar %in% c('reachdeviation_deg', 'aiming')) { # soooo... always? oh... this goes the other direction than localization/proprioception... whatever, yes: always
    df$depvar <- -1 * df$depvar
  }
  # df <- df[which(df$phase == 'rotation' & df$trialno < 77),]
  
  # cat('sampling participants...\n')
  participants <- unique(df$participant)
  BSparticipants <- sample(participants, size=iterations*length(participants), replace=TRUE)
  BSparticipants <- matrix(BSparticipants, nrow=iterations)
  
  # data <- list()
  # for (participant in participants) {
  #   data[[participant]] <- df[which(df$participant == participant),]
  # }
  
  if (is.null(clust)) {
    shutdownCluster <- TRUE
    ncores   <- parallel::detectCores()
    clust <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  } else {
    shutdownCluster <- FALSE
  }
  
  prepend <- c()
  # if (type %in% c('nocursors')) {
  #   prepend <- c(0)
  # }
  
  # cat('handing of work to cluster...\n')
  
  a <- parApply(cl = clust,
                X = BSparticipants,
                MARGIN = 1,
                FUN = fitExp,
                df = df,
                prepend = prepend,
                mode = 'washout')
  
  outdf <- as.data.frame(t(a))
  
  outfilename <- sprintf('data/exp%d/%s_%s_washout_exp-fits.csv',exp,condition,type)
  
  write.csv(outdf, outfilename, row.names=FALSE)
  
  if (shutdownCluster) {
    stopCluster(clust)
  }
  
}


# group average and individual fits -----

groupAvgFits <- function(conditions=NULL) {
  
  # first make a cluster:
  ncores <- parallel::detectCores()
  clust  <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  
  # get some meta info:
  info       <- groupInfo()
  
  if (is.null(conditions)) { conditions <- info$condition }
  
  for (condition in conditions) {
    print(condition)
    condition_idx <- which(info$condition == condition)
    
    exp <- info$exp[condition_idx]
    conditiontypes <- c('reaches', 'nocursors')
    if (condition == 'aiming') {conditiontypes <- c(conditiontypes, 'aiming')}
    
    # outputs stored here:
    cdf <- NA
    
    for (type in conditiontypes) {
      df <- read.csv(sprintf('data/exp%d/%s_%s.csv',exp,condition,type))
      
      depvar <- list('reaches'='reachdeviation_deg',
                     'nocursors'='reachdeviation_deg',
                     'aiming'='aimingdeviation_deg')[[type]]
      
      prepend <- c()
      if (type == 'nocursors') {prepend <- c(0)}
      
      df <- removeOutliers(df, depvar=depvar)
      df <- baseline(df, depvar=depvar)
      
      df$depvar <- -1 * df[,depvar]
      df$type <- type
      
      # first the learning/adaptation timecourses
      ldf <- df[which(df$phase == 'rotation' & df$trialno < 77),]
      
      pps <- list()
      pps[['all']] <- unique(ldf$participant)
      for (pp in unique(ldf$participant)) {
        pps[[pp]] <- pp
      }
      
      # print(pps)
      
      a <- parLapply(cl = clust,
                    X = pps,
                    # MARGIN = 1,
                    fun = fitExp,
                    df = ldf,
                    prepend = prepend,
                    mode = 'learning')
      
      # print(a)
      
      participant <- c()
      lambda <- c()
      N0 <- c()
      for (idx in names(a)) {
        participant <- c(participant, idx)
        lambda <- c(lambda, a[[idx]]['lambda'])
        N0 <- c(N0, a[[idx]]['N0'])
      }
      
      adf <- data.frame(participant, lambda, N0)
      adf$phase <- 'learning'
      adf$trialtype <- type
      
      if (is.data.frame(cdf)) {
        cdf <- rbind(cdf, adf)
      } else {
        cdf <- adf
      }
      
      if (exp == 1) {
        
        # also do washout
        wdf <- getWashout(df)

        # no baseline (or steady state / asymptote) appended for no-cursors
        prepend <- c()
        
        a <- parLapply(cl = clust,
                       X = pps,
                       fun = fitExp,
                       df = wdf,
                       prepend = prepend,
                       mode = 'washout')
        
        participant <- c()
        lambda <- c()
        N0 <- c()
        for (idx in names(a)) {
          participant <- c(participant, idx)
          lambda <- c(lambda, a[[idx]]['lambda'])
          N0 <- c(N0, a[[idx]]['N0'])
        }
        
        adf <- data.frame(participant, lambda, N0)
        adf$phase <- 'washout'
        adf$trialtype <- type
        
        cdf <- rbind(cdf, adf)
        
      }
      
    }
    
    write.csv(x         = cdf, 
              file      = sprintf('data/exp%d/%s_individual_exp-fits.csv',exp,condition), 
              row.names = FALSE,
              quote     = TRUE)
    
  }
  
  stopCluster(clust)
  
} 
