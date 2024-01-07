library('parallel')
source('R/utilities.R')
# advanced pre-processing -----


bootstrapExponentialLearning <- function(condition, type, iterations=1000, mode='learning', clust=NULL, depvar='reachdeviation_deg', asymptoteRange=NULL) {
  
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
                mode = mode,
                asymptoteRange = asymptoteRange)
  
  outdf <- as.data.frame(t(a))
  
  outfilename <- sprintf('data/exp%d/%s_%s_exp-fits.csv',exp,condition,type)

  write.csv(outdf, outfilename, row.names=FALSE)
  
}

fitExp <- function(participants, df, prepend=c(), mode='learning', asymptoteRange=NULL) {
  
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
  
  pars <- Reach::exponentialFit(signal, timepoints=length(signal), mode=mode, gridpoints=11, gridfits=10, asymptoteRange=asymptoteRange)
  
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
    rotation <- info$rotation[which(info$condition == condition)]
    for (type in types) {
      cat(sprintf('exponential fit (%d iterations) for: %s %s\n',iterations,condition,type))
      bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust, asymptoteRange=c(0,rotation+5))
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
  bootstrapExponentialLearning(condition, type, iterations=iterations, mode='learning', clust=clust, depvar='aimingdeviation_deg', asymptoteRange=c(0,50))

  # bootstrapExponentialLearning(condition, type, iterations=iterations, clust=clust)
  
  stopCluster(clust)
  
}

# washouts ------


bootstrapAllWashoutExpFits <- function(iterations=5000, conditions=NULL) {
  
  # cat('setting up cluster...\n')
  ncores <- parallel::detectCores()
  clust  <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))

  info       <- groupInfo()
  if (is.null(conditions)) {
    conditions <- info$condition[which(info$exp == 1)]
  }

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
    rotation <- info$rotation[condition_idx]
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
                    mode = 'learning',
                    asymptoteRange = c(0,rotation+5))
      
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

behaviorDescriptors <- function() {
  
  info <- groupInfo()
  
  exp <- c(1,1,1,1,1,1,1,1,2,2,2,3,3,4)
  mode <- c(rep(c('learning','washout'),each=4),rep('learning',6))
  condition <- c(rep(sprintf('%ddeg_distance',c(15,30,45,60)),2),'control','cursorjump','terminal','delay-FB','delay-trial','aiming')
  
  df <- data.frame(exp=exp,
                   mode=mode,
                   condition=condition)
  
  rowlabels <- c()
  
  reaches_RofC <- c()
  reaches_asymptote <- c()
  nocursors_RofC <- c()
  nocursors_asymptote <- c()
  aiming_RofC <- c()
  aiming_asymptote <- c()
  
  for (rown in c(1:dim(df)[1])) {
    
    exp <- df$exp[rown]
    mode <- df$mode[rown]
    condition <- df$condition[rown]
    
    rowlabels <- c(rowlabels, sprintf('%s %s', info$label[which(info$condition == condition)], list('learning'='', 'washout'='(washout)')[[mode]]))
    
    for (trialtype in c('reaches','nocursors','aiming')) {
      
      if (trialtype == 'aiming' & exp < 4) {
        aiming_RofC <- c(aiming_RofC, '')
        if (mode == 'washout') {
          aiming_asymptote <- c(aiming_asymptote, '')
        } else {
          aiming_asymptote <- c(aiming_asymptote, getAimingAsymptote(condition=condition))
        }
      } else {
        
        descriptors <- getDescriptors(exp=exp,
                                      condition=condition,
                                      mode=mode,
                                      trialtype=trialtype)
        
        if (trialtype == 'reaches') {
          reaches_RofC      <- c(reaches_RofC,      descriptors[['lambda']])
          reaches_asymptote <- c(reaches_asymptote, descriptors[['N0']])
        }
        if (trialtype == 'nocursors') {
          nocursors_RofC      <- c(nocursors_RofC,      descriptors[['lambda']])
          nocursors_asymptote <- c(nocursors_asymptote, descriptors[['N0']])
        }
        if (trialtype == 'aiming') {
          aiming_RofC      <- c(aiming_RofC,      descriptors[['lambda']])
          aiming_asymptote <- c(aiming_asymptote, descriptors[['N0']])
        }
        
      }
      
      
    }
    
  }
  
  outdf <- data.frame("reaches\nRofC"= reaches_RofC,
                      "reaches\nasymptote"=reaches_asymptote,
                      "no-cursors\nRofC"=nocursors_RofC,
                      "no-cursors\nasymptote"=nocursors_asymptote,
                      "re-aiming\nRofC"=aiming_RofC,
                      "re-aiming\nasymptote"=aiming_asymptote)
  row.names(outdf) <- rowlabels
  
  return(outdf)
}

getDescriptors <- function(exp, condition, mode, trialtype) {
  
  idf <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv',exp,condition), stringsAsFactors = FALSE)
  lambda_all <- idf$lambda[which(idf$participant == 'all' & idf$phase == mode & idf$trialtype == trialtype)]
  N0_all     <- idf$N0[    which(idf$participant == 'all' & idf$phase == mode & idf$trialtype == trialtype)]
  
  df <- read.csv(sprintf('data/exp%d/%s_%s%s_exp-fits.csv',exp,condition,trialtype,list('learning'='','washout'='_washout')[[mode]]), stringsAsFactors = FALSE)
  
  lambdaCI <- quantile(df$lambda, probs=c(0.025, 0.50, 0.975)) 
  N0CI     <- quantile(df$N0,     probs=c(0.025, 0.50, 0.975)) 
  
  return(list('lambda' = sprintf('%0.3f/%0.3f (%0.3f-%0.3f)',lambda_all, lambdaCI[2], lambdaCI[1],lambdaCI[3]) ,
              'N0' = sprintf('%0.2f°/%0.2f° (%0.2f°-%0.2f°)',N0_all,N0CI[2],N0CI[1],N0CI[3]) ) )
  
}

getAimingAsymptote <- function(condition) {
  
  info <- groupInfo()
  exp <- info$exp[which(info$condition == condition)]
  
  aims <- read.csv(sprintf('data/exp%d/%s_aiming.csv', exp, condition), stringsAsFactors = FALSE)
  
  aims <- aggregate(aimingdeviation_deg ~ participant, data=aims, FUN=mean, na.rm=TRUE)$aimingdeviation_deg * -1
  
  CI <- Reach::getConfidenceInterval(data=aims, method='b')
  
  return(sprintf('%0.2f° (%0.2f - %0.2f)', mean(aims), CI[1], CI[2]))
  
}