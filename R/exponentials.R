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
  
  close_cluster <- FALSE
  if (is.null(clust)) {
    ncores   <- parallel::detectCores()
    clust <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
    close_cluster <- TRUE
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
  
  if (close_cluster) {stopCluster(clust)}
  
  
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
      bootstrapExponentialLearning(condition=condition, 
                                   type=type,
                                   iterations=iterations,
                                   clust=clust, 
                                   asymptoteRange=c(0,rotation+5))
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

behaviorDescriptors <- function(timecoursemode='relative') {
  
  demographics <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  demographics <- demographics[which(demographics$learner == TRUE),]
  
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
  aiming_extent <- c()
  aiming_RofC <- c()
  aiming_asymptote <- c()
  
  for (rown in c(1:dim(df)[1])) {
    
    exp <- df$exp[rown]
    mode <- df$mode[rown]
    condition <- df$condition[rown]
    
    if (condition == 'control') {
      condem <- demographics[which(demographics$condition_label %in% c('45deg_distance', 'control')),]
    } else {
      condem <- demographics[which(demographics$condition_label == condition),]
    }
    N <- dim(condem)[1]
    
    rowlabels <- c(rowlabels, sprintf('%s %s (N=%d)', info$label[which(info$condition == condition)], list('learning'='', 'washout'='(washout)')[[mode]], N))
    
    if (mode == 'washout') {
      aiming_extent <- c(aiming_extent, '')
    } else {
      aiming_extent <- c(aiming_extent, getAimingAsymptote(condition=condition))
    }
    
    for (trialtype in c('reaches','nocursors','aiming')) {
      
      if (trialtype == 'aiming' & exp < 4) {
        aiming_RofC <- c(aiming_RofC, '')
        aiming_asymptote <- c(aiming_asymptote, '')
      } else {
        
        descriptors <- getDescriptors(exp=exp,
                                      condition=condition,
                                      mode=mode,
                                      trialtype=trialtype,
                                      centralvalue = 'group',
                                      timecoursemode=timecoursemode,
                                      othermode='central')
        
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
  
  outdf <- data.frame("reaches_RofC"= reaches_RofC,
                      "reaches_asymptote"=reaches_asymptote,
                      "no_cursors_RofC"=nocursors_RofC,
                      "no_cursors_asymptote"=nocursors_asymptote,
                      "re_aiming_extent"=aiming_extent,
                      "re_aiming_RofC"=aiming_RofC,
                      "re_aiming_asymptote"=aiming_asymptote)
  row.names(outdf) <- rowlabels
  
  return(outdf)
  
}

getDescriptors <- function(exp, condition, mode, trialtype, centralvalue = 'group', ratesas = 'perc', timecoursemode='absolute', othermode='central') {
  
  idf <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv',exp,condition), stringsAsFactors = FALSE)
  lambda_all <- idf$lambda[which(idf$participant == 'all' & idf$phase == mode & idf$trialtype == trialtype)]
  N0_all     <- idf$N0[    which(idf$participant == 'all' & idf$phase == mode & idf$trialtype == trialtype)]
  
  df <- read.csv(sprintf('data/exp%d/%s_%s%s_exp-fits.csv',exp,condition,trialtype,list('learning'='','washout'='_washout')[[mode]]), stringsAsFactors = FALSE)
  
  lambdaCI <- quantile(df$lambda, probs=c(0.025, 0.50, 0.975)) 
  N0CI     <- quantile(df$N0,     probs=c(0.025, 0.50, 0.975)) 
  
  if (timecoursemode == 'absolute') {
    otherlambda_all <- lambda_all
    otherlambdaCI   <- lambdaCI
    lambda_all <- lambda_all * N0_all
    lambdaCI <- lambdaCI * N0CI[2]
    dp <- 1
    un <- '°'
    oun <- list('perc'='%', 'prop'='')[[ratesas]]
    if (ratesas == 'perc') {
      otherlambda_all <- 100 * otherlambda_all
      otherlambdaCI <- 100 * otherlambdaCI
    }
  }
  if (timecoursemode == 'relative') {
    otherlambda_all <- lambda_all * N0_all
    otherlambdaCI <- lambdaCI * N0CI[2]
    oun <- '°'
    if (ratesas == 'perc') {
      lambda_all <- 100 * lambda_all
      lambdaCI <- 100 * lambdaCI
      dp <- 1
      un <- '%'
    }
    if (ratesas == 'prop') {
      dp <- 3
      un <- ''
    }
  }
  
  lambda <- ''
  if (centralvalue %in% c('group', 'both')) {
    lambda <- sprintf('%s%0.*f%s', lambda, dp, lambda_all, un)
  }
  if (centralvalue == 'both') {
    lambda <- sprintf('%s/', lambda)
  }
  if (centralvalue %in% c('perc50','both')) {
    lambda <- sprintf('%s%0.*f%s', lambda, dp, lambdaCI[2], un)
  }
  lambda <- sprintf('%s (%0.*f%s-%0.*f%s)',lambda, dp,lambdaCI[1],un, dp,lambdaCI[3],un )
  
  if (othermode %in% c('central','full')) {
    lambdaCentral <- list('group'=otherlambda_all , 'perc50'=otherlambdaCI[2])[[centralvalue]]
    lambda <- sprintf('%s %0.*f%s', lambda, dp,lambdaCentral,oun)
  }
  if (othermode %in% c('full','CI')) {
    lambda <- sprintf('%s (%0.*f%s-%0.*f%s)',lambda, dp,otherlambdaCI[1],oun, dp,otherlambdaCI[3],oun )
  }

  adp <- 1
  if (centralvalue == 'group') {
    N0 = sprintf('%0.*f° (%0.*f°-%0.*f°)',adp,N0_all, adp,N0CI[1], adp,N0CI[3])
  }
  if (centralvalue == 'perc50') {
    N0 = sprintf('%0.*f° (%0.*f°-%0.*f°)',adp,N0CI[2], adp,N0CI[1], adp,N0CI[3])
  }
  if (centralvalue == 'both') {
    N0 = sprintf('%0.*f°/%0.*f° (%0.*f°-%0.*f°)',adp,N0_all, adp,N0CI[2], adp,N0CI[1], adp,N0CI[3])
  }
  
  return(list('lambda' = lambda,
              'N0'     = N0))
  
}

getAimingAsymptote <- function(condition) {
  
  info <- groupInfo()
  exp <- info$exp[which(info$condition == condition)]
  
  aims <- read.csv(sprintf('data/exp%d/%s_aiming.csv', exp, condition), stringsAsFactors = FALSE)
  
  aims <- aggregate(aimingdeviation_deg ~ participant, data=aims[which(aims$trialno %in% c(77,81,85,89,93,97,101,105)),], FUN=mean, na.rm=TRUE)$aimingdeviation_deg * -1
  
  CI <- Reach::getConfidenceInterval(data=aims, method='b')
  
  return(sprintf('%0.1f° (%0.1f°-%0.1f°)', mean(aims), CI[1], CI[2]))
  
}

expTable <- function(exp) {
  
  info <- groupInfo()
  conditions <- expConditions(exp=exp) # do we even need this?
  
  expTable <- behaviorDescriptors()
  
  if (exp %in% c(1,2,3)) {
    expTable <- expTable[,c('reaches_RofC','reaches_asymptote','no_cursors_RofC','no_cursors_asymptote','re_aiming_extent')]
  }
  
  if (exp==1) {
    # trainingTable <- expTable[c("15° ","30° ","45° ","60° "),]
    trainingTable <- expTable[c(1,2,3,4),]
    # washoutTable  <- expTable[c("15° (washout)","30° (washout)","45° (washout)","60° (washout)"),]
    washoutTable <- expTable[c(5,6,7,8),]
    washoutTable  <- washoutTable[,c('reaches_asymptote','no_cursors_asymptote')]
    rownames(washoutTable) <- NULL
    expTable <- cbind(trainingTable, washoutTable)
  }
  
  if (exp == 2) {
    # expTable <- expTable[c('control','cursor-jump','terminal '),]
    expTable <- expTable[c(9,10,11),]
  }
  
  if (exp == 3) {
    # expTable <- expTable[c('control','terminal ','terminal -> delay', 'delay -> terminal'),]
    expTable <- expTable[c(12,13,11,9),]
  }
  
  if (exp == 4) {
    # expTable <- expTable[c('control','aiming '),]
    expTable <- expTable[c(14,9),]
  }
  
  expTable <- t(expTable)
  
  return(expTable)
  
  
}

# temp -----

redoReachBS <- function() {
  
  ncores <- parallel::detectCores()
  clust  <- parallel::makeCluster(max(c(1,floor(ncores*0.75))))
  
  cat('15deg')
  bootstrapExponentialLearning(condition='15deg_distance', type='reaches', iterations=5000, asymptoteRange = c(0,20), clust=clust)
  cat('30deg')
  bootstrapExponentialLearning(condition='30deg_distance', type='reaches', iterations=5000, asymptoteRange = c(0,35), clust=clust)
  cat('45deg')
  bootstrapExponentialLearning(condition='45deg_distance', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  cat('60deg')
  bootstrapExponentialLearning(condition='60deg_distance', type='reaches', iterations=5000, asymptoteRange = c(0,65), clust=clust)
  cat('control')
  bootstrapExponentialLearning(condition='control', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  cat('cursorjump')
  bootstrapExponentialLearning(condition='cursorjump', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  cat('delay-trial')
  bootstrapExponentialLearning(condition='delay-trial', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  cat('delay-FB')
  bootstrapExponentialLearning(condition='delay-FB', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  cat('aiming')
  bootstrapExponentialLearning(condition='aiming', type='reaches', iterations=5000, asymptoteRange = c(0,50), clust=clust)
  
  stopCluster(clust)
  
}