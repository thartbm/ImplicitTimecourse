library('BayesFactor')

# generic -----

getComparisons <- function(exp, mode) {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  if (exp == 1) {
    if (mode == 'learning') {
      comparisons <- list(c('15deg_distance', '30deg_distance'),
                          c('30deg_distance', '45deg_distance'),
                          c('45deg_distance', '60deg_distance'))
    }
    if (mode == 'washout') {
      comparisons <- list(c('15deg_distance', '30deg_distance'),
                          c('15deg_distance', '45deg_distance'),
                          c('15deg_distance', '60deg_distance'))
    }
  } else {
    comparisons <- list()
    if (exp==3) { conditions <- conditions[-which(conditions == 'control')]; reference <- 'terminal'} else {reference <- 'control'}
    for (condition in conditions) {
      # if (exp==3) {reference <- 'terminal'} else {reference <- 'control'}
      if (condition == reference) {
        next
      } else {
        comparisons[[length(comparisons)+1]]  <- c(reference,condition)
      }
    }
  }
  
  return(comparisons)
  
}


# reaches & no-cursors -----

differenceCI <- function(conditions, type, mode, lambda=TRUE, N0=TRUE) {
  
  info <- groupInfo()
  
  if (mode == 'washout') {
    df1 <- read.csv(sprintf('data/exp%d/%s_%s_washout_exp-fits.csv',info$exp[which(info$condition == conditions[1])], conditions[1], type), stringsAsFactors = FALSE)
    df2 <- read.csv(sprintf('data/exp%d/%s_%s_washout_exp-fits.csv',info$exp[which(info$condition == conditions[2])], conditions[2], type), stringsAsFactors = FALSE)
  } else if (type == 'aiming') {
    if ('control' %in% conditions) {
      df1 <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
      df2 <- read.csv('data/exp4/aiming_aiming_exp-fits.csv', stringsAsFactors = FALSE)
    } else {
      df1 <- read.csv('data/exp4/aiming_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
      df2 <- read.csv('data/exp4/aiming_aiming_exp-fits.csv', stringsAsFactors = FALSE)
    }
  } else {
    df1 <- read.csv(sprintf('data/exp%d/%s_%s_exp-fits.csv',info$exp[which(info$condition == conditions[1])], conditions[1], type), stringsAsFactors = FALSE)
    df2 <- read.csv(sprintf('data/exp%d/%s_%s_exp-fits.csv',info$exp[which(info$condition == conditions[2])], conditions[2], type), stringsAsFactors = FALSE)
  }
  
  if (lambda) {
    # ld <- df1$lambda - df2$lambda
    ld <- as.vector(matrix(df1$lambda, nrow=5000, ncol=5000, byrow=FALSE) - matrix(df2$lambda, nrow=5000, ncol=5000, byrow=TRUE))
    CI <- quantile(ld, probs=c(0.025, 0.975))
    cat('\n')
    # cat(sprintf('comparing %s and %s rate of change (lambda):\n',conditions[1],conditions[2]))
    if (all(CI > 0)) {cat(sprintf('%s has a LARGER rate of change in %s than %s\n', conditions[1], type, conditions[2]))}
    if (all(CI < 0)) {cat(sprintf('%s has a SMALLER rate of change in %s than %s\n', conditions[1], type, conditions[2]))}
    if (all(c((CI[1] < 0), (CI[2] > 0)))) {cat(sprintf('rate of change in %s of %s and %s are not different\n', type, conditions[1], conditions[2]))}
    print(CI)
  }
  if (N0) {
    # ld <- df1$N0 - df2$N0
    ld <- as.vector(matrix(df1$N0, nrow=5000, ncol=5000, byrow=FALSE) - matrix(df2$N0, nrow=5000, ncol=5000, byrow=TRUE))
    CI <- quantile(ld, probs=c(0.025, 0.975))
    cat('\n')
    # cat(sprintf('comparing %s and %s asymptote (N0):\n',conditions[1],conditions[2]))
    if (all(CI > 0)) {cat(sprintf('%s has a LARGER asymptote in %s than %s\n', conditions[1], type, conditions[2]))}
    if (all(CI < 0)) {cat(sprintf('%s has a SMALLER asymptote in %s than %s\n', conditions[1], type, conditions[2]))}
    if (all(c((CI[1] < 0), (CI[2] > 0)))) {cat(sprintf('asymptote in %s of %s and %s are not different\n', type, conditions[1], conditions[2]))}
    print(CI)
  }
  
}

expCIdiffs <- function(exp, type, mode='learning') {
  
  params <- c('lambda','N0')
  if (exp == 4 & type == 'aiming') {
    params <- c('lambda')
  }
  
  for (param in params) {
    
    if (param == 'lambda') {lambda=TRUE; N0=FALSE}
    if (param == 'N0') {lambda=FALSE; N0=TRUE}
    
    comparisons <- getComparisons(exp=exp, mode=mode)
    
    if (exp == 4 & type == 'aiming') {
      comparisons <- list(c('control','aiming'), c('aiming','aiming'))
    }
    
    for (comparison in comparisons) {
      
      differenceCI(conditions = comparison,
                   type=type,
                   mode=mode,
                   lambda=lambda,
                   N0=N0)
      
    }
    
  }
  
  
}

# subtraction -----

subtractionRofCtest <- function(lambda=TRUE, N0=TRUE) {
  
  compareWith <- c('reaches', 'aiming', 'nocursors')
  
  df1 <- read.csv('data/exp4/aiming_subtraction_exp-fits.csv', stringsAsFactors = FALSE)
  
  for (cv in compareWith) {
    
    df2 <- read.csv(sprintf('data/exp4/aiming_%s_exp-fits.csv', cv), stringsAsFactors = FALSE)
    
    if (lambda) {
      # ld <- df1$lambda - df2$lambda
      ld <- as.vector(matrix(df1$lambda, nrow=5000, ncol=5000, byrow=FALSE) - matrix(df2$lambda, nrow=5000, ncol=5000, byrow=TRUE))
      CI <- quantile(ld, probs=c(0.025, 0.975))
      cat('\n')
      # cat(sprintf('comparing %s and %s rate of change (lambda):\n',conditions[1],conditions[2]))
      if (all(CI > 0)) {cat(sprintf('subtraction has a LARGER rate of change than %s\n', cv))}
      if (all(CI < 0)) {cat(sprintf('subtraction has a SMALLER rate of change than %s\n', cv))}
      if (all(c((CI[1] < 0), (CI[2] > 0)))) {cat(sprintf('rate of change in subtraction and %s are not different\n', cv))}
      print(CI)
    }
      
    
  }
  
  compareWith <- c('nocursors')
  
  df1 <- read.csv('data/exp4/aiming_subtraction_exp-fits.csv', stringsAsFactors = FALSE)
  
  for (cv in compareWith) {
    
    df2 <- read.csv(sprintf('data/exp4/aiming_%s_exp-fits.csv', cv), stringsAsFactors = FALSE)
    
    if (N0) {
      # ld <- df1$lambda - df2$lambda
      ld <- as.vector(matrix(df1$N0, nrow=5000, ncol=5000, byrow=FALSE) - matrix(df2$N0, nrow=5000, ncol=5000, byrow=TRUE))
      CI <- quantile(ld, probs=c(0.025, 0.975))
      cat('\n')
      # cat(sprintf('comparing %s and %s rate of change (lambda):\n',conditions[1],conditions[2]))
      if (all(CI > 0)) {cat(sprintf('subtraction has a LARGER asymptote than %s\n', cv))}
      if (all(CI < 0)) {cat(sprintf('subtraction has a SMALLER asymptote than %s\n', cv))}
      if (all(c((CI[1] < 0), (CI[2] > 0)))) {cat(sprintf('asymptote in subtraction and %s are not different\n', cv))}
      print(CI)
    }
    
    
  }
  
}

subtractionRegressions <- function() {
  
  
  subtractiveData <- getAvgSubtractiveData()
  
  avgadapt <- mean(subtractiveData$adaptation, na.rm=TRUE)
  
  for (subset in c('all', 'aiming')){
    
    if (subset == 'all') {
      conditions <- unique(subtractiveData$condition)
      conditions <- conditions[conditions != 'aiming']
    }
    if (subset == 'aiming') {
      conditions <- c('aiming')
    }
    
    subData <- subtractiveData[which(subtractiveData$condition %in% conditions),]
    
    
    # lm with 95% CI:
    impl <- subData$implicit
    expl <- subData$explicit
    e2i <- lm(impl ~ expl)
    
    # print(summary(e2i))
    slope <- as.numeric(e2i$coefficients[2])
    slope_ci <- confint(e2i,parm='expl',level=0.95)
    
    cat(sprintf('linear prediction of implicit over explicit measure in %s\n', toupper(subset)))
    
    corr <- cor.test(x=expl, y=impl)
    
    cat(sprintf('r=%0.3f, p=%0.3f, t=%0.3f, df=%d\n',corr$estimate, corr$p.value, corr$statistic, length(impl)-2))
    
    cat(sprintf('slope: %0.3f CI: [%0.3f - %0.3f]\n\n', slope, slope_ci[1], slope_ci[2]))
    
    
  }
  
  
  for (subset in c('all', 'aiming')){
    
    if (subset == 'all') {
      conditions <- unique(subtractiveData$condition)
      conditions <- conditions[conditions != 'aiming']
    }
    if (subset == 'aiming') {
      conditions <- c('aiming')
    }
    
    subData <- subtractiveData[which(subtractiveData$condition %in% conditions),]
    
    # lm with 95% CI:
    impl <- subData$implicit
    pred <- subData$adaptation - subData$explicit
    e2i <- lm(impl ~ pred)
    
    slope <- as.numeric(e2i$coefficients[2])
    slope_ci <- confint(e2i,parm='pred',level=0.95)
    
    cat(sprintf('subtractive linear prediction of implicit in %s\n', toupper(subset)))
    
    corr <- cor.test(x=pred, y=impl)
    
    cat(sprintf('r=%0.3f, p=%0.3f, t=%0.3f, df=%d\n',corr$estimate, corr$p.value, corr$statistic, length(impl)-2))
    
    cat(sprintf('slope: %0.3f CI: [%0.3f - %0.3f]\n\n', slope, slope_ci[1], slope_ci[2]))
    
  }  
  
  
  
  
  
  
}

# old approach -----

getExpFitDF <- function(conditions, mode, type, timecoursemode='relative') {
  
  info <- groupInfo()
  
  df <- NA
  
  # make data frame:
  for (condition in conditions) {
    idx <- which(info$condition == condition)
    filename <- sprintf('data/exp%d/%s_individual_exp-fits.csv',info$exp[idx],condition)
    cdf <- read.csv(filename, stringsAsFactors = FALSE)
    cdf <- cdf[which(cdf$participant %notin% c('all')),]
    cdf <- cdf[which(cdf$phase==mode & cdf$trialtype==type),]
    cdf$condition <- condition
    if (is.data.frame(df)) {
      df <- rbind(df, cdf)
    } else {
      df <- cdf
    }
  }
  
  if (timecoursemode == 'absolute') {
    df$lambda <- df$lambda*df$N0
  }
  
  # set condition as factor:
  df$condition <- as.factor(df$condition)
  
  return(df)
  
}

expFitBayesianFtest <- function(exp, type, mode='learning', timecoursemode='relative') {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  df <- getExpFitDF(conditions = conditions, 
                    mode = mode, 
                    type = type, 
                    timecoursemode = timecoursemode)
  
  # do the bayesian anova's:
  cat(sprintf('\nexp %d [%s, %s]----> LAMBDA / RofC\n', exp, type, mode))
  lambdaAOV <- BayesFactor::anovaBF(lambda ~ condition, data = df)
  bf <- extractBF(lambdaAOV)[[1]]
  if (bf > 1000) {
    cat( sprintf( 'BF: %s\n', formatC(bf, format = "e", digits = 2) ) )
  } else {
    cat( sprintf('BF: %0.3f\n', bf) )
  }
  
  cat(sprintf('\nexp %d [%s, %s] ----> N0 / asymptote\n',exp, type, mode))
  N0AOV     <- BayesFactor::anovaBF(N0 ~ condition, data = df)
  bf <- extractBF(N0AOV)[[1]]
  if (bf > 1000) {
    cat( sprintf( 'BF: %s\n', formatC(bf, format = "e", digits = 2) ) )
  } else {
    cat( sprintf('BF: %0.3f\n', bf) )
  }
}

expFitGroupBayesianTtest <- function(exp,
                                     type, 
                                     mode='learning', 
                                     timecoursemode='relative',
                                     lambda=TRUE,
                                     N0=TRUE) {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  comparisons <- getComparisons(exp=exp, mode=mode)
  
  df <- getExpFitDF(conditions = conditions, 
                    mode = mode, 
                    type = type, 
                    timecoursemode = timecoursemode)
  
  # print(df)
  # print(comparisons)
  
  for (comparison in comparisons) {
    
    con1 <- comparison[1]
    con2 <- comparison[2]
    
    cat(sprintf('\ncomparing %s with %s\n', toupper(con1), toupper(con2)))
    
    if (lambda) {
      cat('RofC / lambda:\n')
      bttL <-  BayesFactor::ttestBF(df[which(df$condition == con1),'lambda'], 
                                    df[which(df$condition == con2),'lambda'])
      # print(bttL)
      # cat( sprintf('BF: %0.3f\n', extractBF(bttL)) )
      bf <- extractBF(bttL)[[1]]
      if (bf > 1000) {
        cat( sprintf( 'BF: %s\n', formatC(bf, format = "e", digits = 2) ) )
      } else {
        cat( sprintf('BF: %0.3f\n', bf) )
      }
      
    }
    if (N0) {
      cat('\nasymptote / N0:\n')
      bttN <- BayesFactor::ttestBF(df[which(df$condition == con1),'N0'], 
                                   df[which(df$condition == con2),'N0'])
      # print(bttN)
      # cat( sprintf('BF: %0.3f\n', extractBF(bttN)) )
      bf <- extractBF(bttN)[[1]]
      if (bf > 1000) {
        cat( sprintf( 'BF: %s\n', formatC(bf, format = "e", digits = 2) ) )
      } else {
        cat( sprintf('BF: %0.3f\n', bf) )
      }
      
    }
  }

}

# re-aiming responses -----

getAimingDF <- function(conditions) {
  
  info <- groupInfo()
  
  df <- NA
  
  for (condition in conditions) {
    
    # prep data:
    impdf <- getImpExpEst(condition=condition, type='nocursors')
    impdf$implicit <- impdf$depvar
    impdf <- impdf[,c('participant','implicit')]
    expdf <- getImpExpEst(condition=condition, type='aiming')
    expdf$explicit <- expdf$depvar
    expdf <- expdf[,c('participant','explicit')]
    cdf <- merge(impdf, expdf, by='participant')
    cdf$condition <- condition
    if (is.data.frame(df)) {
      df <- rbind(df, cdf)
    } else {
      df <- cdf
    }
  }
  
  return(df)
  
}

aimingBayesianFtest <- function(exp) {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  df <- getAimingDF(conditions = conditions)
  df$condition <- as.factor(df$condition)
                    
  cat(sprintf('\nexp %d: re-aiming F-test\n', exp))
  aimingAOV <- BayesFactor::anovaBF(explicit ~ condition, data = df)
  # print(aimingAOV)
  
  bf <- extractBF(aimingAOV)[[1]]
  if (bf > 1000) {
    cat( sprintf( 'BF: %s\n\n', formatC(bf, format = "e", digits = 2) ) )
  } else {
    cat( sprintf('BF: %0.3f\n\n', bf) )
  }
  
  
}

aimingZero <- function() {
  
  info <- groupInfo()
  conditions <- expConditions(exp=1)
  
  df <- getAimingDF(conditions = conditions)
  df$condition <- as.factor(df$condition)
  
  for (condition in unique(df$condition)) {
    cat(sprintf('comparing re-aiming in %s to zero:\n', toupper(condition)))
    cdat <- df$explicit[which(df$condition == condition)]
    CI <- Reach::getConfidenceInterval(data=cdat, method='b')
    cat(sprintf('mean: %0.2f, 95CI: %0.2f - %0.2f\n', mean(cdat, na.rm=TRUE), CI[1], CI[2]))
    zeroTtest <- BayesFactor::ttestBF(x = cdat, mu=0)
    # cat( sprintf('BF: %0.3f\n\n', extractBF(zeroTtest)$bf ) )
    # print(zeroTtest['bayesFactor'])
    bf <- extractBF(zeroTtest)[[1]]
    if (bf > 1000) {
      cat( sprintf( 'BF: %s\n\n', formatC(bf, format = "e", digits = 2) ) )
    } else {
      cat( sprintf('BF: %0.3f\n\n', bf) )
    }
  }
  
}

aimingGroupTtest <- function(exp) {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  comparisons <- getComparisons(exp=exp, mode='learning')
  
  df <- getAimingDF(conditions)
  
  for (comparison in comparisons) {
    
    con1 <- comparison[1]
    con2 <- comparison[2]
    
    cat(sprintf('\ncomparing %s with %s\n', toupper(con1), toupper(con2)))
    
    bttRA <-  BayesFactor::ttestBF(df[which(df$condition == con1),'explicit'],
                                   df[which(df$condition == con2),'explicit'])
    
    bf <- extractBF(bttRA)[[1]]
    if (bf > 1000) {
      cat( sprintf( 'BF: %s\n', formatC(bf, format = "e", digits = 2) ) )
    } else {
      cat( sprintf('BF: %0.3f\n', bf) )
    }
    
  }
  
}

# implicit over explicit -----

testGroupLinearAdditivity <- function(exp) {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  df <- getAimingDF(conditions)
  
  for (condition in conditions) {
    
    expl <- df$explicit[which(df$condition == condition)]
    impl <- df$implicit[which(df$condition == condition)]
    
    ei.lm <- lm(impl ~ expl)
    
    slope <- as.numeric(ei.lm$coefficients[2])
    CI <- confint(ei.lm,parm='expl',level=0.95)
    
    cat(sprintf('linear prediction of implicit over explicit measure in %s\n', toupper(condition)))
    corr <- cor.test(x=expl, y=impl)
    
    cat(sprintf('r=%0.3f, p=%0.3f, t=%0.3f, df=%d\n',corr$estimate, corr$p.value, corr$statistic, length(impl)-2))
    
    cat(sprintf('slope: %0.3f CI: [%0.3f - %0.3f]\n\n', slope, CI[1], CI[2]))
    
    
  }
  
}


# step and exponential function fits -----


stepFunction <- function(par, trials) {
  
  if (length(trials) == 1) {
    trials <- c(0:(trials-1))
  }
  
  predictions <- rep(0, length(trials))
  
  predictions[which(trials >= par['t'])] <- par['s']
  
  # print(predictions)
  
  return(predictions)
  
}

stepMSE <- function(par, data) {
  
  # trials <- unique(data$trial)
  # 
  # predictions <- stepFunction(par    = par,
  #                             trials = trials)
  # 
  # errors <- data$deviation - predictions
  # 
  # MSE <- mean(errors^2, na.rm=TRUE)
  # 
  # return(MSE)
  
  return(mean((stepFunction(par = par, trials = unique(data$trial)) - data$deviation)^2, na.rm=TRUE))
  
}

require('Reach')

stepFit <- function(data, gridpoints=11, gridfits=10) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  stepsizes <- (parvals * diff(range(data$deviation, na.rm=TRUE))) - min(data$deviation, na.rm=TRUE)
  # stepsizes <- (parvals * 55) - 5
  
  steptimes <- parvals * max(data$trial, na.rm=TRUE)
  # print(max(data$trial, na.rm=TRUE))
  # steptimes <- round(parvals * 55)
  
  searchgrid <- expand.grid('t' = steptimes,
                            's' = stepsizes)
  
  MSE <- apply(searchgrid, FUN=stepMSE, MARGIN=c(1), data=data)
  # print(MSE)
  
  # print(data$deviation)
  
  lo <- c(0,min(data$deviation, na.rm=TRUE))
  hi <- c(max(data$trial, na.rm=TRUE), max(data$deviation, na.rm=TRUE))
  
  # lo <- c(0, -20)
  # hi <- c(55, 85)
  
  # print(lo)
  # print(hi)
  
  # print(data.frame(searchgrid[order(MSE)[1:gridfits],]))
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=stepMSE,
                            # method     = 'L-BFGS-B',
                            # lower      = lo,
                            # upper      = hi,
                            data       = data) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  winpar <- unlist(win[1:2])
  
  # return the best parameters:
  return(winpar)
  
}


allStepExpoFits <- function() {
  
  explicit <- read.csv('data/exp4/aiming_aiming.csv', stringsAsFactors = F)
  explicit$aimingdeviation_deg <- -1*explicit$aimingdeviation_deg
  implicit <- read.csv('data/exp4/aiming_nocursors.csv', stringsAsFactors = F)
  implicit$reachdeviation_deg <- -1*implicit$reachdeviation_deg
  adaptation <- read.csv('data/exp4/aiming_reaches.csv', stringsAsFactors = F)
  adaptation$reachdeviation_deg <- -1*adaptation$reachdeviation_deg
  
  # trials: pre = 13-20, post = 21-52
  
  # in the CONTINUOUS aiming group we can use all of the trials, since they ALL have aiming  
  expl <- explicit[   which(explicit$trialno   %in% c(21:120)), ]
  impl <- implicit[   which(implicit$trialno   %in% c(20:120)), ]
  adpt <- adaptation[ which(adaptation$trialno %in% c(21:120)), ]
  
  expl$trialno <- expl$trialno - 21
  impl$trialno <- impl$trialno - 20 # what to subtract? 
  adpt$trialno <- adpt$trialno - 21
  
  expl <- expl[,c('participant','trialno','aimingdeviation_deg')]
  impl <- impl[,c('participant','trialno','reachdeviation_deg')]
  adpt <- adpt[,c('participant','trialno','reachdeviation_deg')]
  
  names(expl) <- c('participant','trial','deviation')
  names(impl) <- c('participant','trial','deviation')
  names(adpt) <- c('participant','trial','deviation')
  
  participants <- unique(expl$participant)
  
  # expl.exp.MSE <- c()
  # expl.exp.l <- c()
  # expl.exp.a <- c()
  # expl.step.MSE <- c()
  # expl.step.t <- c()
  # expl.step.s <- c()
  # 
  # impl.exp.MSE <- c()
  # impl.exp.l <- c()
  # impl.exp.a <- c()
  # impl.step.MSE <- c()
  # impl.step.t <- c()
  # impl.step.s <- c()
  # 
  # adpt.exp.MSE <- c()
  # adpt.exp.l <- c()
  # adpt.exp.a <- c()
  # adpt.step.MSE <- c()
  # adpt.step.t <- c()
  # adpt.step.s <- c()
  
  participant <- c()
  process <- c()
  
  exp.MSE <- c()
  exp.l <- c()
  exp.a <- c()
  step.MSE <- c()
  step.t <- c()
  step.s <- c()
  
  for (pp_no in c(1:length(participants))) {
    
    cat(sprintf('working on participant %d / %d\n',pp_no,length(participants)))
    
    ppid <- participants[pp_no]
    
    pex <- expl[which(expl$participant == ppid),]
    pim <- impl[which(impl$participant == ppid),]
    pad <- adpt[which(adpt$participant == ppid),]
    
    
    for (datatype in c('ex','im','ad')) {
      
      df <- list('ex'=pex, 'im'=pim, 'ad'=pad)[[datatype]]
      
      exppar <- Reach::exponentialFit(signal = df$deviation)
      # print(exppar)
      MSEexp <- Reach::exponentialMSE(par    = exppar,
                                      signal = df$deviation)
      
      steppar <- stepFit(data = df)
      # print(steppar)
      MSEstep <- stepMSE(par=steppar, data=df)
      
      # if (datatype == 'ex') {
      #   expl.exp.MSE  <- c(expl.exp.MSE,  expMSE)
      #   expl.exp.l    <- c(expl.exp.l,    exppar['lambda'])
      #   expl.exp.a    <- c(expl.exp.a,    exppar['N0'])
      #   expl.step.MSE <- c(expl.step.MSE, step.MSE)
      #   expl.step.t   <- c(expl.step.t,   steppar['t'])
      #   expl.step.s   <- c(expl.step.s,   steppar['s'])
      # }
      # 
      # if (datatype == 'im') {
      #   impl.exp.MSE  <- c(impl.exp.MSE,  expMSE)
      #   impl.exp.l    <- c(impl.exp.l,    exppar['lambda'])
      #   impl.exp.a    <- c(impl.exp.a,    exppar['N0'])
      #   impl.step.MSE <- c(impl.step.MSE, step.MSE)
      #   impl.step.t   <- c(impl.step.t,   steppar['t'])
      #   impl.step.s   <- c(impl.step.s,   steppar['s'])
      # }
      # 
      # if (datatype == 'ad') {
      #   adpt.exp.MSE  <- c(adpt.exp.MSE,  expMSE)
      #   adpt.exp.l    <- c(adpt.exp.l,    exppar['lambda'])
      #   adpt.exp.a    <- c(adpt.exp.a,    exppar['N0'])
      #   adpt.step.MSE <- c(adpt.step.MSE, step.MSE)
      #   adpt.step.t   <- c(adpt.step.t,   steppar['t'])
      #   adpt.step.s   <- c(adpt.step.s,   steppar['s'])
      # }
      
      participant <- c(participant, ppid)
      
      process <- c(process, list('ex'='explicit', 'im'='implicit', 'ad'='adaptation')[[datatype]])
      
      exp.MSE <- c(exp.MSE, MSEexp)
      exp.l   <- c(exp.l, exppar[['lambda']])
      exp.a   <- c(exp.a, exppar[['N0']])
      
      step.MSE <- c(step.MSE, MSEstep)
      step.t   <- c(step.t, steppar[['t']])
      step.s   <- c(step.s, steppar[['s']])
      
    }
    
  }
  
  # print(length(participant))
  # print(length(process))
  # print(length(exp.MSE))
  # print(length(exp.l))
  # print(length(exp.a))
  # print(length(step.MSE))
  # print(length(step.t))
  # print(length(step.s))
  
  # df <- data.frame( participant = participants,
  #                   expl.exp.MSE,
  #                   expl.exp.l,
  #                   expl.exp.a,
  #                   expl.step.MSE,
  #                   expl.step.t,
  #                   expl.step.s,
  #                   impl.exp.MSE,
  #                   impl.exp.l,
  #                   impl.exp.a,
  #                   impl.step.MSE,
  #                   impl.step.t,
  #                   impl.step.s,
  #                   adpt.exp.MSE,
  #                   adpt.exp.l,
  #                   adpt.exp.a,
  #                   adpt.step.MSE,
  #                   adpt.step.t,
  #                   adpt.step.s                 )
  # 
  # print(df)
  df <- data.frame( participant,
                    process,
                    exp.MSE,
                    exp.l,
                    exp.a,
                    step.MSE,
                    step.t,
                    step.s  )
  
  print(df)
  
  write.csv( df,
             file = 'data/exp4/aiming_step-exp-fits.csv',
             quote = TRUE,
             row.names = FALSE)
  
}

compareFunctionFits <- function() {
  
  df <- read.csv('data/exp4/aiming_step-exp-fits.csv', stringsAsFactors = FALSE)
  
  for (process in c('adaptation', 'implicit', 'explicit')) {
    
    sdf <- df[which(df$process == process),]
    MSEstep <- sdf$step.MSE
    MSEexp  <- sdf$exp.MSE
    
    
    md <- mean(MSEstep - MSEexp)
    cat(sprintf('%s (step - exp) RMSEs: %0.6f\n', process, md))
    
    if (md > 0) {
      cat('exponential RMSE is smallest... ')
    } else {
      cat('step function RMSE is smallest... ')
    }
    
    cat('is it significant?\n')
    tt <- t.test(MSEstep, MSEexp, paired=TRUE)
    print(tt)
    
    btt <- BayesFactor::ttestBF(MSEstep, MSEexp, paired=TRUE)
    print(btt)
    
  }
  
  
}