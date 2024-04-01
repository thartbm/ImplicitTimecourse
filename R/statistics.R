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