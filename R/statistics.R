library('BayesFactor')

expFitBF <- function(exp, type, mode='learning', timecoursemode='relative') {
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  
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
  
  # do the bayesian anova's:
  cat(sprintf('exp %d [%s / %s]----> LAMBDA / RofC\n', exp, type, mode))
  lambdaAOV <- BayesFactor::anovaBF(lambda ~ condition, data = df)
  print(lambdaAOV)
  cat(sprintf('exp %d [%s / %s] ----> N0 / asymptote\n',exp, type, mode))
  N0AOV     <- BayesFactor::anovaBF(N0 ~ condition, data = df)
  print(N0AOV)
  
  # return(df)
}