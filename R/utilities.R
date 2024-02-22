library('Reach')


`%notin%` <- Negate(`%in%`)


groupInfo <- function() {
  
  exp <- c(1,1,1,1,2,2,2,3,3,4)
  # lty <- c(1,1,1,1,1,1,1,1,1,2)
  condition <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance',
                 'control','cursorjump','terminal',
                 'delay-trial', 'delay-FB',
                 'aiming')
  color <- c('darkred', '#ae0a23', '#e51636', 'salmon',
             '#e51636', 'orange', 'darkturquoise', # 'deepskyblue'
             'blue','darkblue',  
             'purple')
  
  # label <- c('15°', '30°', '45°', '60°', 'control', 'cursor-jump', 'terminal', 'aiming', 'delay->terminal', 'terminal->delay')
  label <- c('15°', '30°', '45°', '60°', 'control', 'cursor-jump', 'terminal', 'terminal -> delay', 'delay -> terminal', 'aiming')
  
  rot <- c(15,30,45,60,45,45,45,45,45,45)
  
  # expression(y %->% x)
  # &#8594;
  
  return( data.frame( exp = exp,
                      condition = condition,
                      color = color,
                      label = label,
                      rotation = rot
                      # lty = lty
                      ) )
  
}

expConditions <- function(exp) {
  
  if (exp == 1) { conditions <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance') }
  if (exp == 2) { conditions <- c('control','cursorjump','terminal') }
  if (exp == 3) { conditions <- c('control','terminal','delay-trial', 'delay-FB')}
  if (exp == 4) { conditions <- c('control','aiming') }

  return(conditions)
  
}

convertLegend <- function(legends) {
  
  for (idx in c(1:length(legends))) {
    if (legends[idx] == 'delay -> terminal') {
      legends[idx] <- expression(delay %->% terminal)
    }
    if (legends[idx] == 'terminal -> delay') {
      legends[idx] <- expression(terminal %->% delay)
    }
  }
  
  return(legends)
  
}

# plotting -----

addLearningCurves <- function(type, conditions=NULL, exp=NULL, phases=c('baseline','rotation','washout'),FUN=mean) {
  
  if (type %notin% c('reaches','nocursors','aiming')) {
    return()
  }
  
  depvar <- list('aiming'='aimingdeviation_deg',
                 'reaches'='reachdeviation_deg',
                 'nocursors'='reachdeviation_deg')[[type]]
  
  info <- groupInfo()
  
  if (is.null(conditions)) {
    conditions <- info$condition[which(info$exp == exp)]
  }
  
  CIs <- list()
  avg <- list()
  color <- list()
  ltys  <- list()
  label <- list()
  
  for (condition in conditions) {
    
    if (condition %notin% info$condition) {
      return()
    }
    
    idx <- which(info$condition == condition)
    exp <- info$exp[idx]
    if (type == 'aiming') {
      # color[[condition]] <- 'magenta'
      color[[condition]] <- info$color[idx]
      ltys[[condition]] <- 2
    } else {
      color[[condition]] <- info$color[idx]
      ltys[[condition]] <- 1
    }
    label[[condition]] <- info$label[idx]
    
    df <- read.csv(sprintf('data/exp%d/%s_%s.csv', exp, condition, type))
    
    df <- removeOutliers(df, depvar=depvar)
    df <- baseline(df, depvar=depvar)
    
    df$depvar <- df[,depvar]
    
    df$depvar <- -1 * df$depvar
    
    avg[[condition]] <- aggregate(depvar ~ trialno, data=df, FUN=FUN, na.rm=TRUE)
    CIs[[condition]] <- aggregate(depvar ~ trialno, data=df, FUN=function(d) Reach::getConfidenceInterval(d, method='b', FUN=FUN))
    
  }
  
  # draw 95% confidence intervals
  for (condition in conditions) {
    cc <- Reach::colorAlpha( col=color[[condition]], alpha=22 )
    CI <- CIs[[condition]]
    polygon(
      x = c(CI$trialno, rev(CI$trialno)),
      y = c(CI$depvar[,1], rev(CI$depvar[,2])),
      border=NA,
      col=cc
    )
  }
  
  # draw median lines
  for (condition in conditions) {
    cc <- color[[condition]]
    central <- avg[[condition]]
    lty <- ltys[[condition]]
    lines(central$trialno, central$depvar, col=cc, lty=lty)
  }
  
  # make a legend
  cl <- c()
  ll <- c()
  for (condition in conditions) {
    cl <- c(cl, color[[condition]])
    ll <- c(ll, label[[condition]])
  }
  return(data.frame(color=cl,
                    label=ll))
  
}

addAimingTrials <- function(mrot) {
  
  trials <- c( 77, 81, 85, 89, 93, 97, 101, 105 )
  
  for (trial in trials) {
    lines(x=rep(trial,2),y=c(0,mrot),col='gray')
  }
  
  arrows(x0=trials,y0=rep(-5,length(trials)),
         x1=trials,y1=rep(-1,length(trials)),
         length=0.01,
         col='black')
  
}

addDensities <- function(conditions, type, flipXY=FALSE, viewscale=c(1,1), offset=c(0,0), from=-15, to=60, n=1001) {
  
  info <- groupInfo()
  
  for (condition in conditions) {
    
    plotdf <- getImpExpEst(condition=condition, type=type)
    
    dens <- density(x=plotdf$depvar,
                    bw=5,
                    from=from,
                    to=to,
                    n=n)
    
    if (flipXY) {
      Y <- dens$x
      X <- (dens$y/max(dens$y))
      # X <- 1-(dens$y * 14)
    } else {
      X <- dens$x
      Y <- (dens$y/max(dens$y))
      # Y <- 1-(dens$y * 14)
    }
    
    color <- info$color[which(info$condition == condition)]
    
    lines(x=(X*viewscale[1])+offset[1],
          y=(Y*viewscale[2])+offset[2],
          col=color)
    
  }
  
}

addImpExpScatters <- function(conditions) {
  
  info <- groupInfo()
  
  for (condition in conditions) {
    
    # prep data:
    impdf <- getImpExpEst(condition=condition, type='nocursors')
    impdf$implicit <- impdf$depvar
    impdf <- impdf[,c('participant','implicit')]
    expdf <- getImpExpEst(condition=condition, type='aiming')
    expdf$explicit <- expdf$depvar
    expdf <- expdf[,c('participant','explicit')]
    df <- merge(impdf, expdf, by='participant')
    
    # prep plotting:
    color  <- info$color[which(info$condition == condition)]
    acolor <- Reach::colorAlpha(col=color, alpha=44)
    
    # actual scatter plot:
    points(df$explicit, df$implicit, col=acolor)
    
    # lm with 95% CI:
    impl <- df$implicit
    expl <- df$explicit
    e2i <- lm(impl ~ expl)
    
    at <- range(df$explicit)
    
    coef <- e2i$coefficients
    lines(at, coef[1]+(at*coef[2]), col=color)
    
    ci <- predict( e2i,
                   newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=acolor,border=NA)
    
  }
  
}

addAdaptationTimecourses <- function(type, conditions, timecoursemode='relative') {
  
  info <- groupInfo()
  
  for (condition in conditions) {
    
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    lty <- 1
    if (type == 'aiming') {
      # color <- 'magenta'
      lty <- 2
    }
    
    # read the exp-fits:
    df <- read.csv(file = sprintf('data/exp%d/%s_%s_exp-fits.csv',exp,condition,type), stringsAsFactors = FALSE)
    
    
    
    lambdaCI <- quantile(df$lambda, probs=c(0.025, 0.50, 0.975))
    N0CI     <- quantile(df$N0, probs=c(0.025, 0.50, 0.975))
    
    PropAsym <- N0CI[1]/N0CI[2]
    names(PropAsym) <- NULL
    
    tau <- log(1-PropAsym)/log(1-lambdaCI)
    names(tau) <- NULL
    
    if (timecoursemode == 'relative') {
      scale <- 1/PropAsym
    }
    if (timecoursemode == 'absolute') {
      scale <- N0CI[2]
    }
    
    X <- c()
    Y <- c()

    thispar <- c(lambdaCI[1], 1)
    names(thispar) <- c('lambda', 'N0')
    X <- c(X, (seq(0,1,length.out=101)^2)*tau[1])
    Y <- c(Y, Reach::exponentialModel(par=thispar, timepoints=(seq(0,1,length.out=101)^2)*tau[1])$output * scale)
    
    thispar <- c(lambdaCI[3], 1)
    names(thispar) <- c('lambda', 'N0')
    X <- c(X, rev( (seq(0,1,length.out=101)^2)*tau[3]) )
    Y <- c(Y, rev( Reach::exponentialModel(par=thispar, timepoints=(seq(0,1,length.out=101)^2)*tau[3])$output * scale) )
    
    if (type == 'reaches') {
      X <- X+1
    }
    
    polygon( x=X,
             y=Y,
             col=colorAlpha(col=color,alpha=11),
             border=NA)
    
    thispar <- c(lambdaCI[2], 1)
    names(thispar) <- c('lambda', 'N0')
    lX <- (seq(0,1,length.out=101)^2)*tau[2]
    lY <- Reach::exponentialModel(par=thispar, timepoints=(seq(0,1,length.out=101)^2)*tau[2])$output * scale
    
    if (type=='reaches') {
      lX <- lX + 1
    }
    
    lines(x   = lX,
          y   = lY,
          col = color,
          lty = lty)

  }
  
  
}


addWashoutTimecourses <- function(type, conditions) {
  
  info <- groupInfo()
  
  for (condition in conditions) {
    
    exp   <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_%s_washout_exp-fits.csv',exp,condition,type), stringsAsFactors = FALSE)
    
    lambdaCI <- quantile(df$lambda, probs=c(0.025, 0.50, 0.975))
    N0CI     <- quantile(df$N0, probs=c(0.025, 0.50, 0.975))
    # print(lambdaCI)
    # print(N0CI)
    
    X <- c()
    Y <- c()
    
    x_points <- (seq(0,1,length.out=101)^2)*23
    
    thispar <- c(lambdaCI[2], N0CI[1])
    names(thispar) <- c('lambda', 'N0')
    X <- c(X, x_points)
    Y <- c(Y, Reach::exponentialModel(par=thispar, timepoints=x_points, mode='washout')$output )
    
    thispar <- c(lambdaCI[2], N0CI[3])
    names(thispar) <- c('lambda', 'N0')
    X <- c(X, rev( x_points ) )
    Y <- c(Y, rev( Reach::exponentialModel(par=thispar, timepoints=x_points, mode='washout')$output ) )
    
    polygon( x=X+1,
             y=Y,
             col=colorAlpha(col=color,alpha=11),
             border=NA)
    
    thispar <- c(lambdaCI[2], N0CI[2])
    names(thispar) <- c('lambda', 'N0')
    lX <- x_points
    lY <- Reach::exponentialModel(par=thispar, timepoints=x_points, mode='washout')$output
    
    lines(x=lX+1,
          y=lY,
          col=color)
    
    
  }
  
}

addAimingResponses <- function(conditions, FUN=mean) {
  
  info <- groupInfo()
  
  CIs <- list()
  avg <- list()
  color <- list()
  
  for (condition in conditions) {
    
    idx <- which(info$condition == condition)
    
    exp <- info$exp[idx]
    df <- read.csv(sprintf('data/exp%d/%s_aiming.csv', exp, condition), stringsAsFactors = FALSE)
    # print(str(df))
    df$depvar <- df$aimingdeviation_deg * -1
    
    avg[[condition]] <- aggregate(depvar ~ trialno, data=df, FUN=FUN, na.rm=TRUE)
    CIs[[condition]] <- aggregate(depvar ~ trialno, data=df, FUN=function(d) Reach::getConfidenceInterval(d, method='b', FUN=FUN))
    
    color[[condition]] <- info$color[idx]
    
  }
  
  for (condition in conditions) {
    cc <- Reach::colorAlpha( col=color[[condition]], alpha=22 )
    CI <- CIs[[condition]]
    polygon(
      x = c(CI$trialno, rev(CI$trialno)),
      y = c(CI$depvar[,1], rev(CI$depvar[,2])),
      border=NA,
      col=cc
    )
  }
  
  # draw central lines
  for (condition in conditions) {
    cc <- color[[condition]]
    central <- avg[[condition]]
    # lty <- ltys[[condition]]
    lty = 2
    lines(central$trialno, central$depvar, col=cc, lty=lty)
  }
  
  
}


# processing -----

removeOutliers <- function(df, depvar='reachdeviation_deg', margin = 15) {
  
  info <- groupInfo()
  condition <- df$condition[1]
  rot <- info$rot[which(info$condition == condition)]
  
  if (sign(rot) == -1) {
    ub <- -rot + margin
    lb <- -margin
  }
  if (sign(rot) == 1) {
    ub <-  margin
    lb <- -rot - margin
  }
  
  df[which(df[,depvar] > ub), depvar] <- NA
  df[which(df[,depvar] < lb), depvar] <- NA
  
  
  # this used to be a one-liner:
  # df[which(abs(df[,depvar]) > (abs(df$rotation) + 30)), depvar] <- NA
  
  return(df)
  
}

baseline <- function(df, ignore=4, depvar='reachdeviation_deg', FUN=median) {
  
  idx <- unique(df$trialno[which(df$phase == 'baseline')])
  idx <- idx[c( (ignore+1) : length(idx) )]
  
  for (pp in unique(df$participant)) {
    bias <- FUN(df[which(df$participant == pp & df$phase == 'baseline' & df$trialno %in% idx), depvar], na.rm=TRUE)
    df[which(df$participant == pp), depvar] <- df[which(df$participant == pp), depvar] - bias
  }
  
  return(df)
  
}

getWashout <- function(df) {
  
  # 
  # washoutStart <- min(df$trialno[which(df$phase == 'washout')])
  # # asymptote_idx <- c((washoutStart-asymptote):(washoutStart-1))
  # 
  # participants <- unique(df$participant)
  # 
  # df$tempvar <- df[,depvar]
  # adf <- df[which(df$trialno %in% asymptote_idx),]
  # 
  # # print(dim(adf))
  # # print(length(participants)*asymptote)
  # 
  # for (participant in participants) {
  #   df[which(df$trialno %in% asymptote_idx & df$participant == participant),depvar] <- median(adf$tempvar[which(adf$participant == participant)], na.rm=TRUE)
  #   # print(median(adf$tempvar[which(adf$participant == participant)], na.rm=TRUE))
  # }
  # 
  # # df <- df[which(df$trialno > (washoutStart-2)),]
  # df <- df[which(df$trialno >= washoutStart),]
  # df$phase <- 'washout'
  # 
  df <- df[which(df$phase == 'washout'),]
  
  return(df)
  
}

# data ------

getImpExpEst <- function(condition,type) {
  
  info <- groupInfo()
  aiming_trials <- c( 77, 81, 85, 89, 93, 97, 101, 105 )
  
  exp <- info$exp[which(info$condition == condition)]
  
  df <- read.csv(file=sprintf('data/exp%d/%s_%s.csv',exp,condition,type), stringsAsFactors = FALSE)
  
  if (type == 'nocursors') {
    df$depvar <- df$reachdeviation_deg
    use_trials <- c(aiming_trials, aiming_trials-1)
  }
  if (type == 'aiming') {
    df$depvar <- df$aimingdeviation_deg
    use_trials <- aiming_trials
  }
  
  
  df[which(df$trialno %in% use_trials),]
  df$depvar <- -1 * df$depvar
  
  newdf <- aggregate(depvar ~ participant, data=df, FUN=mean)
  
  return(newdf)

}

# demographics ------

demographicsTable <- function(exp=1, mergeControls=TRUE, onlyLearners=TRUE) {
  
  # read the demographics file:
  demographics <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  
  # remove non-learners?
  if (onlyLearners) {
    demographics <- demographics[which(demographics$learner == TRUE),]
  }
  
  # get conditions for the experiment:
  if (exp > 0) { conditions <- expConditions(exp) } else {conditions <- unique(demographics$condition_label)}
  
  # merge controls, or keep 45deg as separate group:
  if (mergeControls) {
    if (exp != 1) {
      demographics$condition_label[which(demographics$condition_label == '45deg_distance')] <- 'control'
    }
  } else {
    if (exp > 0) {
      conditions <- unique(c(conditions, '45deg_distance'))
    }
  }
  
  alldem <- demographics[which(demographics$condition_label %in% conditions),]
  alldem$condition_label <- 'all'
  
  demographics <- rbind(demographics, alldem)
  
  conditions <- c('all', conditions)
  if (exp == 0) {conditions <- c('all')}
  
  gender_table <- table(demographics$condition_label, demographics$sex)
  
  dgroup <- rownames(gender_table)
  
  females <- gender_table[,1]
  males   <- gender_table[,2]
  other   <- gender_table[,3]
  totalN  <- females + males + other
  
  gender_df <- data.frame(group = dgroup,
                          females = females,
                          males = males,
                          other_gender = other,
                          total_N = totalN
                          )
  
  age_mean <- aggregate(age ~ condition_label, data=demographics, FUN=mean)
  names(age_mean) <- c('group', 'age_mean')
  df <- merge(gender_df, age_mean, by.x='group', by.y='group')
  
  age_sd   <- aggregate(age ~ condition_label, data=demographics, FUN=sd)
  names(age_sd) <- c('group', 'age_sd')
  df <- merge(df, age_sd, by.x='group', by.y='group')
  
  hand_table <- table(demographics$condition_label, demographics$handedness)
  
  hgroup <- rownames(hand_table)
  right <- hand_table[,3]
  left  <- hand_table[,1]
  other <- hand_table[,2]
  
  hand_df <- data.frame(group = hgroup,
                        right_handed = right,
                        left_handed = left,
                        other_handed = other)
  
  df <- merge(df, hand_df, by.x='group', by.y='group')
  
  df$age_mean <- round(df$age_mean, digits=3)
  df$age_sd <- round(df$age_sd, digits=3)
  
  df <- df[which(df$group %in% conditions),]
  
  if (dim(df)[1] > 1) {
    groups <- df$group
    df <- df[ , !(names(df) %in% c('group'))]
    df <- t(sapply(df, as.character))
    df <- as.data.frame(df)
    names(df) <- groups
  }
  
  return(df)
  
}
