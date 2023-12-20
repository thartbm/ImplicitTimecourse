library('Reach')


`%notin%` <- Negate(`%in%`)


groupInfo <- function() {
  
  exp <- c(1,1,1,1,2,2,2,3,4,4)
  condition <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance',
                 'control','cursorjump','terminal',
                 'aiming',
                 'delay-trial', 'delay-FB')
  color <- c('darkred', '#ae0a23', '#e51636', 'salmon',
             'orange', 'darkturquoise', 'deepskyblue',   # deepskyblue
             'purple',
             'blue','darkblue') # blue
  
  label <- c('15°', '30°', '45°', '60°', 'control', 'cursor-jump', 'terminal', 'aiming', 'lag-terminal', 'terminal-lag')
  
  return( data.frame( exp = exp,
                      condition = condition,
                      color = color,
                      label = label ) )
  
}

expConditions <- function(exp) {
  
  if (exp == 1) { conditions <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance') }
  if (exp == 2) { conditions <- c('control','cursorjump','terminal') }
  if (exp == 3) { conditions <- c('control','aiming') }
  if (exp == 4) { conditions <- c('control','terminal','delay-trial', 'delay-FB')}
  
  return(conditions)
  
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
  label <- list()
  
  for (condition in conditions) {
    
    if (condition %notin% info$condition) {
      return()
    }
    
    idx <- which(info$condition == condition)
    exp <- info$exp[idx]
    if (type == 'aiming') {
      color[[condition]] <- 'magenta'
    } else {
      color[[condition]] <- info$color[idx]
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
    lines(central$trialno, central$depvar, col=cc)
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

addDensities <- function(conditions, type, flipXY, viewscale) {
  
  info <- groupInfo()
  
  for (condition in conditions) {
    
    plotdf <- getImpExpEst(condition=condition, type=type)
    
    dens <- density(x=plotdf$depvar,
                    bw=5,
                    from=-15,
                    to=75,
                    n=1001)
    
    if (flipXY) {
      Y <- dens$x
      X <- 1-(dens$y/max(dens$y))
      # X <- 1-(dens$y * 14)
    } else {
      X <- dens$x
      Y <- 1-(dens$y/max(dens$y))
      # Y <- 1-(dens$y * 14)
    }
    
    color <- info$color[which(info$condition == condition)]
    
    lines(x=X,
          y=Y,
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

# processing -----

removeOutliers <- function(df, depvar='reachdeviation_deg') {
  
  df[which(abs(df[,depvar]) > (abs(df$rotation) + 60)), depvar] <- NA
  
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

getWashout <- function(df, asymptote=8, depvar='reachdeviation_deg') {
  
  
  washoutStart <- min(df$trialno[which(df$phase == 'washout')])
  asymptote_idx <- c((washoutStart-asymptote):(washoutStart-1))
  
  participants <- unique(df$participant)
  
  df$tempvar <- df[,depvar]
  adf <- df[which(df$trialno %in% asymptote_idx),]
  
  # print(dim(adf))
  # print(length(participants)*asymptote)
  
  for (participant in participants) {
    df[which(df$trialno %in% asymptote_idx & df$participant == participant),depvar] <- median(adf$tempvar[which(adf$participant == participant)], na.rm=TRUE)
    # print(median(adf$tempvar[which(adf$participant == participant)], na.rm=TRUE))
  }
  
  # df <- df[which(df$trialno > (washoutStart-2)),]
  df <- df[which(df$trialno >= washoutStart),]
  df$phase <- 'washout'
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

# exponential fits -----

exponentialModel <- function(par, timepoints, mode='learning', setN0=NULL) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] = setN0
  }
  
  if (mode == 'learning') {
    output = par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints )
  }
  if (mode == 'washout') {
    output = par['N0'] * (par['lambda'])^timepoints
  }
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}

exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode='learning', setN0=NULL) {
  
  MSE <- mean((exponentialModel(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}


exponentialFit <- function(signal, timepoints=length(signal), mode='learning', gridpoints=11, gridfits=10, setN0=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  # if (is.numeric(setN0)) {
  #   searchgrid <- expand.grid('lambda' = parvals)
  #   lo <- c(0)
  #   hi <- c(1)
  # }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
    lo <- c(0,asymptoteRange[1])
    hi <- c(1,asymptoteRange[2])
  } else {
    searchgrid <- expand.grid('lambda' = parvals )
    lo <- c(0)
    hi <- c(1)
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  if (is.null(setN0)) {
    X <- data.frame(searchgrid[order(MSE)[1:gridfits],])
  } else {
    X <- data.frame('lambda'=searchgrid[order(MSE)[1:3],])
  }
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( X,
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:2])
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0)
    names(winpar) <- c('lambda', 'N0')
  }
  
  # return the best parameters:
  return(winpar)
  
}
