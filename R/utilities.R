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

# plotting -----

addLearningCurves <- function(type, conditions=NULL, exp=NULL, phases=c('baseline','rotation','washout')) {
  
  if (type %notin% c('reaches','nocursors')) {
    return()
  }

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
    color[[condition]] <- info$color[idx]
    label[[condition]] <- info$label[idx]
    
    df <- read.csv(sprintf('data/exp%d/%s_%s.csv', exp, condition, type))
    
    df$reachdeviation_deg <- -1 * df$reachdeviation_deg
    df <- removeOutliers(df)
    df <- baseline(df)
    
    avg[[condition]] <- aggregate(reachdeviation_deg ~ trialno, data=df, FUN=median, na.rm=TRUE)
    CIs[[condition]] <- aggregate(reachdeviation_deg ~ trialno, data=df, FUN=function(d) Reach::getConfidenceInterval(d, method='b', FUN=median))
    
  }
  
  # draw 95% confidence intervals
  for (condition in conditions) {
    cc <- Reach::colorAlpha( col=color[[condition]], alpha=22 )
    CI <- CIs[[condition]]
    polygon(
      x = c(CI$trialno, rev(CI$trialno)),
      y = c(CI$reachdeviation_deg[,1], rev(CI$reachdeviation_deg[,2])),
      border=NA,
      col=cc
    )
  }
  
  # draw median lines
  for (condition in conditions) {
    cc <- color[[condition]]
    central <- avg[[condition]]
    lines(central$trialno, central$reachdeviation_deg, col=cc)
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
  
  df <- df[which(df$trialno > (washoutStart-2)),]
  df$phase <- 'washout'
  return(df)
  
}
