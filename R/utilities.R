library('Reach')


`%notin%` <- Negate(`%in%`)


groupInfo <- function() {
  
  exp <- c(1,1,1,1,2,2,2,3,4,4)
  condition <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance',
                 'control','cursorjump','terminal',
                 'aiming',
                 'delay-FB', 'delay-trial')
  color <- c('darkred', '#ae0a23', '#e51636', 'salmon',
             'orange', 'darkturquoise', 'deepskyblue',   # deepskyblue
             'purple',
             'blue','darkblue') # blue
  
  label <- c('15°', '30°', '45°', '60°', 'control', 'cursor-jump', 'terminal', 'aiming', 'delay-feedback', 'feedback-delay')
  
  return( data.frame( exp = exp,
                      condition = condition,
                      color = color,
                      label = label ) )
  
}



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
  
  for (condition in conditions) {
    cc <- color[[condition]]
    central <- avg[[condition]]
    lines(central$trialno, central$reachdeviation_deg, col=cc)
    print(length(central$trialno))
  }
  
}

removeOutliers <- function(df) {
  
  df$reachdeviation_deg[which(abs(df$reachdeviation_deg) > (abs(df$reachdeviation_deg) + 30))] <- NA
  
  return(df)
  
}

baseline <- function(df, ignore=4) {
  
  idx <- unique(df$trialno[which(df$phase == 'baseline')])
  idx <- idx[c( (ignore+1) : length(idx) )]
  
  for (pp in unique(df$participant)) {
    bias <- median(df$reachdeviation_deg[which(df$participant == pp & df$phase == 'baseline' & df$trialno %in% idx)], na.rm=TRUE)
    df$reachdeviation_deg[which(df$participant == pp)] <- df$reachdeviation_deg[which(df$participant == pp)] - bias
  }
  
  return(df)
  
}