library(svglite)

source('R/utilities.R')

# figure 1: setup, manipulations, trialtypes


expFig <- function(exp, target='inline') {
  
  # in inches:
  width=7
  height=7
  
  if (target == 'svg') {
    svglite::svglite( filename = sprintf('doc/fig%d.svg',exp+1),
                      width = width,
                      height = height,
                      fix_text_size = FALSE)
  }
  # if (target == 'png') {
  #   
  # }
  # if (target == 'pdf') {
  #   
  # }
  # if (target == 'tiff') {
  #   
  # }
  
  
  info <- groupInfo()
  
  if (exp == 1) { conditions <- c('15deg_distance', '30deg_distance', '45deg_distance', '60deg_distance') }
  if (exp == 2) { conditions <- c('control','cursorjump','terminal') }
  if (exp == 3) { conditions <- c('control','aiming') }
  if (exp == 4) { conditions <- c('control','terminal','delay-trial', 'delay-FB')}
  
  if (exp > 1) { ntrials = 120; mrot=45; xticks=c(1,21,120) } else { ntrials = 144; mrot=60; xticks=c(1,21,121,144) }
  
  layout( mat=matrix(c(1,2),nrow=2, byrow=TRUE) )
  
  relfontsize <- 0.8
  
  par(mar=c(4,4,4,0.1),
      cex.axis=relfontsize, cex.lab=relfontsize, cex.main=relfontsize)
  
  plot(x=-1000, y=-1000,
       main='training reaches',xlab='trial',ylab='deviation [°]',
       xlim=c(0,ntrials+1),ylim=c(-15,mrot+15),
       ax=F,bty='n')
  
  if (exp == 1) {
    lines(x=c(1,21,21,121,121,144),
          y=c(0,0,mrot,mrot,0,0),col='gray')
    for (rot in c(15,30,45)) {
      lines(x=c(21,121),y=c(rot,rot),col='gray')
    }
  } else {
    lines(x=c(1,21,21,120),
          y=c(0,0,mrot,mrot),col='gray')
  }
  
  addAimingTrials(mrot=mrot)
  
  leg.info <- addLearningCurves(type='reaches',
                                conditions=conditions)
  
  legend(x=c(-5,120)[(exp==1)+1],y=mrot+20,
         legend=leg.info$label,
         col=leg.info$color,
         lty=1, bty='n',
         cex=relfontsize)
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  plot(x=-1000, y=-1000,
       main='no-cursor reaches',xlab='trial',ylab='deviation [°]',
       xlim=c(0,ntrials+1),ylim=c(-15,mrot+15),
       ax=F,bty='n')
  
  if (exp == 1) {
    lines(x=c(1,21,21,121,121,144),
          y=c(0,0,mrot,mrot,0,0),col='gray')
    for (rot in c(15,30,45)) {
      lines(x=c(21,121),y=c(rot,rot),col='gray')
    }
  } else {
    lines(x=c(1,21,21,120),
          y=c(0,0,mrot,mrot),col='gray')
  }
  
  addAimingTrials(mrot=mrot)
  
  leg.info <- addLearningCurves(type='nocursors',
                                conditions=conditions)
  
  # legend(x=-5,y=mrot+20,
  #        legend=leg.info$label,
  #        col=leg.info$color,
  #        lty=1, bty='n')
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}