library(svglite)

source('R/utilities.R')

# figure 1: setup, manipulations, trialtypes


expBehaviorFig <- function(exp, target='inline') {
  
  # in inches:
  width = 6
  if (exp==1) { height = 10 } else { height = 7.5 }
  dpi = 300
  outfilename <- sprintf('doc/fig%d',exp+1)
  
  if (target == 'svg') {
    svglite::svglite( filename = sprintf('%s.svg',outfilename),
                      width = width,
                      height = height,
                      fix_text_size = FALSE)
  }
  # if (target == 'png') {
  #   png( filename = sprintf('%s.png',outfilename),
  #        width = width*dpi,
  #        height = height*dpi
  #        )
  # }
  if (target == 'pdf') {
    pdf( file = sprintf('%s.pdf', outfilename),
         width=width,
         height=height)
  }
  # if (target == 'tiff') {
  #   tiff( filename = sprintf('%s.tiff',outfilename),
  #         compression = 'lzw',
  #         width = width*dpi,
  #         height = height*dpi
  #   )
  # }
  
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  
  if (exp > 1) { ntrials = 120; mrot=45; xticks=c(1,21,120) } else { ntrials = 144; mrot=60; xticks=c(1,21,121,144) }
  
  if (exp == 1) {
    layout( mat=matrix(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,4,4,4,5,5,6,6,7,7),nrow=4,ncol=6, byrow=TRUE) )
  } else {
    layout( mat=matrix(c(1,1,1,2,2,2,3,4,5),nrow=3,ncol=3, byrow=TRUE) )
  }
  
  relfontsize <- 0.8
  
  par(mar=c(4,4,4,0.1),
      cex.axis=relfontsize, cex.lab=relfontsize, cex.main=relfontsize)
  
  
  # # # # # #   First plot: reaches
  
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
  
  legends <- convertLegend(leg.info$label)
  
  legend(x=c(-5,120)[(exp==1)+1],y=mrot+20,
         legend=legends,
         col=leg.info$color,
         lty=1, bty='n',
         cex=relfontsize)
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  # # # # # #   Second plot: no-cursors
  
  main <- 'no-cursor reaches'
  if (exp == 3) {
    main <- 'no-cursor reaches and aiming'
  }
  
  plot(x=-1000, y=-1000,
       main=main,xlab='trial',ylab='deviation [°]',
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
                                conditions=conditions
                                )
  
  if (exp == 3) {
    leg.info <- rbind(leg.info, addLearningCurves(type='aiming',conditions=c('aiming'),FUN=mean))
  }
  # legend(x=-5,y=mrot+20,
  #        legend=leg.info$label,
  #        col=leg.info$color,
  #        lty=1, bty='n')
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  plot(-1000,-1000,
       main='learning timecourse',xlab='trial',ylab='adaptation',
       xlim=c(0,30),ylim=c(0,1),
       ax=F,bty='n'
       )
  
  addAdaptationTimecourses(type='reaches',conditions=conditions)
  
  axis(side=1,at=c(1,10,20,30))
  axis(side=2,at=c(0,0.25,0.5,0.75,1),las=1,labels=c('0%','25%','50%','75%','100%'))
  
  
  plot(-1000,-1000,
       main='no-cursor timecourse',xlab='trial',ylab='adaptation',
       xlim=c(0,30),ylim=c(0,1),
       ax=F,bty='n'
  )
  
  addAdaptationTimecourses(type='nocursors',conditions=conditions)
  
  if (exp == 3) {
    addAdaptationTimecourses(type='aiming',conditions=c('aiming'))
  }
  

  axis(side=1,at=c(0,10,20,30),labels=c('baseline','10','20','30'))
  axis(side=2,at=c(0,0.25,0.5,0.75,1),las=1,labels=c('0%','25%','50%','75%','100%'))
  
  
  if (exp == 1) {
    plot(-1000,-1000,
         main='washout reach timecourses',xlab='trial',ylab='reach aftereffects',
         xlim=c(0,30),ylim=c(0,45),
         ax=F,bty='n'
    )
    
    addWashoutTimecourses(type='reaches',conditions=conditions)
    
    axis(side=1,at=c(1,12,24))
    axis(side=2,at=c(0,15,30,45),las=1)
    
    plot(-1000,-1000,
         main='washout no-cursor timecourses',xlab='trial',ylab='no-cursor aftereffects',
         xlim=c(0,30),ylim=c(0,45),
         ax=F,bty='n'
    )
    
    addWashoutTimecourses(type='nocursors',conditions=conditions)
    
    axis(side=1,at=c(1,12,24))
    axis(side=2,at=c(0,15,30,45),las=1)
    
  }
  
  
  # now the implicit/explicit scatters:
  
  if (exp > 1) { mrot=45 } else { mrot=60 }
  mrot <- 60
  
  plot(x=-1000, y=-1000,
       main='',ylab='implicit (no-cursors) [°]',xlab='explicit (aiming) [°]',
       xlim=c(-15,mrot+15),ylim=c(-15,mrot+15),
       ax=F,bty='n',asp=1)
  
  addImpExpScatters(conditions)
  
  addDensities(conditions, type='aiming', viewscale=c(1,12.5), offset=c(0,mrot+2.5))
  
  addDensities(conditions, type='nocursors', viewscale=c(12.5,1), flipXY=TRUE, offset=c(mrot+2.5,0))
  
  axis(side=1,at=seq(0,mrot,15))
  axis(side=2,at=seq(0,mrot,15))
  
  
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}


# implExplFig <- function(exp, target='inline') {
#   
#   # in inches:
#   width  =  4
#   height = 4
#   dpi = 300
#   outfilename <- sprintf('doc/fig%d',exp+5)
#   
#   if (target == 'svg') {
#     svglite::svglite( filename = sprintf('%s.svg',outfilename),
#                       width = width,
#                       height = height,
#                       fix_text_size = FALSE)
#   }
#   # if (target == 'png') {
#   #   png( filename = sprintf('%s.png',outfilename),
#   #        width = width*dpi,
#   #        height = height*dpi
#   #        )
#   # }
#   if (target == 'pdf') {
#     pdf( file = sprintf('%s.pdf', outfilename),
#          width=width,
#          height=height)
#   }
#   # if (target == 'tiff') {
#   #   tiff( filename = sprintf('%s.tiff',outfilename),
#   #         compression = 'lzw',
#   #         width = width*dpi,
#   #         height = height*dpi
#   #   )
#   # }
#   
#   
#   
#   info <- groupInfo()
#   conditions <- expConditions(exp)
#   
#   if (exp > 1) { mrot=45 } else { mrot=60 }
#   
#   
#   layout( mat=matrix(c(2,3,4,1),nrow=2,ncol=2, byrow=TRUE), widths = c(1,3), heights = c(3,1) )
#   
#   relfontsize <- 0.8
#   
#   par(mar=c(4,4,0.1,0.1),
#       cex.axis=relfontsize, cex.lab=relfontsize, cex.main=relfontsize)
#   
#   # implicit <- list()
#   # explicit <- list()
#   
#   plot(x=-1000, y=-1000,
#        main='',xlab='explicit (aiming) [°]',ylab='',
#        ylim=c(0,1),xlim=c(-15,mrot+15),
#        ax=F,bty='n')
#   
#   addDensities(conditions, type='aiming', viewscale=c(1,-1), flipXY=FALSE)
# 
#   plot(x=-1000, y=-1000,
#        main='',ylab='implicit (no-cursor) [°]',xlab='',
#        xlim=c(0,1),ylim=c(-15,mrot+15),
#        ax=F,bty='n')
#   
#   addDensities(conditions, type='nocursors', flipXY=TRUE, viewscale=c(-1,1))
#   
#   plot(x=-1000, y=-1000,
#        main='',ylab='',xlab='',
#        xlim=c(-15,mrot+15),ylim=c(-15,mrot+15),
#        ax=F,bty='n')
#   
#   addImpExpScatters(conditions)
#   
#   axis(side=1,at=seq(-15,mrot+15,15))
#   axis(side=2,at=seq(-15,mrot+15,15))
#   
#   
#   if (target %in% c('svg','png','pdf','tiff')) {
#     dev.off()
#   }
#   
# }