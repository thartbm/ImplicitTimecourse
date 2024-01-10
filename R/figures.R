library(svglite)

source('R/utilities.R')

# figure 1: setup, manipulations, trialtypes


expBehaviorFig <- function(exp, target='inline', timecoursemode='absolute') {
  
  # in inches:
  width = 6
  if (exp==1) { height = 9 } else { height = 7.5 }
  dpi = 300
  outfilename <- sprintf('doc/fig%d',exp+1)
  
  if (target == 'svg') {
    svglite::svglite( filename = sprintf('%s.svg',outfilename),
                      width = width,
                      height = height,
                      fix_text_size = FALSE)
  }
  if (target == 'png') {
    png( filename = sprintf('%s.png',outfilename),
         width = width*dpi,
         height = height*dpi,
         res = dpi
         )
  }
  if (target == 'pdf') {
    pdf( file = sprintf('%s.pdf', outfilename),
         width=width,
         height=height)
  }
  if (target == 'tiff') {
    tiff( filename = sprintf('%s.tiff',outfilename),
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
  
  
  info <- groupInfo()
  conditions <- expConditions(exp)
  
  if (exp > 1) { ntrials = 120; mrot=45; xticks=c(1,21,120) } else { ntrials = 144; mrot=60; xticks=c(1,21,121,144) }
  
  if (exp == 1) {
    layout( mat=matrix(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,4,4,4,5,5,6,6,7,7),nrow=4,ncol=6, byrow=TRUE) )
  } else {
    layout( mat=matrix(c(1,1,1,2,2,2,3,4,5),nrow=3,ncol=3, byrow=TRUE) )
  }
  
  relfontsize <- 0.8
  
  # text(0,0.95,'B: less explicit', font.main=1, cex=1.35*1.5, adj=0)
  # title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
  # title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
  
  par(mar=c(3.2,3.5,2,0.1),
      cex.axis=relfontsize, 
      cex.lab=relfontsize,
      cex.main=relfontsize*1.5,
      xpd=TRUE)
  
  
  # # # # # #   First plot: reaches
  
  plot(x=-1000, y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0,ntrials+1),ylim=c(-15,mrot+15),
       ax=F,bty='n')
  
  title(main='A: training reaches', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  
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
  
  if (exp < 4) {
    legends <- convertLegend(leg.info$label)
    color <- leg.info$color
    ltys <- rep(1,length(legends))
    
    legend(x=c(125,0,-5)[exp],y=mrot,
           legend=legends,
           col=color,
           lty=ltys, bty='n',
           cex=relfontsize)
  }
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  # # # # # #   Second plot: no-cursors
  
  main <- 'B: no-cursor reaches'
  if (exp == 4) {
    main <- 'B: no-cursor reaches and re-aiming responses'
  }
  
  plot(x=-1000, y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0,ntrials+1),ylim=c(-15,mrot+15),
       ax=F,bty='n')
  
  title(main=main, line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  
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
  
  if (exp == 4) {
    leg.info <- rbind(leg.info, addLearningCurves(type='aiming',conditions=c('aiming'),FUN=mean))
    
    legends <- c(convertLegend(leg.info$label)[c(1,2)],'re-aiming')
    color <- leg.info$color
    ltys <- c(1,1,2)
    
    legend(x=0,y=mrot,
           legend=legends,
           col=color,
           lty=ltys, bty='n',
           cex=relfontsize)
    
  }
  
  # legend(x=-5,y=mrot+20,
  #        legend=leg.info$label,
  #        col=leg.info$color,
  #        lty=1, bty='n')
  
  axis(side=1,at=xticks)
  axis(side=2,at=seq(-15,(mrot+15),15),las=1)
  
  
  # # # # # # # 3 # # #
  # exp time courses
  
  if (timecoursemode == 'relative') {
    ylim <- c(0,1)
    yticklocs <- c(0,0.25,0.5,0.75,1)
    yticklabels <- c('0%','25%','50%','75%','100%')
  }
  if (timecoursemode == 'absolute') {
    ylim <- c(0,mrot)
    yticklocs <- seq(0,mrot,15)
    yticklabels <- sprintf('%d',yticklocs)
  }
  
  # # # # # # # # # # # # # # # #
  # reach training time course
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,30),ylim=ylim,
       ax=F,bty='n'
       )
  
  title(main='C: training timecourse', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  addAdaptationTimecourses(type='reaches',conditions=conditions, timecoursemode=timecoursemode)
  
  axis(side=1,at=c(1,10,20,30))
  axis(side=2,at=yticklocs,las=1,labels=yticklabels)
  
  main <- 'D: implicit timecourse'
  if (exp == 4) {
    main <- 'D: implicit timecourse'
  }
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,30),ylim=ylim,
       ax=F,bty='n'
  )
  
  title(main=main, line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  addAdaptationTimecourses(type='nocursors',conditions=conditions, timecoursemode=timecoursemode)
  
  if (exp == 4) {
    addAdaptationTimecourses(type='aiming',conditions=c('aiming'), timecoursemode=timecoursemode)
  }
  

  axis(side=1,at=c(0,10,20,30),labels=c('baseline','10','20','30'))
  axis(side=2,at=yticklocs,las=1,labels=yticklabels)
  
  
  if (exp == 1) {
    plot(-1000,-1000,
         main='',xlab='',ylab='',
         xlim=c(0,30),ylim=c(0,45),
         ax=F,bty='n'
    )
    
    title(main='E: washout reaches', line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    
    
    addWashoutTimecourses(type='reaches',conditions=conditions)
    
    axis(side=1,at=c(1,12,24))
    axis(side=2,at=c(0,15,30,45),las=1)
    
    plot(-1000,-1000,
         main='',xlab='',ylab='',
         xlim=c(0,30),ylim=c(0,45),
         ax=F,bty='n'
    )
    
    title(main='F: washout no-cursors', line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    
    addWashoutTimecourses(type='nocursors',conditions=conditions)
    
    axis(side=1,at=c(1,12,24))
    axis(side=2,at=c(0,15,30,45),las=1)
    
  }
  
  
  # now the implicit/explicit scatters:
  
  if (exp > 1) { mrot=45 } else { mrot=60 }
  mrot <- 60
  
  if (exp == 1) {
    main <- 'G: linear model'
  } else {
    main <- 'E: linear model'
  }
  
  plot(x=-1000, y=-1000,
       main='',ylab='',xlab='',
       xlim=c(-15,mrot+15),ylim=c(-15,mrot+15),
       ax=F,bty='n',asp=1)
  
  title(main=main, line=0.25, adj=0)
  title(xlab='explicit (aiming) [°]', line = 1.75)
  title(ylab='implicit (no-cursors) [°]', line = 2.5)
  
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