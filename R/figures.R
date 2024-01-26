library(svglite)

source('R/utilities.R')

# figure 1: setup, manipulations, trialtypes


expBehaviorFig <- function(exp, target='inline', timecoursemode='absolute') {
  
  # in inches:
  width = 6
  if (exp==1) { height = 9 } else { height = 7.5 }
  dpi = 300
  outfilename <- sprintf('doc/fig%d',exp+2)
  
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
    layout( mat=matrix(c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,4,4,5,5,6,6,7,7,8,8),nrow=4,ncol=6, byrow=TRUE) )
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
    
    addAimingResponses(conditions=c('control'))
    
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
    ymrot <- 45
    ylim <- c(0,ymrot)
    yticklocs <- seq(0,ymrot,15)
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
  
  # now the implicit/explicit scatters:
  
  if (exp > 1) { mrot=45 } else { mrot=60 }
  mrot <- 60
  
  main <- 'E: linear model'
  
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
  
  
  if (exp == 1) {
    plot(-1000,-1000,
         main='',xlab='',ylab='',
         xlim=c(0,30),ylim=c(0,45),
         ax=F,bty='n'
    )
    
    title(main='F: washout reaches', line=0.25, adj=0)
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
    
    title(main='G: washout no-cursors', line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    
    addWashoutTimecourses(type='nocursors',conditions=conditions)
    
    axis(side=1,at=c(1,12,24))
    axis(side=2,at=c(0,15,30,45),las=1)
    
  }
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}


discussionPlot <- function(target='inline') {
  
  # in inches:
  width = 4
  height = 8
  dpi = 300
  outfilename <- 'doc/fig7'
  
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
  
  layout( mat=matrix(c(1,2,3,4,5),nrow=5,ncol=1, byrow=TRUE) )
  
  info <- groupInfo()
  conditions <- c("15deg_distance",
                  "30deg_distance",
                  "45deg_distance",
                  "60deg_distance",
                  "control",
                  "cursorjump",
                  "terminal",
                  "delay-trial",
                  "delay-FB",
                  "aiming")
  
  # we'll make 5 plots showing parameters for each of those 10 groups:
  # - reach training: rate of change
  # - reach training: asymptote
  # - no-cursor: rate of change
  # - no-cursor: asymptote
  # - aiming extent
  
  condition_labels <- convertLegend(info$label)
  
  relfontsize <- 0.8
  
  par(mar=c(3.5,4,2,0.1),
      cex.axis=relfontsize, 
      cex.lab=relfontsize,
      cex.main=relfontsize*1.5,
      xpd=TRUE)
  
  xtick_distance <- -0.2
  
  
  # # # # # # # # # # # # # # # # # # #
  # reach training rate of change
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,0.4),
       ax=F,bty='n')
  
  df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- quantile(df$lambda, probs=c(0.5))
  # lines(x=c(.5,10.5), y=rep(avg,2), col=Reach::colorAlpha(info$color[which(info$condition == 'control')], alpha=99), lty=2)
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    avg <- quantile(df$lambda, probs=c(0.5))
    # df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    # avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
    
    # print(condition)
    # print(CI)
    # print(avg)
    
    # print(color)
    # print(Reach::colorAlpha(color,alpha=22))
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 0.4,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=c(0,0.20,0.40),labels=c('0%','20%','40%'),las=2)
  
  title(main='A: training reaches rate of change', line=0.25, adj=0)
  title(ylab='rate of change [%]', line=3)
  
  # # # # # # # # # # # # # # # # # # #
  # reach training asymptote
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,50),
       ax=F,bty='n')
  
  
  df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- quantile(df$N0, probs=c(0.5))
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$N0, probs=c(0.025, 0.975))
    avg <- quantile(df$N0, probs=c(0.5))
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }

  # axis(side=1,at=c(1:length(conditions)),labels=condition_labels)
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 50,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,50,10),las=2)
  
  title(main='B: training reaches asymptote', line=0.25, adj=0)
  title(ylab='asymptote [°]', line=3)
  
  
  # # # # # # # # # # # # # # # # # # #
  # no-cursor rate of change
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,0.4),
       ax=F,bty='n')
  
  
  df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- quantile(df$lambda, probs=c(0.5))
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_nocursors_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    avg <- quantile(df$lambda, probs=c(0.5))
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 0.4,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=c(0,0.20,0.4),labels=c('0%','20%','40%'),las=2)
  
  title(main='C: no-cursor reaches (implicit) rate of change', line=0.25, adj=0)
  title(ylab='rate of change [%]', line=3)
  
  
  
  # # # # # # # # # # # # # # # # # # #
  # no-cursor asymptote
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,30),
       ax=F,bty='n')
  
  
  
  df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- quantile(df$N0, probs=c(0.5))
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_nocursors_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$N0, probs=c(0.025, 0.975))
    avg <- quantile(df$N0, probs=c(0.5))
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  
  
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 30,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,30,10),las=2)
  
  title(main='D: no-cursors reaches (implicit) asymptote', line=0.25, adj=0)
  title(ylab='asymptote [°]', line=3)
  
  
  # # # # # # # # # # # # # # # # # # #
  # aiming extent
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,30),
       ax=F,bty='n')
  
  
  
  df <- read.csv('data/exp2/control_aiming.csv', stringsAsFactors = FALSE)
  avg <- mean(aggregate(aimingdeviation_deg ~ participant, data=df, FUN=mean, na.rm=TRUE)$aimingdeviation_deg, na.rm=TRUE) * -1
  # avg <- quantile(df$N0, probs=c(0.5))
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_aiming.csv', exp, condition), stringsAsFactors = FALSE)
    df$aimingdeviation_deg <- df$aimingdeviation_deg * -1
    df <- df[which(df$trialno %in% c(77, 81, 85, 89, 93, 97, 101, 105)),]
    df <- aggregate(aimingdeviation_deg ~ participant, data=df, FUN=mean, na.rm=TRUE)
    
    CI <- Reach::getConfidenceInterval(data=df$aimingdeviation_deg, method='b')
    avg <- mean(df$aimingdeviation_deg, na.rm=TRUE)
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  
  
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 30,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,30,10),las=2)
  
  title(main='E: aiming responses (explicit) extent', line=0.25, adj=0)
  title(ylab='aiming responses [°]', line=3)
  
  
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}

