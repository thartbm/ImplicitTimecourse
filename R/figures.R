library(svglite)

source('R/utilities.R')

# figure 1: setup, manipulations, trialtypes


expBehaviorFig <- function(exp, target='inline', timecoursemode='absolute') {
  
  # in inches:
  width = 6
  if (exp==1) { height = 9 } else { height = 7.5 }
  dpi = 300
  outfilename <- sprintf('doc/fig_%d',c(3,5,7,8)[exp])
  
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
       xlim=c(0,ntrials+1),ylim=c(-5,mrot+5),
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
  
  leg.info <- addLearningCurves(type       = 'reaches',
                                conditions = conditions)
  
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
  axis(side=2,at=seq(0,mrot,15),las=1)
  
  # # # # # #   Second plot: no-cursors
  
  main <- 'B: no-cursor reaches'
  if (exp == 4) {
    main <- 'B: no-cursor reaches and re-aiming responses'
  }
  
  plot(x=-1000, y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0,ntrials+1),ylim=c(-5,mrot+5),
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
    
    legends <- c(convertLegend(leg.info$label)[c(1,2)],'aiming report')
    color <- leg.info$color
    color[3] <- 'gray'
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
  axis(side=2,at=seq(0,mrot,15),las=1)
  
  
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
  outfilename <- 'doc/fig_11'
  
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
  
  # df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$lambda, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
  # lines(x=c(.5,10.5), y=rep(avg,2), col=Reach::colorAlpha(info$color[which(info$condition == 'control')], alpha=99), lty=2)
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    # avg <- quantile(df$lambda, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
    
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
  
  
  # df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$N0, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
  
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$N0, probs=c(0.025, 0.975))
    # avg <- quantile(df$N0, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
    
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
       ylim=c(0,0.6),
       ax=F,bty='n')
  
  
  # df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$lambda, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
  
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_nocursors_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    # avg <- quantile(df$lambda, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
    
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.2), 
       y=xtick_distance * 0.6,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=c(0,0.2,0.4,0.6),labels=c('0%','20%','40%','60%'),las=2)
  
  title(main='C: no-cursor reaches (implicit) rate of change', line=0.25, adj=0)
  title(ylab='rate of change [%]', line=3)
  
  
  
  # # # # # # # # # # # # # # # # # # #
  # no-cursor asymptote
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,30),
       ax=F,bty='n')
  
  
  
  # df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$N0, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
  
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


subtractionPlot <- function(target='inline') {
  
  width = 6
  height = 5
  dpi = 300
  outfilename <- 'doc/fig_9'
  
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
  
  
  color <- c('#e51636', 'orange', 'darkturquoise', 'purple')
  color <- c('#e51636', 'orange', 'blue', 'purple')
  color <- c('#e51636', 'blue', 'darkturquoise', 'purple')
  color <- c('darkturquoise', 'blue', '#e51636', 'purple')
  depvar <- c('reaches', 'nocursors', 'aiming', 'subtraction')
  lty <- c(3,3,3,1)
  
  info <- data.frame(color,
                     depvar,
                     lty)
  
  
  layout( mat = matrix(c(1,1,1,1,1,1,
                         2,2,3,3,4,4),
                       nrow=2, ncol=6,
                       byrow=TRUE)
        )
  
  
  
  relfontsize <- 0.8
  
  # text(0,0.95,'B: less explicit', font.main=1, cex=1.35*1.5, adj=0)
  # title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
  # title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
  
  par(mar=c(3.2,3.5,2,0.1),
      cex.axis=relfontsize, 
      cex.lab=relfontsize,
      cex.main=relfontsize*1.5,
      xpd=TRUE)
  
  
  # # # # # #   First plot: data time courses
  
  
  plot(x=-1000, y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0,121),ylim=c(-5,50),
       ax=F,bty='n')
  
  title(main='A: subtractive estimate of implicit', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  
  lines(x=c(1,21,21,120),
        y=c(0,0,45,45),col='gray')

  addAimingTrials(mrot=45)
  
  
  for (dvn in c(1:length(info$depvar))) {
    
    leg.info <- addLearningCurves(type=info$depvar[dvn], 
                                  condition='aiming', 
                                  lty=info$lty[dvn], 
                                  col=info$color[dvn], 
                                  FUN=mean)
    
  }
  
  legend(x=0,y=45,
         # legend=info$depvar,
         legend=c('reaches', 'no-cursors', 'aiming reports', 'subtraction'),
         col=info$color,
         lty=c(3,3,3,1), bty='n',
         cex=relfontsize)

  axis(side=1,at=c(1,21,120))
  axis(side=2,at=seq(0,45,15),las=1)
  

  # # # # # Second plot: fitted time courses + 95% CI
  
  ymrot <- 45
  ylim <- c(0,ymrot)
  yticklocs <- seq(0,ymrot,15)
  yticklabels <- sprintf('%d',yticklocs)

  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(0,30),ylim=ylim,
       ax=F,bty='n')  
  
  
  
  title(main='B: fitted timecourses', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='deviation [°]', line = 2.5)
  
  for (dvn in c(1:length(info$depvar))) {
    
    addAdaptationTimecourses(type=info$depvar[dvn],
                             conditions=c('aiming'), 
                             timecoursemode='absolute',
                             col=info$color[dvn],
                             lty=info$lty[dvn],
                             offset=0)
    
  }
  
  axis(side=1,at=c(0,10,20,30))
  axis(side=2,at=yticklocs,las=1,labels=yticklabels)
  
  
  
  
  # print(info)
  
  # here come the end of training additivity scatter plots:
  
  subtractiveData <- getAvgSubtractiveData()
  
  avgadapt <- mean(subtractiveData$adaptation, na.rm=TRUE)
  
  # layout(mat=matrix(c(1,2),byrow=TRUE, ncol=2,nrow=1))
  
  plot(-1000, -1000,
       main='',
       xlab='', ylab='',
       xlim=c(-30,60), ylim=c(-30,60),
       bty='n', ax=FALSE,asp=1)
  
  
  title(main='C: additivity', line=0.25, adj=0)
  title(xlab='explicit [aiming]', line = 1.75)
  title(ylab='implicit [no-cursors]', line = 2.5)
  
  
  lines(x=c(-20,60),y=c(0,0),col='#CCCCCC',lty=1)
  lines(x=c(0,0),y=c(-20,60),col='#CCCCCC',lty=1)
  
  # average adaptation is only 24.8 degrees?
  lines(x=c(-10,50),y=(c(-10,50)*-1)+avgadapt,col='#666666',lty=2)
  
  for (subset in c('all', 'aiming')){
    
    if (subset == 'all') {
      conditions <- unique(subtractiveData$condition)
      conditions <- conditions[conditions != 'aiming']
      col1 <- '#CCCCCC'
      col2 <- '#999999'
      pch <- 16
      op <- 44
    }
    if (subset == 'aiming') {
      conditions <- c('aiming')
      col1 <- info$color[which(info$depvar == 'aiming')]
      col2 <- info$color[which(info$depvar == 'aiming')]
      pch <- 1
      op <- 100
    }
    
    subData <- subtractiveData[which(subtractiveData$condition %in% conditions),]
    
    points(subData$explicit, subData$implicit,
           pch=pch, col=Reach::colorAlpha(col1,op), cex=1.0)
    
    
    # lm with 95% CI:
    impl <- subData$implicit
    expl <- subData$explicit
    e2i <- lm(impl ~ expl)
    
    # print(summary(e2i))
    
    slope_ci <- confint(e2i,parm='expl',level=0.95)
    # print(slope_ci)
    
    at <- range(expl)
    
    coef <- e2i$coefficients
    lines(at, coef[1]+(at*coef[2]), col=col2)
    
    ci <- predict( e2i,
                   newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=Reach::colorAlpha(col2),border=NA)
    
  }
  
  
  axis(side=1,at=seq(-20,60,20))
  axis(side=2,at=seq(-20,60,20))
  
  plot(-1000, -1000,
       main='',
       xlab='', ylab='',
       xlim=c(-30,60), ylim=c(-30,60),
       bty='n', ax=FALSE,asp=1)
  
  title(main='D: subtraction', line=0.25, adj=0)
  title(xlab='implicit [adaptation - aiming]', line = 1.75)
  title(ylab='implicit [no-cursors]', line = 2.5)
  
  lines(x=c(-20,60),y=c(0,0),col='#CCCCCC',lty=1)
  lines(x=c(0,0),y=c(-20,60),col='#CCCCCC',lty=1)
  lines(x=c(-20,60),y=c(-20,60),col='#999999',lty=2)
  
  for (subset in c('all', 'aiming')){
    
    if (subset == 'all') {
      conditions <- unique(subtractiveData$condition)
      conditions <- conditions[conditions != 'aiming']
      col1 <- '#CCCCCC'
      col2 <- '#999999'
      pch <- 16
      op <- 44
    }
    if (subset == 'aiming') {
      conditions <- c('aiming')
      col1 <- info$color[which(info$depvar == 'subtraction')]
      col2 <- info$color[which(info$depvar == 'subtraction')]
      pch <- 1
      op <- 100
    }
    
    subData <- subtractiveData[which(subtractiveData$condition %in% conditions),]
    
  
    points(subData$adaptation - subData$explicit, subData$implicit,
           pch=pch, col=Reach::colorAlpha(col1, op), cex=1.0)
    
    # lm with 95% CI:
    impl <- subData$implicit
    pred <- subData$adaptation - subData$explicit
    e2i <- lm(impl ~ pred)
    
    # print(summary(e2i))
    
    slope_ci <- confint(e2i,parm='pred',level=0.95)
    # print(slope_ci)
    
    at <- range(pred)
    
    coef <- e2i$coefficients
    lines(at, coef[1]+(at*coef[2]), col=col2)
    
    ci <- predict( e2i,
                   newdata=data.frame(pred=seq(at[1],at[2],length.out=40)),
                   interval = "confidence")
    
    X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
    Y <- c(ci[,'lwr'],rev(ci[,'upr']))
    polygon(x=X,y=Y,col=Reach::colorAlpha(col2),border=NA)
    
  }  
  
  axis(side=1,at=seq(-20,60,20))
  axis(side=2,at=seq(-20,60,20))
  
  
  
    
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}
  
  # here come the individual timecourses:
  
individualTimeCoursePlot <- function(target='inline') {
  
  width = 6
  height = 7.5
  dpi = 300
  outfilename <- 'doc/fig_10'
  
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
  
  
  # color <- c('#e51636', 'orange', 'darkturquoise', 'purple')
  # color <- c('#e51636', 'orange', 'blue', 'purple')
  # color <- c('#e51636', 'blue', 'darkturquoise', 'purple')
  color <- c('darkturquoise', 'blue', '#e51636', 'purple','orange')
  depvar <- c('reaches', 'nocursors', 'aiming', 'exp', 'step')
  lty <- c(3,3,3,3,3)
  
  info <- data.frame(color,
                     depvar,
                     lty)
  
  
  layout( mat = matrix(c(1,2,3,4,5,6),
                       nrow=3, ncol=2,
                       byrow=FALSE),
          widths=c(5,2)
  )
  
  
  
  relfontsize <- 0.8
  
  # text(0,0.95,'B: less explicit', font.main=1, cex=1.35*1.5, adj=0)
  # title(xlab='explicit [°]', line = 0.5,cex.lab=textsize)
  # title(ylab='implicit [°]', line = 2.5,cex.lab=textsize)
  
  par(mar=c(3.2,3.5,2,0.1),
      cex.axis=relfontsize, 
      cex.lab=relfontsize,
      cex.main=relfontsize*1.5,
      xpd=TRUE)
  
  
  
  explicit <- read.csv('data/exp4/aiming_aiming.csv', stringsAsFactors = F)
  explicit$aimingdeviation_deg <- -1*explicit$aimingdeviation_deg
  implicit <- read.csv('data/exp4/aiming_nocursors.csv', stringsAsFactors = F)
  implicit$reachdeviation_deg <- -1*implicit$reachdeviation_deg
  adaptation <- read.csv('data/exp4/aiming_reaches.csv', stringsAsFactors = F)
  adaptation$reachdeviation_deg <- -1*adaptation$reachdeviation_deg
  
  expl <- explicit[   which(explicit$trialno   %in% c(13:52)), ]
  impl <- implicit[   which(implicit$trialno   %in% c(13:52)), ]
  adpt <- adaptation[ which(adaptation$trialno %in% c(13:52)), ]
  
  
  
  plot(NULL,NULL,
       main='',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60), 
       bty='n',ax=F)
  
  title(main='A: individual training reaches', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='adaptation [°]', line = 2.5)
  
  
  for (ID in unique(adpt$participant)) {
    
    sdf <- adpt[which(adpt$participant == ID),]
    lines(x = adpt$trialno[which(adpt$participant == ID)],
          y = adpt$reachdeviation_deg[which(adpt$participant == ID)],
          col = rgb(127, 0,   216,  66, max = 255))
    
  }
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  # avg <- aggregate(adpt$reachdeviation_deg ~ adpt$trialno, data=adpt, FUN=mean)
  # lines(avg,
  #       col='#000',lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
  
  
  plot(NULL,NULL,
       main='',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60),
       bty='n',ax=F)
  
  title(main='C: individual no-cursors', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='implicit [°]', line = 2.5)
  
  
  
  for (ID in unique(impl$participant)) {
    
    sdf <- impl[which(impl$participant == ID),]
    lines(x = impl$trialno[which(impl$participant == ID)],
          y = impl$reachdeviation_deg[which(impl$participant == ID)],
          col = rgb(96,   96,  255, 66, max = 255))
    
  }
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  # avg <- aggregate(impl$reachdeviation_deg ~ impl$trialno, data=impl, FUN=mean)
  # lines(avg,
  #       col=rgb(229, 22,  54,  255, max = 255),lty=1,lw=2)
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
    
  
  plot(NULL,NULL,
       main='',xlab='',ylab='',
       xlim=c(13,52),ylim=c(-15,60), 
       bty='n',ax=F)
  
  title(main='E: individual aiming', line=0.25, adj=0)
  title(xlab='trial', line = 1.75)
  title(ylab='explicit [°]', line = 2.5)
  
  
  for (ID in unique(expl$participant)) {
    
    sdf <- expl[which(expl$participant == ID),]
    lines(x = expl$trialno[which(expl$participant == ID)],
          y = expl$aimingdeviation_deg[which(expl$participant == ID)],
          col = rgb(229, 22,  54,  66, max = 255))
    
  }
  
  lines(x=c(12,20,20,52),
        y=c(0,0,45,45),
        col='#999',lty=1,lw=2)
  
  
  axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  axis(side=2, at=c(0,45))
  
  
  
  df <- read.csv('data/expStepFits.csv', stringsAsFactors = FALSE)
  set.seed(1337)
  
  exp.colors  <- c('#0066FFFF', '#0066FF33')
  step.colors <- c('#FF6600FF', '#FF660033')
  
  
  for (process in c('adaptation', 'implicit', 'explicit')) {
    
    if (process %in% c('implicit','adaptation')) {
      YL <- c(0,300)
    } else {
      YL <- c(0,300)
    }
    
    

    plot(NULL,NULL,
         main='',xlab='',ylab='',
         xlim=c(0.5,2.5),ylim=YL,
         bty='n',ax=F)
    
    main = c('adaptation' = 'B',
             'implicit'   = 'D',
             'explicit'   = 'F')[process]
    title(main=main, line=0.25, adj=0)
    title(xlab='function', line = 1.75)
    title(ylab='RMSE', line = 2.5)
    
    
    sdf <- df[which(df$process == process),]
    MSEstep <- sdf$step.MSE
    MSEexp  <- sdf$exp.MSE
    
    # EXPONENTIAL FIT
    points(x=rep(0.75,length(MSEexp)),
           y=MSEexp,
           pch=16, cex=2,
           col=exp.colors[2])
    
    avg <- mean(MSEexp)
    CI  <- Reach::getConfidenceInterval(MSEexp,method='b')
    
    polygon(x=c(1,1.25,1.25,1),
            y=rep(c(CI[1],CI[2]),each=2),
            border=NA,
            col=exp.colors[2])
    lines(x=c(1,1.25),
          y=rep(avg,2),
          col=exp.colors[1])
    
    # STEP FUNCTION FIT
    
    points(x=rep(1.75,length(MSEstep)),
           y=MSEstep,
           pch=16, cex=2,
           col=step.colors[2])
    
    avg <- mean(MSEstep)
    CI  <- Reach::getConfidenceInterval(MSEstep,method='b')
    
    polygon(x=c(2,2.25,2.25,2),
            y=rep(c(CI[1],CI[2]),each=2),
            border=NA,
            col=step.colors[2])
    lines(x=c(2,2.25),
          y=rep(avg,2),
          col=step.colors[1])
    
    
    # lines(x=c(1,2),
    #       y=rep(YL[2]*0.85,2),
    #       col='#000',lty=1)
    text(x=1.5,y=YL[2]*0.9,adj=0.5,
         # labels=c('adaptation'='~= (BF 2.4)',
         #          'implicit'='< (BF 10.7)',
         #          'explicit'='> (BF 7.3)')[process],
         labels=c('adaptation'='~=',
                  'implicit'='<',
                  'explicit'='>')[process],
         col='#000', cex=relfontsize*1.5)
    
    axis(side=2,at=YL)
    # if (process %in% c('implicit','adaptation')) {
    #   axis(side=2,at=seq(0,YL))
    # } else {
    #   axis(side=2,at=seq(0,400,200))
    # }
    
    axis(side=1, at=c(1,2), labels=c('exp.','step'))
    
  }
  
  
  
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}


# NCM 2024 figures -----

NCMfigure1 <- function(target='inline', timecoursemode='absolute') {
  
  width=10.5
  height=7.5
  
  outfilename <- 'doc/posters/NCM2024/fig1'
  if (target == 'svg') {
    svglite::svglite( filename = sprintf('%s.svg',outfilename),
                      width = width,
                      height = height,
                      fix_text_size = FALSE)
  }
  if (target == 'pdf') {
    pdf( file = sprintf('%s.pdf', outfilename),
         width=width,
         height=height)
  }
  
  layout(mat=matrix(c(1:16), ncol=4, nrow=4, byrow=TRUE), widths = c(3,3,2,2))
  relfontsize <- 0.8
  
  par(mar=c(3.2,3.5,2,0.1),
      cex.axis=relfontsize, 
      cex.lab=relfontsize,
      cex.main=relfontsize*1.5,
      xpd=TRUE)
  
  
  for (exp in c(1:4)) {
    
    info <- groupInfo()
    conditions <- expConditions(exp)
    
    if (exp > 1) { ntrials = 120; mrot=45; xticks=c(1,21,120) } else { ntrials = 144; mrot=60; xticks=c(1,21,121,144) }
    
    
    plot(x=-1000, y=-1000,
         main='',xlab='',ylab='',
         xlim=c(0,ntrials+1),ylim=c(-15,mrot+15),
         ax=F,bty='n')
    
    # title(main='A: training reaches', line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    if (exp == 1) { title(main='training reaches', line=0.25, adj=0) }
    
    
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
      
      legend(x=c(121,23,23,23)[exp],
             y=c(mrot,15,15,15)[exp],
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
    
    # title(main=main, line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    if (exp == 1) { title(main='no-cursor reaches', line=0.25, adj=0) }
    
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
      color[3] <- 'gray'
      ltys <- c(1,1,2)
      
      legend(x=23,y=15,
             legend=legends,
             col=color,
             lty=ltys, bty='n',
             cex=relfontsize)
      
    }
    

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
    
    
    
    main <- 'D: implicit timecourse'
    if (exp == 4) {
      main <- 'D: implicit timecourse'
    }
    
    plot(-1000,-1000,
         main='',xlab='',ylab='',
         xlim=c(0,30),ylim=ylim,
         ax=F,bty='n'
    )
    
    # title(main=main, line=0.25, adj=0)
    title(xlab='trial', line = 1.75)
    title(ylab='deviation [°]', line = 2.5)
    if (exp == 1) { title(main='implicit fit', line=0.25, adj=0) }
    
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
    
    # title(main=main, line=0.25, adj=0)
    title(xlab='explicit (aiming) [°]', line = 1.75)
    title(ylab='implicit (no-cursors) [°]', line = 2.5)
    if (exp == 1) { title(main='additivity', line=0.25, adj=0) }
    
    addImpExpScatters(conditions)
    
    addDensities(conditions, type='aiming', viewscale=c(1,12.5), offset=c(0,mrot+2.5))
    
    addDensities(conditions, type='nocursors', viewscale=c(12.5,1), flipXY=TRUE, offset=c(mrot+2.5,0))
    
    axis(side=1,at=seq(0,mrot,15))
    axis(side=2,at=seq(0,mrot,15))
    
    
    
    
  }
  
  if (target %in% c('svg','pdf')) {
    dev.off()
  }
  
}



NCMfigure2 <- function(target='inline') {
  
  # in inches:
  width = 5
  height = 5
  dpi = 300
  outfilename <- 'doc/posters/NCM2024/fig2'
  
  if (target == 'svg') {
    svglite::svglite( filename = sprintf('%s.svg',outfilename),
                      width = width,
                      height = height,
                      fix_text_size = FALSE)
  }
  # if (target == 'png') {
  #   png( filename = sprintf('%s.png',outfilename),
  #        width = width*dpi,
  #        height = height*dpi,
  #        res = dpi
  #   )
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
  #         height = height*dpi,
  #         res = dpi
  #   )
  # }
  
  layout( mat=matrix(c(1,2,5,3,4,6),nrow=3,ncol=2, byrow=FALSE) )
  
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
       ylim=c(0,0.6),
       ax=F,bty='n')
  
  # df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$lambda, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
  # lines(x=c(.5,10.5), y=rep(avg,2), col=Reach::colorAlpha(info$color[which(info$condition == 'control')], alpha=99), lty=2)
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    # avg <- quantile(df$lambda, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
    
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
  text(x=c(1:length(conditions)+0.5), 
       y=xtick_distance * 0.6,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=c(0,0.20,0.40,0.60),labels=c('0%','20%','40%','60%'),las=2)
  
  title(main='training rate of change', line=0.25, adj=0)
  title(ylab='rate of change [%]', line=3)
  
  # # # # # # # # # # # # # # # # # # #
  # reach training asymptote
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,50),
       ax=F,bty='n')
  
  
  # df <- read.csv('data/exp2/control_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$N0, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
  
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_reaches_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$N0, probs=c(0.025, 0.975))
    # avg <- quantile(df$N0, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'reaches')]
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  # axis(side=1,at=c(1:length(conditions)),labels=condition_labels)
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.5), 
       y=xtick_distance * 50,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,50,10),las=2)
  
  title(main='training asymptote', line=0.25, adj=0)
  title(ylab='asymptote [°]', line=3)
  
  
  # # # # # # # # # # # # # # # # # # #
  # no-cursor rate of change
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,0.6),
       ax=F,bty='n')
  
  
  # df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$lambda, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
  
  lines(x=c(.5,10.5), y=rep(avg,2), col='#99999999', lty=1)
  
  for (cond_idx in c(1:length(conditions))) {
    
    condition <- conditions[cond_idx]
    exp <- info$exp[which(info$condition == condition)]
    color <- info$color[which(info$condition == condition)]
    
    df <- read.csv(sprintf('data/exp%d/%s_nocursors_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    CI <- quantile(df$lambda, probs=c(0.025, 0.975))
    # avg <- quantile(df$lambda, probs=c(0.5))
    df <- read.csv(sprintf('data/exp%d/%s_individual_exp-fits.csv', exp, condition), stringsAsFactors = FALSE)
    avg <- df$lambda[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
    
    
    polygon(x=c(-.4,.4,.4,-.4)+cond_idx,
            y=rep(CI, each=2),
            col=Reach::colorAlpha(color,alpha=22),
            border=NA)
    lines(x=c(-.4,.4)+cond_idx, y=rep(avg,2), col=color)
    
  }
  
  
  
  axis(side=1,at=c(1:length(conditions)),labels=rep('',length(conditions)))
  text(x=c(1:length(conditions)+0.5), 
       y=xtick_distance * 0.6,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=c(0,0.2,0.4,0.6),labels=c('0%','20%','40%','60%'),las=2)
  
  title(main='implicit rate of change', line=0.25, adj=0)
  title(ylab='rate of change [%]', line=3)
  
  
  
  # # # # # # # # # # # # # # # # # # #
  # no-cursor asymptote
  
  plot(x=-1000,y=-1000,
       main='',xlab='',ylab='',
       xlim=c(0.5,(length(conditions)+0.5)),
       ylim=c(0,50),
       ax=F,bty='n')
  
  
  
  # df <- read.csv('data/exp2/control_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  # avg <- quantile(df$N0, probs=c(0.5))
  df <- read.csv('data/exp2/control_individual_exp-fits.csv', stringsAsFactors = FALSE)
  avg <- df$N0[which(df$participant == 'all' & df$phase == 'learning' & df$trialtype == 'nocursors')]
  
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
  text(x=c(1:length(conditions)+0.5), 
       y=xtick_distance * 50,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,50,10),las=2)
  
  title(main='implicit asymptote', line=0.25, adj=0)
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
  text(x=c(1:length(conditions)+0.5), 
       y=xtick_distance * 30,
       labels=condition_labels,
       xpd=TRUE, 
       srt=25, 
       pos=2,
       cex=relfontsize)
  axis(side=2,at=seq(0,30,10),las=2)
  
  title(main='explicit extent', line=0.25, adj=0)
  title(ylab='aiming responses [°]', line=3)
  
  
  
  if (target %in% c('svg','png','pdf','tiff')) {
    dev.off()
  }
  
}

# subtractive method -----

getAvgSubtractiveData <- function() {
  
  grinfo <- groupInfo()
  
  subtractiveData <- NA
  
  for (condition in grinfo$condition) {
    
    if (grinfo$rotation[which(grinfo$condition == condition)] != 45) {
      next
    }
    
    implicit <- getImpExpEst(condition=condition, type='nocursors')
    names(implicit)[which(names(implicit) == 'depvar')] <- 'implicit'
    explicit <- getImpExpEst(condition=condition, type='aiming')
    names(explicit)[which(names(explicit) == 'depvar')] <- 'explicit'
    adaptation <- getImpExpEst(condition=condition, type='reaches')
    names(adaptation)[which(names(adaptation) == 'depvar')] <- 'adaptation'
    
    cdata <- merge(implicit, explicit, by='participant')
    cdata <- merge(cdata, adaptation, by='participant')
    cdata$condition <- condition
    
    if (is.data.frame(subtractiveData)) {
      subtractiveData <- rbind(subtractiveData, cdata)
    } else {
      subtractiveData <- cdata
    }
    
  }
  
  # the 45 degree participants from exp 1 also appear in the control group:
  subtractiveData <- subtractiveData[!duplicated(subtractiveData$participant),]
  
  
  return(subtractiveData)
  
  
}

plotSubtractiveMethod <- function() {
  
  info <- groupInfo()
  
  subtractiveData <- NA
  
  for (condition in info$condition) {
    
    if (info$rotation[which(info$condition == condition)] != 45) {
      next
    }
    
    implicit <- getImpExpEst(condition=condition, type='nocursors')
    names(implicit)[which(names(implicit) == 'depvar')] <- 'implicit'
    explicit <- getImpExpEst(condition=condition, type='aiming')
    names(explicit)[which(names(explicit) == 'depvar')] <- 'explicit'
    adaptation <- getImpExpEst(condition=condition, type='reaches')
    names(adaptation)[which(names(adaptation) == 'depvar')] <- 'adaptation'
    
    cdata <- merge(implicit, explicit, by='participant')
    cdata <- merge(cdata, adaptation, by='participant')
    cdata$condition <- condition
    
    if (is.data.frame(subtractiveData)) {
      subtractiveData <- rbind(subtractiveData, cdata)
    } else {
      subtractiveData <- cdata
    }
    
  }
  
  subtractiveData <- subtractiveData[!duplicated(subtractiveData$participant),]
  
  avgadapt <- mean(subtractiveData$adaptation, na.rm=TRUE)
  
  layout(mat=matrix(c(1,2),byrow=TRUE, ncol=2,nrow=1))
  
  plot(-1000, -1000,
       main='',
       xlab='explicit [aiming]', ylab='implicit [no-cursors]',
       xlim=c(-30,60), ylim=c(-30,60),
       bty='n', ax=FALSE,asp=1)
  
  lines(x=c(-20,60),y=c(0,0),col='#999999',lty=2)
  lines(x=c(0,0),y=c(-20,60),col='#999999',lty=2)
  
  lines(x=c(-20,40),y=(c(-20,40)*-1)+avgadapt,col='#999999',lty=2)
  
  
  points(subtractiveData$explicit, subtractiveData$implicit,
         pch=16, col='#99999944', cex=1.5)
  
  
  # lm with 95% CI:
  impl <- subtractiveData$implicit
  expl <- subtractiveData$explicit
  e2i <- lm(impl ~ expl)
  
  print(summary(e2i))
  
  slope_ci <- confint(e2i,parm='expl',level=0.95)
  print(slope_ci)
  
  at <- range(expl)
  
  coef <- e2i$coefficients
  lines(at, coef[1]+(at*coef[2]), col='red')
  
  ci <- predict( e2i,
                 newdata=data.frame(expl=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=Reach::colorAlpha('red'),border=NA)
  
  
  
  axis(side=1,at=seq(-20,60,20))
  axis(side=2,at=seq(-20,60,20))
  
  plot(-1000, -1000,
       main='',
       xlab='implicit [adaptation - aiming]', ylab='implicit [no-cursors]',
       xlim=c(-30,60), ylim=c(-30,60),
       bty='n', ax=FALSE,asp=1)
  
  lines(x=c(-20,60),y=c(0,0),col='#999999',lty=2)
  lines(x=c(0,0),y=c(-20,60),col='#999999',lty=2)
  lines(x=c(-20,60),y=c(-20,60),col='#999999',lty=2)
  
  points(subtractiveData$adaptation - subtractiveData$explicit, subtractiveData$implicit,
         pch=16, col='#99999944', cex=1.5)
  
  # lm with 95% CI:
  impl <- subtractiveData$implicit
  pred <- subtractiveData$adaptation - subtractiveData$explicit
  e2i <- lm(impl ~ pred)
  
  print(summary(e2i))
  
  slope_ci <- confint(e2i,parm='pred',level=0.95)
  print(slope_ci)
  
  at <- range(pred)
  
  coef <- e2i$coefficients
  lines(at, coef[1]+(at*coef[2]), col='red')
  
  ci <- predict( e2i,
                 newdata=data.frame(pred=seq(at[1],at[2],length.out=40)),
                 interval = "confidence")
  
  X <- c(seq(at[1],at[2],length.out=40),rev(seq(at[1],at[2],length.out=40)))
  Y <- c(ci[,'lwr'],rev(ci[,'upr']))
  polygon(x=X,y=Y,col=Reach::colorAlpha('red'),border=NA)
  
  
  axis(side=1,at=seq(-20,60,20))
  axis(side=2,at=seq(-20,60,20))
  
}


plotExplicitTimecourses <- function() {
  
  
  layout(mat=matrix(c(1:3),nrow=3,ncol=1))
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(15,45),ylim=c(-10,100),
       bty='n')
  
  df <- read.csv('data/exp4/aiming_aiming.csv', stringsAsFactors = FALSE)
  
  df <- baseline(df, depvar='aimingdeviation_deg')
  
  participants <- unique(df$participant)
  
  for (participant in participants) {
    
    pdf <- df[which(df$participant == participant),]
    lines(pdf$trialno, -1 * pdf$aimingdeviation_deg, col='#FF000066')
  }
  title(main='aiming')
  
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(15,45),ylim=c(-10,100),
       bty='n')
  
  df <- read.csv('data/exp4/aiming_nocursors.csv', stringsAsFactors = FALSE)
  
  df <- baseline(df, depvar='reachdeviation_deg')
  
  participants <- unique(df$participant)
  
  for (participant in participants) {
    
    pdf <- df[which(df$participant == participant),]
    lines(pdf$trialno, -1 * pdf$reachdeviation_deg, col='#FF000066')
  }
  title(main='nocursors')
  
  
  plot(-1000,-1000,
       main='',xlab='',ylab='',
       xlim=c(15,45),ylim=c(-10,100),
       bty='n')
  
  df <- read.csv('data/exp4/aiming_subtraction.csv', stringsAsFactors = FALSE)
  
  df <- baseline(df, depvar='subtraction')
  
  participants <- unique(df$participant)
  
  for (participant in participants) {
    
    pdf <- df[which(df$participant == participant),]
    lines(pdf$trialno, pdf$subtraction, col='#FF000066')
  }
  title(main='subtraction')
  
}

# bootstrapExponentialLearning(condition='aiming', type='subtraction', depvar='subtraction', iterations=5000)


plotSubtractiveBootstrapParameterDistributions <- function() {
  
  subtraction_fits <- read.csv('data/exp4/aiming_subtraction_exp-fits.csv', stringsAsFactors = FALSE)
  nocursor_fits <- read.csv('data/exp4/aiming_nocursors_exp-fits.csv', stringsAsFactors = FALSE)
  aiming_fits <- read.csv('data/exp4/aiming_aiming_exp-fits.csv', stringsAsFactors = FALSE)
  reach_fits <- read.csv('data/exp4/aiming_reaches_exp-fits.csv', stringsAsFactors = FALSE)
  
  subdens <- density(subtraction_fits$lambda, from=-0.2, to=1.2)
  nocdens <- density(nocursor_fits$lambda,    from=-0.2, to=1.2)
  aimdens <- density(aiming_fits$lambda,      from=-0.2, to=1.2)
  readens <- density(reach_fits$lambda,      from=-0.2, to=1.2)
  
  layout(mat=matrix(c(1:2), nrow=1, ncol=2))
  
  plot(-1000,-1000,
       main='', xlab='learning rate (lambda)', ylab='relative density',
       xlim=c(-0.2, 1.2), ylim=c(0,30.0),
       bty='n')
  
  lines(x=readens$x,
        y=readens$y,
        col='gray')
  
  lines(x=subdens$x,
        y=subdens$y,
        col='blue')
  
  lines(x=nocdens$x,
        y=nocdens$y,
        col='red')
  
  lines(x=aimdens$x,
        y=aimdens$y,
        col='green')
  
  legend(0.5,30,legend=c('subtraction', 'direct', 'aiming', 'adaptation'),lty=c(1,1),seg.len = 1.5, col=c('blue','red','green','gray'),bty='n')
  
  
  subdens <- density(subtraction_fits$N0, from=-10, to=50)
  nocdens <- density(nocursor_fits$N0,    from=-10, to=50)
  aimdens <- density(aiming_fits$N0,      from=-10, to=50)
  readens <- density(reach_fits$N0,       from=-10, to=50)
  
  plot(-1000,-1000,
       main='', xlab='asymptote (N0)', ylab='relative density',
       xlim=c(-10, 50), ylim=c(0,0.4),
       bty='n')
  
  lines(x=readens$x,
        y=readens$y,
        col='gray')
  
  lines(x=subdens$x,
        y=subdens$y,
        col='blue')
  
  lines(x=nocdens$x,
        y=nocdens$y,
        col='red')
  
  lines(x=aimdens$x,
        y=aimdens$y,
        col='green')
  
  
}

subtractionPairwiseExploration <- function() {
  
  idf <- read.csv('data/exp4/aiming_individual_exp-fits.csv', stringsAsFactors = FALSE)
  
  idf <- idf[which(idf$participant != 'all'),]
  idf <- idf[,c('participant','lambda','N0','trialtype')]
  
  alldf <- NA
  
  for (trialtype in unique(idf$trialtype)) {
    
    subdf <- idf[which(idf$trialtype == trialtype),]
    subdf <- subdf[,c('participant','lambda','N0')]
    names(subdf) <- c('participant',sprintf('%s_lambda',trialtype), sprintf('%s_N0',trialtype))
    if (is.data.frame(alldf)) {
      alldf <- merge(alldf, subdf)
    } else {
      alldf <- subdf
    }
    
  }
  # return(alldf)
  
  pairs(alldf[,c(2,3,6,7)])

}
