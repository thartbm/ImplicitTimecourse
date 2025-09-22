
# plotting functions ----

setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  if (target == 'pdf') {
    pdf(file   = filename, 
        width  = width, 
        height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    png( filename = filename,
         width = width*dpi,
         height = height*dpi,
         res = dpi
    )
  }
  if (target == 'tiff') {
    tiff( filename = filename,
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
}




hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # code below, somewhat based on:
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  }
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = T, left.open = F, all.inside = F)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = T, left.open = F, all.inside = F)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  dimnames( freq2D ) <- c()
  rownames( freq2D ) <- c()
  colnames( freq2D ) <- c()
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}

getColorPalette <- function(n=100, bg='#FFFFFF', fg=rgb(229, 22,  54,  255, max = 255)) {
  
  # get a color palette with n colors, starting from the background color
  # and ending with the foreground color
  
  # bg <- '#FFFFFF'
  # fg <- rgb(229, 22,  54,  255, max = 255)
  
  pal <- grDevices::colorRampPalette(c(bg, fg))(n)
  
  return(pal)
  
  
}



# D'Amario data -----

plotDAmario_exp4 <- function(add=FALSE) {
  
  if (!add) {
    layout(mat=matrix(c(1:3),nrow=3,ncol=1,byrow=TRUE))
    par(mar=c(4.1,4,1.5,0.1))
  }
  
  explicit <- read.csv('data/exp4/aiming_aiming.csv', stringsAsFactors = F)
  explicit$aimingdeviation_deg <- -1*explicit$aimingdeviation_deg
  implicit <- read.csv('data/exp4/aiming_nocursors.csv', stringsAsFactors = F)
  implicit$reachdeviation_deg <- -1*implicit$reachdeviation_deg
  adaptation <- read.csv('data/exp4/aiming_reaches.csv', stringsAsFactors = F)
  adaptation$reachdeviation_deg <- -1*adaptation$reachdeviation_deg
  
  # trials: pre = 13-20, post = 21-52
  
  expl <- explicit[   which(explicit$trialno   %in% c(13:52)), ]
  impl <- implicit[   which(implicit$trialno   %in% c(13:52)), ]
  adpt <- adaptation[ which(adaptation$trialno %in% c(13:52)), ]
  
  # ADAPTATION 1 (should be lines)
  
  plot(x=1000,y=-1000,
       main='adaptation',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
  
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
  
  
  # IMPLICIT 1 (should be lines)
  
  plot(x=1000,y=-1000,
       main='implicit',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60),
       ax=F,bty='n')
  
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
  
  
  
  # EXPLICIT 1 should be (lines)
  
  plot(x=1000,y=-1000,
       main='explicit',xlab='trial',ylab='deviation [°]',
       xlim=c(13,52),ylim=c(-15,60), 
       ax=F,bty='n')
  
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
  
  
  # expl <- expl[,c('trialno','aimingdeviation_deg')]
  # names(expl) <- c('x','y')
  

  
  # impl <- impl[,c('trialno','reachdeviation_deg')]
  # names(impl) <- c('x','y')
  
  

  # plot(x=1000,y=-1000,
  #      main='implicit',xlab='trial',ylab='deviation [°]',
  #      xlim=c(13,52),ylim=c(-15,60), 
  #      ax=F,bty='n')
  # 
  # img_info <- hist2d(x=impl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  # img <- img_info$freq2D
  # 
  # img <- log(img + 1)
  # 
  # image(x=img_info$x.edges,
  #       y=img_info$y.edges,
  #       z=img,
  #       add=TRUE,
  #       col=getColorPalette(fg=rgb(96,   96,  255, 255, max = 255)))
  # 
  # lines(x=c(12,20,20,52),
  #       y=c(0,0,45,45),
  #       col='#999',lty=1,lw=2)
  # 
  # avg <- aggregate(y ~ x, data=impl, FUN=mean)
  # lines(avg,
  #       col=rgb(229, 22,  54,  255, max = 255),lty=1,lw=2)
  # 
  # axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  # axis(side=2, at=c(0,45))
  
  
  
  # adpt <- adpt[,c('trialno','reachdeviation_deg')]
  # names(adpt) <- c('x','y')
  # 
  # plot(x=1000,y=-1000,
  #      main='adaptation',xlab='trial',ylab='deviation [°]',
  #      xlim=c(13,52),ylim=c(-15,60), 
  #      ax=F,bty='n')
  # 
  # img_info <- hist2d(x=adpt, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  # img <- img_info$freq2D
  # 
  # img <- log(img + 1)
  # 
  # image(x=img_info$x.edges,
  #       y=img_info$y.edges,
  #       z=img,
  #       add=TRUE,
  #       col=getColorPalette(fg=rgb(127, 0,   216, 255, max = 255)))
  # 
  # lines(x=c(12,20,20,52),
  #       y=c(0,0,45,45),
  #       col='#999',lty=1,lw=2)
  # 
  # avg <- aggregate(y ~ x, data=adpt, FUN=mean)
  # lines(avg,
  #       col='#000',lty=1,lw=2)
  # 
  # axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  # axis(side=2, at=c(0,45))
  
  # plot(x=1000,y=-1000,
  #      main='explicit',xlab='trial',ylab='deviation [°]',
  #      xlim=c(13,52),ylim=c(-15,60), 
  #      ax=F,bty='n')
  # 
  # img_info <- hist2d(x=expl, nbins=NA, edges=list(seq(12.5,52.5,1),seq(-15,60,2.5)))
  # img <- img_info$freq2D
  # 
  # img <- log(img + 1)
  # 
  # image(x=img_info$x.edges,
  #       y=img_info$y.edges,
  #       z=img,
  #       add=TRUE,
  #       col=getColorPalette())
  # 
  # lines(x=c(12,20,20,52),
  #       y=c(0,0,45,45),
  #       col='#999',lty=1,lw=2)
  # 
  # avg <- aggregate(y ~ x, data=expl, FUN=mean)
  # lines(avg,
  #       col='#66F',lty=1,lw=2)
  # 
  # axis(side=1, at=c(12, 20, 28, 36, 44, 52), labels=c(-8,0,8,16,24,32))
  # axis(side=2, at=c(0,45))
  
  
  
}

stepFunction <- function(par, trials) {
  
  if (length(trials) == 1) {
    trials <- c(0:(trials-1))
  }
  
  predictions <- rep(0, length(trials))
  
  predictions[which(trials >= par['t'])] <- par['s']
  
  return(predictions)
  
}


# plotStepSizeDistribution <- function(add=FALSE) {
#   
#   df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
#   
#   explSteps <- df[which(df$process == 'explicit'),]
#   
#   
#   plot(NULL,NULL,
#        main='fitted step functions',xlab='trial',ylab='deviation [°]',
#        xlim=c(-8,33),ylim=c(-15,60),
#        ax=F,bty='n',)
#   
#   for (participant in explSteps$participant) {
#     
#     par <- c('t'=explSteps$step.t[which(explSteps$participant == participant)],
#              's'=explSteps$step.s[which(explSteps$participant == participant)])
#     
#     trials=c(-8:32)
#     
#     # step function
#     predictions <- stepFunction(par    = par,
#                                 trials = trials)
#     
#     lines(x=trials, y=predictions, col='#6666FF33', lw=2)
#     
#   }
#   
#   lines(x=c(-8,0,0,32),
#         y=c(0,0,45,45),
#         col='#999',lty=2,lw=2)
#   
#   
#   stepsizes <- seq(-10,55,1)
#   
#   stepdens <- density(x=explSteps$step.s,
#           bw='nrd0',
#           from=min(stepsizes),
#           to=max(stepsizes),
#           adjust=1,
#           kernel='gaussian',
#           n=length(stepsizes))
#   
#   
#   polygon(x=(c(0,stepdens$y,0)*100)-8,
#           y=c(min(stepsizes),stepsizes, max(stepsizes)),
#           col='#0066FF33',
#           border=NA)
#   lines(x=(stepdens$y*100)-8,
#         y=stepsizes,
#         col='#0066FF', lw=2, lty=2)
#   
#   
#   axis(side=1, at=c(-8,0,8,16,24,32))
#   axis(side=2, at=c(0,45))
#   
# }

# plotStepTimeDistribution <- function(add=FALSE) {
#   
#   df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
#   
#   explSteps <- df[which(df$process == 'explicit'),]
#   
#   # only those with a strategy larger than 5 degrees in the right direction
#   # this excludes 5 / 47 participants (10.6%)
#   explSteps <- explSteps[which(explSteps$step.s > 5),]
#   
#   # plot the distribution of step times
#   stepdistr <- ecdf(round(explSteps$step.t))
#   
#   X <- c(-8,1,knots(stepdistr))
#   Y <- stepdistr(X)
#   
#   plot(NULL,NULL,
#        main='step time distribution',
#        xlab='trial',
#        ylab='probability density',
#        xlim=c(-8,32),ylim=c(0,0.16),
#        ax=F,bty='n')
#   
#   # plot the empirical distribution
#   # lines(x=X, y=Y, col='#666666', lw=2)
#   
#   empdens <- density(x=explSteps$step.t,
#                   bw=1,
#                   from=-8,
#                   to=32,
#                   adjust=1,
#                   kernel='gaussian',
#                   n=1000)
#   lines(x=empdens$x, y=empdens$y, col='#666666', lw=2)
#   
#   
#   # fit poisson, using ppois (cumulative ppois distribution function)
#   x <- knots(stepdistr)
#   y <- stepdistr(x)
#   x <- x-0
#   # fit the model using optim
#   fit <- optim(par=c(1),
#                fn=MSE.ppois,
#                x=x,
#                y=y,
#                method="L-BFGS-B",
#                lower=c(0),
#                upper=c(60),
#                control=list(maxit=1000))
#   
#   xp <- seq(-8,32,1)
#   yp <- dpois(xp, lambda=fit$par)
#   
#   lines(x=xp, y=yp, col='#FF6600', lw=2, lty=2)
#     
#   # fit gamma using pgamma (cumulative pgamma distribution function)
#   # fit the model using optim
#   fit <- optim(par=c(1,1),
#                fn=MSE.pgamma,
#                x=x,
#                y=y,
#                method="L-BFGS-B",
#                lower=c(0,0),
#                upper=c(60,60),
#                control=list(maxit=1000))
#   
#   xp <- seq(-8,32,0.01)
#   yp <- dgamma(xp, shape=fit$par[1], scale=fit$par[2])
#   
#   # print(fit$par)
#   
#   lines(x=xp, y=yp, col='#0066FF', lw=2, lty=2)
#   
#   legend( x=16,
#           y=0.15,
#           legend=c('empirical','poisson','gamma'),
#           col=c('#666666','#FF6600','#0066FF'),
#           lw=2, lty=c(1,2,2),
#           bty='n')
#   
#   axis(side=1, at=c(-8,0,8,16,24,32))
#   axis(side=2, at=c(0,0.16))
#   
# }

# MSE.ppois <- function(par, x, y) {
#   
#   # par is the parameter of the poisson distribution
#   # x is the x-axis of the empirical distribution
#   # y is the y-axis of the empirical distribution
#   
#   # calculate the MSE between the empirical and theoretical distribution
#   mse <- sum((y - ppois(x, lambda=par))^2)
#   
#   return(mse)
#   
# }

# MSE.pgamma <- function(par, x, y) {
#   
#   # par is the parameter of the gamma distribution
#   # x is the x-axis of the empirical distribution
#   # y is the y-axis of the empirical distribution
#   
#   # calculate the MSE between the empirical and theoretical distribution
#   mse <- sum((y - pgamma(x, shape=par[1], scale=par[2]))^2)
#   
#   return(mse)
#   
# }


# plotSimulatedStrategyTimecourse <- function(add=FALSE) {
#   
#   df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
#   
#   explSteps <- df[which(df$process == 'explicit'),]
#   
#   sizes <- explSteps$step.s
#   times <- round(explSteps[which(explSteps$step.s > 5),]$step.t)
#   
#   bootstraps <- 300
#   
#   bs.sizes <- sample(sizes, size=bootstraps, replace=TRUE)
#   # bs.times <- sample(times, size=bootstraps, replace=TRUE)
#   
#   # plot(hist(bs.times))
#   
#   # get fitted time distribution:
#   explSteps <- explSteps[which(explSteps$step.s > 5),]
#   
#   # plot the distribution of step times
#   stepdistr <- ecdf(round(explSteps$step.t))
#   X <- c(-8,1,knots(stepdistr))
#   Y <- stepdistr(X)
#   
#   gfit <- optim(par=c(1,1),
#                fn=MSE.pgamma,
#                x=X,
#                y=Y,
#                method="L-BFGS-B",
#                lower=c(0,0),
#                upper=c(60,60),
#                control=list(maxit=1000))
#   
#   # xp <- seq(-8,32,0.01)
#   # yp <- dgamma(xp, shape=fit$par[1], scale=fit$par[2])
#   
#   bs.times <- rgamma(bootstraps, shape=gfit$par[1], scale=gfit$par[2])
#   
#   
#   trials <- c(-8:32)
#   
#   timecourses <- NA
#   
#   for (bs in c(1:bootstraps)) {
#     
#     par=c('t'=bs.times[bs], 
#           's'=bs.sizes[bs])
#     
#     rgamma(1, shape=gfit$par[1], scale=gfit$par[2])
#     
#     # print(par)
#     
#     timecourse <- stepFunction( par=par, 
#                                 trials=trials)
#     timecourse <- timecourse + rnorm(length(trials), mean=0, sd=3.5)
#     
#     ndf <- data.frame('x'=trials, 'y'=timecourse)
#     if (is.data.frame(timecourses)) {
#       timecourses <- rbind(timecourses, ndf)
#     } else {
#       timecourses <- ndf
#     }
#     
#   }
#   
#   # print(timecourses)
#   
#   plot(NULL,NULL,
#        main='simulated strategy',xlab='trial',ylab='deviation [°]',
#        xlim=c(-8,32),ylim=c(-15,60),
#        ax=F,bty='n')
#   
#   img_info <- hist2d(x=timecourses, nbins=NA, edges=list(seq(-8.5,32.5,1),seq(-15,60,2.5)))
#   img <- img_info$freq2D
#   
#   # img <- log(img + 1)
#   
#   image(x=img_info$x.edges,
#         y=img_info$y.edges,
#         z=img,
#         add=TRUE,
#         col=getColorPalette())
#   
#   lines(x=c(-8,0,0,32),
#         y=c(0,0,45,45),
#         col='#999',lty=1,lw=2)
#   
#   avg <- aggregate(y ~ x, data=timecourses, FUN=mean)
#   lines(avg,
#         col='#66F',lty=1,lw=2)
#   
#   axis(side=1, at=c(-8,0,8,16,24,32))
#   axis(side=2, at=c(0,45))
#   
# }


plotImplicitMSE <- function(add=FALSE) {
  
  
  df <- read.csv('data/implicit/expStepFits.csv', stringsAsFactors = FALSE)
  
  if (!add) {
    layout(mat=matrix(c(1:3),nrow=1,ncol=3,byrow=TRUE))
  }
  
  set.seed(1337)
  
  exp.colors  <- c('#0066FFFF', '#0066FF33')
  step.colors <- c('#FF6600FF', '#FF660033')
  
  for (process in c('adaptation', 'implicit', 'explicit')) {
    
    if (process %in% c('implicit','adaptation')) {
      YL <- c(0,3200)
    } else {
      YL <- c(0,400)
    }
    
    plot(-1000,-1000,main=process,
         xlab='function',ylab='MSE',
         xlim=c(0.5,2.5),ylim=YL,
         bty='n',ax=F)
    
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
    
    if (process %in% c('implicit','adaptation')) {
      axis(side=2,at=seq(0,3200,1600))
    } else {
      axis(side=2,at=seq(0,400,200))
    }
    
    axis(side=1, at=c(1,2), labels=c('exp.','step'))
    
  }
  
}



# new data sets ----


loadNewData <- function(rotations=c(20,30,40,50,60)) {
  
  df <- NA
  
  for (rotation in rotations) {
    
    rdf <- getRotationData(rotation=rotation)
    rdf$condition <- rotation
    
    if (is.data.frame(df)) {
      df <- rbind(df, rdf)
    } else {
      df <- rdf
    }
    
  }
  
  return(df)
  
}


getRotationData <- function(rotation) {
  
  path <- sprintf('data/phase_2/aiming%d/',rotation)
  
  allcsvfiles <- list.files(path=path, pattern='*.csv')
  
  all_data <- NA
  
  for (file in allcsvfiles) {

    csvfile <- sprintf('%s%s', path, file)

    df <- read.csv(csvfile, stringsAsFactors = FALSE)
    
    # since participants had some differences in paradigms,
    # here we select the relevant part for each of them:
    
    if (length(unique(df$task_idx)) == 9) {
      # old paradigm
      task_idx <- c(7,8)
    }
    if (length(unique(df$task_idx)) == 12) {
      # new paradigm
      task_idx <- c(11,12)
    }
    sdf <- df[which(df$task_idx %in% task_idx & df$trial_idx < 33),]
    
    sdf <- sdf[,c('task_idx','trial_idx','rotation_deg','reachdeviation_deg','aimdeviation_deg')]
    
    sdf$participant <- substr(file, 18, 23)
    
    sdf$trialno <- c(1:dim(sdf)[1])-9
    
    if (is.data.frame(all_data)) {
      all_data <- rbind(all_data, sdf)
    } else {
      all_data <- sdf
    }
    
  }
  
  return(all_data)
  
}





plotNewData <- function(add=FALSE) {
  
  df <- loadNewData()
  
  if (!add) {
    layout(mat=matrix(c(1:5),ncol=1,byrow=TRUE))
  }
  
  par(mar=c(4.1,4,1.5,0.1))
  
  for (rotation in c(20,30,40,50,60)) {
    
    sdf <- df[which(df$condition == rotation),]
    
    N <- length(unique(sdf$participant))
    
    plot(x=1000,y=-1000,
         main=sprintf('rotation %d (N=%d)',rotation,N),xlab='trial',ylab='deviation [°]',
         xlim=c(-8,32),ylim=c(-10,70), 
         ax=F,bty='n')
    
    sdf <- sdf[,c('trialno','aimdeviation_deg')]
    names(sdf) <- c('x','y')
    
    img_info <- hist2d(x=sdf, nbins=NA, edges=list(seq(-8.5,32.5,1),seq(-10,70,2.5)))
    img <- img_info$freq2D
    
    img <- log(img + 1)
    
    image(x=img_info$x.edges,
          y=img_info$y.edges,
          z=img,
          add=TRUE,
          col=getColorPalette(fg=rgb(255, 147, 41,  127, max = 255)))
    
    lines(x=c(0,8,8,40)-9,
          y=c(0,0,rotation,rotation),
          col='#999',lty=1,lw=2)
    
    avg <- aggregate(y ~ x, data=sdf, FUN=mean)
    lines(avg,
          col=rgb(127, 0,   216,  255, max = 255),lty=1,lw=2)
    
    axis(side=1, at=c(0, 8, 16, 24, 32, 40)-9, labels=c(-8,0,8,16,24,32))
    axis(side=2, at=c(0,rotation))
    
  }
  
  
}


unifiedPosterPlot <- function() {
  
  setupFigureFile(target='pdf',width=20,height=10,dpi=300,filename='doc/NCM_plots.pdf')
  
  layout(mat=matrix(c(18,
                      18,
                       1,
                       2,
                       4,
                      
                      18,
                      18,
                      19,
                       3,
                       5,
                      
                       6,
                       7,
                       8,
                       9,
                       10,

                      6,
                      7,
                      8,
                      9,
                      11,

                      6,
                      7,
                      8,
                      9,
                      12,

                      13,
                      14,
                      15,
                      16,
                      17 ),ncol=6,nrow=5,byrow=FALSE),
         
         widths = c(3,3,1,1,1,3),
         height = c(2,2,2,2,2))
  
  par(mar=c(4.1,4,1.5,0.1))
  
  plotDAmario_exp4(add=TRUE)
  #plotStepSizeDistribution(add=TRUE)
  #plotStepTimeDistribution(add=TRUE)
  #plotSimulatedStrategyTimecourse(add=TRUE)
  plotImplicitMSE(add=TRUE)
  #plotNewData(add=TRUE)
  
  dev.off()
  
}

