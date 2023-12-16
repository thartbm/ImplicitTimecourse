library('osfr')
library('Reach')


`%notin%` <- Negate(`%in%`)


downloadData <- function() {
  
  filelist <- list('\\' = c('demographics.csv', 'meta_info.csv', 'exp1.zip', 'exp2.zip', 'exp3.zip', 'exp4.zip') )
  
  Reach::downloadOSFdata( repository = 'ajwyr',
                          filelist   = filelist,
                          folder     = 'data',
                          unzip      = TRUE,
                          removezips = TRUE )
  
}

# basic pre-processing -----

extractAngularDeviations <- function() {
  
  distance <- 0.2
  
  extractReachDeviations(distance=distance)
  extractNoCursorDeviations(distance=distance)
  extractAimingDeviations()
  
}

extractReachDeviations <- function(distance=0.2) {
  
  info <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cond_df <- NA
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype != 0),]
        
        # print(c(condition, participant, unique(ppdf$phase)))
        # print(dim(ppdf))
        
        reachdeviation_deg <- c()
        for (trialno in c(1:dim(ppdf)[1])) {
          reachdeviation_deg <- c(reachdeviation_deg, getReachDev(target=ppdf$target[trialno],
                                                                  X=convertCellToNumVector(ppdf$mousex_rel[trialno]),
                                                                  Y=convertCellToNumVector(ppdf$mousey_rel[trialno]),
                                                                  distance=distance)) 
        }
        
        
        pdf <- data.frame(exp                = rep(exp,dim(ppdf)[1]),
                          condition          = rep(condition,dim(ppdf)[1]),
                          participant        = rep(participant,dim(ppdf)[1]),
                          trialno            = c(1:dim(ppdf)[1]),
                          target             = ppdf$target,
                          rotation_deg       = ppdf$rotation_deg,
                          reachdeviation_deg = reachdeviation_deg,
                          points             = ppdf$points,
                          phase              = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/exp%d/%s_reaches.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

extractNoCursorDeviations <- function(distance=0.2) {
  
  info <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cond_df <- NA
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype == 0),]
        
        # print(c(condition, participant, unique(ppdf$phase)))
        # print(dim(ppdf))
        
        reachdeviation_deg <- c()
        for (trialno in c(1:dim(ppdf)[1])) {
          reachdeviation_deg <- c(reachdeviation_deg, getReachDev(target=ppdf$target[trialno],
                                                                  X=convertCellToNumVector(ppdf$mousex_rel[trialno]),
                                                                  Y=convertCellToNumVector(ppdf$mousey_rel[trialno]),
                                                                  distance=distance)) 
        }
        
        
        pdf <- data.frame(exp                = rep(exp,dim(ppdf)[1]),
                          condition          = rep(condition,dim(ppdf)[1]),
                          participant        = rep(participant,dim(ppdf)[1]),
                          trialno            = c(1:dim(ppdf)[1]),
                          target             = ppdf$target,
                          rotation_deg       = ppdf$rotation_deg,
                          reachdeviation_deg = reachdeviation_deg,
                          phase              = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/exp%d/%s_nocursors.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

extractAimingDeviations <- function() {
  
  info <- read.csv('data/demographics.csv', stringsAsFactors = FALSE)
  
  for (exp in unique(info$exp)) {
    
    for (condition in unique(info$condition_label[which(info$exp == exp)])) {
      
      cond_df <- NA
      
      for (participant in info$participant[which(info$exp == exp & info$condition_label == condition)]) {
        
        infilename <- sprintf('data/exp%d/%s/%s.csv', exp, condition, participant)
        ppdf <- read.csv(infilename, stringsAsFactors = FALSE)
        
        # get rid of practice phase:
        ppdf <- ppdf[which(ppdf$phase %notin% c('practice','counter','errorclamp')),]
        
        # only keep feedback reaches:
        ppdf <- ppdf[which(ppdf$trialtype == 1),]
        
        ppdf$trialno <- c(1:dim(ppdf)[1])
        
        ppdf <- ppdf[which(ppdf$aiming_type == 1),]
        
        ppdf$aimingdeviation_deg <- ppdf$aiming_direction - ppdf$aiming_target
        
        # correct impossible responses:
        while (any(ppdf$aimingdeviation_deg > 180)) {
          ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg > 180)] <- ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg > 180)] - 360
        }
        while (any(ppdf$aimingdeviation_deg < -180)) {
          ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg < -180)] <- ppdf$aimingdeviation_deg[which(ppdf$aimingdeviation_deg < -180)] + 360
        }
        
        pdf <- data.frame(exp                 = rep(exp,dim(ppdf)[1]),
                          condition           = rep(condition,dim(ppdf)[1]),
                          participant         = rep(participant,dim(ppdf)[1]),
                          trialno             = ppdf$trialno,
                          aiming_target       = ppdf$aiming_target,
                          rotation_deg        = ppdf$rotation_deg,
                          aimingdeviation_deg = ppdf$aimingdeviation_deg,
                          arrow_offset        = ppdf$arrow_offset,
                          phase               = ppdf$phase)
        
        if (is.data.frame(cond_df)) {
          cond_df <- rbind(cond_df, pdf)
        } else {
          cond_df <- pdf
        }
        
      }
      
      outfilename <- sprintf('data/exp%d/%s_aiming.csv', exp, condition)
      write.csv(cond_df, outfilename, row.names=FALSE, quote=TRUE)
      
    }
  }
  
}

# utility functions -----

convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}


getReachDev <- function(target, X, Y, distance=0.2) {
  
  theta <- -1*(target/180)*pi
  R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)
  relx <- convertCellToNumVector(X)
  rely <- convertCellToNumVector(Y)
  d <- sqrt(relx^2 + rely^2)
  idx <- which(d > distance)[1]
  xy <- matrix(c(relx[idx],rely[idx]), nrow=2, ncol=1)
  npos <- R %*% xy
  
  reach_dev <- (atan2(npos[2],npos[1])/pi)*180
  
  return(reach_dev)
  
}

