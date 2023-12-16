

# Qualtrics ------

readQualtricsData <- function(exp=1, part='intake') {
  
  if (exp == 1) {
    if (part == 'intake') { file <- 'fast+forward+lab+intake_December+14,+2023_18.07.csv' }
    if (part == 'task')   { file <- 'fast+forward+lab+task_December+14,+2023_18.07.csv' }
  }
  if (exp == 2) {
    if (part == 'intake') { file <- 'new+dataset+fast+forward+lab+intake_December+14,+2023_18.08.csv' }
    if (part == 'task')   { file <- 'new+dataset+fast+forward+lab+task_December+14,+2023_18.08.csv' }
  }
  
  filepath <- sprintf('data//new//%s',file)
  
  if (part == 'intake') {
    columnnames <- c('IntakeDate', 'EndDate', 'Status', 'IPAddress',	'Progress_intake', 'Duration (in seconds)', 'Finished_intake', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group_intake')
    use_cols <- c('IntakeDate', 'Progress_intake', 'Finished_intake', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'id', 'group_intake')
  }
  if (part == 'task') {
    if (exp == 1) {
      columnnames <- c('TaskDate', 'EndDate', 'Status', 'IPAddress',	'Progress_task', 'Duration (in seconds)', 'Finished_task', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'id', 'group_task', 'condition')
    }
    if (exp == 2) {
      columnnames <- c('TaskDate', 'EndDate', 'Status',	'IPAddress',	'Progress_task', 'Duration (in seconds)',	'Finished_task', 'RecordedDate', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 'ExternalReference', 'LocationLatitude', 'LocationLongitude', 'DistributionChannel', 'UserLanguage', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'Q11_1', 'Q13_1', 'Q14_1', 'Q15_1', 'Q16_1', 'Q17_1', 'Q18_1', 'id',	'group_task',	'condition')
    }
    use_cols <- c('TaskDate','Progress_task', 'Finished_task', 'needs_vision_correction', 'has_vision_correction', 'strategy_verbalization', 'id', 'group_task', 'condition')
  }
  
  df <- read.csv(filepath)
  df <- df[c(3:dim(df)[1]),]
  names(df) <- columnnames
  
  write.csv(df, 'data/temp.csv', row.names = FALSE)
  df <- read.csv('data/temp.csv', stringsAsFactors = FALSE)
  
  
  return(df[,use_cols])
  
  
}

processQualtrics <- function(exp=1) {
  
  # read the data:
  df_intake <- readQualtricsData(exp=exp, part='intake')
  df_task <- readQualtricsData(exp=exp, part='task')
  
  # remove empty IDs:
  df_intake <- df_intake[which(df_intake$id != ""),]
  df_taks <- df_task[which(df_task$id != ""),]
  
  # remove id's with the word 'test' or 'demo' in it
  df_intake <- df_intake[which(!grepl('test', df_intake$id, fixed=TRUE)),]
  df_intake <- df_intake[which(!grepl('demo', df_intake$id, fixed=TRUE)),]
  df_task <- df_task[which(!grepl('test', df_task$id, fixed=TRUE)),]
  df_task <- df_task[which(!grepl('demo', df_task$id, fixed=TRUE)),]
  
  # remove people who did not provide consent, or finished all parts of the experiment:
  df_intake <- df_intake[which(df_intake$consent == 'I agree to participate in the study'),]
  df_intake <- df_intake[which(df_intake$Finished_intake == 'True'),]
  df_task <- df_task[which(df_task$Finished_task == 'True'),]
  
  # remove people who did not have normal or corrected-to-normal vision:
  df_task[which(df_task$needs_vision_correction == 'No' | df_task$has_vision_correction == 'Yes'),]
  
  # remove columns we will not use:
  df_task <- df_task[,which(!colnames(df_task) %in% c('strategy_verbalization'))]
  df_intake <- df_intake[,which(!colnames(df_intake) %in% c('group_intake'))]
  
  # combine the two data frames:
  df <- merge(df_intake, df_task, by=c('id'))
  
  names(df)[which(names(df) == 'condition')] <- 'condition_q'
  
  return(df)
  
}

# Pavlovia sources -----

getPavloviaParticipants <- function(exp=1) {
  
  # we need to look in specific folders for every experiment: 
  if (exp == 1) {folders <- c('itc3tab_ffw')}
  # if (exp == 2) {folders <- c('itc6tab_dt')}
  if (exp == 2) {folders <- c('itc4tab_expl', 'itc5tab_rot', 'itc6tab_dt')}
  
  all_paths <- c()

  for (fold in folders) {
    # from each folder we need csv files with the string "_implicit_time_" in their name:
    all_paths <- c(all_paths, Sys.glob(sprintf('data/new/%s/*_implicit_time_*.csv',fold)))
  }
  
  remove_ids <- c()
  
  id <- c()
  timestamp <- c()
  path <- c()
  folder <- c()
  condition <- c()
  condition_label <- c()
  rotation <- c()
  
  for (one_path in all_paths) {
    
    added <- FALSE
    
    # extract participant id (and pavlovia timestamp):
    id_time   <- getIDtimestamp(basename(one_path), task='implicit_time')
    
    # check if the file makes sense (just counting lines for now):
    pav_check <- checkPavloviaFile(path=one_path)
    # print(pav_check['check'])
    if ( pav_check['check'] ) {
      adapt_check <- checkAdapted(path=one_path)
      if (adapt_check['check']) {
        path <- c(path, one_path)
        added <- TRUE
      } else{
        # or else?
      }
    } else {
      if (pav_check['rotated']) {
        remove_ids <- c(remove_ids, id_time['participant'])
      }
      # if there are too few lines in the file, we skip it:
      next()
    }
    
    # print(added)
    
    # add participant ID and file timestamp to lists:
    if (added) {
      id              <- c( id,              id_time['participant'] )
      timestamp       <- c( timestamp,       id_time['timestamp']   )
      folder          <- c( folder,          substr(dirname(one_path),6,nchar(dirname(one_path))))
      condition       <- c( condition,       adapt_check['condition'])
      condition_label <- c( condition_label, adapt_check['condition_label'])
      rotation        <- c( rotation,        adapt_check['rotation'])
    }
    
  }
  
  # make a new data frame with the collected info
  df <- data.frame(id, timestamp, path, folder, condition, condition_label, rotation)
  
  # remove participants who did rotated trials in incomplete files:
  df <- df[which(!df$id %in% remove_ids),]
  
  # see if there are duplicate participants:
  df <- df[order(df$timestamp),] # sort alphabetically by timestamp
  if (length(which(duplicated(df$id)))) {
    first_id <- which(!duplicated(df$id))
    df <- df[first_id,]
  }
  
  return(df)
  
}

getIDtimestamp <- function(filename, task) {
  
  pattern <- sprintf('_%s_', task)
  
  pos <- gregexpr(pattern=pattern, filename)[[1]][1]
  pp <- substr(filename, 1, pos-1)
  ts <- substr(filename, pos+nchar(pattern), nchar(filename)-4)
  
  return(c('participant'=pp, 'timestamp'=ts))
  
}

checkPavloviaFile <- function(path) {
  
  # _ffw  -> 272
  # _expl -> 264
  # _rot  -> 320
  
  directory <- dirname(path)
  min_lines <- as.numeric(c('data/new/itc5tab_rot'=320,
                            'data/new/itc4tab_expl'=264,
                            'data/new/itc3tab_ffw'=272,
                            'data/new/itc6tab_dt'=272)[directory])
  
  con <- file(path, 'r')
  rotated <- FALSE
  nlines <- -1 # header line adds another one to the count...
  while(TRUE) {
    line_n <- readLines(con,n=1)
    if (nlines == -1) {
      rot_col <- which( strsplit(line_n,',')[[1]] == "rotation_deg" )
    }
    if (length(line_n)==0) {
      break()
    }
    if (nlines > -1 & length(rot_col)) {
      line_data <- strsplit(line_n,',')[[1]]
      if (length(line_data) >= rot_col) {
        if (line_data[rot_col] != "0") {
          rotated <- TRUE
        }
      } else {
        # this should NOT ever happen:
        print(line_n)
      }
    }
    
    nlines <- nlines+1
  }
  close(con)
  
  check <- nlines >= min_lines

  return(c('check'=check, 'nlines'=nlines, 'rot_col'=rot_col, 'rotated'=rotated))
  
}

checkAdapted <- function(path) {
  
  df <- read.csv(path)
  
  # print(str(df))
  condition       <- df$condition[1]
  condition_label <- df$condition_label[1]
  
  df <- df[which(df$trialtype == 1),]
  rot_start <- which(df$rotation_deg != 0)[1] # always 37!
  # lets take the last 16 trials:
  df <- df[c(rot_start-1+c(85:100)),]

  X <- c()
  Y <- c()
  for (trial in c(dim(df)[1])) {
    target <- c(df$target[trial])
    theta <- -1*(target/180)*pi
    R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)
    #step <- convertCellToNumVector(df$step[trial])
    relx <- convertCellToNumVector(df$mousex_rel[trial])
    rely <- convertCellToNumVector(df$mousey_rel[trial])
    d <- sqrt(relx^2 + rely^2)
    idx <- which(d > 0.2)[1]
    xy <- c(relx[idx],rely[idx])
    npos <- R %*% xy
    X <- c(X, npos[1])
    Y <- c(Y, npos[2])
  }
  
  reach_deviations <- (atan2(Y,X)/pi)*180
  
  rotation <- df$rotation_deg[1]
  criterion <- abs(rotation/2)
  # if (abs(rotation)==15) {criterion==5}
  
  if (rotation > 0) {reach_deviations <- reach_deviations * -1}
  
  return(c('check'=median(reach_deviations, na.rm=TRUE) > criterion, 'rotation'=rotation, 'condition'=condition, 'condition_label'=condition_label) ) 
  
}

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

# combined data -----

getExpParticipantTable <- function(exp=1) {
  
  Qdf <- processQualtrics(exp=exp)
  Pdf <- getPavloviaParticipants(exp=exp)
  
  df <- merge(Qdf, Pdf, by='id')
  
  return(df)
  
} 

writeParticipantTables <- function(exps=c(1,2)) {
  
  for (exp in exps) {
    
    df <- getExpParticipantTable(exp=exp)
    write.csv(df,file=sprintf('data/participants_exp_%d.csv',exp), row.names=FALSE)
    
  }
  
}

# additivity stuff -----

# we need total adaptation, a measure of explicit, and a measure of implicit

# for the 7 groups that do only a few aiming trials, we take those for explicit
# and maybe the total and implicit adaptation from the trials before and after the aiming

# for the aiming group, we could use the same section of trials, but use all of them?

extractAdditivityMeasures <- function() {
  
  df1 <- read.csv('data/participants_exp_1.csv', stringsAsFactors = FALSE)
  df2 <- read.csv('data/participants_exp_2.csv', stringsAsFactors = FALSE)
  
  df <- rbind(df1, df2)
  
  df <- df[order(df$TaskDate),]
  df <- df[!duplicated(df$id),]
  
  print(unique(df$condition_label))
  
  rids <- read.csv('data/randomIDs.csv')
  
  IDno <- 1
  
  
  group	            <- c()
  participant       <- c()	
  rotation          <- c()
  adaptation        <- c()
  adaptation_sd     <- c()
  implicit          <- c()
  implicit_sd       <- c()
  explicit          <- c()
  explicit_sd       <- c()
  explicit.method   <- c()
  
  
  
  for (grlab in unique(df$condition_label)) {
    # path	folder	condition	condition_label
    print(grlab)
    ppidx <- which(df$condition_label == grlab)
    # print(ppidx)
    # print( df$path[ppidx])
    for (path in df$path[ppidx]) {
      
      ID   <- rids$randomIDs[IDno]
      IDno <- IDno + 1
      
      pdf <- read.csv(path, stringsAsFactors = FALSE)
      
      pdf <- pdf[which(!pdf$phase == 'practice'),]
      
      if (grlab == 'aiming') {
        expl_idx <- c(153, 161, 169, 177, 185, 193, 201, 209)
      } else {
        expl_idx <- which(pdf$aiming_type == 1)
      }
      
      adapt <- list()
      aiming_errors <- pdf$aiming_direction[expl_idx] - pdf$aiming_target[expl_idx]
      adapt[['explicit']]    <- median( aiming_errors )
      adapt[['explicit_sd']] <-     sd( aiming_errors )
      
      for (trialtype in c(0,1)) {
      
        baseline_idx <- (c(4:19) * 2) + (2-trialtype)
        
        reachdevs <- c()
        
        for (trial in baseline_idx) {
          
          reachdevs <- c(reachdevs, getReachDev(target = pdf$target[trial],
                                                X      = pdf$mousex_rel[trial],
                                                Y      = pdf$mousey_rel[trial]) )
          
        }
        
        adapt[[c('implicit','adaptation')[trialtype+1]]] <- median(reachdevs)
        
        reachdevs <- c()
        
        tt_idx <- which(pdf$trialtype == trialtype)

        for (idx in expl_idx) {
          
          t_idx <- tt_idx[which(abs((tt_idx - idx)) <= 1)]
          if (length(t_idx) == 1) {t_idx <- c(t_idx, t_idx-2)}
          for ( trial in t_idx) {
            reachdevs <- c(reachdevs, getReachDev(target = pdf$target[trial],
                                                  X      = pdf$mousex_rel[trial],
                                                  Y      = pdf$mousey_rel[trial]) )
            
          }
          
        }
        
        adapt[[c('implicit','adaptation')[trialtype+1]]] <- c( adapt[[c('implicit','adaptation')[trialtype+1]]], median(reachdevs) )
        adapt[[c('implicit_sd','adaptation_sd')[trialtype+1]]] <- c( adapt[[c('implicit_sd','adaptation_sd')[trialtype+1]]], sd(reachdevs) )
      
      }
      
      for (trialtype in c(0,1)) {
        adapt[[c('implicit','adaptation')[trialtype+1]]] <- diff( adapt[[c('implicit','adaptation')[trialtype+1]]] )
      }
      
      # APPEND STUFF 
      
      grnam <- list('control'        = 'control',
                    'cursorjump'     = 'cursorjump', 
                    'terminal'       = 'terminal', 
                    'aiming'         = 'aiming', 
                    '15deg_distance' = 'deg15',
                    '30deg_distance' = 'deg30',
                    '45deg_distance' = 'deg45', 
                    '60deg_distance' = 'deg60')[[grlab]]
      
      # print(df$rotation[which(df$path == path)])
      group             <- c(group,           grnam)	
      participant	      <- c(participant,     ID)
      rotation          <- c(rotation,        df$rotation[which(df$path == path)])
      adaptation	      <- c(adaptation,      adapt[['adaptation']] * -1)
      adaptation_sd	    <- c(adaptation_sd,   adapt[['adaptation_sd']])
      implicit	        <- c(implicit,        adapt[['implicit']] * -1)
      implicit_sd	      <- c(implicit_sd,     adapt[['implicit_sd']])
      explicit	        <- c(explicit,        adapt[['explicit']] * -1)
      explicit_sd	      <- c(explicit_sd,     adapt[['explicit_sd']])
      explicit.method   <- c(explicit.method, 'aim.reports')
      
      if (ID == 'f0dc30') {
        print(path)
      }
      
    }
    
  }
  
  print('** LENGTHS **')
  print(length(group))
  print(length(participant))
  print(length(rotation))
  print(length(adaptation))
  print(length(adaptation_sd))
  print(length(implicit))
  print(length(implicit_sd))
  print(length(explicit))
  print(length(explicit_sd))
  print(length(explicit.method))
  
  df <- data.frame( 
                    group,
                    participant,
                    rotation,
                    adaptation,
                    adaptation_sd,
                    implicit,
                    implicit_sd,
                    explicit,
                    explicit_sd,
                    explicit.method)
  
  return(df)
  
}

# utility functions -----

getReachDev <- function(target, X, Y) {
  
  theta <- -1*(target/180)*pi
  R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),nrow=2)
  relx <- convertCellToNumVector(X)
  rely <- convertCellToNumVector(Y)
  d <- sqrt(relx^2 + rely^2)
  idx <- which(d > 0.2)[1]
  xy <- c(relx[idx],rely[idx])
  npos <- R %*% xy

  reach_dev <- (atan2(npos[2],npos[1])/pi)*180
  
  return(reach_dev)
  
}

fit1exponential <- function() {
  
  pps <- read.csv('data/participants_exp_2.csv', stringsAsFactors = FALSE)
  
  # remove duplicate participant IDs (keep the first one)
  pps <- pps[order(pps$TaskDate),]
  pps <- pps[!duplicated(pps$id),]
  
  # pick one participant at random
  idx <- sample(dim(pps)[1],1)
  print(idx)
  # read the data file
  path <- pps$path[idx]
  pdf <- read.csv(path, stringsAsFactors = FALSE)
  # remove practice phase and training/aiming trials:
  pdf <- pdf[which(!pdf$phase == 'practice' & pdf$trialtype == 0),]
  
  reachdevs <- c()
  # extract reach deviations:
  for (trialno in c(1:dim(pdf)[1])) {
    
    target <- pdf$target[trialno]
    X      <- pdf$mousex_rel[trialno]
    Y      <- pdf$mousey_rel[trialno]
    
    reachdevs <- c(reachdevs, getReachDev(target, X, Y))
    
  }
  
  # flip signal so it goes up from 0
  reachdevs <- reachdevs * -1
  
  # remove extreme outliers:
  reachdevs[which(abs(reachdevs) > 90)] <- NA
  
  # make a fresh plot:
  plot(reachdevs, main=pps$id[idx])
  
  # fit exponential function:
  expfit <- exponentialFit(signal=reachdevs[21:120])
  print(expfit)
  
  expmod <- exponentialModel(par=expfit, timepoints=100)
  print(str(expmod))
  lines(x=expmod$trial + 21,
        y=expmod$output,
        col='blue')
}

# organize data -----

organizeData <- function() {
  
  bindParticipantTables()
  organizeFiles()
  
}

bindParticipantTables <- function() {
  
  exp1 <- read.csv('data/participants_exp_1.csv', stringsAsFactors = FALSE)
  exp2 <- read.csv('data/participants_exp_2.csv', stringsAsFactors = FALSE)
  
  exp <- rbind(exp1, exp2)
  
  exp$exp <- NA
  
  exp$exp[which(exp$condition_label == '15deg_distance')] <- 1
  exp$exp[which(exp$condition_label == '30deg_distance')] <- 1
  exp$exp[which(exp$condition_label == '45deg_distance')] <- 1
  exp$exp[which(exp$condition_label == '60deg_distance')] <- 1
  
  exp$exp[which(exp$condition_label == 'control')] <- 2
  exp$exp[which(exp$condition_label == 'terminal')] <- 2
  exp$exp[which(exp$condition_label == 'cursorjump')] <- 2
  
  exp$exp[which(exp$condition_label == 'aiming')] <- 3
  
  exp$exp[which(exp$condition_label == 'delay-FB')] <- 4
  exp$exp[which(exp$condition_label == 'delay-trial')] <- 4
  
  exp <- exp[order(exp$condition_label),]
  exp <- exp[order(exp$exp),]
  
  exp <- exp[which(exp$condition_q == exp$condition),]
  
  randomIDs <- read.csv('data/randomIDs.csv', stringsAsFactors = FALSE)
  exp$participant <- randomIDs$randomIDs[c(1:dim(exp)[1])]
  print(exp$participant)
  
  write.csv(exp, file="data/participants_combined.csv", row.names = F)
  
}

organizeFiles <- function() {
  
  ppt <- read.csv('data/participants_combined.csv', stringsAsFactors = FALSE)
  
  experiments <- unique(ppt$exp)
  
  dir.create('data/osf/')
  
  for (exp in experiments) {
    
    dir.create(sprintf('data/osf/exp%d',exp))
    
    conditions <- unique(ppt$condition_label[which(ppt$exp == exp)])
    
    for (condition in conditions) {
      
      dir.create(sprintf('data/osf/exp%d/%s/',exp,condition))
      
      idx <- which(ppt$exp == exp & ppt$condition_label == condition)
      
      for (id in idx) {
        infilename <- ppt$path[id]
        participant <- ppt$participant[id]
        
        df <- read.csv(file = infilename,
                       stringsAsFactors = FALSE)
        
        # remove unnecesasry columns:
        remove_columns <- c('trials.thisRepN',	'trials.thisTrialN',	'trials.thisN',	'trials.thisIndex',	'trials.ran',	'participant',	'condition',	'qualtrics',	'group', 'expName',	'psychopyVersion',	'OS',	'frameRate')
        df <- df[ , !(names(df) %in% remove_columns)]
        
        outfilename <- sprintf('data/osf/exp%d/%s/%s.csv', exp, condition, participant )

        write.csv(df, outfilename, row.names=FALSE)
        
      }
      
    }
    
  }
  
  demographics <- ppt[,c('participant', 'exp', 'condition_label', 'consent', 'age', 'sex', 'height_cm', 'handedness', 'needs_vision_correction', 'has_vision_correction')]
  write.csv(demographics, 'data/osf/demographics.csv', row.names=FALSE)
  meta_info <- ppt[,c('participant', 'exp', 'condition_label', 'IntakeDate', 'Progress_intake', 'Finished_intake', 'TaskDate', 'Progress_task', 'Finished_task', 'timestamp')]
  write.csv(meta_info, 'data/osf/meta_info.csv', row.names=FALSE)
  
    
}