
generateRandomIDs <- function(checkFile='randomIDs.csv', IDlength=3, number=100, addToFile=checkFile) {
  # This function generates a NUMBER of random IDs of length IDlength * 2, and 
  # checks if they already exist in file checkFile.
  
  # we use non-reproducible and cryptographically secure sequences
  # from the openssl package
  library(openssl)
  
  # we need to know which ones we already generated/used
  if (file.exists(checkFile)) {
    existingList <- read.csv(checkFile, stringsAsFactors=F)
  } else {
    existingList <- data.frame('randomIDs'=c())
  }
  
  existingList$randomIDs <- as.character(existingList$randomIDs)
  
  # we store the new ones here:
  newIDs <- c()
  
  # we generate new IDs until we have the required number of new, unique ones:
  while(length(newIDs) < number) {
    
    # get a random byte string from openssl:
    my_randcypher <- openssl::rand_bytes(n=IDlength)
    
    # convert to a string:
    my_randid <- as.character(paste(as.character(my_randcypher), sep='', collapse=''))
    
    # check if it exists, and keep if it is new:
    if (my_randid %in% c(existingList$randomIDs, newIDs)) {
      # nothing to do here...
    } else {
      # put in list
      newIDs <- c(newIDs,my_randid)
    }
    
  }
  
  # convert list to data frame
  newList <- data.frame('randomIDs'=newIDs)
  
  # combine contents:
  idList <- rbind(existingList,newList)
  
  # write to file:
  write.csv(idList, file=addToFile, row.names=FALSE, quote=TRUE)
  
}