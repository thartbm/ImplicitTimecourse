library('osfr')
library('Reach')

downloadData <- function() {
  
  filelist <- list('\\' = c('demographics.csv', 'meta_info.csv', 'exp1.zip', 'exp2.zip', 'exp3.zip', 'exp4.zip') )
  
  Reach::downloadOSFdata( repository = 'ajwyr',
                          filelist   = filelist,
                          folder     = 'data',
                          unzip      = TRUE,
                          removezips = TRUE )
  
}