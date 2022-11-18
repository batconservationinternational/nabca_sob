calc_total_impact <- function(dataDate,
                              dataFolder,
                              speciestoAnalyze,
                              countrytoAnalyze){
  files <- list.files(dataFolder, full.names = T)
  files <- files[grepl("_pooled.RDS", files)]
  
  # get path to data file for specified species and country
  f <- files[str_detect(files, speciestoAnalyze) & str_detect(files, countrytoAnalyze)]
  print(paste0('reading: ', f))
  
  #load in data
  sppData <- readRDS(f)
}