make_PopGraphs <- function(DataDate,
                           Dir) {
  saveDir <- paste0(Dir, '/popGraphs/', thisDataDate)
  if (!dir.exists(saveDir)) {dir.create(saveDir, recursive=T)}
  DataFolder = paste0(here::here(), '/Data/derived/Analysis_Export_', DataDate)
  DataFiles <- list.files(DataFolder, full.names = T)
  DataFiles <- DataFiles[!grepl("_T2.RDS", DataFiles)]
  
  #Table of threat labels
  # Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv'))
    
    sppData <- readRDS(f) %>%
      mutate(plots = case_when(perPlots == 'single elicitation' ~ D_plot,
                               T ~ perPlots)) %>%
      select(-perPlots, -D_plot)
    
    thisSpecies = unique(sppData$sppCode)
    thisCountry = unique(sppData$cntry)
    
    #### Combine Treat Graphs ####
    popData <- sppData %>%
      filter(Q_group == 'pop')
    #IF NO THREAT DATA GO TO NEXT FILE
    if (nrow(popData) < 1) {
      rm(sppData)
      rm(popData)
      next
    }
    pbapply::pbapply(popData,
            MARGIN = 1,
            save_PersonalPlot,
            PPsaveDir=saveDir,
            PPcntry=thisCountry,
            PPspp=thisSpecies)
}
