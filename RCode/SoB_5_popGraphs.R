# library(dplyr)
# library(tidyr)
# library(ggpubr)
# DataDate = '20210126'

make_PopGraphs <- function(DataDate,
                           Dir) {

  saveDir <- paste0(Dir, '/PersonalGraphs/', thisDataDate)
  
  if (!dir.exists(saveDir)) {
    dir.create(saveDir)
  }
  
  source(paste0(Dir, '/RCode/SoB_f_general.R'))
  
  DataFolder = paste0(here::here(), '/Data/derived/Analysis_Export', DataDate)
  DataFiles <- list.files(DataFolder, full.names = T)
  DataFiles <- DataFiles[!grepl("_T2.RDS", DataFiles)]
  # DataFiles <- DataFiles[grepl("Canada_LACIN", DataFiles)]
  
  #Table of threat lables
  # Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv'))

    #ForEachDataFile

    
    require(doParallel)
    require(foreach)
    
    
    cl <- makeCluster(detectCores()-1)
    clusterExport(cl = cl, "save_PersonalPlot")
    clusterExport(cl = cl, 'saveDir', envir=environment())
    
    registerDoParallel(cl)
    
    # for (f in DataFiles) {
    
    foreach(f = DataFiles) %dopar% {
    # f = DataFiles[1] #For Debugging
    # print(f)
    
    require(dplyr)
    
    sppData <- readRDS(f) %>%
      select(
        cntry,
        spp,
        sppCode,
        LimeGroup,
        subQ,
        experts,
        Q_group,
        Q_ss,
        Q_sub,
        randomDraw,
        perPlots,
        D_plot
      ) %>%
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
    # require(pbapply)
    # require(parallel)
    # cl = makeCluster(16)
    # clusterExport(cl = cl,
    #               varlist = c('saveDir', 'thisCountry', 'thisSpecies'))
    pbapply::pbapply(popData,
            MARGIN = 1,
            save_PersonalPlot,
            PPsaveDir=saveDir,
            PPcntry=thisCountry,
            PPspp=thisSpecies)
  }
  
  
}
