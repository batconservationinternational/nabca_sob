
# source(paste0(here::here(), '/RCode/SoB_f_general.R'))

analyze_SoB <- function(data,
                        OutputFolder,
                        cntrytoAnalyze=NULL,
                        SpptoAnalyze=NULL,
                        PersonalPlots=F
                        ) {

require(dplyr)
# library(purrr)
require(SHELF)
  
data <- nestedData
cntrytoAnalyze <- "MX"
SpptoAnalyze <- c("ANTROZOUS_PALLIDUS", "TADARIDA_BRASILIENSIS")
  
  # mydata2 <- data %>%
  #   ungroup() %>%
  #   filter(Q_group != 'other') %>%
  #   mutate(dist = map2(Q_group, Q_sub, choose_dist),
  #          row = row_number())
  # 
  # # rm(data)
  
  if (!is.null(SpptoAnalyze)){
    data <- data[SpptoAnalyze]
  }
  
  if (is.null(SpptoAnalyze)){ SpptoAnalyze <- 'AllSpecies'}

  
  require(pbapply)
  require(parallel)
  
  cl <- makeCluster(16, outfile=paste0(here::here(), "/outlog_", lubridate::today() ,".txt"))
  
  # message("Add labels (A,B,C...) to token values for consistent graphs")
  # mydata2$experts <- pblapply(mydata2$experts, expertNames, cl = cl)
  # mydata2$experts <- lapply(mydata2$experts, expertNames)
  
  # message("Format min, mean, max properly for analysis")
  # mydata2$mmm <- pblapply(mydata2$data, extract_vals, cl = cl)
  # 
  # message("Format probabilities properly for analysis")
  # mydata2$probs <- pblapply(mydata2$data, extract_probs, cl = cl)
  
  #Prepare for graphs
  clusterEvalQ(cl = cl, require(SHELF))
  clusterEvalQ(cl = cl, require(ggplot2))
  
  # message("Fit distributions to answers")
  # mydata2$distFit <- pbapply(mydata2[, ], 1, generate_dist, cl = cl)
  
  message("Fit distributions")
  mydata2$distFit <- pbapply(mydata, 1, getSPPdistributions)
  
  message("get 95% confidence interval of answers")
  mydata2$Quantiles <- pbapply(mydata2, 1, find_quantiles, cl = cl)

  message("Make Plot")
  mydata2$D_plot <-
    pbapply(mydata2[,],
            1,
            generate_Densityplot,
            cl = cl,
            save = F)
  
  # mydata2$D_plot <-
  #   apply(mydata2[,],
  #           1,
  #           generate_Densityplot,
  #           save = T)
  
  message("Draw values from Dist")
  mydata2$randomDraw <-
    pbapply(mydata2[,],
            1,
            generate_sample,
            Nsamples = 1000,
            cl = cl)
  
  message("Calculate Overlap")
  mydata2$overlap <- pbapply(mydata2[,], 1, calc_Overlap, cl = cl)
  
  
  if(PersonalPlots){
  message("Create Personal Plots")
  mydata2$perPlots <-
    pbapply(mydata2, 1, generate_PersonalPlot, cl = cl)
  }
  
  mydata2$dataSetName <- paste0(mydata2$cntry, "_", mydata2$sppCode)
  

  thisOutputFolder <- OutputFolder
  if (!dir.exists(thisOutputFolder)){dir.create(thisOutputFolder)}

  ##Non parallel version
  # for(s in unique(mydata2$dataSetName)) {
  #
  #   print(s)
  #   saveRDS(filter(mydata2, dataSetName==s),
  #           file=paste0(OutputFolder, '/SurveyAnswers_', s ,'_nestedQ_analyzed_', DataDate, '.RDS')
  #   )
  # }
  # 
  # clusterExport(cl = cl,
  #               varlist = c('thisOutputFolder', 'DataDate'))
  
  DataDate <- stringr::str_sub(OutputFolder, -8)
  print(DataDate)
    
  clusterExport(cl = cl,
                varlist = c('cntrytoAnalyze', 'DataDate'),
                envir = environment()
  )
  
  sppDataL <- split(mydata2, mydata2$dataSetName)
  message("Save Data")
  
  pblapply(sppDataL,
           function(spp) {

             
             fn = paste0(
               OutputFolder,
               '/SurveyAnswers_',
               DataDate,
               '_',
               cntrytoAnalyze,
               '_',
               unique(spp$dataSetName) ,
               '_nestedQ_analyzed',
               '.RDS'
             )
             saveRDS(spp, file = fn)
           } 
           # cl = cl
           )
  

  stopCluster(cl)
  
}