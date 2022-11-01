analyze_SoB <- function(data,
                        OutputFolder,
                        cntrytoAnalyze=NULL,
                        SpptoAnalyze=NULL,
                        PersonalPlots=F
                        ) {

SpptoAnalyze <- c("ANTROZOUS_PALLIDUS", "EUMOPS_PEROTIS")
data = nestedData
  
  if (!is.null(SpptoAnalyze)){
    data <- data[SpptoAnalyze]
  }
  
  data <- enframe(data)
 
 for (i in seq(nrow(data))){
   data$value[[i]][["popSizeData"]] <- data$value[[i]][["popSizeData"]] %>% mutate(dist = map2(Q_group, Q_sub, choose_pop_dist))
   data$value[[i]][["popTrendData"]] <- data$value[[i]][["popTrendData"]] %>% mutate(dist = map2(Q_group, Q_sub, choose_pop_dist))
   data$value[[i]][["threatData"]] <- data$value[[i]][["threatData"]] %>% mutate(dist = map(Q_group, choose_threats_dist))
 }
  
  mydata <- data
  
  cl <- makeCluster(16, outfile=paste0(here::here(), "/outlog_", lubridate::today() ,".txt"))
  
  # message("Add labels (A,B,C...) to token values for consistent graphs")
  # mydata2$experts <- pblapply(mydata2$experts, expertNames, cl = cl)
  # mydata2$experts <- lapply(mydata2$experts, expertNames)
  
  # Prepare for graphs
  clusterEvalQ(cl = cl, require(SHELF))
  clusterEvalQ(cl = cl, require(ggplot2))

  # Fit distributions
  message("Fit pop distributions to answers")
  mydata$popSize <- pbapply(mydata, 1, generateDist, dat_type = "popSizeData", scope_sev = NA)
  mydata$popTrend <- pbapply(mydata, 1, generateDist, dat_type = "popTrendData", scope_sev = NA)
  
  message("Fit threat scope distributions to answers")
  mydata$Scope <- pbapply(mydata, 1, generateDist, dat_type = "threatData", scope_sev = "scope")
  
  message("Fit threat severity distributions to answers")
  mydata$Severity <- pbapply(mydata, 1, generateDist, dat_type = "threatData", scope_sev = "sev")
  
  # Unnest distributions
  mydata <- mydata %>% pivot_longer(!c(name, value),
                                  names_to = "q_type",
                                  values_to = "dist_info") %>% 
    unnest_longer(dist_info, values_to = "dist_info")
  
  # Get quantiles
  quants <- list()
  for (i in seq(1:nrow(mydata))){
    out <- find_quantiles(mydata[i,])
    print(paste0(i, "/", nrow(mydata)))
    print(out)
    quants <- append(quants, list(out))
  }
  mydata$popQuantiles <- quants
  
  # Pull out distribution type info
  dist_types <- list()
  for (i in seq(nrow(mydata))){
    q_type <- mydata[i,]$q_type
    thisRow <- mydata[i,]$value[[1]]
    if (q_type == "popSize"){
      dist_type <- thisRow$popSizeData$dist
    } else if (q_type == "popTrend"){
      dist_type <- thisRow$popTrendData$dist
    } else if (q_type == "Scope" | q_type == "Severity"){
      dist_type <- thisRow$threatData$dist
    }
  dist_types <- append(dist_types, list(dist_type))
  }
  mydata$dist_type <- dist_types

  # Make plot
  message("Make Plot")
  mydata$plots <- mydata %>% 
    select(value, q_type, dist_info, popQuantiles, dist_type) %>% 
    pmap_dfr(generate_Densityplot)
  
  # Random draw
  message("Draw values from Dist")
  mydata$randomDraw <- map(mydata$dist_info, generate_sample, Nsamples = 1000)
  
  # Calculate overlap
  message("Calculate Overlap")
  mydata2$overlap <- pbapply(mydata2[,], 1, calc_Overlap, cl = cl)
  
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
