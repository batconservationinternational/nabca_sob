analyze_SoB <- function(data,
                        OutputFolder,
                        cntrytoAnalyze=NULL,
                        SpptoAnalyze=NULL,
                        PersonalPlots=F
                        ) {

SpptoAnalyze <- c("ANTROZOUS_PALLIDUS")
  
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
    print("Generating quantiles:")
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

  #Set up progress bar for map functions
  ticks <- nrow(mydata)
  pb <- progress::progress_bar$new(total = ticks)
  
  # Density plots
  message("Make density plots")
  mydata$plots <- mydata %>% 
    select(value, q_type, dist_info, popQuantiles, dist_type) %>% 
    pmap(generate_Densityplot, pb=pb)
  
  # Random draw
  pb <- progress::progress_bar$new(total = ticks)
  message("Draw random values from dist")
  mydata$randomDraw <- map(mydata$dist_info, generate_sample, Nsamples = 1000, pb=pb)
  
  # Calculate overlap
  pb <- progress::progress_bar$new(total = ticks)
  message("Calculate Overlap")
  mydata$overlap <- mydata %>% select(dist_type, randomDraw) %>% 
    pmap(calc_Overlap, pb=pb)
  
  
  message("Save Data")
  if (!dir.exists(OutputFolder)){dir.create(OutputFolder, recursive = T)}
  dataDate <- stringr::str_sub(OutputFolder, -8)
  dataSetName <- paste0(cntrytoAnalyze, "_", SpptoAnalyze, "_", dataDate, ".RDS")
  
  saveRDS(mydata, here::here(OutputFolder, dataSetName))
  
  return(mydata)
}
