analyze_SoB <- function(mydata,
                        OutputFolder,
                        SpptoAnalyze,
                        dataDate,
                        cntrytoAnalyze,
                        PersonalPlots=F
                        ) {
  
  # Put nested list into data frame structure
  mydata <- enframe(mydata)
 
  # Set distribution type and limits to use for fitting distribution
  mydata$value[[1]][["popSizeData"]] <- mydata$value[[1]][["popSizeData"]] %>% mutate(dist = map2(Q_group, Q_sub, choose_pop_dist))
  mydata$value[[1]][["popTrendData"]] <- mydata$value[[1]][["popTrendData"]] %>% mutate(dist = map2(Q_group, Q_sub, choose_pop_dist))
  mydata$value[[1]][["threatData"]] <- mydata$value[[1]][["threatData"]] %>% mutate(dist = map(Q_group, choose_threats_dist))

  
  # Pluck country to be its own column
  mydata$cntry <- mydata %>% pluck("value", 1, "country", 1)

  # Fit distributions
  message("Fit pop distributions to answers")
  mydata$popSize <- pbapply(mydata, 1, generateDist, dat_type = "popSizeData", scope_sev = NA)
  mydata$popTrend <- pbapply(mydata, 1, generateDist, dat_type = "popTrendData", scope_sev = NA)
  
  message("Fit threat scope distributions to answers")
  mydata$Scope <- pbapply(mydata, 1, generateDist, dat_type = "threatData", scope_sev = "scope")
  
  message("Fit threat severity distributions to answers")
  mydata$Severity <- pbapply(mydata, 1, generateDist, dat_type = "threatData", scope_sev = "sev")
  
  # Unnest distributions
  mydata <- mydata %>% pivot_longer(!c(name, value, cntry),
                                  names_to = "q_type",
                                  values_to = "dist_info") %>% 
    unnest_longer(dist_info, values_to = "dist_info")
  
  # Get quantiles
  quants <- list()
  for (i in seq(1:nrow(mydata))){
    out <- find_quantiles(mydata[i,])
    quants <- append(quants, list(out))
  }
  mydata$Quantiles <- quants
  
  # Pull out distribution type info for easier access to use in functions below
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

  # Set up progress bar for map functions
  ticks <- nrow(mydata)
  pb <- progress::progress_bar$new(total = ticks)
  
  # Make density plots
  message("Make density plots")
  mydata$plots <- mydata %>%
    select(value, q_type, dist_info, Quantiles, dist_type) %>%
    pmap(generate_Densityplot, pb=pb)
  
  # Random draw from fitted distribution
  pb <- progress::progress_bar$new(total = ticks)
  message("Draw random values from dist")
  mydata$randomDraw <- mydata %>% select(dist_info, dist_type) %>% 
    pmap(generate_sample, Nsamples = 1000, pb=pb)
  
  # Calculate overlap
  # message("Calculate Overlap")
  # mydata$overlap <- map(mydata$randomDraw, overlapping::ovmult) %>% map(1, "OV")
  
  message("Save Data")
  if (!dir.exists(OutputFolder)){dir.create(OutputFolder, recursive = T)}
  dataSetName <- paste0(cntrytoAnalyze, "_", SpptoAnalyze, "_", dataDate, ".RDS")
  saveRDS(mydata, here::here(OutputFolder, dataSetName))
}
