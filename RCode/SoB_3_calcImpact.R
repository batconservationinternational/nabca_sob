
calc_Impact <- function(dataDate, 
                        dataFolder,
                        speciestoAnalyze=NULL,
                        countrytoAnalyze=NULL,
                        doPar=T) {
  
  # DataFolder=OutputFolder
  # spp = "ANTROZOUS_PALLIDUS"
  # speciestoAnalyze=spp
  # countrytoAnalyze=thisCountry

  files <- list.files(dataFolder, full.names = T)
  files <- files[!grepl("pooled", files)]
   
   #Table of threat labels
   Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv')) %>% 
     unite("threat_abbr", Q_group, Q_sub, sep = "_", remove = F)
   
   # get path to data file for specified species and country
   if (!is.null(speciestoAnalyze) & !is.null(countrytoAnalyze)) {
     f <- files[str_detect(files, speciestoAnalyze) & 
                              str_detect(files, countrytoAnalyze)]
   } else {return(message("Country or species not provided to calc_Impact function"))}
   
   print(paste0('reading: ', f))

   #load in data
   sppData <- readRDS(f)
     
   #make sure there is only one species and one country in the dataset
   thisSpecies = unique(sppData$name)
   thisCountry = unique(sppData$cntry)
   if(length(thisSpecies) > 1 | length(thisCountry) > 1) {
        stop( cat('More than 1 species or country in data file', 
                  '\nSpecies: ', thisSpecies, 
                  '\nCountry: ', thisCountry, 
                  '\ndatafile: ', f)
              )}
      
   threat_data <- sppData %>% filter(q_type == "Scope" | q_type == "Severity") 
   pop_data <- sppData %>% filter(q_type == "popSize" | q_type == "popTrend") 
   
   if (nrow(threat_data)>0){ #only do if there is threat_data
     # Pivot Scope and Severity info wider
     threat_data <- threat_data %>% select(name, cntry, value, q_type, dist_info_id, randomDraw) %>% 
         pivot_wider(names_from = "q_type", values_from = "randomDraw") 
     
     # Multiply across the random draws of scope and severity to get impact
     threat_data$impact <- map2(threat_data$Scope, threat_data$Severity, ~ .x * .y)
   }
     
     # For each row, and expert, calculate distribution and quantiles and do a random draw
     ticks <- nrow(threat_data)
     pb <- progress::progress_bar$new(total = ticks)
     if (nrow(threat_data)>0){ #only do if there is threat_data
       threat_data$expert_impact <- purrr::map2(threat_data$dist_info_id, 
                                               threat_data$impact,
                                               calc_expert_impact,
                                               pb=pb)
     }
     pb <- progress::progress_bar$new(total = ticks)
     pop_data$expert_impact <- purrr::map2(pop_data$dist_info_id,
                                          pop_data$randomDraw, 
                                          calc_expert_impact,
                                          pb = pb)
     
    # Calc pooled distributions
    pb <- progress::progress_bar$new(total = ticks)
    if (nrow(threat_data)>0){
      threat_data$pooled_dist <- purrr::map2(threat_data$dist_info_id,
                                            threat_data$expert_impact, 
                                            calc_total_impact,
                                            pb = pb)
    }
    pb <- progress::progress_bar$new(total = ticks)
    pop_data$pooled_dist <- purrr::map2(pop_data$dist_info_id,
                                       pop_data$expert_impact, 
                                       calc_total_impact,
                                       pb = pb)
    
    # Bind pop and threat data back together
    all_data <- bind_rows(threat_data, pop_data) %>% 
      select(name, cntry, value, dist_info_id, expert_impact, pooled_dist) %>% 
      separate(dist_info_id, into = c("Q_group", "Q_sub"), sep = "_")
    
    #Save data
    out_file <- paste0(tools:::file_path_sans_ext(f), '_pooled.RDS')
    print(paste('Saving pooled data to:', out_file))
    saveRDS(all_data, file = out_file)
}
