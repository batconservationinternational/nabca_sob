
calc_Impact <- function(dataDate, 
                        dataFolder,
                        speciestoAnalyze,
                        countrytoAnalyze) {

  # dataFolder=OutputFolder
  # spp = "MYOTIS_YUMANENSIS"
  # speciestoAnalyze=spp
  # countrytoAnalyze=thisCountry

  files <- list.files(dataFolder, full.names = T)
  files <- files[!grepl("pooled", files) & !grepl("threat_impact_plots", files)]
   
   # Table of threat labels
   Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv')) %>% 
     unite("threat_abbr", Q_group, Q_sub, sep = "_", remove = F)
   
   # Get path to data file for specified species and country
   f <- files[str_detect(files, speciestoAnalyze) & str_detect(files, countrytoAnalyze)]
   
   print(paste0('reading: ', f))

   # Load in data
   sppData <- readRDS(f)
     
   # Make sure there is only one species and one country in the dataset
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
   
   if (nrow(threat_data)>0){ # only do if there is threat_data
     # Get list of responses for scope and severity
     threat_data$responses <- purrr::map(threat_data$randomDraw, names)
     # Pivot Scope and Severity info wider
     threat_data <- threat_data %>% select(name, cntry, value, q_type, dist_info_id, randomDraw, responses) %>% 
         pivot_wider(names_from = "q_type", values_from = "randomDraw") 
     
     # Multiply across the random draws of scope and severity to get impact
      threat_data$impact <- purrr::map2(threat_data$Scope, threat_data$Severity, `*`)
   }
     
     # For each row, and expert, calculate distribution and quantiles and do a random draw
     ticks <- nrow(threat_data)
     pb <- progress::progress_bar$new(total = ticks)
     if (nrow(threat_data)>0){ # only do if there is threat_data
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
    
    # Make table of pooled quanitles for pop
    pop <- purrr::map(pop_data$pooled_dist, 2)
    pop <- purrr::map(pop, .f = ~list(Q1=.x[1], Median=.x[2], Q3=.x[3]))
    pop_info <- do.call(rbind.data.frame, pop)
    pop_info <- pop_info %>%  mutate(question = pop_data$q_type, .before=1) %>% 
      mutate(species = speciestoAnalyze, .before=1)
    
    # Make table of pooled quantiles for threat impact
    t <- purrr::map(threat_data$pooled_dist, 2)
    t <- purrr::map(t, .f = ~list(Q1=.x[1], Median=.x[2], Q3=.x[3]))
    threat_info <- do.call(rbind.data.frame, t) %>% 
      mutate(question = threat_data$dist_info_id, .before=1) %>% 
      mutate(species = speciestoAnalyze, .before=1)
    
    # Make table of quantiles for scope and sev
    ss <- sppData %>% filter(q_type %in% c("Scope", "Severity"))
    ss_quantiles <- purrr::pluck(ss$Quantiles)
    ss_info <- do.call(rbind.data.frame, ss_quantiles)
    
    # If more than one expert response, filter to linear pool, otherwise keep the
    # one expert's response
    lp_filter <- function(item){
      rownames(item) <- NULL
      if (nrow(item)>1){
        out <- item %>% filter(expert=='linear pool')
        return(out)
      } else {return(item)}
    }
    
    ss_lp <- ss_quantiles %>% purrr::map(lp_filter)
    ss_summary <- do.call(rbind.data.frame, ss_lp) %>% 
      mutate(scope_sev = ss$q_type, .before=1) %>% 
      mutate(threat = ss$dist_info_id, .before=1) %>% 
      mutate(species = speciestoAnalyze, .before=1) %>% select(-expert)
    
    # Bind pop and threat data back together
    all_data <- bind_rows(threat_data, pop_data) %>% 
      select(name, cntry, value, dist_info_id, expert_impact, pooled_dist) %>% 
      separate(dist_info_id, into = c("Q_group", "Q_sub"), sep = "_")
    
    # Save data
    out_file <- paste0(tools:::file_path_sans_ext(f), '_pooled.RDS')
    print(paste('Saving pooled data to:', out_file))
    saveRDS(all_data, file = out_file)
    
    return(list(pop_info = pop_info, 
                threat_info = threat_info,
                ss_summary = ss_summary))
}
