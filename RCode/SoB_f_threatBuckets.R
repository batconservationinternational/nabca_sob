get_impact_bins <- function(dataFolder, countrytoAnalyze, dataDate){

  # countrytoAnalyze <- "Mexico"
  # dataDate <- "20221116"
  # dataFolder <- "/Users/ngoodby/Desktop/nabca_sob/Data/derived/AnalysisExport_20221116_Mexico"
  
  # Load in data
  files <- list.files(dataFolder, full.names = T)
  ss_data <- files[str_detect(files, "scope_sev_quantiles_agg_")]
  print(paste0('reading: ', ss_data))
  threat_data <- read_csv(ss_data)
  
  # Bin median scope and severity values----------------------------------------
  # bin_scope, bin_sev, and bin_impact function in SoB_f_general.R
  binned_data <- threat_data %>% 
    mutate(scope_sev_bin = case_when(
      scope_sev == "Scope" ~ bin_scope(Median),
      scope_sev == "Severity" ~ bin_sev(Median)
    )) %>% select(species, threat, scope_sev, scope_sev_bin) %>% 
    pivot_wider(names_from = scope_sev, values_from = scope_sev_bin) %>% 
    mutate(impact_bin = bin_impact(Scope, Severity)) %>% 
    separate(col = threat, into = c("level_1", "level_2"), sep = "_") %>% 
    mutate(impact_bin = factor(impact_bin, 
                               levels = c("Negligible","Low","Medium","High","Very High"),
                               ordered = TRUE))
  
  # Roll up level 2 threats and calculate level 1 threat-----------------------
  level_one_bins <- threat_data %>% 
    separate(col = threat, into = c("level_1", "level_2"), sep = "_") %>% 
    group_by(species, level_1, scope_sev) %>% summarize(l2_mean = sum(Median)) %>% 
    mutate(scope_sev_bin = case_when(
      scope_sev == "Scope" ~ bin_scope(l2_mean),
      scope_sev == "Severity" ~ bin_sev(l2_mean)
    )) %>% select(species, level_1, scope_sev, scope_sev_bin) %>% 
    pivot_wider(names_from = scope_sev, values_from = scope_sev_bin) %>% 
    mutate(impact_bin = bin_impact(Scope, Severity))
    mutate(impact_bin = factor(impact_bin, 
                               levels = c("Negligible","Low","Medium","High","Very High"),
                               ordered = TRUE))
  
  # Alternative method using Nature Serve l1 to overall bins
  # level_one_bins <- binned_data %>% group_by(species, level_1, impact_bin) %>%
  #   # Take highest level sub threat and use that as overall threat - not sure if this is a good way to do this
  #   summarise(count = n()) %>% 
  #   pivot_wider(names_from = impact_bin, values_from = count, values_fill=0)
  #   
  # my_cols <- c("Negligible","Low","Medium","High","Very High")
  # 
  # # Add column of 0s if any of the bins weren't present in the level 2 impacts
  # # so it doesn't error when calculating level 1 impact bins
  # for (i in my_cols){
  #   if (!i %in% names(level_one_bins)){
  #     col_i <- paste0(i)
  #     level_one_bins[col_i] <- 0
  #   }
  # }
  
  # # Roll up counts of level 2 impacts to overall level 1 impact
  # # Using NatureServe guidelines to assign overall impact value for the species 
  # # b/c guidance for this step was vague
  # level_one_bins <- level_one_bins %>% 
  #   mutate(impact_bin = case_when(
  #     `Very High`>=1 | High>=2 | (High==1 & Medium>=2) ~ "Very High",
  #     High==1 | Medium>=3 | (Medium==2 & Low==2) | (Medium==1 & Low>=3) ~ "High",
  #     Medium==1 | Low>=4 ~ "Medium",
  #     Low>=1 | Low<=3 ~ "Low"
  #   )) %>% select(species, level_1, impact_bin)
  
  # Assign overall impact value for each species--------------------------------
    
  overall_impact <- level_one_bins %>% group_by(species, impact_bin) %>% 
    summarise(count = n()) %>% 
    pivot_wider(names_from = impact_bin, values_from = count, values_fill = 0)
  
  # Add column of 0s if any of the impact bins weren't present in the dataset 
  # so it doesn't error when calculating overall impact bins
  for (i in my_cols){
    if (!i %in% names(overall_impact)){
      col_i <- paste0(i)
      overall_impact[col_i] <- 0
    }
  }
  
  # Bin overall impact based on level 1 threat bins
  overall_impact <- overall_impact %>% 
    mutate(overall_impact_val = case_when(
      `Very High`>=1 | High>=2 | (High==1 & Medium>=2) ~ "Very High",
      High==1 | Medium>=3 | (Medium==2 & Low==2) | (Medium==1 & Low>=3) ~ "High",
      Medium==1 | Low>=4 ~ "Medium",
      Low>=1 | Low<=3 ~ "Low"
    )) %>% select(species, overall_impact_val)
  
  
  return(list(overall_impact = overall_impact,
              binned_data = binned_data,
              level_one_bins = level_one_bins))
}
