get_impact_bins <- function(dataFolder, countrytoAnalyze, dataDate){

  # countrytoAnalyze <- "Mexico"
  # dataFolder <- "/Users/ngoodby/Desktop/nabca_sob/Data/derived/AnalysisExport_20221116_Mexico"
  
  # Load in data
  files <- list.files(dataFolder, full.names = T)
  files <- files[grepl("scop_sev_quantiles_agg_*", files)]
  f <- files[str_detect(files, countrytoAnalyze) & str_detect(files, )]
  print(paste0('reading: ', f))
  threat_data <- read_csv(f)
  
  # Bin median scope and severity values and pivot wider
  binned_data <- threat_data %>% 
    mutate(scope_sev_bin = case_when(
      scope_sev == "Scope" ~ bin_scope(Median),
      scope_sev == "Severity" ~ bin_sev(Median)
    )) %>% select(species, threat, scope_sev, scope_sev_bin) %>% 
    pivot_wider(names_from = scope_sev, values_from = scope_sev_bin) %>% 
    mutate(impact_bin = bin_impact(Scope, Severity))
  
  return(binned_data)
}