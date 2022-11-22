generate_impact_plots <- function(dataDate, 
                          speciestoAnalyze,
                          dataFolder,
                          cntrytoAnalyze){
  # Load in data
  files <- list.files(dataFolder, full.names = T)
  files <- files[grepl("_pooled.RDS", files)]
  f <- files[str_detect(files, speciestoAnalyze) & str_detect(files, cntrytoAnalyze)]
  data <- readRDS(f)
  
  # Filter out pop data so that it only uses make_threat_plots on threat data
  data <- data %>% filter(Q_group!="pop")

  # Map impact plot creation for all threats
  data$plots <- data %>% select(value, expert_impact, pooled_dist) %>% 
    pmap(make_impact_plots)
  
  # Drop excess data that is already saved and just save plots and their Q groups
  data <- data %>% select(value, Q_group, Q_sub, plots)
  
  # Save data
  out_file <- paste0(dataFolder, '/', cntrytoAnalyze, '_', speciestoAnalyze, 
                     '_', 'threat_impact_plots.RDS')
  print(paste('Saving threat plots to:', out_file))
  saveRDS(data, file = out_file)
}

