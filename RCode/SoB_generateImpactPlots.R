generate_impact_plots <- function(dataDate, 
                          speciestoAnalyze,
                          dataFolder,
                          cntrytoAnalyze){
  # Load in data
  files <- list.files(dataFolder, full.names = T)
  files <- files[grepl("_pooled.RDS", files)]
  f <- files[str_detect(files, speciestoAnalyze) & str_detect(files, cntrytoAnalyze)]
  data <- readRDS(f)
  
  #filter out pop data so that it only uses make_threat_plots on threat data
  data <- data %>% filter(Q_group!="pop")
  
  ticks <- nrow(data)
  pb <- progress::progress_bar$new(total = ticks)
  # Map impact plot creation for all threats
  data$pooled_plots <- data %>% select(value, expert_impact, pooled_dist) %>% 
    pmap(make_impact_plots, pb=pb)
  
  # Drop excess data that is already saved and just save plots and their Q groups
  data <- data %>% select(Q_group, Q_sub, pooled_plots)
  
  # Save data
  out_file <- paste0(tools:::file_path_sans_ext(f), '_threat_plots.RDS')
  print(paste('Saving threat plots to:', out_file))
  saveRDS(data, file = out_file)
}

