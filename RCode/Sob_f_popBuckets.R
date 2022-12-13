get_pop_bins <- function(dataFolder, countrytoAnalyze, dataDate){
  files <- list.files(dataFolder, full.names = T)
  files <- files[str_detect(files, "pop_quantiles_agg_")]
  f <- files[str_detect(files, countrytoAnalyze) & str_detect(files, dataDate)]
  print(paste0('reading: ', f))
  pop_data <- read_csv(f) %>% filter(question=="popSize")

  binned_pop_df <- pop_data %>% 
    mutate(pop_bin= case_when(
      Median == 0 ~ '0',
      Median < 100 ~ '< 100',
      Median >= 100 & Median < 250 ~ '100-250',
      Median >= 250 & Median < 1000 ~ '250-1,000',
      Median >= 1000 & Median < 5000 ~ '1,000-5,000',
      Median >= 5000 & Median < 20000 ~ '5,000-20,000',
      Median >= 20000 & Median < 200000 ~ '20,000-200,000',
      Median >= 200000 & Median < 2500000 ~ '200,000-2,500,000',
      Median > 2500000 ~ '>2,500,000'
      )
    ) %>% select(species, Median, pop_bin)

  return(binned_pop_df)
    