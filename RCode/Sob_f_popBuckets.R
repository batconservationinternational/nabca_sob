get_pop_bins <- function(dataFolder, countrytoAnalyze, dataDate){
  files <- list.files(dataFolder, full.names = T)
  files <- files[str_detect(files, "pop_quantiles_agg_")]
  f <- files[str_detect(files, countrytoAnalyze) & str_detect(files, dataDate)]
  print(paste0('reading: ', f))
  pop_data <- read_csv(f)

  binned_popsize_df <- pop_data %>% filter(question=="popSize") %>% 
    mutate(pop_size_bin= case_when(
      Median == 0 ~ 'Zero, no individuals believed extant (presumed extinct)',
      Median >=1 & Median < 50 ~ '1-50 individuals',
      Median >= 50 & Median < 250 ~ '50-250 individuals',
      Median >= 250 & Median < 1000 ~ '250-1,000 individuals',
      Median >= 1000 & Median < 2500 ~ '1,000-2,500 individuals',
      Median >= 2500 & Median < 10000 ~ '2,500-10,000 individuals',
      Median >= 10000 & Median < 100000 ~ '10,000 - 100,000 individuals',
      Median >= 100000 & Median < 1000000 ~ '100,000 - 1,000,000 individuals',
      Median > 1000000 ~ '>1,000,000 individuals'
      )
    ) %>% select(species, Median, pop_size_bin)
  
  binned_poptrend_df <- pop_data %>% filter(question=="popTrend") %>% 
    mutate(pop_trend_bin = case_when(
      Median <= -0.9 ~ "Decline of >90%",
      Median <= -0.8 & Median > -0.9 ~"Decline of 80-90%",
      Median <= -0.7 & Median > -0.8 ~ "Decline of 70-80%",
      Median <= -0.5 & Median > -0.7 ~ "Decline of 50-70%",
      Median <= -0.3 & Median > -0.5 ~ "Decline of 30-50%",
      Median <= -0.1 & Median > -0.3 ~ "Decline of 10-30%",
      Median > -0.1 & Median < 0.1 ~ "Relatively Stable (<=10% change)",
      Median >= 0.1 & Median < 0.25 ~ "Increase of 10-25%",
      Median >= 0.25 ~ "Increase of >25%"
    )) %>% select(species, Median, pop_trend_bin)

  return(list(binned_popsize_df = binned_popsize_df,
              binned_poptrend_df = binned_poptrend_df
         ))
}
    