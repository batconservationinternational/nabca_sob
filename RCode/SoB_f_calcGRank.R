get_g_rank <- function(dataFolder, countrytoAnalyze, dataDate){
  
  files <- list.files(dataFolder, full.names = T)
  
  # load range extent data
  f <- files[str_detect(files, "range_")]
  print(paste0('reading range file: ', f))
  range_df <- read_csv(f) %>% 
    mutate(species = str_to_upper(species)) %>% 
    mutate(species = str_replace(species, " ", "_"))
  
  # load overall threat impact data
  f <- files[str_detect(files, "impact_bins_")]
  print(paste0('reading threat impact file: ', f))
  impact <- read_excel(f, sheet=1)
  
  # load pop data
  f <- files[str_detect(files, "pop_bins_")]
  print(paste0('reading pop file: ', f))
  pop_size <- read_excel(f, sheet=1)
  pop_trend <- read_excel(f, sheet=2)
  
  pop_size_df <- pop_size %>% 
    mutate(pop_size_weighted_val = case_when(
      # values below come from NatureServe calculator "RULES" tab
      pop_size_bin=="Zero, no individuals believed extant (presumed extinct)" | 
        pop_size_bin=="1-50 individuals" ~ 0,
      pop_size_bin=="50-250 individuals" ~ 1.58,
      pop_size_bin=="250-1,000 individuals" ~ 3.14,
      pop_size_bin=="1,000-2,500 individuals" ~ 4.72,
      pop_size_bin=="2,500-10,000 individuals" ~ 6.28,
      pop_size_bin=="10,000 - 100,000 individuals" ~ 7.86,
      pop_size_bin=="100,000 - 1,000,000 individuals" ~ 9.42,
      pop_size_bin==">1,000,000 individuals" ~ 11
    )) %>% select(-Median)
  
  pop_trend_df <- pop_trend %>% 
    mutate(pop_trend_weighted_val = case_when(
      # values below come from NatureServe calculator "RULES" tab
      pop_trend_bin=="Decline of >90%" ~ -1,
      pop_trend_bin=="Decline of 80-90%" ~ -0.8,
      pop_trend_bin=="Decline of 70-80%" ~ -0.62,
      pop_trend_bin=="Decline of 50-70%" ~ -0.44,
      pop_trend_bin=="Decline of 30-50%" ~ -0.28,
      pop_trend_bin=="Decline of 10-30%" ~ -0.14,
      pop_trend_bin=="Relatively Stable (<=10% change)" ~ 0,
      pop_trend_bin=="Increase of 10-25%" ~ 0.14,
      pop_trend_bin=="Increase of >25%" ~ 0.28
    )) %>% select(-Median)
  
  impact_df <- impact %>% 
    mutate(threat_weighted_val = case_when(
      # values below come from NatureServe calculator "RULES" tab
      overall_impact_val=="Very High" ~ 0,
      overall_impact_val=="High" ~ 1.83,
      overall_impact_val=="Medium" ~ 3.67,
      overall_impact_val=="Low" ~ 5.5
    )) 
  
  range_df <- range_df %>% 
    mutate(range_weighted_val = case_when(
      # values below come from NatureServe calculator "RULES" tab
      range=="0" | range=="< 100" ~ 0,
      range=="100-250" ~ 0.79,
      range=="250-1,000" ~ 1.57,
      range=="1,000-5,000" ~ 2.36,
      range=="5,000-20,000" ~ 3.14,
      range=="20,000-200,000" ~ 3.93,
      range=="200,000-2,500,000" ~ 4.71,
      range ==">2,500,000" ~ 5.5
    )) 
  
  df <- pop_size_df %>% left_join(pop_trend_df) %>% 
    left_join(impact_df) %>% 
    left_join(range_df) %>% 
    # next two steps follow NatureServe "Calculate" tab formulas, with the constants
    # being their assigned weights
    mutate(
      pop_weighted_val = (pop_size_weighted_val + range_weighted_val)/3
    ) %>% 
    mutate(
      g_rank_num = pop_weighted_val*0.7 + threat_weighted_val*0.3 + pop_trend_weighted_val
    ) %>% 
    # G-Rank assignment ranges on bottom of "RULES" tab in NatureServe tool
    mutate(
      g_rank = case_when(
        g_rank_num<=1.5 ~ "G1",
        g_rank_num>1.5 & g_rank_num<=2.5 ~ "G2",
        g_rank_num>2.5 & g_rank_num<=3.5 ~ "G3",
        g_rank_num>3.5 & g_rank_num<=4.5 ~ "G4",
        g_rank_num>4.5 ~ "G5"
      )
    )
  
  out <- df %>% select(species, pop_size_bin, pop_trend_bin, overall_impact_val,
                       range, g_rank)
  return(out)
}
