


files <- list.files(dataFolder, full.names = T)
files <- files[grepl("scope_sev_quantiles_agg_", files)]
f <- files[str_detect(files, countrytoAnalyze)]
print(paste0('reading: ', f))
threat_data <- readRDS(f)

bin_threat <- function(val){
  bin <- case_when(
    val < 0.01 ~ "Negligible (<1%)",
    val >= 1 & val < 10
  )
  return(bin)
}
bin_threat(val)
