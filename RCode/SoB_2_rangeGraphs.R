graphRangeQ <- function(d) {
  
library(purrr)
library(tidyr)
library(dplyr)
options(scipen = 999)
source(paste0(here::here(), '/RCode/SoB_f_general.R'))

range <- d %>%
  dplyr::select('cntry', 'spp', 'token',
                matches('range_[A-Z]')) %>%
  group_by(cntry, spp) %>%
  tidyr::nest() %>%
  mutate(longD=map(data, make_rangeGraphs, spp, cntry))


return(range)

}