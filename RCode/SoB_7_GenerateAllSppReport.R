
library(dplyr)
options(scipen = 999)
source(paste0(here::here(), '/RCode/SoB_f_general.R'))

DataDate='20210126'

dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]

data <- read.csv(paste0(here::here(), '/Data/results-survey718871_', DataDate, '.csv'), stringsAsFactors = F) %>% 
  select(sppCode, spp, cntry) %>% 
  filter(cntry=='Canada') %>%
  distinct()

# pbapply::pbapply(data[1,], 1, generateSpeciesReports, thisDate=DataDate)

pbapply::pbapply(data[,], 1, generateSpeciesReports, thisDate=DataDate, fileType="pdf_document")
