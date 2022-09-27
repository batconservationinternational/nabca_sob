

library(purrr)
library(tidyr)
library(dplyr)
options(scipen = 999)
source(paste0(here::here(), '/RCode/SoB_f_general.R'))

DataDate='20210126'

surveyQ <- readxl::read_excel(paste0(here::here(), 
                                     "/Data/surveyQuestions.xlsx")) %>% 
  select(group=id2, contains('varName')) %>% 
  tidyr::unite(varName, varName8, col=question_sub, na.rm=T) %>% 
  unique() %>% 
  filter(!is.na(question_sub), question_sub !='')

dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]


# source(paste0(here::here(), '/RCode/survey_718871_R_syntax_file.R'), encoding = 'UTF-8')
data <- read.csv(paste0(here::here(), '/Data/results-survey718871_', DataDate, '.csv'), stringsAsFactors = F) %>% 
  # rename(token=1) %>% 
  select(all_of(dataCols)) %>% 
  select(token, cntry, sppCode, spp) %>% 
  distinct()

# generatePersonalReports <- function(thisRow) {
#   rmarkdown::render(paste0(here::here(), "/RCode/SoB_6a_PersonalReport.Rmd"),
#                 output_file=paste0(thisRow$token, '_', thisRow$cntry, '_', thisRow$sppCode, '.html'),
#                 output_dir= paste0(here::here(), '/PersonalReports/', DataDate, '/'),
#                 params= list(spp=thisRow$spp,
#                              sppCode=thisRow$sppCode,
#                              cntry=thisRow$cntry,
#                              exprt=thisRow$token,
#                              date=DataDate)
#   )
# }

pbapply(data, 1, generatePersonalReports)
