

graphRangeQ <- function(thisDataDate,
                        thisCountry) {
  
library(purrr)
library(tidyr)
library(dplyr)
options(scipen = 999)
source(paste0(here::here(), '/RCode/SoB_f_general.R'))

surveyQ <- readxl::read_excel(paste0(here::here(), 
                                     "/Data/surveyQuestions.xlsx")) %>% 
  select(group=id2, contains('varName')) %>% 
  tidyr::unite(varName, varName8, col=question_sub, na.rm=T) %>% 
  unique() %>% 
  filter(!is.na(question_sub), question_sub !='')

dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]

thisDataFile <- paste0('Data/',thisCountry, '_results_', thisDataDate, '.csv')

# source(paste0(here::here(), '/RCode/survey_718871_R_syntax_file.R'), encoding = 'UTF-8')
data <- read.csv(thisDataFile, stringsAsFactors = F) %>%
  select(id = 1, submitdate, all_of(dataCols)) %>%
  mutate(submitdate = lubridate::mdy_hm(submitdate)) %>%
  #Remove duplicate submissions
  group_by(token, cntry, sppCode) %>%
  mutate(
    max_ID = max(id),
    lastsubmission = max(submitdate),
    keep = case_when(
      submitdate == lubridate::mdy_hm("1/1/1980 0:00") &
        max(id) == id ~ T ,
      submitdate == max(submitdate)  ~ T,
      T ~ F
    )
  ) %>%
  filter(keep==T) %>%
  select(-id,-max_ID, -submitdate, -keep)


# surveyQ <- readxl::read_excel(paste0(here::here(), 
#                                      "/Data/surveyQuestions.xlsx")) %>% 
#   select(group=id2, contains('varName')) %>% 
#   tidyr::unite(varName, varName8, col=question_sub, na.rm=T) %>% 
#   unique() %>% 
#   filter(!is.na(question_sub), question_sub !='')
# 
# dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]
# 
# 
# # source(paste0(here::here(), '/RCode/survey_718871_R_syntax_file.R'), encoding = 'UTF-8')
# data <- read.csv(paste0(here::here(), '/Data/results-survey718871_', DataDate, '.csv'), stringsAsFactors = F) %>% 
#   # rename(token=1) %>% 
#   select(all_of(dataCols)) 


range <- data %>%
  dplyr::select('cntry', 'spp', 'sppCode', 'token',
                matches('range\\.[A-Z]\\.')) %>%
  # filter(sppCode=="TABRA") %>% 
  group_by(cntry, spp, sppCode) %>%
  tidyr::nest() %>%
  mutate(longD=map(data, make_rangeGraphs, spp, cntry, sppCode))

return(range)

}