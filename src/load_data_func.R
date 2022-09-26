formatData <- function(thisDataDate,
                       thisCountry,
                       saveData=T) {
  require(dplyr)
  require(tidyverse)
  require(tidyr)
  require(purrr)
  require(janitor)
  
  options(scipen = 999)
  
  source(paste0(here::here(), '/src/SoB_f_general.R'))
  
  thisDataDate='20220914'
  thisCountry = 'MX'
  
  # prep ------------------------------------------------------------------------------
  
  thisSheet <- "US_CA"
  
  if (thisCountry == "MX") {
    thisSheet <- "MX"
  }
  
  # get questions and group number from xml export of survey
  surveyQ <- readxl::read_excel(paste0(here::here(),
                                       "/Data/surveyQuestions.xlsx")) %>%
    select(group = id2,
           contains('varName')) %>%
    tidyr::unite(varName, varName8, col = question_sub, na.rm = T) %>%
    unique() %>%
    filter(!is.na(question_sub), question_sub != '')
  
  # get what column headings are actual data
  ## other columns are often instructions
  dataCols <-
    read.csv(paste0(here::here(), '/Data/dataColumns.csv'),
             stringsAsFactors = F)[, 1]
  
  # Read Data -----------------------------------------------------------------------------------
  
  thisDataFile <-
    paste0('Data/', thisCountry, 'results-survey687664_', thisDataDate, '.csv')
  
  data <- read.csv(thisDataFile, stringsAsFactors = F)
  
  spp_replace <- data$spp %>% str_remove("Species") %>% str_trim(side="both")
  
  NuniqueSpp <- unique(spp_replace) %>% .[.!="" & .!= "None"] %>% length()
  
  data <- data %>% 
    select(id = 1, submitdate, all_of(dataCols)) %>%
    mutate(spp = spp_replace) %>% 
    mutate(submitdate = lubridate::ymd_hms(submitdate)) %>%
    #Remove duplicate submissions
    group_by(token, cntry, sppCode) %>%
    mutate(
      max_ID = max(id),
      lastsubmission = max(submitdate),
      keep = case_when(
        submitdate >= lubridate::mdy_hm("01/01/1980 00:00") & max_ID == id & submitdate == lastsubmission  ~ T,
        T ~ F),
      # 5 letter Species Code is not unique! there is one repeat. Change to all Caps of SPPname with no spaces
      sppCode = toupper(stringr::str_replace_all(spp, " ", "_"))
    ) %>%
    filter(keep == T) %>%
    select(-id, -max_ID,-submitdate,-keep)
  
  # cN <- tolower(colnames(data))
  cN <- colnames(data)
  cN <- stringr::str_replace(cN, "\\.$", "")
  cN <- stringr::str_replace(cN, "\\.", "_")
  cN <- cN %>% tolower()
  
  colnames(data) <- cN
  
  
  # Format Data ---------------------------------------------------------------------------------
  
  d2 <- data %>%
    dplyr::select(
      'token',
      'cntry',
      'spp',
      'sppcode',
      contains('_conf'),
      contains('_min'),
      contains('_mean'),
      contains('_max'),
      matches('[a-z]{2,3}n_sub[1-9]')
    ) %>%
    mutate(across(!any_of(c(
      'token', 'cntry', 'spp', 'sppcode'
    )), fn_convertC)) %>%
    pivot_longer(
      cols = c(-1, -2, -3, -4),
      names_to = "question",
      values_to = "answer"
    ) %>%
    mutate(
      Q_group = stringr::str_extract(question, "^other|([a-z]{2,3})"),
      Q_sub = stringr::str_extract(question, "(sub|user)([1-9]0?)"),
      Q_ss = case_when(
        stringr::str_detect(question, "Sco") ~ "Scope",
        stringr::str_detect(question, "Sev") ~ "Severity",
        stringr::str_detect(question, "n_sub(10|[1-9])") ~ "Neg"
      ),
      Q_val = case_when(
        stringr::str_detect(question, "mean") ~ "mean",
        stringr::str_detect(question, "min") ~ "min",
        stringr::str_detect(question, "max") ~ "max",
        stringr::str_detect(question, "conf") ~ "conf"
      )
      # Q_sub = stringr::str_replace(question, "^[a-z]{2,3}([A-Z][a-z]*)\\.", "\\1")
    ) %>%
    mutate(Q_sub = case_when(
      grepl("popsize", question) ~ 'Size',
      grepl("poptrend", question) ~ 'Trend',
      T ~ Q_sub
    )) %>%
    arrange(Q_group, Q_sub)
  
  # Get negligible answers and code min, mean, max values
  negAns <- d2 %>%
    filter(Q_ss == "Neg") %>%
    rename(neg = answer) %>%
    select(token, cntry, spp, sppcode, Q_group, Q_sub, neg)
  
  d3 <- d2 %>%
    filter(Q_ss != "Neg" | is.na(Q_ss))
  
  if (nrow(d3) + nrow(negAns) != nrow(d2))
    warning('something went wrong generating negligible answers')
  
  # add negligible answers back to data
  
  listSPPdata <- function(thislist) {
    return(list(rawdata=thislist))
  }
  
  
  data_l <- left_join(d3, 
                      negAns,
                      by = c("token", "cntry", "spp", "sppCode", "Q_group",
                             "Q_sub")) %>%
    mutate(
      neg = case_when(is.na(neg) ~ 0,
                      T ~ neg),
      answer_C = case_when(neg == 1 ~ as.numeric(NA),
                           T ~ answer)
    ) %>%
    mutate(
      #when neg box checked, set all effects to minimum
      answer_C = case_when(
        Q_val == 'conf' & (neg == 1) ~ 100,
        Q_val == 'min' & neg == 1 ~ 0,
        Q_val == 'mean' & neg == 1  ~ 0.5,
        Q_val == 'max' & neg == 1  ~ 1,
        T ~ answer_C
      ),
      #100% confidence not accepted (at least on lower end, e.g., 0 probability), so set to almost 100
      answer_C = case_when(Q_val == 'conf' & answer_C == 100 ~ 99.99,
                           T ~ answer_C),
      #0 not a viable minimum
      answer_C = case_when(Q_val == 'min' & answer_C == 0 ~ 0.01,
                           T ~ answer_C)
    ) %>%
    left_join(surveyQ, by = c('question' = "question_sub")) %>%
    mutate(answer_C = case_when(group != 3 |
                                  Q_val == 'conf' ~ answer_C / 100,
                                T ~ answer_C)) %>%
    # remove bad answers
    filter(!(is.na(answer) & is.na(answer_C)))%>%
    select(-question,-answer) %>%
    pivot_wider(names_from = Q_val, values_from = answer_C) %>%
    #if any are NA remove whole row
    rowwise() %>%
    mutate(N_na = sum(is.na(min), is.na(mean), is.na(max), is.na(conf))) %>%
    ungroup() %>%
    filter(N_na == 0) %>%
    filter(min < mean, mean < max, min < max, conf >= 0.5) %>%
    # make into a list
    split(., .$sppCode) %>% 
    # make data into a list element 'rawdata')
    lapply(., listSPPdata) %>% 
    #extract unique Expert names from each species and create graphing parameters
    lapply(., formatSPPdata)
  
  
  
  
  if(saveData){
    saveRDS(
      data_l,
      file = paste0(
        here::here(),
        '/Data/',
        thisCountry,
        '_nestedQ_',
        thisDataDate,
        '.RDS'
      )
    )
  }
  
  return(data_l)
}
