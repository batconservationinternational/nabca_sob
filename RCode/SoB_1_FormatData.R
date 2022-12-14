formatData <- function(thisDataDate,
                       thisCountry,
                       countryAbbr,
                       saveData=T) {
  
  # prep ------------------------------------------------------------------------------
  clean_q_names <- function(list_of_qs){
    list_of_qs <- stringr::str_replace(list_of_qs, "\\.$", "")
    list_of_qs <- stringr::str_replace(list_of_qs, "\\.", "_")
    list_of_qs <- list_of_qs %>% tolower()
    return(list_of_qs)
  }
  
  # get questions and group number from xml export of survey
  surveyQ <- readxl::read_excel(paste0(here::here(), "/Data/surveyQuestions.xlsx")) %>%
    select(group = id2, contains('varName')) %>%
    tidyr::unite(varName, varName8, col = question_sub, na.rm = T) %>%
    unique() %>%
    filter(!is.na(question_sub), question_sub != '')
  
  surveyQ$question_sub <- clean_q_names(surveyQ$question_sub)
  
  #write cleaned surveyQ df for use downstream. 
  write_csv(surveyQ, here::here("Data", "cleanedSurveyQuestions.csv"))
  
  # get what column headings are actual data. Other columns are often instructions
  dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[, 1]
  
  # Read and clean data --------------------------------------------------------
  
  thisDataFile <- paste0('Data/',countryAbbr, '_results_', thisDataDate, '.csv')
  
  data <- read.csv(thisDataFile, stringsAsFactors = F)
  
  #fill in blank countries in MX data and merge spp columns
  if (thisCountry == "MX") {
     data <- data %>% unite("spp", NorthSpp, TropSpp, MXspp, sep="", remove=T, na.rm=T) %>% 
      mutate(cntry = case_when(
        cntry=="" ~ "Mexico", 
        T ~ cntry
      ))
  }
  
  data <- data %>% mutate(across(everything(), ~na_if(., ""))) %>% 
    filter(!grepl("[0-9]+", sppCode)) %>%  #filter out spp that are numbers
    mutate(spp = str_replace(spp, '\\(.*\\)', '')) %>% 
    mutate(spp = str_trim(spp, side = "both"))
  
  data <- data %>% 
    select(id = 1, submitdate, spp, all_of(dataCols)) %>%
    mutate(submitdate = lubridate::ymd_hms(submitdate)) %>%
    #Remove duplicate submissions
    group_by(token, cntry, spp) %>%
    mutate(
      max_ID = max(id),
      lastsubmission = max(submitdate),
      keep = case_when(
        submitdate >= lubridate::mdy_hm("01/01/1980 00:00") & max_ID == id & submitdate == lastsubmission  ~ T,
        T ~ F),
      # 5 letter spp Code is not unique! there is one repeat. Change to all caps of SPPname with no spaces
      sppCode = toupper(stringr::str_replace_all(spp, " |-", "_"))
      ) %>%
    filter(keep == T) %>% 
    select(-id, -max_ID, -submitdate,-keep)
  
  colnames(data) <- clean_q_names(colnames(data))
  
  #filter out negative pop size estimates
  weird_pop_size <- data %>% filter(popsize_sz_min<=0 | popsize_sz_mean<=0 | popsize_sz_max<=0) 
  
  data <- data %>% anti_join(weird_pop_size)
  
  data <- data %>% mutate(sppcode = gsub("SPECIES_", "", sppcode)) %>% 
    mutate(spp = gsub('Species ', '', spp))

  # Format Data ----------------------------------------------------------------
  
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
      Q_group = stringr::str_extract(question, ".*(?=sco)|.*(?=sev)|(pop)|.*(?=n_sub)"), 
      Q_sub = stringr::str_extract(question, "(sub|user)([1-9]0?)"),
      Q_ss = case_when(
        stringr::str_detect(question, "sco") ~ "Scope",
        stringr::str_detect(question, "sev") ~ "Severity",
        stringr::str_detect(question, "n_sub(10|[1-9])") ~ "Neg"
      ),
      Q_val = case_when(
        stringr::str_detect(question, "mean") ~ "mean",
        stringr::str_detect(question, "min") ~ "min",
        stringr::str_detect(question, "max") ~ "max",
        stringr::str_detect(question, "conf") ~ "conf"
      )
    ) %>%
    mutate(Q_sub = case_when(
      grepl("popsize", question) ~ 'Size',
      grepl("poptrend", question) ~ 'Trend',
      T ~ Q_sub
    )) %>%
    arrange(Q_group, Q_sub)
  
  weird_percentage <- d2 %>% 
    filter(Q_sub != "Size") %>% 
    filter(answer < -100 | answer > 100)
  
  # filter out instances where %'s are outside of -100 to 100
  d2 <- d2 %>% anti_join(weird_percentage) 
  
  # Get negligible answers and code min, mean, max values
  negAns <- d2 %>%
    filter(Q_ss == "Neg") %>%
    rename(neg = answer) %>%
    select(-Q_ss, -Q_val, -question)
  
  d3 <- d2 %>% filter(Q_ss != "Neg" | is.na(Q_ss)) %>% #include NA values b/c otherwise they are dropped. NA's are pop size q's. 
    mutate(answer = case_when(
      stringr::str_detect(question, "poptrend|sev|scope") & stringr::str_detect(question, "min$|mean$|max$") ~ answer/100,
      T ~ answer
    )) #turn poptrend and scope and severity responses into percentage between 0-1 
      #but not for conf because that gets done in the formatProbs function
  
  if (nrow(d3) + nrow(negAns) != nrow(d2)){
    warning('something went wrong generating negligible answers')
  }
  
  # add negligible answers back to data
  listSPPdata <- function(thislist) {
    return(list(rawdata=thislist))
  }
  
  d4 <- left_join(d3, negAns,
                      by = c("token", "cntry", "spp", "sppcode", "Q_group",
                             "Q_sub")) %>%
    mutate(
      neg = case_when(is.na(neg) ~ 0,
                      T ~ neg),
      answer_C = case_when(neg == 1 ~ as.numeric(NA),
                           T ~ answer)
    ) %>%
    mutate(
      #when neg box checked, set all effects to minimum
      #100% confidence not accepted (at least on lower end, e.g., 0 probability), so set to almost 100
      #0 not a viable minimum so set to 0.01
      answer_C = case_when(
        Q_val == 'conf' & neg == 1 ~ 99.99, # will get divided by 100 in formatProbs
        Q_val == 'min' & neg == 1 ~ 0.0001,
        Q_val == 'mean' & neg == 1  ~ 0.005,
        Q_val == 'max' & neg == 1  ~ 0.01,
        T ~ answer_C
      ),
    ) %>% 
    select(-question, -answer) %>%
    pivot_wider(names_from = Q_val, values_from = answer_C) %>%
    # correct for instances where people failed to answer min b/c couldn't input 0.
    # assume that they meant to put 0 if they answered for min, mean, and conf but
    # left min blank
    mutate(across(c('min','mean','max','conf'), as.numeric)) %>% 
    mutate(min = case_when(
      # use min of 0 if other values were filled out but min was missed
      (is.na(min) & mean>0 & max>0 & conf>0) ~ 0.0001,
      T ~ min
    )) %>% 
    mutate(mean = case_when(
      # impute mean if they filled out the other three values but left mean blank
      (is.na(mean) & !is.na(min) & !is.na(max) & !is.na(conf)) ~ ((min+max)/2),
      T ~ mean
    )) %>% 
    # set confidence for them if they put in min, mean, max but failed to put conf
    mutate(conf = case_when(
      (is.na(conf) & !is.na(min) & !is.na(mean) & !is.na(max)) ~ 100,
       T ~ conf
    ))
  
  # create a df where any answer is na
  na_answers <- d4 %>% filter_at(vars(conf:max), any_vars(is.na(.)))
  
  data_l <- d4 %>% rowwise() %>%
    # if any fields are NA remove whole row
    mutate(N_na = sum(is.na(min), is.na(mean), is.na(max), is.na(conf))) %>%
    ungroup() %>%
    filter(N_na == 0) %>%
    select(-N_na) %>% 
    filter(min < mean, mean < max, min < max, conf >= 50) %>% 
    # replace conf of 100 with 99.99
    mutate(conf = if_else(conf==100, 99.99, conf)) %>% 
    # replace 0s in min/mean/max with 0.0001
    mutate(across(c(min, mean, max), ~if_else(.==0, 0.0001, .))) %>%
    # replace 1s in min/mean/max with 0.9999
    mutate(across(c(min, mean, max), ~if_else(.==1, 0.9999, .))) %>% 
    # replace -1s in min/mean/max with -0.9999
    mutate(across(c(min, mean, max), ~if_else(.== -1, -0.9999, .)))
    # 0s were causing issues with fitting log distributions later on. 

  
  # Identify instances where people answered Scope but not Sev q's or vice versa
  tot_answers <- data_l %>% filter(Q_ss %in% c('Scope','Severity')) %>%
    group_by(spp, cntry, Q_group, Q_sub, Q_ss) %>% summarise(total_answers=n())
  
  missing_answers <- data_l %>% filter(Q_ss %in% c('Scope','Severity')) %>%
    group_by(token, spp, cntry, Q_group, Q_sub) %>% summarise(num_answers=n()) %>%
    filter(num_answers != 2) %>% select(-num_answers)
  
  missing_answers <- missing_answers %>% left_join(tot_answers, by = c("spp", "cntry","Q_group","Q_sub"))
  
  # Filter out instances where people didn't answer either scope or severity for a q
  # Filter down to relevant country
  data_l <- data_l %>% anti_join(missing_answers) %>% 
    filter(cntry==thisCountry)
  
  # Code to drop identified outliers for the MX dataset
  if (thisCountry=="Mexico"){
    outliers <- read_excel(here::here('Data','mx_identified_outliers.xlsx')) %>%
      select(1,2,4) %>% mutate(Q_group = "pop") %>%
      mutate(Q_sub = case_when(
        Question=="PS" ~ "Size",
        Question=="PT" ~ "Trend"
      )) %>% select(-Question) %>%
      rename("token"=Id, "spp"=Species)

    outliers_full <- outliers %>% left_join(data_l) %>% select(-Q_ss)
    data_l <- data_l %>% anti_join(outliers)
  }
  
  data_nested <- data_l %>% 
    # make into a list
    split(., .$sppcode) %>% 
    # make data into a list element 'rawdata'
    lapply(., listSPPdata) %>% 
    #extract unique Expert names from each spp and create graphing parameters
    lapply(., formatSPPdata)
  
  
  if(saveData){
    saveRDS(data_l, file = paste0(here::here(), '/Data/', thisCountry, '_nestedQ_',
        thisDataDate, '.RDS'))
  }
  
  return(list(data_nested = data_nested, 
              data_l = data_l, 
              data = data,
              weird_pop_size = weird_pop_size,
              weird_percentage = weird_percentage,
              na_answers = na_answers))
}
