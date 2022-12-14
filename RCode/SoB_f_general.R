formatSPPdata <- function(sppList) {
  
  experts <- unique(sppList[['rawdata']]$token)
  cntry <- unique(sppList[['rawdata']]$cntry)

  labels <- c(experts, 'Combined')
  names(labels) <- c(experts, 'linear pool')
  
  expertColors <- ggsci::pal_igv()(length(experts))
  names(expertColors) <- experts
  expertColors['linear pool'] = 'black'
  expertColors <- expertColors[!is.na(names(expertColors))]
  
  expertLT <- rep('solid', times = length(experts))
  names(expertLT) <- experts
  expertLT['linear pool'] = 'solid'
  
  expertSZ <- rep(0.5, times = length(experts))
  names(expertSZ) <- experts
  expertSZ['linear pool'] = 1
  
  popSizeData <- sppList[['rawdata']] %>% 
    filter(Q_group == 'pop' & Q_sub == 'Size') %>%
    dplyr::select(-Q_ss) %>%
    group_by(Q_group, Q_sub) %>%
    nest() %>%
    mutate(mmm = map(data, formatMMM),
           probs=map(data, formatProbs)
    ) %>% 
    select(-data)
  
  popTrendData <- sppList[['rawdata']] %>% 
    filter(Q_group == 'pop' & Q_sub == 'Trend') %>%
    dplyr::select(-Q_ss) %>%
    group_by(Q_group, Q_sub) %>%
    nest() %>%
    mutate(mmm = map(data, formatMMM),
           probs=map(data, formatProbs)
    ) %>% 
    select(-data)
  
  threatData <-  sppList[['rawdata']]  %>% 
    filter(Q_group != 'pop') %>%
    group_by(Q_group, Q_sub, Q_ss) %>% 
    nest() %>%
    ungroup() %>%
    pivot_wider(names_from = Q_ss, values_from = data) %>%
    left_join(., read.csv(file = paste0(here::here(), '/Data/ThreatNum.csv')),
              by = c("Q_group", "Q_sub")) %>% 
    arrange(Q_group, Q_sub) %>%
    group_by(Q_group, Q_sub) %>%
    mutate( 
      ScopeMMM = map(Scope, formatMMM),
      ScopeProbs = map(Scope, formatProbs),
      SeverityMMM = map(Severity, formatMMM),
      SeverityProbs = map(Severity, formatProbs)
    ) %>% 
    nest()
  
  return(
    list(
      experts = c(experts, 'linear pool'),
      country = cntry,
      labels = labels,
      colors = expertColors,
      expertLT = expertLT,
      expertSZ = expertSZ,
      popSizeData = popSizeData,
      popTrendData = popTrendData,
      threatData = threatData
    )
  )
}





formatMMM <- function(thisData, type) {
  mmm <- t(thisData[, c('min', 'mean', 'max')])
  colnames(mmm) <- thisData$token
  return(mmm)
}






formatProbs <- function(thisData, type) {
  conf = thisData$conf/100
  dif = conf/2
  lowerP = 0.5-dif
  upperP = 0.5+dif
  medianP = rep(0.5, times = length(lowerP))
  
  probs <- matrix(
    data = c(lowerP, medianP, upperP),
    nrow = 3,
    byrow = T
  )
  
  rownames(probs) <- c('lowerP', 'medianP', 'upperP')
  colnames(probs) <- thisData$token
  
  return(probs)
}
















fn_convertC <- function(col) {
  rc <- vector(mode = 'numeric', length = length(col))
  for (i in 1:length(col)) {
    if (is.na(col[i])) {
      rc[i] = as.numeric(NA)
    } else if (toupper(col[i]) == "YES" | toupper(col[i]) == "Y") {
      rc[i] = 1
    } else if (toupper(col[i]) == "NO" |
               toupper(col[i]) == "N" | toupper(col[i]) == "") {
      rc[i] = 0
    } else if (is.character(col[i])) {
      rc[i] = as.numeric(stringr::str_replace_all(
        string = col[i],
        pattern = ",",
        replacement = ""
      ))
    } else {
      rc[i] = as.numeric(col[i])
    }
  }
  return(rc)
}









choose_pop_dist <- function(myGroup_Q, mySubGroup) {

  # Population Size
  if (myGroup_Q == 'pop' & mySubGroup == 'Size') {
    thisU = 1e9 # 10 times the max TABR estimate (100M)
    thisL = 0
    thisD = 'gamma'
    myXlab = 'Population Size'
  }
  
  #Population Trend
  if (myGroup_Q == 'pop' & mySubGroup == 'Trend') {
    thisU = 1.4 #median empirical population growth rate = 1.0025 - the median expert lambda = 1.015. Given distribution (slack pub posted by WF) 1.4 is about max
    thisL = -1
    thisD = 'normal'
    myXlab = 'Population Trend \n(% Change last 15 years)'
  }

  return(list(
    thisU = thisU,
    thisL = thisL,
    thisD = thisD,
    myXlab = myXlab
  ))
}








choose_threats_dist <- function(myGroup){
  thisU = 1
  thisL = 0
  thisD = 'beta'
  myXlab = 'Percent'
    
  return(list(
    thisU = thisU,
    thisL = thisL,
    thisD = thisD,
    myXlab = myXlab
  ))
  
}











generateDist <- function(thisRow, dat_type, scope_sev){
  thisRow <- thisRow$value[[dat_type]] %>% as_tibble()
  distributions = list()
  list_names = list()
  for (i in 1:nrow(thisRow)){
    dist_info <- make_dist(thisRow[i,], dat_type, scope_sev)
    list_names <- append(list_names, paste0(thisRow$Q_group[i],"_", thisRow$Q_sub[i]))
    to_add <- list(dist_info)
    distributions <- append(distributions, to_add)
  }
  names(distributions) <- list_names
  return(distributions)
}











make_dist <- function(thisRow, dat_type, scope_sev){
  
  lower <-  thisRow$dist %>% flatten() %>% .$thisL
  upper <-  thisRow$dist %>% flatten() %>% .$thisU
  dist <- thisRow$dist %>% flatten() %>% .$thisD
  
  if (dat_type == "popSizeData" | dat_type == "popTrendData"){
    mmm <- thisRow$mmm[[1]] %>% as_tibble() %>% as.matrix()
    probs <- thisRow$probs[[1]] %>% as_tibble() %>% as.matrix()
    
  } else if (dat_type == "threatData" & scope_sev == "scope"){
    mmm <- thisRow$data[[1]]$ScopeMMM[[1]] %>% as_tibble() %>% as.matrix()
    probs <- thisRow$data[[1]]$ScopeProbs[[1]] %>% as_tibble() %>% as.matrix()
    
  } else if (dat_type == "threatData" & scope_sev == "sev"){
    mmm <- thisRow$data[[1]]$SeverityMMM[[1]] %>% as_tibble() %>% as.matrix()
    probs <- thisRow$data[[1]]$SeverityProbs[[1]] %>% as_tibble() %>% as.matrix()
  }
  
  expert_names <- colnames(mmm)
  
  tryCatch(
    expr = {
      out <- SHELF::fitdist(
        vals = mmm,
        probs = probs,
        lower = lower,
        upper = upper,
        expertnames = expert_names
      )  
    }
  )
  
  return(out)
}








find_quantiles <- function(thisRow) {
  
  dist_id <- thisRow$dist_info_id
  thisDist <- thisRow$dist_info[[dist_id]]
  
  if (thisRow$q_type == "popSize"){
    dist_type <- "gamma"
  } else if (thisRow$q_type == "popTrend"){
    dist_type <- "normal"
  } else if (thisRow$q_type == "Scope" | thisRow$q_type == "Severity"){
    dist_type <- "beta"
  }
  
  # If more than one expert, then calc pooled quantiles
  if (nrow(thisDist$ssq) > 1) {
    Q50 <- SHELF:::qlinearpool(thisDist,
                               q = c(0.25, 0.5, 0.75, 0.999),
                               d = dist_type)
    lp_data <-
      data.frame(
        'Q1' = Q50[1],
        'Median' = Q50[2],
        'Q3' = Q50[3],
        'P99' = Q50[4],
        expert = 'linear pool'
      )
    
    data_Q50 <- SHELF::feedback(
      thisDist,
      quantiles = c(0.25, 0.5, 0.75, 0.999),
      dist = dist_type
    )$expert.quantiles
  
    data_Q50 <- as.data.frame(t(data_Q50))
    colnames(data_Q50) <- c('Q1', 'Median', 'Q3', 'P99')
    data_Q50$expert <- rownames(thisDist$best.fitting)
  
    data_Q50 <- rbind(lp_data, data_Q50)
    row.names(data_Q50) <- data_Q50$expert
    return(data_Q50)
  }
  
  # If only one expert, calc quantiles but no need to pool
  if (nrow(thisDist$ssq) == 1){
    Q50 <- SHELF::feedback(thisDist, 
                    quantiles = c(0.25, 0.5, 0.75, 0.999))$fitted.quantiles[dist_type]
    
    lp_data <-data.frame(
        'Q1' = Q50[1,1],
        'Median' = Q50[2,1],
        'Q3' = Q50[3,1],
        'P99' = Q50[4,1],
        'expert' = rownames(thisDist$best.fitting)
      )
    return(lp_data)
  }
}












generate_sample <- function(dist_info, dist_type, Nsamples = 10000, pb) {
  
  # Get distribution that we care about to use to filter samples
  dist_of_interest <- dist_type[[1]]$thisD
  
  # Get elicitation object
  thisDist <- dist_info
  tokens <- dist_info$best.fitting %>% rownames()
  samples <- vector('list', length(tokens))
  names(samples) <- tokens
  
  for (i in 1:length(tokens)) {
    # Generate random sample
    samples[[i]] <- SHELF::sampleFit(thisDist, expert = i, n = Nsamples) %>% 
      as_tibble()
  }
  
  # Pluck only the distribution of interest out for all experts
  samples <- map(samples, dist_of_interest) %>% as_tibble()
  
  pb$tick()
  return(samples)
}








generate_Densityplot <- function(value, q_type, dist_info, Quantiles, dist_type, pb) {
  
  maxX <- Quantiles[["Median"]][1] +
    (2.5 * (Quantiles[["Q3"]][1] - Quantiles[["Q1"]][1]))

  # Use SHELF package to make a plot of distributions for all experts and linear pool
  myplot <- SHELF:::plotfit(dist_info,
                               xl=dist_type[[1]][["thisL"]],
                               xu=maxX,
                               d=dist_type[[1]][["thisD"]],
                               lwd=2,
                               xlab=dist_type[[1]][["myXlab"]],
                               ylab=expression(f[X](x)),
                               lp = T,
                               returnPlot = T) +
    labs(title = element_blank())
  
  fx <- myplot$data$fx
  fx[is.infinite(fx)] <- NA
  maxY <- max(fx, na.rm = T)
  
  Quantiles$y = seq(
    from = -maxY * (1 / 1.75),
    to = -maxY * (1 / 10),
    length = nrow(Quantiles)
  )
  Quantiles$size = 1
  Quantiles[Quantiles$expert == 'linear pool', 'size'] = 2
    
  # Add point range of quantiles of experts to the plot
  myplotLP <-   myplot +
    geom_pointrange(data = Quantiles,
      aes(
        x = Median,
        xmin = Q1,
        xmax = Q3,
        y = y,
        color = expert
      ),
      size = Quantiles$size / 1.5
    ) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    coord_cartesian(expand = F, ylim = c(min(Quantiles$y) * 1.1, maxY * 1.1))
    
  pb$tick()
  return(myplotLP)
}
  
  
  
  






graphRangeQ <- function(data) {
  range <- data %>% 
    dplyr::select(cntry, spp, token, matches('range_[A-Z]')) %>%
    group_by(cntry, spp) %>%
    tidyr::nest() %>% 
    mutate(longD = map(data, make_rangeGraphs, spp, cntry))
  return(range)
}









make_rangeGraphs <- function(data, spp, thiscntry) {
  
  sciName = stringr::str_remove(spp, " \\(.*\\)")
  
  sppRangeData <-
    read.csv(paste0(here::here(), "/Data/speciesRangeArea.csv")) %>%
    mutate(
      cntry = case_when(
        stringr::str_detect(country, 'Canada') ~ 'Canada',
        stringr::str_detect(country, 'USA') ~ 'United States',
        stringr::str_detect(country, 'Mexico') ~ 'Mexico'
      )
    ) %>%
    group_by(species, spp_abbrev, cntry) %>%
    filter(area>0) %>%
    summarize(area = sum(area)) %>% # do we really want to be summing these?
    filter(species == spp, cntry == thiscntry) %>%
    mutate(
      area = measurements::conv_unit(area, from = 'm2', to = 'km2'),
      rangeClass = case_when(
        area == 0 ~ '0',
        area < 100 ~ '< 100',
        area >= 100 & area < 250 ~ '100-250',
        area >= 250 & area < 1000 ~ '250-1,000',
        area >= 1000 & area < 5000 ~ '1,000-5,000',
        area >= 5000 & area < 20000 ~ '5,000-20,000',
        area >= 20000 & area < 200000 ~ '20,000-200,000',
        area >= 200000 & area < 2500000 ~ '200,000-2,500,000',
        area > 2500000 ~ '>2,500,000'
      )
    )
  
  sppRangeVal <- sppRangeData$rangeClass
  
  D <- data %>% tidyr::pivot_longer(cols = !token,
                        names_to = 'range_size',
                        values_to = 'selected') %>%
    mutate(
      selected2 = case_when(selected == 'Yes' | selected == 'Y' ~ T,
                            T ~ F),
      range_size = str_sub(range_size,-1,-1),
      rangeKM = recode(
        .x = range_size,
        'z' = '0',
        'a' = '< 100',
        'b' = '100-250',
        'c' = '250-1,000',
        'd' = '1,000-5,000',
        'e' = '5,000-20,000',
        'f' = '20,000-200,000',
        'g' = '200,000-2,500,000',
        'h' = '>2,500,000'
      )
    ) %>%
    group_by(rangeKM) %>%
    summarize(total = sum(selected2), .groups = 'drop')
  
  
  D$rangeKM = factor(
    D$rangeKM,
    levels = c(
      '0',
      '< 100',
      '100-250',
      '250-1,000',
      '1,000-5,000',
      '5,000-20,000',
      '20,000-200,000',
      '200,000-2,500,000',
      '>2,500,000'
    ), ordered=T
  )
  
  h <- ggplot(data = D,
              aes(x = rangeKM,
                  y = total)) +
    geom_col() +
    labs(title = paste(spp, "in", thiscntry),
         y = "Number of Experts who Selected\n(experts could select more than one)",
         x = "Range Size\n(square km)") +
    ## add asterisk for default value
    annotate(
      geom = "text",
      x = sppRangeVal,
      y = max(D$total)/2, 
      label = "*",
      color = 'red',
      size = 25
    ) +
    theme_classic() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(5, max(D$total)))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = paste0(
      here::here(),
      '/RangeGraphs/range_',
      spp,
      '_',
      thiscntry,
      '.png'
    ),
    plot = h
  )
  
  return(list(range_map=h, range_data=D))
}








# Function to bin scope values
bin_scope <- function(val){
  bin <- case_when(
    val < 0.01 ~ "Negligible (<1%)",
    val >= 0.01 & val <= 0.1 ~ "Small (1-10%)",
    val >= 0.1 & val <= 0.3 ~ "Restricted (11-30%)",
    val >= 0.3 & val <= 0.7 ~ "Large (71-100%)",
    val > 0.7 ~ "Pervasive (71-100%)",
    is.na(val) ~ "Unknown"
  )
  return(bin)
}







# Function to bin severity values
bin_sev <- function(val){
  bin <- case_when(
    val < 0.01 ~ "Negligible or <1% pop. decline",
    val >= 0.01 & val <= 0.1 ~ "Slight or 1-10% pop. decline",
    val >= 0.1 & val <= 0.3 ~ "Moderate or 11-30% pop. decline",
    val >= 0.3 & val <= 0.7 ~ "Serious or 31-70% pop. decline",
    val > 0.7 ~ "Extreme or 71-100% pop. decline",
    is.na(val) ~ "Unknown"
  )
  return(bin)
}







# Function to determine impact of threat
bin_impact <- function(scope, sev){
  bin <- case_when(
    scope == "Small (1-10%)" | sev == "Slight or 1-10% pop. decline" ~ "Low",
    scope == "Restricted (11-30%)" & sev == "Moderate or 11-30% pop. decline" ~ "Low",
    scope == "Restricted (11-30%)" & 
      sev %in% c("Serious or 31-70% pop. decline", "Extreme or 71-100% pop. decline") ~ "Medium",
    scope %in% c("Large (71-100%)", "Pervasive (71-100%)") & sev == "Moderate or 11-30% pop. decline" ~ "Medium",
    scope == "Large (71-100%)" & 
      sev %in% c("Serious or 31-70% pop. decline", "Extreme or 71-100% pop. decline") ~ "High",
    scope == "Pervasive (71-100%)" & sev == "Serious or 31-70% pop. decline" ~ "High",
    scope == "Pervasive (71-100%)" & sev == "Extreme or 71-100% pop. decline" ~ "Very High",
    scope == "Negligible (<1%)" & sev == "Negligible or <1% pop. decline" ~ "Negligible",
    scope == "Unknown" | sev == "Unknown" ~ "Unknown"
  )
  return(bin)
}

