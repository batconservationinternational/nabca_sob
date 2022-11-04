formatSPPdata <- function(sppList) {
  
  experts <- unique(sppList[['rawdata']]$token)
  cntry <- unique(sppList[['rawdata']]$cntry)
  
  # message('add labels')
  labels <- c(experts, 'Combined')
  names(labels) <- c(experts, 'linear pool')
  
  # message('add colors')
  # expertColors <- RColorBrewer::brewer.pal(n=length(experts), name="Dark2")
  expertColors <- ggsci::pal_igv()(length(experts))
  names(expertColors) <- experts
  expertColors['linear pool'] = 'black'
  expertColors <- expertColors[!is.na(names(expertColors))]
  
  # message('add line style')
  expertLT <- rep('solid', times = length(experts))
  names(expertLT) <- experts
  expertLT['linear pool'] = 'solid'
  
  # message('add line size')
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

  #Population Size
  if (myGroup_Q == 'pop' & mySubGroup == 'Size') {
    thisU = 1e9 #max pop size = 1 B which is 10 times the max TABR estimate (100M)
    thisL = 0
    thisD = 'gamma'
    myXlab = 'Population Size'
  }
  
  #Population Trend
  ##normal distribution?
  if (myGroup_Q == 'pop' & mySubGroup == 'Trend') {
    thisU = 1.4 #median empiracle population growth rate = 1.0025 - the median expert lambda = 1.015. Given distribution (slack pub posted by WF) 1.4 is about max
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
  print(distributions)
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

  print(paste("Probs:"))
  print(probs)
  print(paste("MMM:"))
  print(mmm)
  print(paste("Lower:"))
  print(lower)
  print(paste("Upper:"))
  print(upper)
  print(paste("Dist:"))
  print(dist)
  print(paste("Expert Names:"))
  print(expert_names)
  
  
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
  
  data_Q50 <- feedback(
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










combine_dist <- function(dist) {
  if (class(dist) != 'elicitation')
    return('Not a valid elicitation')
  
  cDist <- tryCatch({
    SHELF::linearPoolDensity(dist)
  },
  error = function(e)
    e)
}







generate_sample <- function(dist_info, dist_type, Nsamples = 10000, pb) {
  
  #get distribution that we care about to use to filter samples
  dist_of_interest <- dist_type[[1]]$thisD
  #get elicitation object
  thisDist <- dist_info
  tokens <- dist_info$best.fitting %>% rownames()
  samples <- vector('list', length(tokens))
  names(samples) <- tokens
  
  # if only one sample, we don't need to worry about calculating overlap, so don't need this sample
  if (length(tokens)==1 | is.null(tokens)){
    return('single elicitation: no overlap')
  }
  
  for (i in 1:length(tokens)) {
    #generate random sample
    samples[[i]] <- SHELF::sampleFit(thisDist, expert = i, n = Nsamples) %>% 
      as_tibble()
  }
  
  # Pluck only the distribution of interest out for all experts
  samples <- map(samples, dist_of_interest) %>% as_tibble()
  
  pb$tick()
  return(samples)
}








# calc_Overlap <- function(dist_type, randomDraw, pb) {
#   
#   samples <- randomDraw
#   experts <- names(samples)
#   thisD <- dist_type[[1]]$thisD
#   thisL <- dist_type[[1]]$thisL
#   thisU <- dist_type[[1]]$thisU
#   
#   # upper <- list(rep(thisU, length(experts)))
#   # lower <- list(rep(thisL, length(experts)))
#   # boundaries <- list(upper, lower)
#   
#   lists_to_compare = list()
#   
#   for (i in 1:length(experts)){
#     exp_name <- experts[i]
#     sample <- samples[exp_name][[1]][,thisD]
#     lists_to_compare <- append(lists_to_compare, list(sample))
#   }
#   
#   names(lists_to_compare) <- experts
# 
#   over <- overlapping::ovmult(samples)$OV
#                       # type = "1", # do we want type 1 or type 2???
#   pb$tick()
#   return(over)
# }







generate_Densityplot <- function(value, q_type, dist_info, popQuantiles, dist_type, pb) {
  
  maxX <- popQuantiles[["Median"]][1] +
    (2.5 * (popQuantiles[["Q3"]][1] - popQuantiles[["Q1"]][1]))

  myplot <- SHELF:::plotfit(dist_info,
                               xl=dist_type[[1]][["thisL"]],
                               xu=maxX,
                               d=dist_type[[1]][["thisD"]],
                               lwd=2,
                               xlab=dist_type[[1]][["myXlab"]],
                               ylab=expression(f[X](x)),
                               lp = T,
                               returnPlot = T)
  pb$tick()
  return(myplot)
}
  
  
  
  




# generate_Densityplot <- function(thisRow,
#                                  save = F) {
#   print(thisRow$row)
#   require(ggplot2)
#   
#   if (class(thisRow$distFit) != 'elicitation')
#     return('Not a valid elicitation to graph')
# 
#   
#   maxX <-
#     thisRow$Quantiles$Median + (2.5 * (thisRow$Quantiles$Q3 - thisRow$Quantiles$Q1))
#   
#   myplot <- SHELF::plotfit(
#     fit = thisRow$distFit,
#     lp = T,
#     returnPlot = T,
#     d = thisRow$dist$thisD,
#     xl = thisRow$dist$thisL,
#     xu = ifelse(thisRow$groupNum <= 4,
#                 maxX,
#                 thisRow$dist$thisU),
#     # expertnames =rownames(thisRow$distFit$ssq),
#     xlab = thisRow$dist$myXlab,
#     ylab = expression(f[X](x)),
#     # lpname='Combined'
#   ) +
#     labs(title = element_blank())
#   
#   
#   if (nrow(thisRow$distFit$ssq) == 1) {
#     q <- ggplot_build(myplot)
#     q
#   }
#   
#   fx <- myplot$data$fx
#   fx[is.infinite(fx)] <- NA
#   maxY <- max(fx, na.rm = T)
#   
#   thisRow$Quantiles$y = seq(
#     from = -maxY * (1 / 1.75),
#     to = -maxY * (1 / 10),
#     length = nrow(thisRow$Quantiles)
#   )
#   thisRow$Quantiles$size = 1
#   thisRow$Quantiles[thisRow$Quantiles$expert == 'linear pool', 'size'] =
#     2
#   
#   
#   myplotLP <-   myplot +
#     scale_size_manual(values = thisRow$experts$SZ, guide = 'none') +
#     geom_pointrange(
#       data = thisRow$Quantiles,
#       aes(
#         x = Median,
#         xmin = Q1,
#         xmax = Q3,
#         y = y,
#         color = expert
#       ),
#       size = thisRow$Quantiles$size / 1.5
#     ) +
#     theme_classic() +
#     coord_cartesian(expand = F, ylim = c(min(thisRow$Quantiles$y) * 1.1, maxY *
#                                            1.1)) +
#     scale_color_manual(
#       values = c(thisRow$experts$colors),
#       name = "Expert",
#       # breaks=thisRow$Quantiles$expert,
#       labels = thisRow$experts$labels
#     ) +
#     scale_linetype_manual(values = thisRow$experts$LT, guide = 'none') +
#     scale_y_continuous(breaks = NULL)
#   
#   
#   if (nrow(thisRow$distFit$ssq) == 1) {
#     colorNOcombine <- thisRow$experts$colors
#     colorNOcombine['linear pool'] <- NA
#     myplotLP <- myplotLP +
#       scale_color_manual(
#         values = colorNOcombine,
#         name = "Expert",
#         breaks = thisRow$Quantiles$expert,
#         labels = thisRow$experts$labels[names(thisRow$experts$labels) ==
#                                           thisRow$Quantiles$expert]
#       )
#   }
#   
#   # myplotLP
#   
#   if (thisRow$Q_sub == 'Size') {
#     myplotLP <- myplotLP +
#       scale_x_log10() +
#       # scale_x_continuous(trans='log')+
#       xlab('Population size \n (axis scaled to log base 10)')
#   }
#   #
#   # print(thisRow$row)
#   if (save) {
#     ggsave(
#       plot = myplotLP,
#       filename = paste0("TestGraphs/row_",
#                         thisRow$row,
#                         ".png"),
#       width = 4,
#       height = 2.5,
#       units = "in"
#     )
#   }
#   
#   return(myplotLP)
# }











make_rangeGraphs <- function(d, spp, thiscntry) {
  
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
    summarize(area = sum(area)) %>% # do we really want to be summing these?
    # filter(area>0) %>%
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
  
  D <- d %>%
    tidyr::pivot_longer(cols = !token,
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
    )
  )
  
  h <- ggplot(data = D,
              aes(x = rangeKM,
                  y = total)) +
    geom_col() +
    labs(title = paste(spp, "in", thiscntry),
         y = "Number of Experts who Selected\n(experts could select more than one)",
         x = "Range Size\n(square km)") +
    ##add asteric for default value
    annotate(
      geom = "text",
      x = sppRangeVal,
      y = max(D$total)/2, #was 4.5 previously but changed to make dynamic
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
  
  return(h)
}









