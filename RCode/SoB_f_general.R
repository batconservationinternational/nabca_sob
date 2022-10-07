formatSPPdata <- function(sppList) {
  
  # print(unique(sppList[['rawdata']]$sppCode))
  
  experts <- unique(sppList[['rawdata']]$token)
  
  # message('add labels')
  labels <- c(experts, 'Combined')
  names(labels) <- c(experts, 'linear pool')
  
  # message('add colors')
  # expertColors <- RColorBrewer::brewer.pal(n=length(experts), name="Dark2")
  expertColors <- ggsci::pal_igv()(length(experts))
  names(expertColors) <- experts
  expertColors['linear pool'] = 'black'
  # expertColors['Combined'] ='black'
  expertColors <- expertColors[!is.na(names(expertColors))]
  
  # message('add line style')
  expertLT <- rep('solid', times = length(experts))
  names(expertLT) <- experts
  expertLT['linear pool'] = 'solid'
  # expertLT['Combined'] ='solid'
  
  # message('add line size')
  expertSZ <- rep(0.5, times = length(experts))
  names(expertSZ) <- experts
  expertSZ['linear pool'] = 1
  # expertSZ['Combined'] =1
  
  popData <- sppList[['rawdata']] %>% #[["ANTROZOUS_PALLIDUS"]]
    filter(Q_group == 'pop') %>%
    dplyr::select(-Q_ss) %>%
    group_by(Q_group, Q_sub) %>%
    nest() %>%
    mutate(mmm = map(data, formatMMM, "mmm"),
           probs=map(data, formatProbs, "mmm")
    ) %>% 
    select(-data)
  
  
  threatData <-  sppList[['rawdata']]  %>% #[["ANTROZOUS_PALLIDUS"]]
    filter(Q_group != 'pop') %>%
    group_by(cntry, spp, sppcode, Q_group, Q_sub, Q_ss) %>%
    nest() %>%
    ungroup() %>%
    pivot_wider(names_from = Q_ss, values_from = data) %>%
    left_join(., read.csv(file = paste0(here::here(), '/Data/ThreatNum.csv')),
              by = c("Q_group", "Q_sub")) %>% #names in these aren't matched up
    arrange(Q_group, Q_sub) %>%
    group_by(Q_group, Q_sub) %>%
    nest() %>% 
    mutate( #issue here with list structure getting passed to the function
      ScopeMMM = map(data, formatMMM, type = 'Scope'),
      ScopeProbs = map(data, formatProbs, type = 'Scope'),
      SeverityMMM = map(data, formatMMM, type = 'Severity'),
      SeverityProbs = map(data, formatProbs, type = 'Severity')
    ) %>% 
    select(-data)
  
  
  
  return(
    list(
      experts = c(experts, 'linear pool'),
      labels = labels,
      colors = expertColors,
      expertLT = expertLT,
      expertSZ = expertSZ,
      popData = popData,
      threatData = threatData
    )
  )
}


formatMMM <- function(thisData, type) {
  
  # print(class(thisData))
  # print(type)
  
  if(type=="mmm"){
    mmm <- t(thisData[, c('min', 'mean', 'max')])
    colnames(mmm) <- thisData$token
  }
  
  
  # thisData = mydata2[['data']][[1]]
  if(type == 'Scope' | type== 'Severity') {
    mmm <- t(thisData[[type]][[1]][, c('min', 'mean', 'max')])
    colnames(mmm) <- thisData[[type]][[1]]$token
  }
  
  return(mmm)
}


formatProbs <- function(thisData, type) {
  
  # print(class(thisData))
  # print(type)
  # print(thisData)
  
  if(type=="mmm"){
    
    # conf=0.8
    # dif=(1-conf)/2 #0.1
    # 
    # minprob=0.5-dif #0.4
    # maxprob=0.5+dif #0.6
    
    conf = thisData$conf/100
    dif = (1-conf)/2
    lowerP = 0.5-dif
    upperP = 0.5+dif
    # lowerP = (1-conf)/2
    # lowerP = (1 - (thisData$conf)) / 2
    # upperP = 1 - lowerP
    medianP = rep(0.5, times = length(lowerP))
    
    probs <- matrix(
      data = c(lowerP, medianP, upperP),
      nrow = 3,
      byrow = T
    )
    
    rownames(probs) <- c('lowerP', 'medianP', 'upperP')
    colnames(probs) <- thisData$token
  }
  
  if(type == 'Scope' | type=='Severity'){
    x = thisData[[type]][[1]]$conf
    conf = x/100
    dif = (1-conf)/2
    lowerP = 0.5-dif
    upperP = 0.5+dif
    # lowerP = (1 - (thisData[[type]]$conf)) / 2
    # upperP = 1 - lowerP
    medianP = rep(0.5, times = length(lowerP))
    
    probs <- matrix(
      data = c(lowerP, medianP, upperP),
      nrow = 3,
      byrow = T
    )
    rownames(probs) <- c('lowerP', 'medianP', 'upperP')
    colnames(probs) <- thisData[[type]]$token
  }
  
  
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
               toupper(col[i]) == "N" | is.na(col[i]) | toupper(col[i]) == "") {
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


getSPPdistributions <- function(thisSPPdata) {
  popDist <-
    lapply(thisSPPdata$popData,
           lowerLimit,
           upperLimit,
           distribution)
  
  
}

generatePopSizeDist <- function(popSizeData,
                                lowerLimit=0,
                                upperLimit=1e9,
                                distribution='gamma'){
  
  
  
}

generateDist <- function(thisRow, 
                         LowerLimit,
                         UpperLimit,
                         Distribution
) {
  # print(thisRow)
  # class(thisRow)
  # require(SHELF)
  # print(thisRow$row)
  
  mmm <- thisRow$mmm
  probs <- thisRow$probs
  dist=Distribution
  
  mmm['min', mmm['min', ] == thisRow$dist$thisL] <-
    sign(thisRow$dist$thisL) * (abs(thisRow$dist$thisL) - 1e-9)
  mmm['max', mmm['max', ] == thisRow$dist$thisU] <-
    sign(thisRow$dist$thisU) * (abs(thisRow$dist$thisU) - 1e-9)
  
  if (ncol(mmm) != ncol(probs)) {
    stop(c("min mean max and probabilities not same number of experts"))
  }
  
  # if (length(thisRow$experts$names) != ncol(mmm)) {
  #   stop(c("expert names not same length as data"))
  # }
  
  # names <- names(thisRow$experts$names)
  names <- (thisRow$data$token)
  na_cols <-
    unique(which(colSums(is.na(mmm)) > 0), which(colSums(is.na(probs)) > 0))
  
  
  if (length(na_cols) > 0) {
    mmm <- mmm[, -na_cols]
    probs <- probs[, -na_cols]
    names <- names[-na_cols]
  }
  
  
  tryCatch({
    dist <- SHELF::fitdist(
      vals = mmm,
      probs = probs,
      # dist=thisRow$dist$thisD,
      lower = thisRow$dist$thisL,
      upper = thisRow$dist$thisU,
      expertnames = names
    )
    
    return(dist)
  },
  error = function(e)
    e)
}
















choose_dist <- function(myGroup_Q, myGroup_sub) {
  # print(myGroup)
  
  # myGroup = as.character(myGroup) #Pass groupID of page
  
  
  #Population Size
  # if(as.numeric(myGroup)==3){
  if (myGroup_Q == 'pop' & myGroup_sub == 'Size') {
    thisU = 1e9 #max pop size = 1 B which is 10 times the max TABR estimate (100M)
    thisL = 0
    thisD = 'gamma'
    myXlab = 'Population Size'
  }
  
  #Population Trend
  ##normal distribution?
  # if(as.numeric(myGroup)==4){
  if (myGroup_Q == 'pop' & myGroup_sub == 'Trend') {
    thisU = 1.4 ^ 15 #median empiracle population growth rate = 1.0025 - the median exbert lambda = 1.015. Given distribution (slack pub posted by WF) 1.4 is about max
    thisL = -1
    thisD = 'normal'
    myXlab = 'Population Trend \n(% Change last 15 years)'
  }
  
  #threats
  ##group 8 = development
  if (stringr::str_detect(myGroup_sub, 'sub')) {
    thisU = 1
    thisL = 0
    thisD = 'beta'
    myXlab = 'Percent'
  }
  
  return(list(
    thisU = thisU,
    thisL = thisL,
    thisD = thisD,
    myXlab = myXlab
  ))
  
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


generate_Densityplot <- function(thisRow,
                                 save = F) {
  print(thisRow$row)
  require(ggplot2)
  
  if (class(thisRow$distFit) != 'elicitation')
    return('Not a valid elicitation to graph')
  
  # #This screws up the names, so have to go to internal SHELF functions to get it right
  # SHELF::plotfit(fit = thisRow$distFit,
  #                      lp = T,
  #                      returnPlot = T,
  #                      d=thisRow$dist$thisD,
  #                      xlab=thisRow$dist$myXlab,
  #
  #                          )
  #
  # myplot <- SHELF:::makeLinearPoolPlot(thisRow$distFit,
  #                              xl=thisRow$dist$thisL,
  #                              xu=ifelse(thisRow$groupNum<=4, Inf, thisRow$dist$thisU),
  #                              d=thisRow$dist$thisD,
  #                              # expertnames =rownames(thisRow$distFit$ssq),
  #                              lwd=2,
  #                              # ql=0.25,
  #                              # qu=0.75,
  #                              xlab=thisRow$dist$myXlab,
  #                              ylab=expression(f[X](x)),
  #                              lpname='linear pool')
  
  #This screws up the names, so have to go to internal SHELF functions to get it right
  
  maxX <-
    thisRow$Quantiles$Median + (2.5 * (thisRow$Quantiles$Q3 - thisRow$Quantiles$Q1))
  
  myplot <- SHELF::plotfit(
    fit = thisRow$distFit,
    lp = T,
    returnPlot = T,
    d = thisRow$dist$thisD,
    xl = thisRow$dist$thisL,
    xu = ifelse(thisRow$groupNum <= 4,
                maxX,
                thisRow$dist$thisU),
    # expertnames =rownames(thisRow$distFit$ssq),
    xlab = thisRow$dist$myXlab,
    ylab = expression(f[X](x)),
    # lpname='Combined'
  ) +
    labs(title = element_blank())
  
  
  if (nrow(thisRow$distFit$ssq) == 1) {
    q <- ggplot_build(myplot)
    q
  }
  
  fx <- myplot$data$fx
  fx[is.infinite(fx)] <- NA
  maxY <- max(fx, na.rm = T)
  
  thisRow$Quantiles$y = seq(
    from = -maxY * (1 / 1.75),
    to = -maxY * (1 / 10),
    length = nrow(thisRow$Quantiles)
  )
  thisRow$Quantiles$size = 1
  thisRow$Quantiles[thisRow$Quantiles$expert == 'linear pool', 'size'] =
    2
  # maxX <-
  
  
  myplotLP <-   myplot +
    scale_size_manual(values = thisRow$experts$SZ, guide = 'none') +
    geom_pointrange(
      data = thisRow$Quantiles,
      aes(
        x = Median,
        xmin = Q1,
        xmax = Q3,
        y = y,
        color = expert
      ),
      size = thisRow$Quantiles$size / 1.5
    ) +
    theme_classic() +
    coord_cartesian(expand = F, ylim = c(min(thisRow$Quantiles$y) * 1.1, maxY *
                                           1.1)) +
    scale_color_manual(
      values = c(thisRow$experts$colors),
      name = "Expert",
      # breaks=thisRow$Quantiles$expert,
      labels = thisRow$experts$labels
    ) +
    scale_linetype_manual(values = thisRow$experts$LT, guide = 'none') +
    scale_y_continuous(breaks = NULL)
  
  
  if (nrow(thisRow$distFit$ssq) == 1) {
    colorNOcombine <- thisRow$experts$colors
    colorNOcombine['linear pool'] <- NA
    myplotLP <- myplotLP +
      scale_color_manual(
        values = colorNOcombine,
        name = "Expert",
        breaks = thisRow$Quantiles$expert,
        labels = thisRow$experts$labels[names(thisRow$experts$labels) ==
                                          thisRow$Quantiles$expert]
      )
  }
  
  # myplotLP
  
  if (thisRow$Q_sub == 'Size') {
    myplotLP <- myplotLP +
      scale_x_log10() +
      # scale_x_continuous(trans='log')+
      xlab('Population size \n (axis scaled to log base 10)')
  }
  #
  # print(thisRow$row)
  if (save) {
    ggsave(
      plot = myplotLP,
      filename = paste0("TestGraphs/row_",
                        thisRow$row,
                        ".png"),
      width = 4,
      height = 2.5,
      units = "in"
    )
  }
  
  return(myplotLP)
}

find_quantiles <- function(thisRow) {
  print(thisRow$row)
  
  if (class(thisRow$distFit) != 'elicitation')
    return('Not a valid elicitation')
  
  
  # Q50 <-
  #   SHELF:::qlinearpool(thisRow$distFit, q = c(0.25,0.5,0.75), d = thisRow$dist$thisD)
  #
  # lp_data <- data.frame('Q1'=Q50[1], 'Median'=Q50[2], 'Q3'=Q50[3],
  #                       expert='Combined')
  # y=-0.5, #y position for graphing
  # size=2) #line size for graphing
  
  
  
  if (nrow(thisRow$distFit$ssq) > 1) {
    print(c(thisRow$row, 'more than 1'))
    
    Q50 <-
      SHELF:::qlinearpool(thisRow$distFit,
                          q = c(0.25, 0.5, 0.75, 0.999),
                          d = thisRow$dist$thisD)
    
    lp_data <-
      data.frame(
        'Q1' = Q50[1],
        'Median' = Q50[2],
        'Q3' = Q50[3],
        'P99' = Q50[4],
        expert = 'linear pool'
      )
    
    data_Q50 <- feedback(
      thisRow$distFit,
      quantiles = c(0.25, 0.5, 0.75, 0.999),
      dist = thisRow$dist$thisD
    )$expert.quantiles
    
    data_Q50 <- as.data.frame(t(data_Q50))
    colnames(data_Q50) <- c('Q1', 'Median', 'Q3', 'P99')
    data_Q50$expert <- rownames(thisRow$distFit$best.fitting)
    # #y position for graphing
    # data_Q50$y= seq(from=-1, to =-3, length.out=nrow(thisRow$distFit$ssq))
    # #line size for graphing
    # data_Q50$size=1
    
    data_Q50 <- rbind(lp_data, data_Q50)
    row.names(data_Q50) <- data_Q50$expert
    return(data_Q50)
  }
  
  if (nrow(thisRow$distFit$ssq) == 1) {
    # lp_data$expert <- rownames(thisRow$distFit$ssq)
    Q50 <-
      feedback(thisRow$distFit, quantiles = c(0.25, 0.5, 0.75, 0.999))$fitted.quantiles[thisRow$dist$thisD]
    lp_data <-
      data.frame(
        'Q1' = Q50[1, 1],
        'Median' = Q50[2, 1],
        'Q3' = Q50[3, 1],
        'P99' = Q50[4, 1],
        expert = rownames(thisRow$distFit$ssq)
      )
  }
  
  return(lp_data)
  
}

generate_PersonalPlot <- function(thisRow) {
  print(thisRow$row)
  require(ggplot2)
  
  if (!('ggplot' %in% class(thisRow$D_plot)))
    return('no data to graph')
  
  experts <- as.character(rownames(thisRow$distFit$ssq))
  if (length(experts) > 1) {
    experts <- sort(experts[experts != 'linear pool'])
  }
  labs_master <- thisRow$experts$labels
  
  # if(length(experts)==1) return('single elicitation')
  
  exPlot <- vector(mode = 'list', length = length(experts))
  names(exPlot) <- experts
  
  for (i in experts) {
    labs = labs_master
    labs[names(labs) == i] = 'YOU'
    # if(length(experts)>1) labs=c('Combined', labs)
    
    fx = (thisRow$D_plot$data$fx)
    max = max(fx[which(fx < Inf)])
    
    exPlot[[i]] <- thisRow$D_plot +
      scale_color_manual(
        values = c(thisRow$experts$colors),
        name = element_blank(),
        # breaks=thisRow$Quantiles$expert,
        labels = labs
      ) +
      # labs(caption=paste0('You overlap other experts by ', round(ThisOverlap, 2)*100, '%'))+
      theme(plot.margin = margin(
        t = 2,
        r = 1,
        b = 0,
        l = 0.25,
        unit = 'cm'
      ))
    
    
    
    
    if (length(experts) == 1) {
      exPlot[[i]] <- exPlot[[i]] +
        scale_colour_manual(values = '#1B9E77', labels = c('You'))
      # geom_line(color=thisRow$experts$colors[names(thisRow$experts$colors)==experts])
      # scale_colour_manual(values=thisRow$experts$colors[names(thisRow$experts$colors)==experts], labels=c('You'))
    }
    
    if (length(experts) > 1) {
      ThisOverlap <- thisRow$overlap[[i]]
      exPlot[[i]] <- exPlot[[i]] +
        labs(caption = paste0(
          'You overlap other experts by ',
          round(ThisOverlap, 2) * 100,
          '%'
        ))
    }
    
  }
  
  names(exPlot) <- experts
  # exPlot
  
  return(exPlot)
}



make_rangeGraphs <- function(d, spp, thiscntry) {
  # # Set arguments if testing code
  # i=2
  # d=range$data[[i]]
  # spp=range$spp[i]
  # sppcode=range$sppcode[i]
  # thiscntry=range$cntry[i]
  
  sciName = stringr::str_remove(spp, " \\(.*\\)")
  #function
  require(ggplot2)
  
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
    summarize(area = sum(area)) %>%
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
        area >= 5000 &
          area < 20000 ~ '5,000-20,000',
        area >= 20000 &
          area < 200000 ~ '20,000-200,000',
        area >= 200000 &
          area < 2500000 ~ '200,000-2,500,000',
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
      y = 4.5,
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



generate_sample <- function(thisRow,
                            Nsamples = 10000) {
  print(thisRow$row)
  
  tokens <- rownames(thisRow$distFit$ssq)
  
  samples <- vector('list', length(tokens))
  names(samples) <- names(tokens)
  
  # if(length(tokens)==1 | is.null(tokens)) return('single elicitation: no overlap')
  
  for (i in 1:length(tokens)) {
    samples[[i]] <-
      SHELF::sampleFit(thisRow$distFit, expert = i, n = Nsamples)
  }
  
  names(samples) <- tokens
  return(samples)
}

calc_Overlap <- function(thisRow) {
  print(thisRow$row)
  
  experts <- rownames(thisRow$distFit$ssq)
  if (length(experts) == 1 |
      is.null(experts))
    return('single valid elicitation: no overlap')
  
  samples <- thisRow$randomDraw
  thisD <- thisRow$dist$thisD
  # names(samples)
  boundaries <-
    list(
      from = c(thisRow$dist$thisL, thisRow$dist$thisL),
      to = c(thisRow$dist$thisU, thisRow$dist$thisU)
    )
  
  
  over <- vector('list', length(samples))
  
  for (i in 1:length(samples)) {
    ex <- samples[[i]][, thisD]
    otherEX = plyr::ldply(samples[-i])[, thisD]
    
    thisCompare <- list(ex, otherEX)
    names(thisCompare) <- c('YOU', 'All Others')
    
    if (thisRow$dist$thisU == Inf) {
      maxB <- max(c(ex, otherEX), na.rm = T)
      
      boundaries <-
        list(
          from = c(thisRow$dist$thisL, thisRow$dist$thisL),
          to = c(maxB, maxB)
        )
    }
    
    
    
    over[[i]] <-
      overlapping::overlap(thisCompare, boundaries = boundaries)$OV
    
  }
  
  names(over) <- experts
  return(over)
}

save_PersonalPlot <- function(thisRow, PPsaveDir, PPcntry, PPspp) {
  # thisRow
  require(ggplot2)
  for (e in names(thisRow$plots)) {
    ggsave(
      thisRow$plots[[e]],
      filename = paste0(
        PPsaveDir,
        '/',
        e,
        '_',
        PPcntry,
        '_',
        PPspp,
        '_Pop_',
        thisRow$Q_sub,
        '.png'
      ),
      width = 9,
      height = 6,
      units = "in"
    )
    
  }
}

generatePersonalReports <- function(thisRow,
                                    thisDate,
                                    outDir = "C:/Users/mwhitby/Documents/SoB_SpeciesReports") {
  outDir <- paste0(outDir, '/', thisDate)
  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }
  
  out_fn = paste0(outDir, '/',
                  thisRow['token'], '_', thisRow['cntry'], '_', thisRow['sppCode'], '.html')
  
  rmarkdown::render(
    paste0(here::here(), "/RCode/SoB_6a_PersonalReport.Rmd"),
    # output_file=paste0(thisRow['token'], '_', thisRow['cntry'], '_', thisRow['sppCode'], '.html'),
    # output_dir= paste0(here::here(), '/PersonalReports/', DataDate, '/'),
    output_file = out_fn,
    params = list(
      spp = thisRow['spp'],
      sppCode = thisRow['sppCode'],
      cntry = thisRow['cntry'],
      exprt = thisRow['token'],
      date = thisDate
    )
  )
}


generateSpeciesReports <- function(thisRow,
                                   thisDate,
                                   fileType = "pdf_document",
                                   outDir = "C:/Users/mwhitby/Documents/SoB_SpeciesReports") {
  if (fileType == 'html_document') {
    ext = 'html'
  }
  if (fileType == 'pdf_document') {
    ext = 'pdf'
  }
  
  outDir <- paste0(outDir, '/', thisDate)
  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }
  
  out_fn = paste0(outDir, '/',
                  thisRow['cntry'], '_', thisRow['sppCode'], '.', ext)
  
  tryCatch(
    rmarkdown::render(
      paste0(here::here(), "/RCode/SoB_7a_SppReport.Rmd"),
      # output_file=paste0(here::here(), '/SpeciesReports/', DataDate, '/',
      #                    thisRow['cntry'], '_', thisRow['sppCode'], '.', ext),
      output_file = out_fn,
      # output_dir= paste0(here::here(), '/SpeciesReports/', DataDate),
      params = list(
        spp = thisRow['spp'],
        sppCode = thisRow['sppCode'],
        cntry = thisRow['cntry'],
        date = thisDate
      ),
      envir = new.env(),
      output_format = fileType
    ),
    error = function(e)
      e
  )
  
}
