
calc_Impact <- function(DataDate, 
                        DataFolder,
                        speciestoAnalyze=NULL,
                        countrytoAnalyze=NULL,
                        doPar=T) {
  
  # DataFolder=OutputFolder
  # speciestoAnalyze=thisSpp
  # countrytoAnalyze=thisCountry
  
  files <- list.files(DataFolder, full.names = T)
   
   #Table of threat labels
   Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv')) %>% 
     unite("threat_abbr", Q_group, Q_sub, sep = "_", remove = F)
   
   if (!is.null(speciestoAnalyze) & !is.null(countrytoAnalyze)) {
     f <- files[str_detect(files, speciestoAnalyze) & 
                              str_detect(files, countrytoAnalyze)]
   } else {return(message("Country or species not provided to calc_Impact function"))}
   
   print(paste0('reading: ', f))
   #For each data file
   
   # cl <- makeCluster(detectCores()-1, outfile=paste0(here::here(), "/outlog_", lubridate::today() ,".txt"))
   
   # registerDoParallel(cl)

     sppData <- readRDS(f)
     
     thisSpecies = unique(sppData$name)
     thisCountry = unique(sppData$cntry)
     
     #make sure there is only one species and one country in the dataset
     if(length(thisSpecies) > 1 | length(thisCountry) > 1) {
        stop( cat('More than 1 species or country in data file', 
                  '\nSpecies: ', thisSpecies, 
                  '\nCountry: ', thisCountry, 
                  '\ndatafile: ', f)
              )}
      
      #If not threat data, skip 
     threat_data <- sppData %>% filter(q_type=="Scope" | q_type == "Severity")
     if (nrow(threat_data)<1) {
         return(message("No threat data present in dataset."))
      }
     
     threat_data <- threat_data %>% select(name, cntry, q_type, dist_info_id, randomDraw) %>% 
       pivot_wider(names_from = "q_type", values_from = "randomDraw") 
     
     # Multiply across the random draws of scope and severity to get impact
     threat_data$impact <- map2(threat_data$Scope, threat_data$Severity, ~ .x * .y)
      
      # For each row, and expert, calculate distribution and quantiles, then grapgh
      for (i in 1:nrow(threat_data)) {
         print(paste0('i=', i, ' in ', f))
         
         thisNames <- threat_data[i,]$impact[[1]] %>% names()
         
         Impact <- vector(mode = 'list', length = 4)
         
         names(Impact) <-
            c('ImpactBeta',
              'Quantiles',
              'Impact_randomDraw',
              'ImpactPlot')
         
         for (e in thisNames) {
           
            print(paste('expert', e))
           
            if (!is.null(threat_data$impact[[i]][[e]])) {
              
               thisImpact <- threat_data[i,]$impact[[1]][[e]]
               
               Impact[['ImpactBeta']][[e]] <-
                  EnvStats::ebeta(thisImpact, method = 'mle')
               
               Impact[['Quantiles']][[e]] <- qbeta(
                  p = c(0.25, 0.5, 0.75),
                  shape1 = Impact[['ImpactBeta']][[e]]$parameters['shape1'],
                  shape2 = Impact[['ImpactBeta']][[e]]$parameters['shape2']
               )
               
               Impact[['Impact_randomDraw']][[e]] <- rbeta(n = 10000,
                                                           Impact[['ImpactBeta']][[e]]$parameters['shape1'],
                                                           Impact[['ImpactBeta']][[e]]$parameters['shape2'])
               
            }
            
         } # End impact for each expert
         
         allImpact_Beta <- NULL
         allImpact_Quantiles <- NULL
         allImpact_Draw <- NULL
         
         # Combine each expert's impact dist to one randomDraw
         if (!is.null(Impact$Impact_randomDraw)) {
            allImpact_Draw <- plyr::ldply(data.frame(Impact$Impact_randomDraw), .id = 'expert') %>%
               pivot_longer(cols = -1,
                  names_to = 'sample',
                  values_to = 'fx') %>% 
              mutate(fx = as.numeric(fx)) %>% 
              dplyr::filter(expert %in% thisNames)
            
            # Get combined dist
            allImpact_Beta <-
               EnvStats::ebeta(allImpact_Draw$fx, method = 'mle')
            
            allImpact_Quantiles <- qbeta(
               p = c(0.25, 0.5, 0.75),
               shape1 = allImpact_Beta$parameters['shape1'],
               shape2 = allImpact_Beta$parameters['shape2']
            )
            
            
            
            Impact[['ImpactBeta']][['linear pool']] <- allImpact_Beta
            Impact[['Quantiles']][['linear pool']] <- allImpact_Quantiles
            Impact[['Impact_randomDraw']][['linear pool']] <- allImpact_Draw
            
            
            combined <- allImpact_Draw
            combined$expert <- 'linear pool'
            allImpact_all <- rbind(allImpact_Draw, combined)
            
            Impact$ImpactPlot = ggplot(allImpact_all) +
               geom_density(aes(x = fx, color = expert), size = 1) +
               xlab('Total Population Impact (%)') +
               ylab(expression(f[X](x))) +
               # coord_cartesian(xlim=c(0,0.0025), expand=F)+
               theme_classic() +
               theme(axis.text.y = element_blank())
            
            # CODE FROM f_general for plots
            maxY <-
               max(ggplot_build(Impact$ImpactPlot)$layout$panel_scales_y[[1]]$range$range)
            
            Quantiles_pos = data.frame(
               y = seq(
                  from = -maxY * (1 / 1.75),
                  to = -maxY * (1 / 10),
                  length = length(Impact$Quantiles)
               ),
               size = 0.5,
               expert = names(Impact$Quantiles)
            )
            
            Quantiles_df <- plyr::ldply(Impact$Quantiles, .id = 'expert')
            colnames(Quantiles_df) <- c('expert', 'Q1', 'Median', 'Q3')
            
            Quantiles_df <- left_join(Quantiles_df, Quantiles_pos) %>%
               left_join(as_tibble(c(threatData[[i, 'experts']][[1]]$labels), rownames =
                                      'expert')) %>%
               rename(label = value)
            
            
            Quantiles_df[Quantiles_df$expert == 'Combined', 'size'] = 1
            
            Impact$ImpactPlot <-   Impact$ImpactPlot +
               scale_size_manual(values = threatData[[i, 'experts']][[1]]$SZ, guide =
                                    'none') +
               # scale_linetype_discrete(guide='none')+
               geom_pointrange(
                  data = Quantiles_df,
                  aes(
                     x = Median,
                     xmin = Q1,
                     xmax = Q3,
                     y = y,
                     color = expert
                  ),
                  size = Quantiles_df$size
               ) +
               theme_classic() +
               coord_cartesian(
                  expand = F,
                  ylim = c(min(Quantiles_df$y) * 1.1, maxY * 1.1),
                  xlim = c(0, 1)
               ) +
               scale_color_manual(
                  values = c(threatData[[i, 'experts']][[1]]$colors),
                  name = "Expert",
                  breaks = Quantiles_df$expert,
                  labels = Quantiles_df$label
               ) +
               scale_linetype_manual(values = threatData[[i, 'experts']][[1]]$LT, guide =
                                        'none') +
               scale_y_continuous(breaks = NULL) +
               theme(legend.position = 'bottom')
            
            #
            # if(nrow(thisRow$distFit$ssq)==1){
            #    colorNOcombine <-thisRow$experts$colors
            #    colorNOcombine['Combined'] <- NA
            #    myplotLP <- myplotLP+
            #       scale_color_manual(values=colorNOcombine,
            #                          name="Expert",
            #                          breaks=thisRow$Quantiles$expert)
            # }
            ##END CODE FROM f_general for plots
            
         }
         
         SS_impact[[i]] <-
            Impact #Store Impact for row (all experts) in SS_impact
         
      }##END CALC IMPACT
      
      threatData$Impact <- SS_impact
      
      # SS_graphs <- vector(mode = 'list', length = nrow(threatData))
      
      # for (i in 1:nrow(threatData)) {
         # i = 1#for debugging
         #
         # # thisNames <- names(threatData$experts[[i]]$names)
         # thisNames <- threatData$experts[[i]]$experts
         # 
         # # if (length(thisNames) == 1) {
         # #    thisNames <- 1
         # # }
         # 
         # Qgraphs <- vector(mode = 'list', length = length(thisNames))
         # names(Qgraphs) <- thisNames
         # # if (is.null(thisNames)) {
         # # if (threatData$Scope[[i]]=="no data to graph") {
         # 
         # #Create blank graph in all
         # for (e in thisNames) {
         #    thisQgraph =  ggplot() + annotate(
         #       geom = 'text',
         #       label = 'BLANK',
         #       x = 1,
         #       y = 1
         #    ) +
         #       theme_classic() +
         #       theme(
         #          axis.text = element_blank(),
         #          axis.line = element_blank(),
         #          axis.ticks = element_blank(),
         #          # plot.margin = unit(c(10,1,1,1), 'cm')
         #       ) +
         #       labs(
         #          x = element_blank(),
         #          y = element_blank(),
         #          title = element_text(
         #             threatData$subT[[i]],
         #             face = 'bold',
         #             size = 12
         #          )
         #       )
         #    
         #    Qgraphs[[e]] <- thisQgraph
         # }
         # Qgraphs = 'none'
         # } else {
         #Single elicitations just have a ggplot object with data as first name
         
         
         
      #    
      #    #Create graph for each expert
      #    for (e in thisNames) {
      #       # e = thisNames[2] ##For Debugging
      #       thisQgraph <- ggarrange(
      #          threatData$plots_Scope[[i]][[e]],
      #          threatData$plots_Severity[[i]][[e]],
      #          ncol = 2,
      #          nrow = 1,
      #          labels = list(c('Scope'), c('Severity')),
      #          label.x = 0.5,
      #          hjust = 0.5,
      #          vjust = 5,
      #          font.label = list(face = 'bold.italic',
      #                            size = 10),
      #          common.legend = T,
      #          legend = "bottom"
      #       )
      #       
      #       thisQgraph <- annotate_figure(thisQgraph,
      #                                     top = text_grob(
      #                                        threatData$subT[[i]],
      #                                        face = 'bold',
      #                                        size = 12,
      #                                        # just=c('center', 'top')
      #                                        vjust = 2.5
      #                                     ))
      #       
      #       thisQgraph <- ggarrange(
      #          thisQgraph,
      #          threatData$Impact[[i]]$ImpactPlot + theme(legend.position =
      #                                                       'none'),
      #          ncol = 1,
      #          nrow = 2
      #       )
      #       
      #       
      #       Qgraphs[[e]] <- thisQgraph
      #       rm(thisQgraph)
      #    }#END Create graph for each expert
      #    
      #    # }
      #    
      #    SS_graphs[[i]] = Qgraphs
      #    rm(Qgraphs)
      # }
      # 
      # threatData$SS_graphs <- SS_graphs
      # rm(SS_graphs)
      # 

      # 
      # #Group and print Graph for each individual and T1 threat
      # for (T1 in unique(threatData$LimeGroup)) {
      #    # T1 = 23 ##For debugging
      #    
      #    print(paste0("T1=", T1))
      #    
      #    thisThreat <- filter(threatData, LimeGroup == T1)
      #    experts <- threatData$experts[[1]]$experts
      #    
      #    for (e in experts) {
      #       print(paste0("expert=", e))
      #       
      #       e_Graphs <-
      #          vector(mode = 'list', length = length(unique(thisThreat$subQ)))
      #       exLab <- threatData$experts[[1]]$labels
      #       
      #       for (T2 in unique(thisThreat$subQ)) {
      #          # T2=3 # For debugging only
      #          print(T2)
      #          # e_Graphs[[T2]] <- thisThreat[thisThreat$subQ==T2,]$SS_graphs[[1]][[exLab]]
      #          e_Graphs[[T2]] <-
      #             tryCatch(
      #                thisThreat[thisThreat$subQ == T2, ]$SS_graphs[[1]][[e]],
      #                error = function(e) {
      #                   NULL
      #                }
      #             )
      #       }
      #       
      #       T1_graph <- ggarrange(plotlist = e_Graphs,
      #                             ncol = 1)
      #       # common.legend = T)
      #       # labels=thisThreat$subT)
      #       
      #       T1_graph <- annotate_figure(T1_graph,
      #                                   top = text_grob(
      #                                      unique(thisThreat$Threat),
      #                                      face = 'bold.italic',
      #                                      size = 18,
      #                                      just = c('left', 'center'),
      #                                      x = unit(0.025, 'npc')
      #                                      # fig.lab=unique(thisThreat$Threat),
      #                                      # fig.lab.size=14,
      #                                      # fig.lab.face='bold.italic',
      #                                      # fig.lab.pos = 'top'
      #                                   ))
      #       
      #       print(paste(T1, thisThreat$Threat, e))
      #       
      #       ggexport(
      #          T1_graph,
      #          filename = paste0(
      #             saveDir,
      #             e,
      #             '_',
      #             thisCountry,
      #             '_',
      #             thisSpecies,
      #             '_',
      #             T1,
      #             '.png'
      #          ),
      #          width = 1080,
      #          height = 1080 * length(unique(thisThreat$subQ))
      #       )
      #       
      #       rm(T1_graph)
      # 
      #    }
      #    rm(thisThreat)
      }#end group and print T1 individual graphs
      
      saveRDS(threatData,
           file = paste0(tools:::file_path_sans_ext(f), '_T2.RDS'))
      rm(threatData)
      # stopCluster(cl)
   } #End File
   
}
