
calc_Impact <- function(DataDate, 
                        DataFolder,
                        speciestoAnalyze=NULL,
                        countrytoAnalyze=NULL,
                        doPar=T,
                        PersonalPlots=FALSE) {
  
  if(PersonalPlots){
   saveDir = paste0(here::here(), '/PersonalGraphs/', DataDate, '/')
   if (!dir.exists(saveDir)) {
      dir.create(saveDir)
   }
  }
   # DataFolder = paste0(here::here(), '/Data/derived/AnalysisExport', DataDate)
   
   DataFiles <- list.files(DataFolder, full.names = T)
   DataFiles <- DataFiles[!grepl("_T2.RDS", DataFiles)]
   
   
   # DataFiles <- DataFiles[grepl("Canada_", DataFiles)]
   
   #Table of threat lables
   Threats <- read.csv(paste0(here::here(), '/Data/ThreatNum.csv'))
   
   if(!is.null(speciestoAnalyze)) {
     fileSPP <- stringr::str_sub(DataFiles, start=-26, end=-22)
     DataFiles <- DataFiles[fileSPP %in% speciestoAnalyze]
   }
   
   if(!is.null(countrytoAnalyze)) {
     fileCNTRY <- stringr::str_sub(DataFiles, start=-29, end=-28)
     DataFiles <- DataFiles[fileCNTRY %in% countrytoAnalyze]
   }
   
   #ForEachDataFile
   
   require(doParallel)
   require(foreach)
   require(parallel)

   
   cl <- makeCluster(detectCores()-1, outfile=paste0(here::here(), "/outlog_", lubridate::today() ,".txt"))
   # cl <- makeCluster(2, outfile=paste0(here::here(), "/outlog_", lubridate::today() ,".txt"))
   registerDoParallel(cl)
   
   # for (f in DataFiles) {
   
   foreach(f =DataFiles) %dopar% {
  # foreach(f =DataFiles) %do% {
      
      require(dplyr)
      require(tidyr)
      require(ggpubr)
      
      
      # f = DataFiles[1] #For Debugging
      print(paste0('reading', f))
      
      if(PersonalPlots){
      sppData <- readRDS(f) %>%
         select(
            cntry,
            spp,
            sppCode,
            LimeGroup,
            subQ,
            experts,
            Q_group,
            Q_ss,
            Q_sub,
            randomDraw,
            perPlots,
            D_plot
         ) %>%
         mutate(plots = case_when(perPlots == 'single elicitation' ~ D_plot,
                                  T ~ perPlots)) %>%
         select(-perPlots,-D_plot)
      } else {
        sppData <- readRDS(f) %>%
          select(
            cntry,
            spp,
            sppCode,
            LimeGroup,
            subQ,
            experts,
            Q_group,
            Q_ss,
            Q_sub,
            randomDraw,
            # perPlots,
            D_plot
          ) %>%
          mutate(plots=D_plot) %>%
          select(-D_plot)
      }
      
      
      
      thisSpecies = unique(sppData$sppCode)
      thisCountry = unique(sppData$cntry)
      
      #### Combine Threat Graphs ####
      threatData <- sppData %>%
         filter(Q_group != 'pop')
      
      #IF NO THREAT DATA GO TO NEXT FILE
      if (nrow(threatData) < 1) {
         rm(sppData)
         rm(threatData)
         next
      }
      
      
      
      threatData <- threatData %>%
         pivot_wider(names_from = Q_ss,
                     values_from = c('randomDraw', 'plots')) %>%
         left_join(Threats, by = c('LimeGroup', 'subQ'))
      
      SS_impact <- vector(mode = 'list', length = nrow(threatData))
      
      
      #For each row, and expert, calculate distribution and quantiles, then grapgh
      for (i in 1:nrow(threatData)) {
         # i = 1   #for debugging
         print(paste0('i=', i, ' in ', f))
         
         # thisNames <- names(threatData$experts[[i]]$names)
         thisNames <- threatData$experts[[i]]$experts
         
         # if (length(thisNames) == 1) {
         #    thisNames <- 1
         # }
         
         Impact <- vector(mode = 'list', length = 4)
         
         names(Impact) <-
            c('ImpactBeta',
              'Quantiles',
              'Impact_randomDraw',
              'ImpactPlot')
         
         # ImpactPlot <- ggplot()
         
         for (e in thisNames) {
            #Impact for each expert
            # e = thisNames[1] ##For Debugging
            
            
            print(paste('expert', e))
            if (!is.null(threatData$randomDraw_Scope[[i]][[e]]) &
                !is.null(threatData$randomDraw_Severity[[i]][[e]])) {
               thisImpact <-
                  threatData$randomDraw_Scope[[i]][[e]][, 'beta'] * threatData$randomDraw_Severity[[i]][[e]][, 'beta']
               
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
               
               # Impact[[e]]$Impact_plot <-
               #    ggplot()+
               #    geom_density(aes(x=Impact[[e]]$Impact_randomDraw), color='red', size=5)+
               #    theme_classic()+
               #    coord_cartesian(xlim=c(0,1), expand=F)
               
            } else {
               Impact[[e]]$ImpactBeta <- NULL
               Impact[[e]]$Quantiles <- NULL
            }
            
         }# End impact for each expert
         
         allImpact_Beta <- NULL
         allImpact_Quantiles <- NULL
         allImpact_Draw <- NULL
         Impact$ImpactPlot <- NULL
         
         #combine each expert's impact dist to one randomDraw
         if (!is.null(Impact$Impact_randomDraw)) {
            allImpact_Draw <-
               plyr::ldply(Impact$Impact_randomDraw, .id = 'expert') %>%
               pivot_longer(
                  cols = -1,
                  names_to = 'sample',
                  values_to = 'fx'
               )
            
            #get combined dist
            allImpact_Beta <-
               EnvStats::ebeta(allImpact_Draw$fx, method = 'mle')
            
            allImpact_Quantiles <- qbeta(
               p = c(0.25, 0.5, 0.75),
               shape1 = allImpact_Beta$parameters['shape1'],
               shape2 = allImpact_Beta$parameters['shape2']
            )
            
            
            
            Impact[['ImpactBeta']][['linear pool']] <- allImpact_Beta
            Impact[['Quantiles']][['linear pool']] <- allImpact_Quantiles
            Impact[['Impact_randomDraw']][['linear pool']] <-
               allImpact_Draw
            
            
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
            
            ##CODE FROM f_general for plots
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