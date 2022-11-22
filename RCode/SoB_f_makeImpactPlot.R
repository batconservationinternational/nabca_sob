make_impact_plots <- function(values, expert_impact, pooled_dist){
  
  # Bind linear pool and expert random draw values for plotting
  expert_rd <- expert_impact$Impact_randomDraw %>% as_tibble() %>% pivot_longer(1:length(.))
  names(expert_rd) <- c('expert','fx')
  graphing_data <- pooled_dist$pooled_randomDraw %>% 
    bind_rows(expert_rd) 
  
  # Make expert list into factor
  graphing_data$expert <- factor(graphing_data$expert)
  
  pooled_plot <- ggplot(graphing_data) +
    geom_density(aes(x = fx, 
                     group = expert,
                     color = expert,
                     size = expert)) +
    scale_size_manual("expert", values = values$expertSZ, guide = 'none') +
    scale_x_continuous(limits=c(0,1))+
    xlab('Total Population Impact (%)') +
    ylab(expression(f[X](x))) +
    theme(axis.text.y = element_blank())
  
  maxY <- max(ggplot_build(pooled_plot)$layout$panel_scales_y[[1]]$range$range)
  
  pooled_quantiles_df <- pooled_dist$pooled_Quantiles %>% as.matrix() %>% t() %>%  as_tibble() %>% 
    rename(Q1 = V1, Median = V2, Q3 = V3) %>% 
    mutate(expert = "linear pool", .before=1)
  Quantiles_df <- plyr::ldply(expert_impact$Quantiles, .id = 'expert')
  colnames(Quantiles_df) <- c('expert', 'Q1', 'Median', 'Q3')
  Quantiles_df <- pooled_quantiles_df %>% bind_rows(Quantiles_df)
  
  Quantiles_pos = data.frame(
    y = seq(
      from = -maxY * (1 / 1.75),
      to = -maxY * (1 / 10),
      length = nrow(Quantiles_df)
    ),
    size=0.5,
    expert = Quantiles_df$expert
  )
  
  Quantiles_df <- left_join(Quantiles_df, Quantiles_pos, by = c("expert"="expert"))
  Quantiles_df <- left_join(Quantiles_df, as_tibble(c(values$labels), rownames = 'expert'), 
                            by = c("expert"="expert")) %>% 
    rename(label=value)
  Quantiles_df[Quantiles_df$expert == 'linear pool', 'size'] = 1
  
  pooled_plot <- pooled_plot +
    # scale_size_manual(values = values$expertSZ, guide = 'none') +
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
      values = c(values$colors),
      name = "Expert",
      breaks = Quantiles_df$expert,
      labels = Quantiles_df$label
    ) +
    scale_linetype_manual(values = values$expertLT, guide = 'none') +
    scale_y_continuous(breaks = NULL) +
    theme(legend.position = 'bottom')
  
  return(pooled_plot)
}
  
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
  
# }

# SS_impact[[i]] <-
#   Impact #Store Impact for row (all experts) in SS_impact

# }##END CALC IMPACT


# 
# threatData$Impact <- SS_impact

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
#end group and print T1 individual graphs