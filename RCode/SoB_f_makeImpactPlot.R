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