calc_expert_impact <- function(dist_info_id, impact){
  thisNames <- impact %>% names()
  Impact <- vector(mode = 'list', length = 4)
  names(Impact) <-
    c('ImpactDist',
      'Quantiles',
      'Impact_randomDraw',
      'ImpactPlot')
  # calc beta dist parameters for threats
  if (dist_info_id != "pop_Size" & dist_info_id != "pop_Trend"){
    for (e in thisNames) {
      if (!is.null(impact[[e]])) {
        thisImpact <- impact[[e]]
        
        Impact[['ImpactDist']][[e]] <- EnvStats::ebeta(thisImpact, method = 'mle')
        
        Impact[['Quantiles']][[e]] <- qbeta(p = c(0.25, 0.5, 0.75),
          shape1 = Impact[['ImpactDist']][[e]]$parameters['shape1'],
          shape2 = Impact[['ImpactDist']][[e]]$parameters['shape2'])
        
        Impact[['Impact_randomDraw']][[e]] <- rbeta(n = 10000,
                                                    Impact[['ImpactDist']][[e]]$parameters['shape1'],
                                                    Impact[['ImpactDist']][[e]]$parameters['shape2'])
      }
    }
  }
    # calc gamma dist parameters for pop size
    if (dist_info_id == "pop_Size"){
      for (e in thisNames) {
        if (!is.null(impact[[e]])) {
          thisImpact <- impact[[e]]
          
          Impact[['ImpactDist']][[e]] <- EnvStats::egamma(thisImpact, method = 'mle')
          
          Impact[['Quantiles']][[e]] <- qgamma(p = c(0.25, 0.5, 0.75),
            shape = Impact[['ImpactDist']][[e]]$parameters['shape'],
            scale = Impact[['ImpactDist']][[e]]$parameters['scale'])
          
          Impact[['Impact_randomDraw']][[e]] <- rgamma(n = 10000,
                                                      shape = Impact[['ImpactDist']][[e]]$parameters['shape'],
                                                      scale = Impact[['ImpactDist']][[e]]$parameters['scale'])
        }
      }
    }
    # calc normal distribution parameters for pop trend
    if (dist_info_id == "pop_Trend"){
      for (e in thisNames) {
        if (!is.null(impact[[e]])) {
          thisImpact <- impact[[e]]
          
          Impact[['ImpactDist']][[e]] <- EnvStats::enorm(thisImpact, method = 'mle')
          
          Impact[['Quantiles']][[e]] <- qnorm(p = c(0.25, 0.5, 0.75),
            mean = Impact[['ImpactDist']][[e]]$parameters['mean'],
            sd = Impact[['ImpactDist']][[e]]$parameters['sd'])
          
          Impact[['Impact_randomDraw']][[e]] <- rnorm(n = 10000,
                                                      mean = Impact[['ImpactDist']][[e]]$parameters['mean'],
                                                      sd = Impact[['ImpactDist']][[e]]$parameters['sd'])
        }
      }
    }
    pb$tick()
    return(Impact)
}
  
  
  
  
  
  
  
  
calc_total_impact <- function(dist_info_id, expert_impact){
  allImpact_Dist <- NULL
  allImpact_Quantiles <- NULL
  allImpact_Draw <- NULL
  thisNames <- expert_impact$Impact_randomDraw %>% names()
  # Combine each expert's impact dist to one randomDraw for threats
  if (dist_info_id != "pop_Size" & dist_info_id != "pop_Trend"){
    if (!is.null(expert_impact$Impact_randomDraw)) {
      allImpact_Draw <- plyr::ldply(data.frame(expert_impact$Impact_randomDraw), .id = 'expert') %>%
        pivot_longer(cols = -1, names_to = 'sample', values_to = 'fx') %>% 
        #change expert name to 'linear pool' for graphing purposes downstream
        mutate(expert = 'linear pool')
      
      # Get combined dist 
      allImpact_Dist <- EnvStats::ebeta(allImpact_Draw$fx, method = 'mle')
      
      allImpact_Quantiles <- qbeta(p = c(0.25, 0.5, 0.75),
        shape1 = allImpact_Dist$parameters['shape1'],
        shape2 = allImpact_Dist$parameters['shape2'])
    }
  }
  # Combine each expert's impact dist to one randomDraw for pop size
    if (dist_info_id == "pop_Size"){
      if (!is.null(expert_impact$Impact_randomDraw)) {
        allImpact_Draw <- plyr::ldply(data.frame(expert_impact$Impact_randomDraw), .id = 'expert') %>%
          pivot_longer(cols = -1, names_to = 'sample', values_to = 'fx') %>% 
          mutate(expert = 'linear pool')
      
        allImpact_Dist <-EnvStats::egamma(allImpact_Draw$fx, method = 'mle')
        
        allImpact_Quantiles <- qgamma(p = c(0.25, 0.5, 0.75),
          shape = allImpact_Dist$parameters['shape'],
          scale = allImpact_Dist$parameters['scale'])
      }
    }
      
      # Combine each expert's impact dist to one randomDraw for pop trend
      if (dist_info_id == "pop_Trend"){
        if (!is.null(expert_impact$Impact_randomDraw)) {
          allImpact_Draw <- plyr::ldply(data.frame(expert_impact$Impact_randomDraw), .id = 'expert') %>%
            pivot_longer(cols = -1, names_to = 'sample', values_to = 'fx') %>% 
            mutate(expert = 'linear pool')
          
          allImpact_Dist <- EnvStats::enorm(allImpact_Draw$fx, method = 'mle')
          
          allImpact_Quantiles <- qnorm(p = c(0.25, 0.5, 0.75),
            mean = allImpact_Dist$parameters['mean'],
            sd = allImpact_Dist$parameters['sd'])
        }
      }
  pooled_impact <- vector(mode = 'list', length = 4)
  names(pooled_impact) <-
    c('pooled_Dist',
      'pooled_Quantiles',
      'pooled_randomDraw',
      'pooled_Plot')
  pooled_impact$pooled_Dist <- allImpact_Dist
  pooled_impact$pooled_Quantiles <- allImpact_Quantiles
  pooled_impact$pooled_randomDraw <- allImpact_Draw 
    pb$tick()
    return(pooled_impact)
  }
  
  
  
  
  
  
  
  
  
  