library(tidyverse) 
library(cluster)
library(fpc)
library(dbscan)
library(factoextra)
library(readxl)

cntry <- "Mexico"
data_date <- "20221116"

scope_sev_path <- sprintf("%s/Data/derived/AnalysisExport_%s_%s/scope_sev_quantiles_agg_%s_%s.csv", 
                               here::here(), data_date, cntry, cntry, data_date)
scope_sev_df <- read_csv(scope_sev_path) %>% select(-Q1, -Q3, -P99) %>% 
  mutate(scope_sev = str_to_lower(scope_sev)) %>% 
  unite(col="question", c('threat','scope_sev'), remove=T, sep="_")

bins_path <- sprintf("%s/Data/derived/AnalysisExport_%s_%s/impact_bins_%s_%s.xlsx", 
                here::here(), data_date, cntry, cntry, data_date)

bins_df <- read_excel(bins_path, sheet = 2) %>% select(-Scope, -Severity) %>% 
  unite(col="question", c('level_1','level_2'), remove=T, sep="_")
bins_df$impact_bin <- as.numeric(factor(bins_df$impact_bin, 
                             levels = c("Negligible","Low","Medium","High","Very High")))
bins_df <- bins_df %>% pivot_wider(names_from = question, values_from = impact_bin)

pop_path <- sprintf("%s/Data/derived/AnalysisExport_%s_%s/pop_quantiles_agg_%s_%s.csv", 
                    here::here(), data_date, cntry, cntry, data_date)
pop_df <- read_csv(pop_path) %>% select(-Q1, -Q3) %>% 
  pivot_wider(names_from = question, values_from = Median)

df <- bins_df %>% left_join(pop_df) %>% 
  select(where(~n_distinct(.) > 1))

# Standardize data to mean zero and standard deviation one
values <- scale(df[2:length(df)]) 
values[is.na(values)] = 0
species <- df[1]

# plot the eps values
eps_plot = kNNdistplot(values, k=3)

# to draw an optimum line
eps_plot %>% abline(h = 10, lty = 2)


set.seed(50)
# creation of an object km which store the output of the function kmeans
d <- dbscan::dbscan(values, eps = 10, minPts =  2)
d

# cluster visualisation
fviz_cluster(d, values, geom = "point")

