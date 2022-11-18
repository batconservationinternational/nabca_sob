thisDataDate='20221116'
# United States, Canada, or Mexico
thisCountry = 'Mexico'
# US_CAN or MX
countryAbbr = 'MX'

OutputFolder = paste0(here::here(), '/Data/derived/AnalysisExport_', thisDataDate)

#export data with completed answers only, answer and Q codes
#Save as MX/US_results_YYYYMMDD.csv

# Load necessary Components ----------------------------------------------
library(tidyverse)
library(pbapply)
library(parallel)
library(ggpubr)
library(rlist)
library(SHELF)
library(progress)
library(overlapping)
library(doParallel)
library(foreach)
library(EnvStats)
library(readxl)
library(openxlsx)
options(scipen = 999)

source(paste0(here::here(), '/RCode/SoB_1_FormatData.R'))
source(paste0(here::here(), '/RCode/SoB_2_analyzeQ.R'))
source(paste0(here::here(), '/RCode/SoB_3_calcImpact.R'))
source(paste0(here::here(), '/RCode/SoB_4_generateImpactPlots.R'))
source(paste0(here::here(), '/RCode/SoB_f_general.R'))
source(paste0(here::here(), '/RCode/SoB_f_calcImpact.R'))
source(paste0(here::here(), '/RCode/SoB_f_makeImpactPlot.R'))

# Format Data Properly for Analysis ---------------------------------------
formattedData <- formatData(thisDataDate, thisCountry, countryAbbr)
nestedData <- formattedData$data_l
d <- formattedData$data
weird_pop_size <- formattedData$weird_pop_size
weird_percentage <- formattedData$weird_percentage
missing_answers <- formattedData$missing_answers

# Make Range Graphs ---------------------------------------------------------
rangeGraphs <- graphRangeQ(d)

# Analyze Stuff -----------------------------------------------------------
all_species <- unique(d$sppcode)
print(all_species)

# Loop through species and analyze data for each expert
count = 1
failed_analyze = list()
for (spp in all_species){
  print(paste0("Analyzing expert data for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    analyze_SoB(nestedData[spp],
                OutputFolder = OutputFolder,
                cntrytoAnalyze = countryAbbr,
                SpptoAnalyze = spp)
    message("Success.")
  },
  error = function(e){
    message(paste("Error for", spp, ":"))
    failed_analyze <- append(failed_analyze, spp)
    print(e)
  }
  )
}

# Loop through species and calculate impact and pool data for all experts
count=1
failed_calc_impact = list()
pop_df = data.frame()
threat_df = data.frame()
for (spp in all_species){
  print(paste0("Calculating Impact for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    df <- calc_Impact(dataDate = thisDataDate, 
                dataFolder = OutputFolder,
                speciestoAnalyze = spp,
                countrytoAnalyze = countryAbbr)
    pop_df <- pop_df %>% bind_rows(df$pop_info)
    threat_df <- threat_df %>% bind_rows(df$threat_info)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
      failed_calc_impact <- append(failed_calc_impact, spp)
    }
  )
}

# Write pop and threat aggregations
pop_df_path <- paste0('Data/derived/AnalysisExport_', thisDataDate, '/', 
                      countryAbbr, "_pop_quantiles_agg.csv")
write_csv(pop_df, here::here(pop_df_path))
threat_df_path <- paste0('Data/derived/AnalysisExport_', thisDataDate, '/', 
                         countryAbbr, "_threat_median_agg.csv")
write_csv(threat_df, here::here(threat_df_path))


# Calc total impact percentage for each threat for each species and expert-----
count=1
failed_total_impact = list()
for (spp in all_species){
  print(paste0("Calculating Total Impact Percent for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    calc_total_impact(dataDate = thisDataDate, 
                dataFolder = OutputFolder,
                speciestoAnalyze = spp,
                countrytoAnalyze = countryAbbr)
    message("Success.")
  },
  error = function(e){
    message(paste("Error for", spp, ":"))
    print(e)
    failed_total_impact <- append(failed_total_impact, spp)
  }
  )
}

# Make threat impact plots --------------------------------------------------
count=1
failed_impact_plots = list()
for (spp in all_species){
  print(paste0("Creating impact plots for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    generate_impact_plots(dataDate = thisDataDate,
                          speciestoAnalyze = spp,
                          dataFolder = OutputFolder,
                          cntrytoAnalyze = countryAbbr)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
      failed_impact_plots = append(failed_impact_plots, spp)
    }
  )
}

# Make Species Reports ------------------------------------------------------
# dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]
# 
# data <- read.csv(paste0(here::here(), '/Data/results-survey718871_', DataDate, '.csv'), stringsAsFactors = F) %>% 
#   select(sppCode, spp, cntry) %>% 
#   filter(cntry=='Canada') %>%
#   distinct()
generateSpeciesReports <- function(thisRow,
                                   thisDate,
                                   fileType = "pdf_document") {
  if (fileType == 'html_document') {ext = 'html'}
  if (fileType == 'pdf_document') {ext = 'pdf'}
  
  outDir <- paste0(here::here(), '/species_reports/', thisDate)
  if (!dir.exists(outDir)) {dir.create(outDir)}
  
  out_fn = paste0(outDir, '/', thisRow['cntry'], '_', thisRow['name'], '.', ext)
  
  tryCatch(
    rmarkdown::render(
      paste0(here::here(), "/RCode/SoB_7a_SppReport.Rmd"),
      output_file = out_fn,
      params = list(
        spp = thisRow['name'],
        sppCode = thisRow['sppCode'],
        cntry = thisRow['cntry'],
        date = thisDate
      ),
      envir = new.env(),
      output_format = fileType
    ),
    error = function(e){e}
  )
  
}

pbapply::pbapply(data[,], 1, generateSpeciesReports, thisDate=thisDataDate, fileType="pdf_document")
