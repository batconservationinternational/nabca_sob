# Export data with completed answers only, answer and Q codes
# Save as MX_results_YYYYMMDD.csv or US_CAN_results_YYYYMMDD.csv

# Date of data export (YYYYMMDD)
thisDataDate='20221116'
# "United States", "Canada", or "Mexico"
thisCountry = 'Mexico'
# "US_CAN" or "MX"
countryAbbr = 'MX'

OutputFolder = paste0(here::here(), '/Data/derived/AnalysisExport_', thisDataDate,
                      "_", thisCountry)
if (!dir.exists(OutputFolder)) {dir.create(OutputFolder)}

# Load necessary Components -------------------------------------------------
library(tidyverse)
library(pbapply)
library(ggpubr)
library(rlist)
library(SHELF)
library(progress)
library(overlapping)
library(EnvStats)
library(readxl)
library(openxlsx)
library(scales)

source(paste0(here::here(), '/RCode/SoB_1_formatData.R'))
source(paste0(here::here(), '/RCode/SoB_2_analyzeQ.R'))
source(paste0(here::here(), '/RCode/SoB_3_calcImpact.R'))
source(paste0(here::here(), '/RCode/SoB_4_generateImpactPlots.R'))
source(paste0(here::here(), '/RCode/SoB_f_general.R'))
source(paste0(here::here(), '/RCode/SoB_f_calcImpact.R'))
source(paste0(here::here(), '/RCode/SoB_f_makeImpactPlot.R'))

options(scipen = 999)

# Format Data Properly for Analysis ---------------------------------------
formattedData <- formatData(thisDataDate, thisCountry, countryAbbr)
nestedData <- formattedData$data_nested
d <- formattedData$data_l
data <- formattedData$data
# spit out info on rows that were dropped for either weird numbers or missing values
weird_pop_size <- formattedData$weird_pop_size
weird_percentage <- formattedData$weird_percentage
na_answers <- formattedData$na_answers

all_species <- unique(d$sppcode)
print(all_species)

write_csv(d, paste0(OutputFolder, '/cleaned_responses_', thisCountry,
                    '_', thisDataDate, '.csv'))

# Make Range Graphs -----------------------------------------------------------
rangeGraphs <- graphRangeQ(data)

# Loop through species and analyze data for each expert------------------------
count = 1
failed_analyze = list()
for (spp in all_species){
  print(paste0("Analyzing expert data for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    analyze_SoB(nestedData[spp],
                OutputFolder = OutputFolder,
                cntrytoAnalyze = thisCountry,
                SpptoAnalyze = spp,
                dataDate = thisDataDate)
      message("Success.")
  },
  error = function(e){
    message(paste("Error for", spp, ":"))
    failed_analyze <<- append(failed_analyze, list(spp, e))
    print(e)
  }
  )
}

# Loop through species and calculate impact and pool data for all experts------
count=1
failed_calc_impact = list()
pop_df = data.frame()
threat_df = data.frame()
ss_summary = data.frame()
for (spp in all_species){
  print(paste0("Calculating Impact for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    df <- calc_Impact(dataDate = thisDataDate, 
                dataFolder = OutputFolder,
                speciestoAnalyze = spp,
                countrytoAnalyze = thisCountry)
    pop_df <- pop_df %>% bind_rows(df$pop_info)
    threat_df <- threat_df %>% bind_rows(df$threat_info)
    ss_summary <- ss_summary %>% bind_rows(df$ss_summary)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
      failed_calc_impact <<- append(failed_calc_impact, list(spp, e))
    }
  )
}

# Write pop and threat aggregations-------------------------------------------
pop_df_path <- paste0(OutputFolder, "/pop_quantiles_agg_", thisCountry, "_", 
                      thisDataDate, ".csv")
write_csv(pop_df, here::here(pop_df_path))
threat_df_path <- paste0(OutputFolder, "/threat_quantiles_agg_", thisCountry, "_", 
                         thisDataDate, ".csv")
write_csv(threat_df, here::here(threat_df_path))
ss_df_path <- paste0(OutputFolder, "/scop_sev_quantiles_agg_", thisCountry, "_", 
                         thisDataDate, ".csv")
write_csv(ss_summary, here::here(ss_df_path))

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
                          cntrytoAnalyze = thisCountry)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
      failed_impact_plots <<- append(failed_impact_plots, spp)
    }
  )
}

# Make species reports ------------------------------------------------------
species = unique(d$spp)
species_code = unique(d$sppcode)

fileType = 'pdf_document'
outDir <- paste0(here::here(), '/species_reports/', 
                 thisDataDate, "_", thisCountry)
if (!dir.exists(outDir)) {dir.create(outDir, recursive=T)}
count=1
failed_markdown = list()
for (i in seq(1:length(species))){
  spp <- species[i]
  sppCode <- species_code[i]
  
  print(paste0("Building report for ", 
               spp, " (species ", count, "/", length(species), ")"))
  
  if (fileType == 'html_document') {ext = 'html'}
  if (fileType == 'pdf_document') {ext = 'pdf'}
  
  out_fn = paste0(outDir, '/', thisCountry, '_', sppCode, '.', ext)
  
  tryCatch({
    rmarkdown::render(
      paste0(here::here(), "/RCode/SoB_5_sppReport.Rmd"),
      output_file = out_fn,
      params = list(
        spp = spp,
        sppCode = sppCode,
        cntry = thisCountry,
        date = thisDataDate,
        rootDir = here::here()
      ),
      envir = new.env(),
      output_format = fileType
    )},
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
      failed_markdown <<- append(failed_markdown, spp)
    }
  )
  count <- count+1
}
