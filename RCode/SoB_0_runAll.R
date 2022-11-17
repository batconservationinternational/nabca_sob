thisDataDate='20221116'
# United States, Canada, or Mexico
thisCountry = 'United States'
# US_CAN or MX
countryAbbr = 'US_CAN'

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
OutputFolder = paste0(here::here(), '/Data/derived/AnalysisExport_', thisDataDate)
all_species <- unique(d$sppcode)

# Loop through species and analyze data for each expert
count = 1
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
    print(e)
  }
  )
}

# Loop through species and calculate impact and pool data for all experts
count=1
for (spp in all_species){
  print(paste0("Calculating Impact for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch({
    calc_Impact(dataDate = thisDataDate, 
                dataFolder = OutputFolder,
                speciestoAnalyze = spp,
                countrytoAnalyze = countryAbbr)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
    }
  )
}

# Make threat impact plots --------------------------------------------------
count=1
for (spp in all_species){
  print(paste0("Creating impact plots for ", 
               spp, " (species ", count, "/", length(all_species), ")"))
  count = count+1
  tryCatch(
    {generate_impact_plots(dataDate = thisDataDate,
                          speciestoAnalyze = spp,
                          dataFolder = OutputFolder,
                          cntrytoAnalyze = thisCountry)
    message("Success.")
    },
    error = function(e){
      message(paste("Error for", spp, ":"))
      print(e)
    }
  )
}

threat_plots <- readRDS('/Users/ngoodby/Desktop/nabca_sob/Data/derived/AnalysisExport_20220914/MX_ANTROZOUS_PALLIDUS_20220914_pooled_threat_plots.RDS')

# Make Species Reports ------------------------------------------------------
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
