thisDataDate='20220914'
thisCountry = 'MX'

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
source(paste0(here::here(), '/RCode/SoB_f_general.R'))
options(scipen = 999)

# Format Data Properly for Analysis ---------------------------------------
source(paste0(here::here(), '/RCode/SoB_1_FormatData.R'))

formattedData <- formatData(thisDataDate,
                         thisCountry)
nestedData <- formattedData$data_l
d <- formattedData$data
weird_pop_size <- formattedData$weird_pop_size
weird_percentage <- formattedData$weird_percentage

# Make Range Graphs ---------------------------------------------------------
source(paste0(here::here(), '/RCode/SoB_2_rangeGraphs.R'))

graphRangeQ <- function(data) {
  source(paste0(here::here(), '/RCode/SoB_f_general.R'))
  range <- data %>%
    dplyr::select('cntry', 'spp', 'token',
                  matches('range_[A-Z]')) %>%
    group_by(cntry, spp) %>%
    tidyr::nest() %>%
    mutate(longD=map(data, make_rangeGraphs, spp, cntry))
  return(range)
}

rangeGraphs <- graphRangeQ(d)

# Analyze Stuff -----------------------------------------------------------
source(paste0(here::here(), '/RCode/SoB_3_analyzeQ.R'))
source(paste0(here::here(), '/RCode/SoB_4_GroupANDcalcTotalImpact.R'))
source(paste0(here::here(), '/RCode/SoB_f_calcImpact.R'))

OutputFolder = paste0(here::here(), '/Data/derived/AnalysisExport_', thisDataDate)
all_species <- unique(d$sppcode)

# Loop through species and analyze data for each expert
for (spp in all_species){
  print(paste("Analyzing expert data for", spp))
  analyze_SoB(nestedData[spp],
              OutputFolder = OutputFolder,
              cntrytoAnalyze = thisCountry,
              SpptoAnalyze = spp)
}

# Loop through species and calculate impact and pool data for all experts
for (spp in all_species){
  print(paste("Calculating impact for", spp))
  calc_Impact(dataDate = thisDataDate, 
              speciestoAnalyze = spp,
              dataFolder = OutputFolder,
              countrytoAnalyze = thisCountry)
}

# Make threat impact plots --------------------------------------------------
source(paste0(here::here(), '/RCode/SoB_generateImpactPlots.R'))
source(paste0(here::here(), '/RCode/SoB_f_makeImpactPlot.R'))
for (spp in all_species){
  print(paste("Creating impact plots for", spp))
  generate_impact_plots(dataDate = thisDataDate,
                        speciestoAnalyze = spp,
                        dataFolder = OutputFolder,
                        cntrytoAnalyze = thisCountry)
}

# Make Species Reports ------------------------------------------------------

generateSpeciesReports <- function(thisRow,
                                   thisDate,
                                   fileType = "pdf_document") {
  if (fileType == 'html_document') {
    ext = 'html'
  }
  if (fileType == 'pdf_document') {
    ext = 'pdf'
  }
  
  outDir <- paste0(here::here(), '/species_reports/', thisDate)
  if (!dir.exists(outDir)) {
    dir.create(outDir)
  }
  
  out_fn = paste0(outDir, '/',
                  thisRow['cntry'], '_', thisRow['name'], '.', ext)
  
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
    error = function(e)
      e
  )
  
}



pbapply::pbapply(data[,], 1, generateSpeciesReports, thisDate=thisDataDate, fileType="pdf_document")
