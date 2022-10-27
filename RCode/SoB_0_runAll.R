thisDataDate='20220914'
thisCountry = 'MX'
thisSpp  = c("ANTROZOUS_PALLIDUS") #NULL for all species

#export data with completed answers only, answer and Q codes
#Save as MX/US_results_YYYYMMDD.csv


# Load necessary Components ----------------------------------------------

library(dplyr)
library(purrr)
require(SHELF)
library(tidyr)
library(ggpubr)

source(paste0(here::here(), '/RCode/SoB_f_general.R'))

options(scipen = 999)

# Format Data Properly for Analysis ---------------------------------------
source(paste0(here::here(), '/RCode/SoB_1_FormatData.R'))

formattedData <- formatData(thisDataDate,
                         thisCountry)
nestedData <- formattedData$data_l
d <- formattedData$data

# Make Range Graphs ---------------------------------------------------------
source(paste0(here::here(), '/RCode/SoB_2_rangeGraphs.R'))

rangeGraphs <- graphRangeQ(d)

# Analyze Stuff -----------------------------------------------------------
source(paste0(here::here(), '/RCode/SoB_3_analyzeQ.R'))
source(paste0(here::here(), '/RCode/SoB_4_GroupANDcalcTotalImpactANDpersonalPlots.R'))

OutputFolder = paste0(here::here(), '/Data/derived/AnalysisExport_', thisDataDate)

analyze_SoB(nestedData,
            OutputFolder = OutputFolder,
            cntrytoAnalyze = thisCountry,
            SpptoAnalyze = thisSpp,
            PersonalPlots=FALSE
            )

calc_Impact(thisDataDate, 
            speciestoAnalyze = thisSpp,
            DataFolder = OutputFolder,
            PersonalPlots=FALSE)


# Make Population Graphs --------------------------------------------------

source(paste0(here::here(), '/RCode/SoB_5_popGraphs.R'))
make_PopGraphs(thisDataDate,
               Dir = paste0(here::here()))

dataCols <- read.csv(paste0(here::here(), '/Data/dataColumns.csv'), stringsAsFactors = F)[,1]

data <- read.csv(paste0(here::here(), '/Data/', thisCountry, '_results_', thisDataDate, '.csv'), stringsAsFactors = F) %>% 
  select(sppCode, spp, cntry) %>% 
  distinct()

pbapply::pbapply(data[,], 1, generateSpeciesReports, thisDate=thisDataDate, fileType="pdf_document")
