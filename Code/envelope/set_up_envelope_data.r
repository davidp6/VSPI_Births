# ------------------------------------------------------------
# David Phillips
#
# 4/13/2017
# Set up data to estimate envelope
# ------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(data.table)
# ------------------


# ---------------------------------------------------------------------------
# Files/directories/lists

# input file
inFilecya = './Data/Country_Data/WPP_Country_Year_Age_Estimates.csv'
inFilecys = './Data/Country_Data/WPP_Country_Year_Sex_Estimates.csv'
inFiletfr = './Data/Country_Data/WPP_Country_Year_TFR_Estimates.csv'
inFilecyap = './Data/Country_Data/HFD_Country_Year_Age_Parity_Estimates.csv'

# output file
outFile = './Data/Country_Data/Envelope_Input.csv'
# ---------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# Load/prep data

# load country-year-age births
cya = fread(inFilecya)

# load country-year-sex ratios
cys = fread(inFilecys)

# load country-year TFR
tfr = fread(inFiletfr)

# load country-year-age-parity births (HFD is high-income only)
cyap = fread(inFilecyap)

# compute proportion of births in each parity by cya
cyap[, parity_proportion:=births/sum(births), by=c('iso3','year','age')]
cyap$births = NULL
cyap = dcast.data.table(cyap, iso3+year+age+country+super_region~parity)
setnames(cyap, c('1','2','3','4'), paste0('parity_proportion', seq(4)))

# merge everything together
data = merge(cya, cys, by=c('iso3','year','country','super_region'))
data = merge(data, tfr, by=c('iso3','year','country','super_region'))
data = merge(data, cyap, by=c('iso3','year','age','country','super_region'), all.x=TRUE)
# --------------------------------------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
