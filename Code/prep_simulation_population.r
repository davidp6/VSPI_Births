# ---------------------------------------------------------
# David Phillips
#
# 3/22/2017
# Generate simulation population from WPP and HFD combined
# ---------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
# ------------------


# ----------------------------------------------------------------------------------
# Files/directories/lists

# root directory
root = 'C:/Users/davidp6/Google Drive/Work/Data for Health Initiative/VSPI_Births/'

# input file
hfdFile = paste0(root, 'Data/Raw/HFD/USAbirthsRRbo.txt')
wppFile = paste0(root, 'Data/Raw/WPP/SexRatioTotPop-20170322063247.xls')

# output file
outFile = paste0(root, 'Data/Simulation_Inputs/simulation_population.csv')
# ----------------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Load/prep data

# load HFD data
hfdData = fread(hfdFile)

# load WPP data
wppData = data.table(read_excel(wppFileSex, sheet='Data', col_names=FALSE))

# prep HFD
hfdData = hfdData[Year>2000]
hfdData$Total=NULL
hfdData = melt(hfdData, id.vars=c('Year','Age'), variable.name='parity', value.name='births')
hfdData[Age=='12-', Age:='12']
hfdData[Age=='55+', Age:='55']
hfdData[, Age:=as.numeric(Age)]
hfdData[, Age:=5*floor(Age/5)]
hfdData[, parity:=gsub('B', '', parity)]
hfdData[, parity:=gsub('p', '', parity)]
hfdData[, parity:=as.numeric(parity)]

# prep WPP sex ratios
wppData = melt(wppData[3])
sex_ratio = mean(wppData[11:14]$value)/100

# collapse HFD to all years by age/parity
hfdData = hfdData[, list(births=sum(births)), by=c('Age','parity')]

# multiple by global sex ratio to split
# ratio is m/f, so m/f=ratio and m+f=T, so (1+ratio)*f=T or f=T/(1+ratio)
hfdData[, f:=births/(sex_ratio+1)]
hfdData[, m:=births-f]
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Do the thing

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Set up to save

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Save

# -------------------------------------------------------------------------
