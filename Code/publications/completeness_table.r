# ---------------------------------------------------------------------------------------------
# David Phillips
#
# 2/3/2018
# Produce a table of completeness in most recent year
# ---------------------------------------------------------------------------------------------


# --------------------
# Set up R
rm(list=ls())
library(data.table)
# --------------------



# ----------------------------------------------
# Files and directories

# input file
vspiFile = './Data/VSPI_Estimates/VSPI_B_Data 041217.csv'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# country codes
outFile = './Documents/Manuscript/Completeness Table.csv'
# ----------------------------------------------


# ----------------------------------------------
# Load/prep data

# load VSPI estimates
data = fread(vspiFile)

# subset to most recent year by country
data[!is.na(completeness), maxyear:=max(year), by='iso3']
data = data[year==maxyear]

# bring in country names
cc = fread(ccFile)
cc = cc[!is.na(iso3) & iso3!='']
cc = unique(cc[,c('iso3','countryname_ihme')])
data = merge(data, cc, by='iso3')
setnames(data, 'countryname_ihme', 'Country')

# subset variables
data = data[, c('Country','year', 'completeness')]

# format
data[, completeness:=completeness*100]
data = data[order(Country)]
setnames(data, c('Country','Year','Completeness (%)'))
# ----------------------------------------------


# ---------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ---------------------------------------
