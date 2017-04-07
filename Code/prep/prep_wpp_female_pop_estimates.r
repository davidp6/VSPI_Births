# ----------------------------------------------
# David Phillips
#
# 3/29/2017
# Prep WPP births estimates by country-year-age
# ----------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(stats)
# ------------------


# ------------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Raw/WPP/WPP2015_POP_F07_3_POPULATION_BY_AGE_FEMALE.XLS'

# output file
outFile = './Data/Country_Data/WPP_Female_Pop_Estimates.csv'
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load
data = data.table(read_excel(inFile))

# set names
setnames(data, names(data), c('Index', 'Variant', 'Country', 'Notes', 'iso_num', 
			'year', '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', 
			'40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', 
			'80+', '80-84', '85-89', '90-94', '95-99', '100+'))

# subset rows/columns
data = data[Country=='WORLD']
data$Index = NULL
data$Variant = NULL
data$Notes = NULL
data$Country = NULL
data$iso_num = NULL

# melt long
data = melt(data, id.vars='year', variable.name='age', value.name='pop')

# keep reproductive ages only
data = data[age %in% c('15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54')]
data[, age:=str_sub(age,1,2)]
# ------------------------------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
