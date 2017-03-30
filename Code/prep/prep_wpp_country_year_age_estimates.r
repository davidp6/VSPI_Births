# ------------------------------------------------
# David Phillips
#
# 3/29/2017
# Prep WPP births estimates by country-year-age
# The current working directory should be VSPI_B/
# ------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(stats)
# ------------------


# --------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Raw/WPP/WPP2015_FERT_F06_BIRTHS_BY_AGE_OF_MOTHER.XLS'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Data/Country_Data/WPP_Country_Year_Age_Estimates.csv'
# --------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load
data = data.table(read_excel(inFile))

# set names
setnames(data, names(data), c('Index', 'Variant', 'Country', 'Notes', 'iso_num', 
								'year', '15', '20', '25', '30', '35', '40', '45'))

# subset rows/columns
data = data[!is.na(Country) & iso_num!='Country code']
data$Index = NULL
data$Variant = NULL
data$Notes = NULL
data$Country = NULL

# variable classes
data[, year:=str_sub(year, 1, 4)]
data[, year:=as.numeric(year)+2]
for(var in names(data)) data[, (var):=as.numeric(get(var))]

# bring in iso codes
codes = fread(ccFile)
codes = codes[ihme_indic_country==1, c('countryname_ihme', 'iso3', 
				'iso_num', 'gbd_super_region_name'), with=FALSE]
setnames(codes, c('countryname_ihme', 'gbd_super_region_name'), 
				c('country', 'super_region'))
codes = unique(codes)
data = merge(data, codes, by='iso_num')
data$iso_num = NULL
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
# Interpolate years

# melt age long
idVars = c('iso3', 'country', 'super_region', 'year')
data = melt(data, id.vars=idVars, variable.name='age', value.name='births')
data[, age:=as.numeric(as.character(age))]

# convert to annual births
data[, births:=births/5]
data[, births:=births*1000]

# expand to all years
blankData = data[year==min(year)]
blankData[, births:=NA]
for(y in seq(min(data$year), 2020)) {
	if(y %in% unique(data$year)) next
	blankData[, year:=y]
	data = rbind(data, blankData)
}

# make sure there are no zero counts
data[births==0, births:=1]

# cubic spline interpolation
j=1
for(i in unique(data$iso3)) {
	for(a in unique(data$age)) {
		fitData = data[iso3==i & age==a]
		lFit = smooth.spline(y=log(fitData[!is.na(births)]$births), fitData[!is.na(births)]$year)
		pred = exp(predict(lFit, x=fitData$year)$y)
		pred = data.table(iso3=i, year=fitData$year, age=fitData$age, interpolation=pred)
		if (j==1) interpolations = pred
		if (j>1) interpolations = rbind(interpolations, pred)
		j=j+1
	}
}
data = merge(data, interpolations, by=c('iso3','year','age'))

# graph 4 random countries to confirm that it looks ok
ggplot(data[iso3 %in% sample(unique(data$iso3), 4)], aes(y=births, x=year, color=factor(age), group=factor(age))) + 
	geom_point() + 
	geom_line(aes(y=interpolation)) + 
	facet_wrap(~country, scales='free')
# -----------------------------------------------------------------------------------------------------


# ----------------------------------------
# Save

# finish up
data[, births:=interpolation]
data$interpolation = NULL

# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
