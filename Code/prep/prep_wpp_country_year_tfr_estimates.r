# ---------------------------------------
# David Phillips
#
# 3/29/2017
# Prep WPP TFR estimates by country-year
# ---------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
library(stringr)
library(stats)
# ------------------


# ------------------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Raw/WPP/WPP2015_FERT_F04_TOTAL_FERTILITY.XLS'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Data/Country_Data/WPP_Country_Year_TFR_Estimates.csv'
# ------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Load/prep data

# load
data = data.table(read_excel(inFile))

# set names
setnames(data, names(data), c('Index', 'Variant', 'Country', 'Notes', 'iso_num', 
							'1950-1955', '1955-1960', '1960-1965', '1965-1970', 
							'1970-1975', '1975-1980', '1980-1985', '1985-1990', 
							'1990-1995', '1995-2000', '2000-2005', '2005-2010', '2010-2015'))

# subset rows/columns
data = data[!is.na(Country) & iso_num!='Country code']
data$Index = NULL
data$Variant = NULL
data$Notes = NULL
data$Country = NULL

# variable classes
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

# melt year long
idVars = c('iso3', 'country', 'super_region')
data = melt(data, id.vars=idVars, variable.name='year', value.name='tfr')
data[, year:=str_sub(year, 1, 4)]
data[, year:=as.numeric(year)+2]

# expand to all years
blankData = data[year==min(year)]
blankData[, tfr:=NA]
for(y in seq(min(data$year), 2020)) {
	if(y %in% unique(data$year)) next
	blankData[, year:=y]
	data = rbind(data, blankData)
}

# cubic spline interpolation
j=1
for(i in unique(data$iso3)) {
	fitData = data[iso3==i]
	lFit = smooth.spline(y=fitData[!is.na(tfr)]$tfr, fitData[!is.na(tfr)]$year)
	pred = predict(lFit, x=fitData$year)$y
	pred = data.table(iso3=i, year=fitData$year, interpolation=pred)
	if (j==1) interpolations = pred
	if (j>1) interpolations = rbind(interpolations, pred)
	j=j+1
}
data = merge(data, interpolations, by=c('iso3','year'))

# graph 6 random countries to confirm that it looks ok
ggplot(data[iso3 %in% sample(unique(data$iso3), 6)], aes(y=tfr, x=year)) + 
	geom_point() + 
	geom_line(aes(y=interpolation)) + 
	facet_wrap(~country, scales='free')
# -----------------------------------------------------------------------------------------------------


# ----------------------------------------
# Save

# finish up
data[, tfr:=interpolation]
data$interpolation = NULL

# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
