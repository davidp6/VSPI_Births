# ------------------------------------------------------------
# David Phillips
#
# 4/13/2017
# Set up data to estimate envelope
# ------------------------------------------------------------


# ------------------
# Set up R
rm(list=ls())
library(readxl)
library(data.table)
# ------------------


# ---------------------------------------------------------------------------
# Files/directories/lists

# input file
inFileWPP = './Data/Country_Data/WPP_Country_Year_Age_Estimates.csv'
inFileDHS = './Data/Country_Data/Birth Data_final.xlsx'
inFileUSS = './Data/Country_Data/Final UK survey data.csv'

# country codes
ccFile = './Data/Country_Data/countrycodes.csv'

# output file
outFile = './Data/Country_Data/Envelope_Input.csv'
# ---------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------
# Load/prep data

# load country-year-age births
WPP = fread(inFileWPP)

# load country-year-age-sex-parity-birthweight numbers from DHS
DHS = data.table(read_excel(inFileDHS))

# load UK Understanding Society Survey to get more high-income data to go with DHS
USS = fread(inFileUSS)

# fix data prep error in birthyears
DHS[b2<100 & b2>80, b2:=b2+1900]

# combine 10-14 and 15-19 age groups to match WPP
DHS[mothage==10, mothage:=15]

# collapse DHS to the cyaspb level (some surveys overlap in time)
byVars = c('country','b2','mothage','sex','bord1','birthweight')
DHS = DHS[, list(births=sum(sample_weight)), by=byVars]
setnames(DHS, c('b2','mothage','bord1'), c('year','age','parity'))

# compute proportion of births in each sex-parity-birthweight by cya
DHS[, prop:=births/sum(births), by=c('country','year','age')]
setnames(DHS, 'births', 'births_obs')

# bring in iso codes
cc = fread(ccFile)
cc = unique(cc[!is.na(iso3),c('countryname', 'iso3'),with=FALSE])
DHS = merge(DHS, cc, by.x='country', by.y='countryname', all.x=TRUE)
DHS$country = NULL

# format USS to match DHS
USS[, sex:=ifelse(sex=='male', 1, 2)]
USS[, birthweight:=as.character(birthweight)]
USS[birthweight=='1', birthweight:='<2500']
USS[birthweight=='2', birthweight:='2500-3499']
USS[birthweight=='3', birthweight:='3500+']
USS[, iso3:='GBR']
setnames(USS, c('yob', 'mother_age', 'birthorder', 'births'), c('year', 'age', 'parity', 'births_obs'))
idVars = c('iso3','year','age','sex','parity','birthweight')
USS = USS[, list('births_obs'=sum(births_obs, na.rm=TRUE)), by=idVars]
USS[, prop:=births_obs/sum(births_obs), by=c('iso3','year','age')]

# drop unknown birthweight and assume MCAR
DHS = DHS[!birthweight %in% c('Don\'t know', 'Not weighed at birth')]
USS = USS[!is.na(birthweight)]

# add USS to DHS
DHS = rbind(DHS, USS)

# expand WPP data to "square"
isos = unique(c(WPP$iso3, DHS$iso3))
years = seq(1980, 2016)
ages = unique(c(WPP$age, DHS$age))
sexes = unique(DHS$sex)
parities = unique(DHS$parity)
birthweights = unique(DHS$birthweight)
ids = expand.grid(isos, years, ages, sexes, parities, birthweights)
setnames(ids, c('Var4','Var5','Var6'), c('sex','parity','birthweight'))
data = merge(WPP, ids, by.x=c('iso3','year','age'), by.y=c('Var1','Var2','Var3'), all.y=TRUE)

# expand DHS to "square" and properly distinguish between 0 and NA
isoyears = unique(DHS[,c('iso3','year'),with=FALSE])
isoyears = paste0(isoyears$iso3, '_', isoyears$year)
ids = data.table(expand.grid(isoyears, ages, sexes, parities, birthweights))
ids[, c('iso3', 'year') := tstrsplit(Var1, '_', fixed=TRUE)]
ids[, year:=as.numeric(year)]
ids$Var1 = NULL
DHS = merge(DHS, ids, by.x=c('iso3','year','age','sex','parity','birthweight'), 
	by.y=c('iso3','year','Var2','Var3','Var4','Var5'), all.y=TRUE)
DHS[is.na(births_obs), births_obs:=0]
DHS[is.na(prop), prop:=0]

# merge proportions to birth numbers
data = merge(data, DHS, by=c('iso3','year','age','sex','parity','birthweight'), all.x=TRUE)
# --------------------------------------------------------------------------------------------


# ----------------------------------------
# Save
write.csv(data, outFile, row.names=FALSE)
# ----------------------------------------
