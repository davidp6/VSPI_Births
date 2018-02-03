# -------------------------------------------------------------------------------------------
# David Phillips
#
# 3/20/2017
# Function that computes VSPI-B for given data
# Inputs
#    - inFile (character): name of input file to evaluate including file extension 
#		must be in standard input format
#		inFile should be stored in VSPI_Births/Data/Country_Data
#    - outFile (character): name of output file to save including file extension 
#4		if NULL, defaults to inFile
# Outputs
#    - nothing. saves file to outFile in VSPI_Births/Data/VSPI_Estimates
# The current working directory should be VSPI_Births/
# -------------------------------------------------------------------------------------------


# to do: look into cases where the total in the bw=999 group is different than bw!=999

# ---------------------------------------------------------------
# Start function
computeVSPIB = function(inFile='Data 061117.csv', outFile=NULL) { 
# ---------------------------------------------------------------
	
	# ----------------------------------------
	# Set up R
	args = c('inFile', 'outFile')
	rm(list=ls()[!ls() %in% c('args', args)])
	library(data.table)
	library(TTR) # for smoother
	# ----------------------------------------
	
	
	# ---------------------------------------------------------------------
	# Test inputs
	if (is.null(outFile)) outFile = paste0('VSPI_B_', inFile)
	for(a in args) {
		if (!a %in% ls()) stop(paste('Must provide', a))
		if (class(get(a))!='character') stop(paste(a, 'must be character'))
	}
	# ---------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------
	# Files/directories/lists
	
	# input files
	inFileLoc = paste0('./Data/Country_Data/', inFile)
	
	# birth estimates to compute completeness
	birthFile = './Data/Envelopes/Envelope.csv'
	
	# simulation estimates
	simFile = './Data/Simulation_Outputs/accuracy_estimates.csv'

	# country codes
	ccFile = './Data/Country_Data/countrycodes.csv'
	
	# output file
	outFileLoc = paste0('./Data/VSPI_Estimates/', outFile)
	# -----------------------------------------------------------------------
	
	
	# ---------------------------------------------------------------------------------
	# Load/prep data
	
	# load data to evaluate
	data = fread(inFileLoc)
	
	# load simulation estimates
	simData = fread(simFile)
	
	# load birth estimates
	birthData = fread(birthFile)
	
	# rename simData
	oldNames = c('age','sex','parity','completeness','bw')
	setnames(simData, oldNames, paste0(oldNames, '_accuracy'))
	
	# collapse birth estimates to country-year level
	byVars = c('iso3', 'year')
	birthData = birthData[, list(envelope=sum(births_est)), by=byVars]
	
	# drop blank columns/rows
	keepVars = c('Country', 'Year', 'Age', 'Sex', 'Birth order', 'Birthweight', 'Number births')
	data = data[!is.na(Year), keepVars, with=FALSE]
	setnames(data, keepVars, c('country', 'year', 'age', 'sex', 'parity', 'bw', 'births'))
	
	# format variables
	data[parity=='4+', parity:='4']
	data[sex==1, sex_str:='m']
	data[sex==2, sex_str:='f']
	data[sex==3, sex_str:='both']
	data[sex==99, sex_str:='99']
	data$sex = NULL
	setnames(data, 'sex_str', 'sex')
	simData[, level:=as.numeric(level)]
	if (class(data$births)=='character') data[, births:=as.numeric(gsub(',','',births))]
	data[bw %in% c('1', '<2500'), bw:='2500']
	data[bw %in% c('2', '2500-3499'), bw:='3000']
	data[bw %in% c('3', '3500+'), bw:='3500']
	data[bw=='Unknown', bw:='99']
	data[bw=='N/A', bw:='All']
	
	# add perfect to simData
	perf = simData[1]
	perf[, level:=0]
	perf[, age_accuracy:=1]
	perf[, sex_accuracy:=1]
	perf[, parity_accuracy:=1]
	perf[, bw_accuracy:=1]
	perf[, completeness_accuracy:=1]
	simData = rbind(perf, simData)
	
	# bring in iso codes/drop non-GBD countries
	codes = fread(ccFile)
	codes = codes[ihme_indic_country==1, c('iso3', 'countryname'), with=FALSE]
	codes = unique(codes)
	data = merge(data, codes, by.x='country', by.y='countryname')
	data$country = NULL
	
	# test if it's possible to estimate completeness
	mc = data$iso3[!data$iso3 %in% birthData$iso3]
	warning = paste('Warning!', paste(unique(mc), collapse=' '), 'not in births estimates!')
	if (length(mc)>0) print(warning)
	# ---------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------
	# Test unique identifiers in input data
	# Duplicates are allowed because sometimes the birthweight numbers are listed separately from the rest
	idVars = c('iso3','year','age','sex','parity','bw')
	n1 = nrow(data)
	n2 = nrow(unique(data[,idVars,with=F])) # check for duplicates again
	if (n1!=n2) { # attempt to collapse out any duplicate rows (some are true duplicates)
		attemptCollapse = data[,list(births=mean(births), sdtest=sd(births)), by=idVars]
		if (!any(attemptCollapse$sdtest>1, na.rm=TRUE)) {
			data = attemptCollapse
			data$sdtest = NULL
		}
	}
	# -------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------
	# Compute proportions
	
	# split out the data that does and doesn't have bw listed separately
	# IS THERE A SMARTER WAY TO DO THIS?
	data_notsep = data[bw!=999]
	data_sep = data[bw==999]
	
	# denominator
	data_notsep[, total:=sum(births, na.rm=TRUE), by=byVars]
	data_sep[, total:=sum(births, na.rm=TRUE), by=byVars]
	
	# age
	data_notsep[age %in% c('All','99'), unspecified_age:=sum(births, na.rm=TRUE), by=byVars]
	data_notsep[, unspecified_age:=as.numeric(unspecified_age)]
	data_notsep[, unspecified_age:=mean(unspecified_age, na.rm=TRUE), by=byVars]
	data_notsep[is.na(unspecified_age), unspecified_age:=0]
	data_notsep[, unspecified_age:=unspecified_age/total]
	data_sep[age %in% c('All','99'), unspecified_age:=sum(births, na.rm=TRUE), by=byVars]
	data_sep[, unspecified_age:=as.numeric(unspecified_age)]
	data_sep[, unspecified_age:=mean(unspecified_age, na.rm=TRUE), by=byVars]
	data_sep[is.na(unspecified_age), unspecified_age:=0]
	data_sep[, unspecified_age:=unspecified_age/total]
	
	# sex
	data_notsep[sex %in% c('both','99'), unspecified_sex:=sum(births, na.rm=TRUE), by=byVars]
	data_notsep[, unspecified_sex:=as.numeric(unspecified_sex)]
	data_notsep[, unspecified_sex:=mean(unspecified_sex, na.rm=TRUE), by=byVars]
	data_notsep[is.na(unspecified_sex), unspecified_sex:=0]
	data_notsep[, unspecified_sex:=unspecified_sex/total]
	data_sep[sex %in% c('both','99'), unspecified_sex:=sum(births, na.rm=TRUE), by=byVars]
	data_sep[, unspecified_sex:=as.numeric(unspecified_sex)]
	data_sep[, unspecified_sex:=mean(unspecified_sex, na.rm=TRUE), by=byVars]
	data_sep[is.na(unspecified_sex), unspecified_sex:=0]
	data_sep[, unspecified_sex:=unspecified_sex/total]
	
	# parity
	data_notsep[parity %in% c('All','99'), unspecified_parity:=sum(births, na.rm=TRUE), by=byVars]
	data_notsep[, unspecified_parity:=as.numeric(unspecified_parity)]
	data_notsep[, unspecified_parity:=mean(unspecified_parity, na.rm=TRUE), by=byVars]
	data_notsep[is.na(unspecified_parity), unspecified_parity:=0]
	data_notsep[, unspecified_parity:=unspecified_parity/total]
	data_sep[parity %in% c('All','99'), unspecified_parity:=sum(births, na.rm=TRUE), by=byVars]
	data_sep[, unspecified_parity:=as.numeric(unspecified_parity)]
	data_sep[, unspecified_parity:=mean(unspecified_parity, na.rm=TRUE), by=byVars]
	data_sep[is.na(unspecified_parity), unspecified_parity:=0]
	data_sep[, unspecified_parity:=unspecified_parity/total]
	
	# bw - note: 999 means there's a duplicate cy for birthweight (listed separately)
	data_notsep[bw %in% c('All','99'), unspecified_bw:=sum(births, na.rm=TRUE), by=byVars]
	data_notsep[, unspecified_bw:=as.numeric(unspecified_bw)]
	data_notsep[, unspecified_bw:=mean(unspecified_bw, na.rm=TRUE), by=byVars]
	data_notsep[is.na(unspecified_bw), unspecified_bw:=0]
	data_notsep[, unspecified_bw:=unspecified_bw/total]
	data_sep[bw %in% c('All','99'), unspecified_bw:=sum(births, na.rm=TRUE), by=byVars]
	data_sep[, unspecified_bw:=as.numeric(unspecified_bw)]
	data_sep[, unspecified_bw:=mean(unspecified_bw, na.rm=TRUE), by=byVars]
	data_sep[is.na(unspecified_bw), unspecified_bw:=0]
	data_sep[, unspecified_bw:=unspecified_bw/total]
	
	# completeness
	data_notsep = merge(data_notsep, birthData, by=c('iso3','year'), all.x=TRUE)
	data_notsep[, completeness:=total/envelope]
	data_notsep[completeness>1, completeness:=1]
	data_sep = merge(data_sep, birthData, by=c('iso3','year'), all.x=TRUE)
	data_sep[, completeness:=total/envelope]
	data_sep[completeness>1, completeness:=1]
	
	# append together the sep and notsep datasets and give each country the best of the two
	data = rbind(data_notsep, data_sep)
	data[, unspecified_age:=min(unspecified_age), by=byVars]
	data[, unspecified_sex:=min(unspecified_sex), by=byVars]
	data[, unspecified_parity:=min(unspecified_parity), by=byVars]
	data[, unspecified_bw:=max(unspecified_bw), by=byVars] # bw is the only one to take the worst of since it's garaunteed to be 0 in 'sep'
	data[, completeness:=max(completeness), by=byVars]
	data[, total:=mean(total), by=byVars] # sometimes the totals are different! shouldn't happen
	
	# store totals
	totals = unique(data[,c('iso3','year','total'),with=FALSE])
	# --------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------------------
	# Transform proportions to accuracy
	
	# reduce to only proportions
	indicators = c('unspecified_age', 'unspecified_parity', 'unspecified_sex', 'unspecified_bw', 'completeness')
	data = unique(data[, c(byVars, indicators, 'total'), with=FALSE])
	
	# test dimensions
	nrowCorrect = nrow(unique(data[,byVars,with=FALSE]))
	if (nrowCorrect!=nrow(data)) stop('Something went wrong. iso3/year don\'t uniquely ID rows.')
	
	# round indicators
	fl = function(x) .01*(ceiling(x/.01))
	data = data[, lapply(.SD, fl), .SDcols=indicators, by=byVars]
	
	# merge age
	ageSim = simData[, c('level','age_accuracy'), with=FALSE]
	data = merge(data, ageSim, by.x='unspecified_age', by.y='level')
	
	# merge sex
	sexSim = simData[, c('level','sex_accuracy'), with=FALSE]
	data = merge(data, sexSim, by.x='unspecified_sex', by.y='level')
	
	# merge parity
	paritySim = simData[, c('level','parity_accuracy'), with=FALSE]
	data = merge(data, paritySim, by.x='unspecified_parity', by.y='level')
	
	# merge bw
	bwSim = simData[, c('level','bw_accuracy'), with=FALSE]
	data = merge(data, bwSim, by.x='unspecified_bw', by.y='level')
	
	# merge completeness
	completenessSim = simData[, c('level','completeness_accuracy'), with=FALSE]
	completenessSim[, level:=rev(completenessSim$level)]
	completenessSim[, level:=as.character(level)]
	data[, completeness:=as.character(completeness)]
	data = merge(data, completenessSim, by.x='completeness', by.y='level')
	data[, completeness:=as.numeric(completeness)]
	# -------------------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------
	# Compute VSPI-B
	
	# product
	data[, vspi_b:=age_accuracy*sex_accuracy*parity_accuracy*bw_accuracy*completeness_accuracy]
	
	# square up country years
	sq = data.table(expand.grid(unique(data$iso3), seq(1970, 2017)))
	setnames(sq, names(sq), c('iso3', 'year'))
	data = merge(sq, data, by=byVars, all.x=TRUE)
	
	# apply moving average for timeliness
	k=5
	for(i in unique(data$iso3)) {
		# isolate time series
		ts = data[iso3==i]$vspi_b
		ts[is.na(ts)] = 0
		
		# extend 1980 backward for MA stability
		idx = which(unique(data$year)==1980)
		start = ts[idx]
		ts[1:idx] = start
		
		# compute
		ma = EMA(ts,5)
		data[iso3==i, vspi_b_ma:=ma]
	}
	
	# drop pre-1980
	data = data[year>=1980]
	
	# bring back total births
	data = merge(data, totals, by=c('iso3','year'), all.x=TRUE)
	# -----------------------------------------------------------------------------
	
	
	# -------------------------------------------
	# Save output
	write.csv(data, outFileLoc, row.names=FALSE)
	# -------------------------------------------
	
# -------------
# End function
}
# -------------
