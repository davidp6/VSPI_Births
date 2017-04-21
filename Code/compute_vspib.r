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


# ---------------------------------------------------------------
# Start function
computeVSPIB = function(inFile='Data 200417.csv', outFile=NULL) { 
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
	oldNames = c('age','sex','parity','completeness')
	setnames(simData, oldNames, paste0(oldNames, '_accuracy'))
	
	# collapse birth estimates to country-year level
	byVars = c('iso3', 'year')
	birthData = birthData[, list(envelope=sum(births)), by=byVars]
	
	# drop blank columns/rows
	keepVars = c('Country', 'Year', 'Age', 'Sex', 'Birth order', 'Number births')
	data = data[!is.na(Year), keepVars, with=FALSE]
	setnames(data, keepVars, c('country', 'year', 'age', 'sex', 'parity', 'births'))
	
	# format variables
	data[parity=='4+', parity:='4']
	data[sex==1, sex_str:='m']
	data[sex==2, sex_str:='f']
	data[sex==3, sex_str:='both']
	data[sex==99, sex_str:='99']
	data$sex = NULL
	setnames(data, 'sex_str', 'sex')
	simData[, level:=as.numeric(level)]

	# add perfect to simData
	perf = simData[1]
	perf[, level:=0]
	perf[, age_accuracy:=1]
	perf[, sex_accuracy:=1]
	perf[, parity_accuracy:=1]
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
	idVars = c('iso3','year','age','sex','parity')
	n1 = nrow(data)
	n2 = nrow(unique(data[,idVars,with=F])) # check for duplicates again
	if (n1!=n2) { # attempt to collapse out any duplicate rows (some are true duplicates)
		attemptCollapse = data[,list(births=mean(births), sdtest=sd(births)), by=idVars]
		if (!any(attemptCollapse$sdtest>1, na.rm=TRUE)) {
			data = attemptCollapse
			data$sdtest = NULL
		}
	}
	n1 = nrow(data)
	n2 = nrow(unique(data[,idVars,with=F])) # check for duplicates again
	if (n1!=n2) stop('Duplicate country-year-age-sex-parities in input data!')
	# -------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------
	# Compute proportions
	
	# denominator
	data[, total:=sum(births, na.rm=TRUE), by=byVars]
	
	# age
	data[age==99, unspecified_age:=sum(births, na.rm=TRUE), by=byVars]
	data[, unspecified_age:=as.numeric(unspecified_age)]
	data[, unspecified_age:=mean(unspecified_age, na.rm=TRUE), by=byVars]
	data[is.na(unspecified_age), unspecified_age:=0]
	data[, unspecified_age:=unspecified_age/total]
	
	# sex
	data[sex %in% c('both','99'), unspecified_sex:=sum(births, na.rm=TRUE), by=byVars]
	data[, unspecified_sex:=as.numeric(unspecified_sex)]
	data[, unspecified_sex:=mean(unspecified_sex, na.rm=TRUE), by=byVars]
	data[is.na(unspecified_sex), unspecified_sex:=0]
	data[, unspecified_sex:=unspecified_sex/total]
	
	# parity
	data[parity %in% c('All','99'), unspecified_parity:=sum(births, na.rm=TRUE), by=byVars]
	data[, unspecified_parity:=as.numeric(unspecified_parity)]
	data[, unspecified_parity:=mean(unspecified_parity, na.rm=TRUE), by=byVars]
	data[is.na(unspecified_parity), unspecified_parity:=0]
	data[, unspecified_parity:=unspecified_parity/total]
	
	# completeness
	data = merge(data, birthData, by=c('iso3','year'), all.x=TRUE)
	data[, completeness:=total/envelope]
	data[completeness>1, completeness:=1]
	# --------------------------------------------------------------------------------------
	
	
	# -------------------------------------------------------------------------------------------
	# Transform proportions to accuracy
	
	# reduce to only proportions
	indicators = c('unspecified_age', 'unspecified_parity', 'unspecified_sex', 'completeness')
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
	
	# merge completeness
	completenessSim = simData[, c('level','completeness_accuracy'), with=FALSE]
	completenessSim[, level:=rev(completenessSim$level)]
	data = merge(data, completenessSim, by.x='completeness', by.y='level')
	# -------------------------------------------------------------------------------------------
	
	
	# -----------------------------------------------------------------------------
	# Compute VSPI-B
	
	# product
	data[, vspi_b:=age_accuracy*sex_accuracy*parity_accuracy*completeness_accuracy]
	
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
	# -----------------------------------------------------------------------------
	
	
	# -------------------------------------------
	# Save output
	write.csv(data, outFileLoc, row.names=FALSE)
	# -------------------------------------------
	
# -------------
# End function
}
# -------------
