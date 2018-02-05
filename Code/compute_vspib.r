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
computeVSPIB = function(inFile='Data 041217.csv', outFile=NULL) { 
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
	data = data[!is.na(births) & births!='']
	
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
	data[bw %in% c('Unknown','N/A'), bw:='99'] 
	
	# clarify cases of "aggregate in this row, but specified separately"
	# 99 always means "missing"
	# "specified_separately" means there are duplicate rows for this country-year that 
	# 		have categories of this variable but not necessarily categories of another variable
	# 		we give every country the best score possible when details aren't cross-tabulated
	data[parity=='All', parity:='specified_separately']
	data[age=='All', age:='specified_separately']
	data[sex=='both', sex:='specified_separately']
	data[bw=='999', bw:='specified_separately']
	
	# ensure only accepted values are present
	data[!parity %in% c('1','2','3','4','99','specified_separately'), parity:='99']
	data[!age %in% c('10','15','20','25','30','35','40','45','50','99','specified_separately'), age:='99']
	data[!sex %in% c('m','f','99','specified_separately'), sex:='99']
	data[!bw %in% c('2500','3000','3500','99','specified_separately'), age:='99']
	
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
	
	
	# -------------------------------------------------------------------------
	# Handle edge cases
	
	# if a country-year-indicator is 100% "specified_separately", 
	# then it clearly isn't actually specified separately. recode these cases to missing
	data[, total:=sum(births, na.rm=TRUE), by=c('iso3','year')]
	vars = c('age','sex','parity','bw')
	for(var in vars) {
		data[, ss_total:=sum((get(var)=='specified_separately')*births, na.rm=TRUE), by=c('iso3','year')]
		data[ss_total==total, (var):='99']
	}
	data$ss_total = NULL
	data$total = NULL
	
	# sometimes birth order is listed as "specified_separately" for just one age group
	# this should be counted as "not specified"
	# examples: SGP 1998, NLD 1990/1991, SVN 2000/2001, SWE 1994
	# test this for all possible combinations of variables, just to be safe
	for(var1 in vars) {
		for (var2 in vars) {
			if (var1==var2) next
			
			data$n_groups=NULL
			data[, n_groups:=length(unique(get(var1))), by=c('iso3','year',var2)]
			data[, max_groups:=max(n_groups), by=c('iso3','year')]
			
			# it's ok for variables to be listed as "specified_separately" among males and females but not sex=99
			if (var1=='sex') data[, n_groups:=n_groups+1]
			
			# count instances
			n = nrow(data[n_groups<max_groups & get(var1)!='specified_separately' & get(var2)=='specified_separately'])
			
			# print messages
			if (n>0) print(paste('sometimes', var2, 'is listed as \"specified_separately\" for just a few', var1, 'groups... they will be counted as \"not specified\"'))
		}
	}
	
	# sometimes birth order is specified separately, but age groups are specified in detail both times
	# this creates a potential conflict in the number of unspecified ages
	# fortunately, no known case has a conflict, but it's still possible
	# examples: NLD 2015, ITA 2014, GBR 2015
	
	# sometimes birth order is specified separately, but sex groups are specified in detail both times
	# this creates a potential conflict in the number of unspecified sexes
	# fortunately, no known case has a conflict, but it's still possible
	# examples (there were 342 at time of writing): ALB 2013, AUT 2000/2006-2013, BHR 1997/2011-2013
	
	# all "specified twice" types of cases (like the two chunks above) are handled by the binary byVars below
	# -------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------
	# Compute proportions

	# compute every proportion amongst all other variable's instances of "specified" and "specified_separately"
	data[, age_ss:= age=='specified_separately']
	data[, sex_ss:= sex=='specified_separately']
	data[, bw_ss:= bw=='specified_separately']
	data[, parity_ss:= parity=='specified_separately']
	ssByVars = c('age_ss','sex_ss','bw_ss','parity_ss')
	
	# compute the denominator by all "specified" and "specified_separately" groupings
	data[, total:=sum(births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	
	# age
	# compute the proportion among every other variable's "specified" and "specified_separately" groupings
	# to make sure to compute it amongst the correct denominator
	data[, unspecified_age:=sum((age=='99')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	data[, unspecified_age:=unspecified_age/total]
	data[is.nan(unspecified_age) | age=='specified_separately', unspecified_age:=NA]
	
	# sex
	data[, unspecified_sex:=sum((sex=='99')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	# data[, sex_denominator:=sum((sex!='specified_separately')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	data[, unspecified_sex:=unspecified_sex/total]
	data[is.nan(unspecified_sex) | sex=='specified_separately', unspecified_sex:=NA]
	# data[sex_denominator==0, sex_denominator:=NA]
	
	# parity
	data[, unspecified_parity:=sum((parity=='99')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	# data[, parity_denominator:=sum((parity!='specified_separately')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	data[, unspecified_parity:=unspecified_parity/total]
	data[is.nan(unspecified_parity) | parity=='specified_separately', unspecified_parity:=NA]
	# data[parity_denominator==0, parity_denominator:=NA]
	
	# bw
	data[, unspecified_bw:=sum((bw=='99')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	# data[, bw_denominator:=sum((bw!='specified_separately')*births, na.rm=TRUE), by=c('iso3','year', ssByVars)]
	data[, unspecified_bw:=unspecified_bw/total]
	data[is.nan(unspecified_bw) | bw=='specified_separately', unspecified_bw:=NA]
	
	# completeness
	data = merge(data, birthData, by=c('iso3','year'), all.x=TRUE)
	data[, completeness:=total/envelope]
	data[completeness>1, completeness:=1]
	
	
	# sometimes a proportion for a variable is different when 
	# computed among another variable's "specified_separately" group and its "specified" group
	# this shouldn't happen, so we take the average
	data[, unspecified_age:=mean(unspecified_age, na.rm=TRUE), by=byVars]
	data[, unspecified_sex:=mean(unspecified_sex, na.rm=TRUE), by=byVars]
	data[, unspecified_parity:=mean(unspecified_parity, na.rm=TRUE), by=byVars]
	data[, unspecified_bw:=mean(unspecified_bw, na.rm=TRUE), by=byVars]
	data[, completeness:=max(completeness, na.rm=TRUE), by=byVars]
	data[, total:=mean(total), by=byVars]
	
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
