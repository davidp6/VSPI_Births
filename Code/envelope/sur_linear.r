# ------------------------------------------------------------
# David Phillips
#
# 3/29/2017
# Function to compute country-year-age-sex-parity births from WPP and HPD
# Uses Seemingly Unrelated Regression with no polynomials
# Takes cya, cys and cy-tfr from WPP, cyap from HPD
# Assumes sex ratio is independent of age
# Assumes parity is independent of sex
# Assumes tfr is independent of age within a country-year
# Assumes parity is a linear function of age and TFR
# ------------------------------------------------------------

# start function
sur_linear = function(data) {
	# ------------------
	# Set up R
	library(boot)
	library(systemfit)
	# ------------------
	
	
	# -------------------------------------------------------------------------------------
	# Predict parity proportions for each CYA
	
	# precision variables
	data[, year2:=year^2]
	data[, age2:=age^2]
	data[, year3:=year^3]
	data[, age3:=age^3]
	
	# SUR formulae
	form1 = logit(parity_proportion1) ~ year*mf_ratio*births*age*tfr
	form2 = logit(parity_proportion2) ~ year*mf_ratio*births*age*tfr
	form3 = logit(parity_proportion3) ~ year*mf_ratio*births*age*tfr
	form4 = logit(parity_proportion4) ~ year*mf_ratio*births*age*tfr
	system = list(proportion1=form1, proportion2=form2, proportion3=form3, proportion4=form4)
	
	# fit system of equations
	sFit = systemfit(system, data=data)
	
	# predict 
	preds = predict(sFit, newdata=data)
	for(v in names(preds)) preds[[v]] = inv.logit(preds[[v]])
	data = cbind(data, preds)
	
	# rescale
	data[, sum:=proportion1.pred+proportion2.pred+proportion3.pred+proportion4.pred]
	data[, proportion1_rescaled:=proportion1.pred/sum]
	data[, proportion2_rescaled:=proportion2.pred/sum]
	data[, proportion3_rescaled:=proportion3.pred/sum]
	data[, proportion4_rescaled:=proportion4.pred/sum]
	# -------------------------------------------------------------------------------------
	
	
	# ------------------------------------------------------------------------------------------------------
	# Compute births by CYASP
	
	# drop unnecessary variables
	vars = c('iso3','year','age','country','super_region', 'births', 'mf_ratio', 
			'proportion1_rescaled', 'proportion2_rescaled', 'proportion3_rescaled', 'proportion4_rescaled')
	data = data[, vars, with=FALSE]
	
	# compute male and female births assuming independence
	# ratio is m/f, so m/f=ratio and m+f=T, so (1+ratio)*f=T or f=T/(1+ratio)
	data[, birthsf:=births/(mf_ratio+1)]
	data[, birthsm:=births-birthsf]
	
	# compute sex-parity births
	data[, birthsf1:=birthsf*proportion1_rescaled]
	data[, birthsf2:=birthsf*proportion2_rescaled]
	data[, birthsf3:=birthsf*proportion3_rescaled]
	data[, birthsf4:=birthsf*proportion4_rescaled]
	data[, birthsm1:=birthsm*proportion1_rescaled]
	data[, birthsm2:=birthsm*proportion2_rescaled]
	data[, birthsm3:=birthsm*proportion3_rescaled]
	data[, birthsm4:=birthsm*proportion4_rescaled]
	
	# drop everything but IDs and final estimates
	data$births = NULL
	data$birthsf = NULL
	data$birthsm = NULL
	data$mf_ratio = NULL
	data$proportion1_rescaled=NULL
	data$proportion2_rescaled=NULL
	data$proportion3_rescaled=NULL
	data$proportion4_rescaled=NULL
	
	# reshape long
	data = melt(data, id.vars=c('iso3','year','age','country','super_region'), value.name='births')
	
	# parse sex and parity
	data[, parity:=as.numeric(str_sub(variable, -1, -1))]
	data[, sex:=str_sub(variable, -2, -2)]
	data$variable = NULL
	# ------------------------------------------------------------------------------------------------------
	
	
	# ------------
	# Return
	return(data)
	# ------------
} # end function
