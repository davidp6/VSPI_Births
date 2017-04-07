# -------------------------------------
# David Phillips
#
# 3/24/2017
# Make graphs of accuracy by indicator
# -------------------------------------


# -------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# -------------------


# -----------------------------------------------------------
# Files/directories/lists

# input file
inFile = './Data/Simulation_Outputs/accuracy_estimates.csv'

# output file
outFile = './Visualizations/accuracy_estimates.pdf'
# -----------------------------------------------------------


# ---------------------------------------------
# Load/prep data

# load
data = fread(inFile)

# add 100%
tmp = data[1]
tmp[, level:=0]
tmp[, age:=1]
tmp[, sex:=1]
tmp[, parity:=1]
tmp[, completeness:=1]
data = rbind(data, tmp)

# melt
data = melt(data, id.vars='level')

# reverse all (including completeness, which comes out of the simulation reversed)
data[, level:=as.numeric(level)]
data[, level:= 1-level]
# ---------------------------------------------


# -------------------------------------------------------------------------
# Set up to graph

# labels
data[variable=='age', variable:='Age Unspecified*']
data[variable=='sex', variable:='Sex Unspecified*']
data[variable=='parity', variable:='Parity Unspecified*']
data[variable=='completeness', variable:='Completeness']

# colors/line types
colors = brewer.pal(4, 'Paired')
types = c(1, 2, 4, 5)

# titles
title = 'Simulated ASPBF Accuracy Associated with Each Indicator'
ytitle = 'ASPBF Accuracy'
xtitle = 'Value of Indicator'
caption = '*Subtracted from one so that higher values are preferable to lower, as with other indicators'
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Graph
p = ggplot(data, aes(x=level, y=value, color=variable, lty=variable, group=variable)) + 
	geom_line(size=1.25) +
	labs(title=title, y=ytitle, x=xtitle, caption=caption) + 
	scale_color_manual('', values=colors) + 
	scale_linetype_manual('', values=types) + 
	theme_bw() + 
	theme(plot.title=element_text(hjust=.5), plot.caption=element_text(size=8))
# -------------------------------------------------------------------------------------


# ------------------------------
# Save
pdf(outFile, height=6, width=9)
p
dev.off()
# ------------------------------
