# -----------------------------------
# David Phillips
#
# 3/29/2017
# Visualize true vs simulated ASPBFs
# -----------------------------------


# --------------------
# Set up R
rm(list=ls())
library(tools)
library(data.table)
library(ggplot2)
library(RColorBrewer)
# --------------------


# -------------------------------------------------------------------
# Files/directories/lists

# setting
var = 'completeness'
Var = toTitleCase(var)

# input file
inFile = paste0('./Data/Simulation_Outputs/', var, '_simulations.csv')

# output file
outFile = paste0('./Visualizations/', var, '_simulations.pdf')
# -------------------------------------------------------------------


# -------------------------------------
# Load/prep data

# load
sim = fread(inFile)

# format variables
sim[sex=='m', sex:='Male']
sim[sex=='f', sex:='Female']
sim[, parity:=as.character(parity)]
sim[parity=='4', parity:='4+']
# -------------------------------------


# ---------------------------------------------------------------------------
# Set up to graph

# levels of graph
vars = names(sim)[grepl('aspbf_', names(sim))]
vars = c(vars[1], vars[seq(10, length(vars), 10)]) # select every 10th

# colors
colors = c('True Distribution'='#4f953b', 'Simulated Distribution'='#F16B6F')

# titles
title = paste(Var, 'Simulation Results\nSimulation Sample Size: ')
# ---------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph
plots = list()
for(v in seq_along(vars)) { 
	loss = gsub('aspbf_', '', vars[v])
	sample = (1-as.numeric(loss)) * 100
	plots[[v]] = ggplot(sim, aes(x=age, y=aspbf, color='True Distribution')) + 
		geom_point() + 
		geom_smooth(se=FALSE) + 
		geom_point(aes(y=get(vars[v]), color='Simulated Distribution')) + 
		geom_smooth(aes(y=get(vars[v]), color='Simulated Distribution'), se=FALSE) + 
		facet_grid(sex~parity, scales='free') + 
		scale_color_manual('', values=colors) + 
		scale_fill_manual('', values='white') + 
		labs(title=paste0(title, sample, '%'), y='ASPBF', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5))
}
# -------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=4.5, width=9)
for(v in seq_along(vars)) print(plots[[v]])
dev.off()
# -------------------------------------------
