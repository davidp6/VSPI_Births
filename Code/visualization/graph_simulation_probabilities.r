# -----------------------------------
# David Phillips
#
# 3/29/2017
# Visualize true vs simulated ASPBFs
# -----------------------------------


# -----------------------------
# Set up R
rm(list=ls())
library(data.table)
library(ggplot2)
library(RColorBrewer)
# -----------------------------


# ------------------------------------------------------------------------
# Files/directories/lists

# input file
inFileAge = './Data/Simulation_Inputs/unspecified_age_probabilities.csv'
inFileSex = './Data/Simulation_Inputs/unspecified_sex_probabilities.csv'
inFileParity = './Data/Simulation_Inputs/unspecified_parity_probabilities.csv'
inFileCompleteness = './Data/Simulation_Inputs/completeness_probabilities.csv'

# output file
outFile = './Visualizations/simulation_probabilities.pdf'
# ------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Load/prep data

# load
ageProbs = fread(inFileAge)
sexProbs = fread(inFileSex)
parityProbs = fread(inFileParity)
completenessProbs = fread(inFileCompleteness)

# rescale variables
ageProbs[, unspecified_age:=unspecified_age/sum(unspecified_age)]
sexProbs[, unspecified_sex:=unspecified_sex/sum(unspecified_sex)]
parityProbs[, unspecified_parity:=unspecified_parity/sum(unspecified_parity)]
completenessProbs[, completeness:=completeness/sum(completeness)]

# rename variables
setnames(ageProbs, 'unspecified_age', 'probabilities')
setnames(sexProbs, 'unspecified_sex', 'probabilities')
setnames(parityProbs, 'unspecified_parity', 'probabilities')
setnames(completenessProbs, 'completeness', 'probabilities')

# format variables
dts = c('ageProbs', 'sexProbs', 'parityProbs', 'completenessProbs')
for (dt in dts) {
	tmp = get(dt)
	tmp[sex=='m', sex:='Male']
	tmp[sex=='f', sex:='Female']
	tmp[, parity:=as.character(parity)]
	tmp[parity=='4', parity:='4+']
	assign(dt, tmp)
}
# ----------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# Set up to graph

# titles
titles = c('Age', 'Sex', 'Parity', 'Completeness')

# ---------------------------------------------------------------------------


# --------------------------------------------------------------------
# Graph
plots = list()
for(d in seq_along(dts)) {
	plots[[d]] = ggplot(get(dts[d]), aes(x=age, y=probabilities)) + 
		geom_point() + 
		geom_smooth() + 
		facet_grid(sex~parity) + 
		scale_color_manual('', values=colors) + 
		scale_fill_manual('', values='white') + 
		labs(title=paste('Probability of Unspecified', titles[d]), 
			y='Probability', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5))
}
# --------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=9)
for(d in seq_along(dts)) print(plots[[d]])
dev.off()
# -------------------------------------------
