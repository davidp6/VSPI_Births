# -------------------------------------------
# David Phillips
#
# 3/29/2017
# Visualize true vs simulated ASPBFs
# Current working directory should be VSPI_B
# -------------------------------------------


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

# output file
outFile = './Visualizations/simulation_probabilities.pdf'
# ------------------------------------------------------------------------


# ----------------------------------------------------------------------------
# Load/prep data

# load
ageProbs = fread(inFileAge)
sexProbs = fread(inFileSex)
parityProbs = fread(inFileParity)

# rescale variables
ageProbs[, unspecified_age:=unspecified_age/sum(unspecified_age)]
sexProbs[, unspecified_sex:=unspecified_sex/sum(unspecified_sex)]
parityProbs[, unspecified_parity:=unspecified_parity/sum(unspecified_parity)]

# rename variables
setnames(ageProbs, 'unspecified_age', 'probabilities')
setnames(sexProbs, 'unspecified_sex', 'probabilities')
setnames(parityProbs, 'unspecified_parity', 'probabilities')

# format variables
dts = c('ageProbs', 'sexProbs', 'parityProbs')
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
titles = c('Age', 'Sex', 'Parity')

# ---------------------------------------------------------------------------


# -------------------------------------------------------------------------------
# Graph
plots = list()
for(d in seq_along(dts)) {
	plots[[d]] = ggplot(get(dts[d]), aes(x=age, y=probabilities)) + 
		geom_point() + 
		geom_smooth() + 
		facet_grid(sex~parity) + 
		scale_color_manual('', values=colors) + 
		scale_fill_manual('', values='white') + 
		labs(title=paste('Probability of Unspecified', titles[d]), y='ASPBF', x='Maternal Age') + 
		theme_bw() + 
		theme(plot.title=element_text(hjust=.5))
}
# -------------------------------------------------------------------------------


# -------------------------------------------
# Save
pdf(outFile, height=6, width=9)
for(d in seq_along(dts)) print(plots[[d]])
dev.off()
# -------------------------------------------
