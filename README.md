# VSPI_Births
Central code for computing the VSPI-B, Vital Statistics Performance Index for Births

#### Basic approach: simulate accuracy under suboptimal data quality, apply simulation results to real data

#### Instructions
1. Clone this repository
2. Put data to be analyzed in ../Data/Country_Data/
3. Set current working directory (in R) to the location of this readme 
3. source(compute_vspib.r)
4. computeVSPIB(fileName='[file name of data]')

#### Code structure:
1. simulate.r
    Function that takes simulation population and sampling probabilities and simulates varying levels of data quality for a specified indicator

2. evaluate_accuracy.r
	Function that takes output from simulate.r and evaluates it according to a metric of accuracy

3. objective_functions.r
	Function that contains the accuracy functions used by evaluate_accuracy.r
	
4. run_simulations.r 
	Function that runs the full simulation with defaults set to the current accepted parameters
	
5. compute_vspib.r
	Function that takes simulated accuracy, new data, and computes the VSPI-B
	
6. make_plots.r
	Function that takes the output from compute_vspib.r and makes a standard set of graphs
	
7. time_series_plot.r 
	Function that makes a time series graph with sub-plots

8. other plot functions to come

#### Data formats:
[fill in]
