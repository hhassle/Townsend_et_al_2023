library(PhyInformR)
library(ape)
library(splines)
library(geiger)

#site rates
source("~/Downloads/all_site_rates.txt")
source("~/Documents/Covid_figures/MASS_PRF/helper_functions.R")

#tree
tree<-read.tree("~/Downloads/Guide_tree.newick")


#get the names of the rate vector objects from your sourced file
rate.vectors <-c("E_extract","M_extract","N_extract","orf1a_extract","orf1b_extract","orf3a_extract","orf6_extract","orf7a_extract","orf7b_extract","orf8_extract","S_extract")

#run this code, note it uses the get() function internally to access the rate vectors
informativeness.profiles(rate.vectors,tree)




