
#dan crowley
#12/01/2019: attempt to run filo factorization again
#updated with new data 
#ignores sampling effort 

setwd("/Users/buckcrowley/phylofactor_paper")

library(phylofactor)
library(parallel)
library(tidyverse)
library(stringi)
require(pscl)
require(boot)
library(pscl)

load("/Users/buckcrowley/phylofactor_paper/data/batphy_revised_with_tax.Rdata")
load("~/Desktop/BDEL/BZDEL/meta_analysis/data/phylofactor work spaces/bat_tree")

Data <- bat_phy_tax %>%
  mutate(filo_surv_bin = ifelse(is.na(filo_surv), 0, filo_surv)) %>%
  mutate(filo_surv_bin = ifelse(filo_surv_bin == 1, 1, 0)) %>%
  mutate(log_filo_samps = log(filo_samps)) %>%
  select(c(filo_samps, filo_surv_bin, tree_species, log_filo_samps))%>%
  dplyr::rename(Species = tree_species) %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  mutate(Species = stri_trans_totitle(Species)) %>%
  mutate(Sample = 1) %>%
  unique()

Data[duplicated(Data$Species),]

tree<-bat_tree
tree <- ape::drop.tip(tree,tree$tip.label[!(tree$tip.label %in% Data$Species)])
Data <- Data[Data$Species %in% tree$tip.label,]

#rm(batphy1, bat_tree)

names(Data) <- c('Z.poisson',"Z.binom", 'Species', 'log_effort', 'Sample')

Data <- Data[Data$Species %in% tree$tip.label,]
n_factors = 10
Data[is.na(Data)] <- 0

source('R/phylo_workspace/negative_binomial/negative binomial no zero inflation tests for all bats sampling effort.R')
source('R/phylo_workspace/negative_binomial/null simulations script all bats try 4.R')

save(list=ls(),file='data/phylofactor work spaces/neg binom no zeroinfl filo_workspace_try2')

rm(list=ls())

plot(logLik.negbinom[[1]])

plot(c(Obj[1],diff(Obj)),type='o',lwd=2,cex=2,ylim=c(0,35),
     ylab = "Increase in Objective Function",
     main = "HNV SE+, 0 Factors?")
for (rr in 1:500){
  lines(c(x[rr,1],diff(x[rr,])),col=rgb(0,0,0,0.2))
}
points(deviances,col='red',pch=16,cex=2)



