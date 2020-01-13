#dan crowley
#12/01/2019: attempt to run filo factorization again
#updated with new data 
#does not ignore sampling effort 
#1/2/2019
#DC: this is for working with the phylo glm while accounting for sampling effor 
#its meant to run on my home laptop

library(phylofactor)
library(parallel)
library(tidyverse)
library(stringi)

sampling_effort = FALSE
ncores=2
reps.per.worker = 250

load("/Users/buckcrowley/phylofactor_paper/data/batphy_revised_with_tax.Rdata")
load("~/Desktop/BDEL/BZDEL/meta_analysis/data/phylofactor work spaces/bat_tree")

Data <- bat_phy_tax %>%
  filter(filo_samps > 0) %>%
  mutate(log_filo_samps = log(filo_samps)) %>%
  select(c(filo_samps, filo_positive, tree_species, log_filo_samps)) %>%
  rename(Species = tree_species) %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  mutate(Species = stri_trans_totitle(Species)) %>%
  mutate(Sample = 1)

tree <- ape::drop.tip(bat_tree,bat_tree$tip.label[!(bat_tree$tip.label %in% Data$Species)])
Data[!(Data$Species %in% bat_tree$tip.label),]

bat_tree$tip.label[grepl("Miniopterus", bat_tree$tip.label)]

#december 1, 2019:  Miniopterus_schreibersii does not appear in the bat tree we have
#this is concerning, for obvious reasons. for now we will have to work around it...

Data <- Data[(Data$Species %in% bat_tree$tip.label),]

#01/04/2020: the tree species are out of order with the data 
#this should fix it

Data <- Data[order(match(Data$Species,tree$tip.label)),]
Data$Species == tree$tip.label

rm(batphy1, bat_tree)

names(Data) <- c('effort','Z', 'Species','log_effort', 'Sample')

# model.sampling.effort <- glm(Z~log_effort,family=binomial,data=Data)
# 
# Data <- Data %>%
#   mutate(effort.fit = coef(model.sampling.effort)['log_effort']*Data$log_effort)

rm(model.sampling.effort)

pf <- gpf(Data,tree,
          frmla.phylo=Z~phylo,
          family=binomial,
          nfactors=10,
          algorithm='phylo')

source('R/phylo_workspace/null simulations script.R')

save(list=ls(),file= '/Users/buckcrowley/phylofactor_paper/data/phylofactor work spaces/filo_workspace_no_SE')

rm(list=ls())
