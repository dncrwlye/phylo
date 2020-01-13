#dan crowley

#01/03/2019: running the HNV factorization, but ignoring sampling effort

sampling_effort = FALSE
ncores=2
reps.per.worker = 250

library(phylofactor)
library(parallel)
library(tidyverse)
library(stringi)
load("/Users/buckcrowley/phylofactor_paper/data/batphy_revised_with_tax.Rdata")
load("~/Desktop/BDEL/BZDEL/meta_analysis/data/phylofactor work spaces/bat_tree")

Data <- bat_phy_tax %>%
  filter(hnv_samps > 0) %>%
  mutate(log_hnv_samps = log(hnv_samps)) %>%
  select(c(hnv_samps, hnv_positive, tree_species, log_hnv_samps)) %>%
  rename(Species = tree_species) %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  mutate(Species = stri_trans_totitle(Species)) %>%
  mutate(Sample = 1)

tree <- ape::drop.tip(bat_tree,bat_tree$tip.label[!(bat_tree$tip.label %in% Data$Species)])
Data[!(Data$Species %in% tree$tip.label),]

#december 1, 2019:  Miniopterus_schreibersii & Gardnerycteris_crenulatum  does not appear in the bat tree we have
#this is concerning, for obvious reasons. for now we will have to work around it...

Data <- Data[(Data$Species %in% bat_tree$tip.label),]

#01/04/2020: the tree species are out of order with the data 
#this should fix it

Data <- Data[order(match(Data$Species,tree$tip.label)),]
Data$Species == tree$tip.label

rm(batphy1, bat_tree)

names(Data) <- c('effort','Z', 'Species','log_effort', 'Sample')

#model.sampling.effort <- glm(Z~log_effort,family=binomial,data=Data)

#Data <- Data %>%
#  mutate(effort.fit = coef(model.sampling.effort)['log_effort']*Data$log_effort)

#rm(model.sampling.effort)

pf <- gpf(Data,tree,
          frmla.phylo=Z~phylo,
          family=binomial,
          nfactors=10,
          algorithm='phylo')
pf$factors

#source('/Users/buckcrowley/phylofactor_paper/R/phylo_workspace/null simulations script.R')
source('R/phylo_workspace/null simulations script.R')

save(list=ls(),file='/Users/buckcrowley/phylofactor_paper/data/phylofactor work spaces/hnv_workspace_no_SE')

rm(list=ls())


