#01-03-2019
#dan crowley

#............phylofactor visualization script...................................

# Libraries ---------------------------------------------------------------

setwd("Users/buckcrowley/phylofactor_paper")
library(phylofactor)
library(parallel)
library(tidyverse)
library(stringi)
library(Cairo)
par(family="Times")   
CairoFonts(regular = 'Times-12')

# hnv SE+ factors-----------------------------------------------

load(file='data/phylofactor work spaces/hnv_workspace_SE')

jpeg(filename = 'figures/hnv sampling effort factors.jpg')
plot(c(Obj[1],diff(Obj)),type='o',lwd=2,cex=2,ylim=c(0,35),
     ylab = "Increase in Objective Function",
     main = "HNV SE+, 0 Factors?")
for (rr in 1:500){
  lines(c(S[rr,1],diff(S[rr,])),col=rgb(0,0,0,0.2))
}
points(deviances,col='red',pch=16,cex=2)
dev.off()

bb <- (t(diff(t(S))))
bb <- cbind(S[,1],bb)
Ojb.bb <- c(Obj[1],diff(Obj))
for (i in 1:10)
{
  print(ecdf(bb[,i])(Ojb.bb[i]))
}
#01/05/2020: 0 factors

plot(c(Obj),type='o',lwd=2,cex=2,ylim=c(0,200))
for (rr in 1:100){
  lines(c(S[rr,]),col=rgb(0,0,0,0.2))
}

rm(list=ls())

# hnv SE - factors-----------------------------------------------

load(file='data/phylofactor work spaces/hnv_workspace_no_SE')

jpeg(filename = 'figures/hnv no sampling effort.jpg')
plot(c(Obj[1],diff(Obj)),type='o',lwd=2,cex=2,ylim=c(0,35),
     ylab = "Increase in Objective Function",
     main = "HNV SE-, 4 Factors?")
for (rr in 1:500){
  lines(c(S[rr,1],diff(S[rr,])),col=rgb(0,0,0,0.2))
}
points(deviances,col='red',pch=16,cex=2)
dev.off()

bb <- (t(diff(t(S))))
bb <- cbind(S[,1],bb)
Ojb.bb <- c(Obj[1],diff(Obj))
for (i in 1:10)
{
  print(ecdf(bb[,i])(Ojb.bb[i]))
}

#4 factors

plot(c(Obj),type='o',lwd=2,cex=2,ylim=c(0,200))
for (rr in 1:500){
  lines(c(S[rr,]),col=rgb(0,0,0,0.2))
}
#going to go with 1 factor! 

rm(list=ls())

# filo SE+ factors-----------------------------------------------

load(file='data/phylofactor work spaces/filo_workspace_SE')
jpeg(filename = 'figures/filo with sampling effort factors.jpg')
plot(c(Obj[1],diff(Obj)),type='o',lwd=2,cex=2,ylim=c(0,50),
     ylab = "Increase in Objective Function",
     main = "FILO SE+, 5 Factors?")
for (rr in 1:500){
  lines(c(S[rr,1],diff(S[rr,])),col=rgb(0,0,0,0.2))
}
points(deviances,col='red',pch=16,cex=2)
dev.off()

bb <- (t(diff(t(S))))
bb <- cbind(S[,1],bb)
Ojb.bb <- c(Obj[1],diff(Obj))
for (i in 1:10)
{
  print(ecdf(bb[,i])(Ojb.bb[i]))
}

plot(c(Obj),type='o',lwd=2,cex=2,ylim=c(0,200))
for (rr in 1:500){
  lines(c(S[rr,]),col=rgb(0,0,0,0.2))
}



rm(list=ls())

# filo SE- factors-----------------------------------------------

load(file='data/phylofactor work spaces/filo_workspace_no_SE')
jpeg(filename = 'figures/filo no sampling effort factors.jpg')
plot(c(Obj[1],diff(Obj)),type='o',lwd=2,cex=2,ylim=c(0,50),
     ylab = "Increase in Objective Function",
     main = "FILO SE-, 2 Factors?")
for (rr in 1:500){
  lines(c(S[rr,1],diff(S[rr,])),col=rgb(0,0,0,0.2))
}
points(deviances,col='red',pch=16,cex=2)
dev.off()

bb <- (t(diff(t(S))))
bb <- cbind(S[,1],bb)
Ojb.bb <- c(Obj[1],diff(Obj))
for (i in 1:10)
{
  print(ecdf(bb[,i])(Ojb.bb[i]))
}


plot(c(Obj),type='o',lwd=2,cex=2,ylim=c(0,200))
for (rr in 1:100){
  lines(c(S[rr,]),col=rgb(0,0,0,0.2))
}


rm(list=ls())

# FIGURE MAKING ----

# hnv SE+ figure----

rm(list=ls())
load(file='data/phylofactor work spaces/hnv_workspace_SE')
load("data/batphy_revised_with_tax.Rdata")
source('R/taxonomy_group_name_function.R')

pf$call

taxonomy <- bat_phy_tax %>%
  select(c(species, tax)) 

names.storage <- list()
for (i in 1:10)
{
  indexes = pf$groups[[i]][[1]]
  species <- gsub("_", " ", tolower(tree$tip.label[indexes]))
  #print(species)
  print(i)
  group_taxonomy_list <- as.data.frame(taxonomy[match(species,taxonomy[,1]),2])
  names.storage[i] <- gsub("\\)|;","", as.character(taxonomy_group_name(group_taxonomy_list)))
}

#0 factors 

colfcn <- function(n) return(c("#440154FF"))

pf.tree <- pf.tree(pf, lwd=1, factors = 7, branch.length = "none", bg.color = NA)

d <- data.frame(x=pf.tree$ggplot$data[1:139,'x'] + .5,
                xend=pf.tree$ggplot$data[1:139,'x'] + 1+ Data$log_effort,
                y=pf.tree$ggplot$data[1:139,'y'],
                yend=pf.tree$ggplot$data[1:139,'y'] )

#PCR positivity 
#indicate on the tree which bats have been PCR+
load("~/Dropbox_gmail/Dropbox/bat virus meta-analysis/seroprevalence_revised.Rdata")

pcr.pos.hnv <- seroprevalence %>%
  filter(methodology_general == 'PCR based method') %>%
  filter(seroprevalence_percentage > 0) %>%
  filter(virus == 'Henipavirus' | virus == "nipah" | virus == "Cedar" | virus == 
           "cedar" | virus == "hendra")

pf$Data <- pf$Data %>%
  mutate(species.mutate = tolower(gsub("_", " ", Species))) %>%
  mutate(pcr.pos = ifelse(species.mutate %in% pcr.pos.hnv$species, 1,0))

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*pf$Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red') +
  #ggtree::geom_tiplab(aes(angle=angle)) +
  #ggtree::geom_tippoint(size=5*Data$ib,col='red') +
  geom_segment(data= d,aes(x=x,y=y,xend=xend,yend=yend, size= pf$Data$log_effort, colour = 'blue'))

ggsave("figures/hnv_tree_with_sampling_effort.png", bg = "transparent", height = 18, width = 18)

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*pf$Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red') +
  ggtree::geom_tiplab(aes(angle=angle)) +
  #ggtree::geom_tippoint(size=5*Data$ib,col='red') +
  geom_segment(data= d,aes(x=x,y=y,xend=xend,yend=yend, size= pf$Data$log_effort, colour = 'blue'))

ggsave("figures/hnv_tree_with_sampling_effort_tiplab.png", bg = "transparent", height = 18, width = 18)

# filo SE+ figure 1 ----
# settled on 1 or 5 factors
rm(list=ls())
load(file='data/phylofactor work spaces/filo_workspace_SE')
load("data/batphy_revised_with_tax.Rdata")
source('R/taxonomy_group_name_function.R')

taxonomy <- bat_phy_tax %>%
  select(c(species, tax)) 

names.storage <- list()

for (i in 1:10)
{
  indexes = pf$groups[[i]][[1]]
  species <- gsub("_", " ", tolower(tree$tip.label[indexes]))
  #print(species)
  #print(i)
  group_taxonomy_list <- as.data.frame(taxonomy[match(species,taxonomy[,1]),2])
  names.storage[i] <- gsub("\\)|;","", as.character(taxonomy_group_name(group_taxonomy_list)))
}

names.storage

factor.1 <- pf$groups[[1]][[1]]
paraphyletic.remainder <-     pf$groups[[1]][[2]]

Z <- Data$Z
factor.1.p <- sapply(factor.1,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
paraphyletic.remainder.p <- sapply(paraphyletic.remainder,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)

length(factor.1)
length(paraphyletic.remainder.p)

factor.1.prob <- sum(factor.1.p)/length(factor.1.p)
paraphyletic.remainder.prob <- sum(paraphyletic.remainder.p)/length(paraphyletic.remainder.p)
factor.1.prob 
# 0.093 for Rhinolophoidea
paraphyletic.remainder.prob
#0.438 for paraphyletric remainder

#now, 1/5/2020
#there are 4 additional factors which may improve model fit,
#however, these are individual species
#i'm not concinved these should be in the model output 

# B <- lapply(pf$groups, function(l) l[[1]])
# # B <- bins(pf$basis[,1:10])
# # B <- B[2:11]
# Z <- Data$Z
# probs <- sapply(B,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
# 

names.storage[[1]]
colfcn <- function(n) return(c("#440154FF"))

pf.tree <- pf.tree(pf, lwd=1, factors = 1, branch.length = "none", bg.color = NA)

tips <- nrow(Data)

d <- data.frame(x=pf.tree$ggplot$data[1:tips,'x'] + .5,
                xend=pf.tree$ggplot$data[1:tips,'x'] + 1+ Data$log_effort,
                y=pf.tree$ggplot$data[1:tips,'y'],
                yend=pf.tree$ggplot$data[1:tips,'y'] )

#PCR positivity 
#indicate on the tree which bats have been PCR+
load("~/Dropbox_gmail/Dropbox/bat virus meta-analysis/seroprevalence_revised.Rdata")

pcr.pos.hnv <- seroprevalence %>%
  filter(methodology_general == 'PCR based method') %>%
  filter(seroprevalence_percentage > 0) %>%
  #fix this soon:
  filter(virus == 'Filovirus' | virus == "Bombali Virus" | virus == 'ebola')

pf$Data <- pf$Data %>%
  mutate(species.mutate = tolower(gsub("_", " ", Species))) %>%
  mutate(pcr.pos = ifelse(species.mutate %in% pcr.pos.hnv$species, 1,0))

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*pf$Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red') +
  geom_segment(data= d,aes(x=x,y=y,xend=xend,yend=yend, size= pf$Data$log_effort, colour = 'blue'))

ggsave("figures/filo_tree_with_sampling_effort.png", bg = "transparent", height = 18, width = 18)

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*pf$Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red') +
  geom_segment(data= d,aes(x=x,y=y,xend=xend,yend=yend, size= pf$Data$log_effort, colour = 'blue')) +
  ggtree::geom_tiplab(aes(angle=angle)) 
  
ggsave("figures/filo_tree_with_sampling_effort_tiplab.png", bg = "transparent", height = 18, width = 18)

# hnv SE- figure 1 ----
# settled on 1 or 5 factors
rm(list=ls())
load(file='data/phylofactor work spaces/hnv_workspace_no_SE')
load("data/batphy_revised_with_tax.Rdata")
source('R/taxonomy_group_name_function.R')

taxonomy <- bat_phy_tax %>%
  select(c(species, tax)) 

names.storage <- list()

for (i in 1:10)
{
  indexes = pf$groups[[i]][[1]]
  species <- gsub("_", " ", tolower(tree$tip.label[indexes]))
  #print(species)
  #print(i)
  group_taxonomy_list <- as.data.frame(taxonomy[match(species,taxonomy[,1]),2])
  names.storage[i] <- gsub("\\)|;","", as.character(taxonomy_group_name(group_taxonomy_list)))
}

names.storage

#looks to be 3-4 factors, the 4th factor is "Pteropus macrotis", so again, single species

factor.1 <- pf$groups[[1]][[1]]
factor.2 <- pf$groups[[2]][[1]]
factor.3 <- pf$groups[[3]][[1]]
paraphyletic.remainder <-     pf$groups[[3]][[2]]

length(factor.1)
length(factor.2)
length(factor.3)
length(paraphyletic.remainder)
sum(143 + 14 + 4 + 2 + 119)


Z <- Data$Z
factor.1.p <- sapply(factor.1,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
factor.2.p <- sapply(factor.2,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
factor.3.p <- sapply(factor.3,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
paraphyletic.remainder.p <- sapply(paraphyletic.remainder,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)

factor.1.prob <- sum(factor.1.p)/length(factor.1.p)
factor.2.prob <- sum(factor.2.p)/length(factor.2.p)
factor.3.prob <- sum(factor.3.p)/length(factor.3.p)
paraphyletic.remainder.prob <- sum(paraphyletic.remainder.p)/length(paraphyletic.remainder.p)

factor.1.prob
factor.2.prob
factor.3.prob
paraphyletic.remainder.prob

# Pteropodinae     0.928      #33FF00FF
# Rousettus        1          #33FF00FF
# Hipposideros     1          #FF9900FF
# everything else  0.14       

colfcn <- function(n) return(c("#33FF00FF", "#6DCD59FF", "#FF9900FF"))

pf.tree <- pf.tree(pf, lwd=1, factors = 1:3, color.fcn=colfcn, branch.length = "none", bg.color = NA)

tips <- nrow(Data)

d <- data.frame(x=pf.tree$ggplot$data[1:tips,'x'] + .5,
                xend=pf.tree$ggplot$data[1:tips,'x'] + 1+ Data$log_effort,
                y=pf.tree$ggplot$data[1:tips,'y'],
                yend=pf.tree$ggplot$data[1:tips,'y'] )
#PCR positivity 
#indicate on the tree which bats have been PCR+
load("~/Dropbox_gmail/Dropbox/bat virus meta-analysis/seroprevalence_revised.Rdata")

pcr.pos.hnv <- seroprevalence %>%
  filter(methodology_general == 'PCR based method') %>%
  filter(seroprevalence_percentage > 0) %>%
  filter(virus == 'Henipavirus')

pf$Data <- pf$Data %>%
  mutate(species.mutate = tolower(gsub("_", " ", Species))) %>%
  mutate(pcr.pos = ifelse(species.mutate %in% pcr.pos.hnv$species, 1,0))

#dont plot sampling effort 

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red')

ggsave("figures/hnv no sampling effort.png", bg = "transparent", height = 18, width = 18)


# filo SE- figure 1 ----
# settled on 1 or 5 factors
rm(list=ls())
load(file='data/phylofactor work spaces/filo_workspace_no_SE')
load("data/batphy_revised_with_tax.Rdata")
source('R/taxonomy_group_name_function.R')

taxonomy <- bat_phy_tax %>%
  select(c(species, tax)) 

names.storage <- list()

for (i in 1:10)
{
  indexes = pf$groups[[i]][[1]]
  species <- gsub("_", " ", tolower(tree$tip.label[indexes]))
  #print(species)
  #print(i)
  group_taxonomy_list <- as.data.frame(taxonomy[match(species,taxonomy[,1]),2])
  names.storage[i] <- gsub("\\)|;","", as.character(taxonomy_group_name(group_taxonomy_list)))
}

names.storage

#looks to be 2 factors, same as last time

factor.1 <- pf$groups[[1]][[1]]
factor.2 <- pf$groups[[2]][[1]]
paraphyletic.remainder <-     pf$groups[[2]][[2]]

length(factor.1)
#14
length(factor.2)
#39
length(paraphyletic.remainder)
sum(14 + 39 + 79)


Z <- Data$Z
factor.1.p <- sapply(factor.1,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
factor.2.p <- sapply(factor.2,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)
paraphyletic.remainder.p <- sapply(paraphyletic.remainder,FUN=function(ix,Z) mean(Z[ix]),Z=Z) %>% signif(.,digits=2)

factor.1.prob <- sum(factor.1.p)/length(factor.1.p)
factor.2.prob <- sum(factor.2.p)/length(factor.2.p)
paraphyletic.remainder.prob <- sum(paraphyletic.remainder.p)/length(paraphyletic.remainder.p)

factor.1.prob
factor.2.prob
paraphyletic.remainder.prob

# Pteropodidae     0.87      #482878FF
# Rhinolophoidea   0.076     #440154FF
# everything else  0.35       

colfcn <- function(n) return(c("#482878FF", "#440154FF"))

pf.tree <- pf.tree(pf, lwd=1, factors = 1:2, color.fcn=colfcn, branch.length = "none", bg.color = NA)

tips <- nrow(Data)

d <- data.frame(x=pf.tree$ggplot$data[1:tips,'x'] + .5,
                xend=pf.tree$ggplot$data[1:tips,'x'] + 1+ Data$log_effort,
                y=pf.tree$ggplot$data[1:tips,'y'],
                yend=pf.tree$ggplot$data[1:tips,'y'] )

#PCR positivity 
#indicate on the tree which bats have been PCR+
load("~/Dropbox_gmail/Dropbox/bat virus meta-analysis/seroprevalence_revised.Rdata")

pcr.pos.hnv <- seroprevalence %>%
  filter(methodology_general == 'PCR based method') %>%
  filter(seroprevalence_percentage > 0) %>%
  filter(virus == 'Filovirus')

pf$Data <- pf$Data %>%
  mutate(species.mutate = tolower(gsub("_", " ", Species))) %>%
  mutate(pcr.pos = ifelse(species.mutate %in% pcr.pos.hnv$species, 1,0))

pf.tree$ggplot +
  ggtree::theme_tree(bgcolor = NA, fgcolor = NA, plot.background = element_rect(fill = "transparent",colour = NA)) +
  ggtree::geom_tippoint(size=10*pf$Data$Z,col='blue')  +
  ggtree::geom_tippoint(size=5*pf$Data$pcr.pos, shape = 17, col='red')

ggsave("figures/filo no sampling effort.png", bg = "transparent", height = 18, width = 18)








