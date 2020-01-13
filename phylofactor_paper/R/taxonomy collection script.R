#........................round 1 of taxonomyfying...............................................
#author:dan crowley
#date: nov 19th 2019
#after becker runs 'batphy' we can run this script 

list.files("/Users/dancrowley/phylofactor_paper/data")
load("/Users/dancrowley/phylofactor_paper/data/batphy_revised copy.Rdata")

library(stringi)
library(tidyverse)
library(taxize)
#.......... clean bat phylogeny

batphy_unique_species <- batphy$species

#skip this until we know what it is
# batphy <- batphy %>%
#   mutate(filo_surv = ifelse(is.na(filo_surv), 0, filo_surv)) %>%
#   mutate(species = as.character(species)) %>%
#   mutate(species = ifelse(species == "triaenops auritus", "paratriaenops auritus",
#                           ifelse(species == "artibeus incomitatus", "dermanura incomitata", species)))

#ran Nov 19th 2019:
#dont rerun unless needed: (takes around an hour)
classifications <- classification((batphy_unique_species), db = 'itis', rows=1)

# ══  Results  ═════════════════
# 
# ● Total: 1113 
# ● Found: 1112 
# ● Not Found: 1

#unlist classifications file format, which a dataframe of lists
m <- matrix(0,length(classifications),2) %>% data.frame()
for(i in 1:length(classifications))
{
  m[i,1] <- paste(as.data.frame(classifications[i])[1], collapse = "; ")
  print(i)
}
rm(i)

colnames(m) <- c('tax', 'species')

#clean classification data and extract species names from the taxonomy 
m <- m %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>%
  mutate(species = tolower(species))

#pull out the species in 'batphy' which were not identified in the original taxonomy search

batphy.missing <- batphy %>%
  filter(!(species %in% m$species)) 

unique(batphy.missing$species)

#........................round 2 of taxonomyfying...............................................
#search for missing species, again, looking at itis?
#17 missing species on november 19th 2019
#7 if you search using species, 17 if you search using 'search_term'

classifications_round2 <- classification((batphy.missing$species), db = 'itis')

#if you use species or 'search term' you get a different set of missing values 

# ══  Results  ═════════════════
# 
# ● Total: 7 (or 17)
# ● Found: 6 (or 16, depending on search term)
# ● Not Found: 1

m.r2 <- matrix(0,length(classifications_round2),2) %>% data.frame()
for(i in 1:length(classifications_round2))
{
  m.r2[i,1] <- paste(as.data.frame(classifications_round2[i])[1], collapse = "; ")
  print(i)
}

colnames(m.r2) <- c('tax', 'species')

m.r2 <- m.r2 %>% mutate(tax = gsub("c\\(", "", tax)) %>% mutate(tax = gsub("\\\"", "", tax)) %>% mutate(tax = gsub(",", ";", tax)) %>% mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>% mutate(species = tolower(species))

batphy.missing <- batphy %>%
  filter(!(species %in% m$species)) %>% filter(!(species %in% m.r2$species)) %>% mutate(species = as.character(species))

#final missing value(s):

#"Miniopterus schreibersii schreibersii" does not work because of this line: [A-Z][a-z]+ [a-z]+'
#remove missing values 
m.r2 <- m.r2[!is.na(m.r2$species),]
m.r2[m.r2$species == 'miniopterus schreibersii',2] <- ('miniopterus schreibersii schreibersii')

#natalus stramineus mexicanus is a bit more complicated
#https://bioone.org/journals/acta-chiropterologica/volume-14/issue-2/150811012X661639/Taxonomic-Status-Assessment-of-the-Mexican-Populations-of-Funnel-Eared/10.3161/150811012X661639.full
"natalus stramineus" %in% m$species
#revalue natalus stramineus mexicanus to natalus stramineus in bat phy
batphy[batphy$species == 'natalus stramineus mexicanus',]$species <- ('natalus stramineus')
m.r2[m.r2$species == 'natalus stramineus mexicanus',] <-NULL

#rerun 

batphy.missing <- batphy %>%  filter(!(species %in% m$species)) %>% filter(!(species %in% m.r2$species)) %>% mutate(species = as.character(species))

#........................last issue:............................................................
#........................myonycteris angolensis.................................................
#........................round 3 of taxonomyfying...............................................

classifications_round3 <- classification('myonycteris angolensis', db = 'eol')[1]

m.r3 <- matrix(0,length(classifications_round3),2) %>% data.frame()
for(i in 1:length(classifications_round3))
{
  m.r3[i,1] <- paste(as.data.frame(classifications_round3[i])[1], collapse = "; ")
  print(i)
}

colnames(m.r3) <- c('tax', 'species')

m.r3 <- m.r3 %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>%
  mutate(species = tolower(species))

batphy.missing <- batphy %>%
  filter(!(species %in% m$species)) %>%
  filter(!(species %in% m.r2$species)) %>%
  filter(!(species %in% m.r3$species)) %>%
  mutate(species = as.character(species))

#if true we can move on!
nrow(batphy.missing) == 0

#........................round 4 of taxonomyfying...............................................
#........................which species didn't actually download.................................

m.r.final <- rbind(m, m.r2, m.r3)
#find the species where the taxonomy didn't work, for whatever reason 
clean.up <- filter(m.r.final, !(grepl('Animalia', m.r.final$tax)))

#38 missing species 

#..............................................................................................
#.........................revaluing certain species names for Itis search......................
#.........................revaluing certain species names for Itis search......................
#..............................................................................................


#clean.up$species[i]
#original" <- 'itis approved'

"mimon crenulatum" <- "gardnerycteris crenulatum" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=1058372#null
batphy[batphy$species == 'mimon crenulatum',]$search_string <- ("gardnerycteris crenulatum")

#"artibeus hartii" <- "enchisthenes hartii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946072#null
batphy[batphy$species == 'artibeus hartii',]$search_string <- ("enchisthenes hartii")

#"hipposideros commersoni" <- 'hipposideros commersonii' 
batphy[batphy$species == "hipposideros commersoni",]$search_string <- ("hipposideros commersonii")

#apparently this is a plant? wtf
"trentepohlia iolithus"

#"neoromicia nanus" <- "neoromicia nana" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947329#null
batphy[batphy$species == 'neoromicia nanus',]$search_string <- ("neoromicia nana")

#"nyctalus velutinus" <- "nyctalus plancyi velutinus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948450#null
batphy[batphy$species == 'nyctalus velutinus',]$search_string <- ("nyctalus plancyi velutinus")

#"nycticeinops schlieffeni" <- "" #no itis entry
batphy[batphy$species == '',]$search_string <- ("")

"taphozous saccolaimus"  
"scotophilus heathi"
"anoura caudifera"  

#"scotonycteris ophiodon" <-  "casinycteris ophiodon" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947361#null
batphy[batphy$species == 'scotonycteris ophiodon',]$search_string <- ("casinycteris ophiodon")

#"artibeus incomitatus" <- "artibeus watsoni" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946071#null
batphy[batphy$species == '"artibeus incomitatus',]$search_string <- ("artibeus watsoni")

#"emballonura atrata" <- "paremballonura atrata" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947262#null
batphy[batphy$species == 'emballonura atrata',]$search_string <- ("paremballonura atrata")

#"eptesicus bobrinskoi" <- "eptesicus gobiensis bobrinskoi" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948891#null
batphy[batphy$species == 'eptesicus bobrinskoi',]$search_string <- ("eptesicus gobiensis bobrinskoi")

#"glauconycteris superba" <- 'niumbaha superba' #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947304#null
batphy[batphy$species == 'glauconycteris superba',]$search_string <- ("niumbaha superba")

#"lonchophylla thomasi" <- "hsunycteris thomasi" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947388#null
batphy[batphy$species == 'lonchophylla thomasi',]$search_string <- ("hsunycteris thomasi")

#"micronycteris homezi" <- "micronycteris minuta" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=631771#null
batphy[batphy$species == 'micronycteris homezi',]$search_string <- ("micronycteris minuta")

#"mimon koepckeae" <- "gardnerycteris koepckeae" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=1058371#null
batphy[batphy$species == 'mimon koepckeae',]$search_string <- ("gardnerycteris koepckeae")

#"mormoops blainvillei" <- "Mormoops blainvillii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=180050#null
batphy[batphy$species == 'mormoops blainvillei',]$search_string <- ("Mormoops blainvillii")

#"murina grisea" <- "harpiola grisea" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947198#null
batphy[batphy$species == 'murina grisea',]$search_string <- ("harpiola grisea")

#"murina huttonii" <-  "murina huttonii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947180#null
batphy[batphy$species == 'murina huttonii',]$search_string <- ("murina huttonii")

#"neoromicia brunneus" <- "neoromicia brunnea" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947328#null
batphy[batphy$species == 'neoromicia brunneus',]$search_string <- ("neoromicia brunnea")

#"neoromicia melckorum" <- "neoromicia capensis" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946211#null
batphy[batphy$species == 'neoromicia melckorum',]$search_string <- ("neoromicia capensis")

#"pipistrellus hesperus" <- "parastrellus hesperus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947298#null
batphy[batphy$species == 'pipistrellus hesperus',]$search_string <- ("parastrellus hesperus")

#"pipistrellus subflavus" <- "perimyotis subflavus subflavus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948895#null
batphy[batphy$species == 'pipistrellus subflavus',]$search_string <- ("perimyotis subflavus subflavus")

#"pteralopex acrodonta" <- "mirimiri acrodonta" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947187#null
batphy[batphy$species == 'pteralopex acrodonta',]$search_string <- ("mirimiri acrodonta")

#"pteropus leucopterus" <- "desmalopex leucopterus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947200#null
batphy[batphy$species == 'pteropus leucopterus',]$search_string <- ("desmalopex leucopterus")

#"triaenops auritus" <- "paratriaenops auritus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947352#null
batphy[batphy$species == 'triaenops auritus',]$search_string <- ("paratriaenops auritus")

#"triaenops furculus" <- "paratriaenops furculus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947353#null
batphy[batphy$species == 'triaenops furculus',]$search_string <- ("paratriaenops furculus")

#"vampyressa bidens" <- "vampyriscus bidens" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947383#null
batphy[batphy$species == 'vampyressa bidens',]$search_string <- ("vampyriscus bidens")

#"vampyressa brocki" <- "vampyriscus brocki" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947384#null
batphy[batphy$species == 'vampyressa brocki',]$search_string <- ("vampyriscus brocki")

#"vampyressa nymphaea" <- "vampyriscus nymphaea" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947385#null
batphy[batphy$species == 'vampyressa nymphaea',]$search_string <- ("vampyriscus nymphaea")

"artibeus hartii"
"natalus stramineus" 
"scotonycteris ophiodon"  



#end of the file. moving forward I will go to a revised version of this, where I will start by changing the names right off the bat!




#now that we've change the species names, we will update with the new search terms 

classifications_round4 <- classification(batphy[batphy$species %in% clean.up$species,]$search_string, db = 'itis', rows =1)

m.r4 <- matrix(0,length(classifications_round4),2) %>% data.frame()
for(i in 1:length(classifications_round4))
{
  m.r4[i,1] <- paste(as.data.frame(classifications_round4[i])[1], collapse = "; ")
  print(i)
}

colnames(m.r4) <- c('tax', 'species')

m.r4 <- m.r4 %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>%
  mutate(species = tolower(species))

#bind all searches together 
m.r.final <- rbind(m, m.r2, m.r3, m.r4)
#find the species where the taxonomy didn't work, for whatever reason 
clean.up <- filter(m.r.final, !(grepl('Animalia', m.r.final$tax)))













# "artibeus hartii" <- doesn't work
classification(clean.up$species[1], db = 'nbn')
clean.up$species[2]
classification(clean.up$species[2], db = 'nbn')

























data <- left_join(batphy, m.r.final)


clean.up <- filter(data, !(grepl('Animalia', data$tax)))






m.r3$tax  <- gsub('; [A-Z].<U\\+00A0>', "", m.r3$tax)
m.r3$tax  <- gsub(')',"", m.r3$tax)


batphy.xy <- left_join(batphy, m.r.final)


batphy.xy[batphy.xy$species == c('natalus stramineus mexicanus'),'tax'] <- m.r3[m.r3$species == c('natalus mexicanus'),'tax']
batphy.xy[batphy.xy$species == c('neoromicia nanus'),'tax'] <- m.r3[m.r3$species == c('neoromicia nana'),'tax']
batphy.xy[batphy.xy$species == c('neoromicia brunneus'),'tax'] <- m.r3[m.r3$species == c('neoromicia brunnea'),'tax']
batphy.xy[batphy.xy$species == c('nyctalus velutinus'),'tax'] <- m.r3[m.r3$species == c('nyctalus plancyi'),'tax']
batphy.xy[batphy.xy$species == c('glauconycteris superba'),'tax'] <- m.r3[m.r3$species == c('niumbaha superba'),'tax']
batphy.xy[batphy.xy$species == c('Myonycteris_angolensis'),'tax'] <- m.r3[m.r3$species == c('epomophorus angolensis'),'tax']


filter(batphy.xy, is.na(tax))
          
batphy1 <- batphy.xy
save(batphy1, file = 'data/bat_taxonomy_data.Rdata')




# 
# 
# #.......................now just do some conditional matching...........................................
# 
# 
# 
# 
# 
# 
# 
# 
# #........................round 4 of taxonomyfying...............................................
# 
# #classifications_round4 <- classification((batphy.x$species), db = "ncbi")
# 
# m.r4 <- matrix(0,8,2) %>% data.frame()
# for(i in 1:8)
# {
#   m.r4[i,1] <- paste(as.data.frame(classifications_round4[i])[1], collapse = "; ")
#   print(i)
# }
# 
# colnames(m.r4) <- c('tax', 'species')
# 
# m.r4 <- m.r4 %>%
#   mutate(tax = gsub("c\\(", "", tax)) %>%
#   mutate(tax = gsub("\\\"", "", tax)) %>%
#   mutate(tax = gsub(",", ";", tax)) %>%
#   mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>%
#   mutate(species = tolower(species))
# 
# batphy.x <- batphy %>%
#   filter(!(species %in% m$species)) %>%
#   filter(!(species %in% m.r2$species)) %>%
#   filter(!(species %in% m.r3$species)) %>%
#   filter(!(species %in% m.r4$species)) %>%
#   mutate(species = as.character(species))
# 
# #........................round 5 of taxonomyfying...............................................
# 
# #classifications_round5 <- classification((batphy.x$species), db = "ncbi")
# 
# m.r5 <- matrix(0,3,2) %>% data.frame()
# for(i in 1:3)
# {
#   m.r5[i,1] <- paste(as.data.frame(classifications_round5[i])[1], collapse = "; ")
#   print(i)
# }
# 
# colnames(m.r5) <- c('tax', 'species')
# 
# m.r5 <- m.r5 %>%
#   mutate(tax = gsub("c\\(", "", tax)) %>%
#   mutate(tax = gsub("\\\"", "", tax)) %>%
#   mutate(tax = gsub(",", ";", tax)) %>%
#   mutate(species = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+')) %>%
#   mutate(species = tolower(species))
# 
# batphy.x <- batphy %>%
#   filter(!(species %in% m$species)) %>%
#   filter(!(species %in% m.r2$species)) %>%
#   filter(!(species %in% m.r3$species)) %>%
#   filter(!(species %in% m.r4$species)) %>%
#   filter(!(species %in% m.r5$species)) %>%
#   mutate(species = as.character(species))
# 
# 
# 
# #........................round 6 of taxonomyfying...............................................
# 
# #classifications_round6 <- classification((batphy.x$species), db = "wiki")
# 
# m.r6 <- matrix(0,1,2) %>% data.frame()
# for(i in 1:1)
# {
#   m.r6[i,1] <- paste(as.data.frame(classifications_round6[i])[1], collapse = "; ")
#   print(i)
# }
# 
# colnames(m.r6) <- c('tax', 'species')
# 
# m.r6[1,2] <- "glauconycteris superba"
# 
# batphy.x <- batphy %>%
#   filter(!(species %in% m$species)) %>%
#   filter(!(species %in% m.r2$species)) %>%
#   filter(!(species %in% m.r3$species)) %>%
#   filter(!(species %in% m.r4$species)) %>%
#   filter(!(species %in% m.r5$species)) %>%
#   filter(!(species %in% m.r6$species)) %>%
#   mutate(species = as.character(species))
# 
# batphy <- batphy %>%
#   mutate(filo_surv = ifelse(is.na(filo_surv), 0, filo_surv)) %>%
#   mutate(species = as.character(species)) %>%
#   mutate(species = ifelse(species == "triaenops auritus", "paratriaenops auritus",
#                           ifelse(species == "artibeus incomitatus", "dermanura incomitata", species)))
# 
# m.rx <- rbind(m,  m.r2, m.r3, m.r4, m.r5, m.r6) %>% unique()
# remove <- c('19','1132','1140','951','1112','1130','1138')
# m.rx1 <- m.rx[!rownames(m.rx) %in% remove, ]
# 
# batphy1 <- left_join(batphy, m.rx1) %>%
#   rename(index = X)
# 
# batphy1 <- batphy1 %>%
#   mutate(tax = gsub("c\\(", "", tax)) %>%
#   mutate(tax = gsub("\"", "", tax)) %>%
#   mutate(tax = gsub(",", ";", tax))
# 
# classifications_Neoromicia_somalica  <- classification('Neoromicia somalica', db = "ncbi")
# 
# batphy1[339,'tax'] <- paste(as.data.frame(classifications_Neoromicia_somalica[1])[1], collapse = "; ")
# 
# setwd("/Users/buckcrowley/Desktop/BDEL/BZDEL/meta_analysis/")
# 
# save(batphy1, file = 'data/bat_taxonomy_data.Rdata')
# # 
# # 
# # inspect <- batphy1 %>%
# #   filter(!grepl('Chiroptera', tax))
