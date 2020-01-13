
#........................round 1, try 2, of taxonomyfying...............................................
#author:dan crowley
#date: nov 20th 2019
#after becker runs 'batphy' we can run this script 
#next, we will change some of the species names to match the ITIS names. These were identified from the first round, try 1 of taxonomyfying 


list.files("/Users/dancrowley/phylofactor_paper/data")
load("/Users/dancrowley/phylofactor_paper/data/batphy_revised copy.Rdata")

library(stringi)
library(tidyverse)
library(taxize)

#..............................................................................................
#.........................revaluing certain species names for Itis search......................
#.........................revaluing certain species names for Itis search......................
#..............................................................................................

#clean.up$species[i]
#original" <- 'itis approved'

#"mimon crenulatum" <- "gardnerycteris crenulatum" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=1058372#null
batphy[batphy$search_string == "mimon crenulatum",]$search_string <- ("gardnerycteris crenulatum") #this one is in the search string only as well

#"artibeus hartii" <- "enchisthenes hartii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946072#null
batphy[batphy$species == 'artibeus hartii',]$search_string <- ("enchisthenes hartii")

#"hipposideros commersoni" <- 'hipposideros commersonii' 
batphy[batphy$species == "hipposideros commersoni",]$search_string <- ("hipposideros commersonii")

#"neoromicia nanus" <- "neoromicia nana" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947329#null
batphy[batphy$species == 'neoromicia nanus',]$search_string <- ("neoromicia nana")

#"nyctalus velutinus" <- "nyctalus plancyi velutinus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948450#null
batphy[batphy$species == 'nyctalus velutinus',]$search_string <- ("nyctalus plancyi velutinus")
batphy[batphy$species == 'nyctalus velutinus',]$species <- ("nyctalus plancyi velutinus") #problem on multiple levels

#"nycticeinops schlieffeni" <- "" #no itis entry
#batphy[batphy$species == '',]$search_string <- ("")

#"scotonycteris ophiodon" <-  "casinycteris ophiodon" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947361#null
batphy[batphy$species == 'scotonycteris ophiodon',]$search_string <- ("casinycteris ophiodon")

#"artibeus incomitatus" <- "artibeus watsoni" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946071#null
batphy[batphy$species == "artibeus incomitatus",]$search_string <- ("artibeus watsoni")
#batphy[batphy$search_string == "artibeus incomitatus",]$search_string <- ("artibeus watsoni") #also in the search_string

#"emballonura atrata" <- "paremballonura atrata" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947262#null
batphy[batphy$species == 'emballonura atrata',]$search_string <- ("paremballonura atrata")

#"eptesicus bobrinskoi" <- "eptesicus gobiensis bobrinskoi" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948891#null
batphy[batphy$species == 'eptesicus bobrinskoi',]$search_string <- ("eptesicus gobiensis bobrinskoi")
batphy[batphy$species == 'eptesicus bobrinskoi',]$species <- ("eptesicus gobiensis bobrinskoi")

#"glauconycteris superba" <- 'niumbaha superba' #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947304#null
batphy[batphy$species == 'glauconycteris superba',]$search_string <- ("niumbaha superba")

#"lonchophylla thomasi" <- "hsunycteris thomasi" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947388#null
batphy[batphy$species == 'lonchophylla thomasi',]$search_string <- ("hsunycteris thomasi")

#"micronycteris homezi" <- "micronycteris minuta" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=631771#null
batphy[batphy$species == 'micronycteris homezi',]$search_string <- ("micronycteris minuta")

#"mimon koepckeae" <- "gardnerycteris koepckeae" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=1058371#null
batphy[batphy$species == 'mimon koepckeae',]$search_string <- ("gardnerycteris koepckeae")

#"mormoops blainvillei" <- "Mormoops blainvillii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=180050#null
batphy[batphy$species == 'mormoops blainvillei',]$search_string <- ("mormoops blainvillii")
batphy[batphy$species == 'mormoops blainvillei',]$species <- ("mormoops blainvillii")

#"murina grisea" <- "harpiola grisea" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947198#null
batphy[batphy$species == 'murina grisea',]$search_string <- ("harpiola grisea")

#"murina huttoni" <-  "murina huttonii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947180#null
batphy[batphy$species == 'murina huttoni',]$search_string <- ("murina huttonii")
batphy[batphy$search_string == 'murina huttoni',]$search_string <- ("murina huttonii") #also in the search string variable

#"neoromicia brunneus" <- "neoromicia brunnea" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947328#null
batphy[batphy$species == 'neoromicia brunneus',]$search_string <- ("neoromicia brunnea")

#"neoromicia melckorum" <- "neoromicia capensis" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=946211#null
batphy[batphy$species == 'neoromicia melckorum',]$search_string <- ("neoromicia capensis")

#"pipistrellus hesperus" <- "parastrellus hesperus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947298#null
batphy[batphy$species == 'pipistrellus hesperus',]$search_string <- ("parastrellus hesperus")

#"pipistrellus subflavus" <- "perimyotis subflavus subflavus" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=948895#null
batphy[batphy$species == 'pipistrellus subflavus',]$search_string <- ("perimyotis subflavus subflavus")
batphy[batphy$species == 'pipistrellus subflavus',]$species <- ("perimyotis subflavus subflavus")

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

#"scotophilus heathi" <- "Scotophilus heathii" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=947385#null
batphy[batphy$search_string == 'scotophilus heathi',]$search_string <- ("scotophilus heathii")

#"anoura caudifer" <-  "anoura caudifera" #https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=632270#null
#batphy[batphy$search_string == "anoura caudifera",]$search_string <- ("anoura caudifer")
#batphy[batphy$species == "anoura caudifera",]$search_string <- ("anoura caudifer")

'miniopterus schreibersii schreibersii' <-  "miniopterus schreibersii"
batphy[batphy$species == 'miniopterus schreibersii schreibersii',]$search_string <- ("miniopterus schreibersii")
batphy[batphy$search_string == 'miniopterus schreibersii schreibersii',]$search_string <- ("miniopterus schreibersii") #also in the search string variable

#.......... identify bat taxonomic information

batphy_unique_species <- batphy$search_string

classifications <- classification((batphy_unique_species), db = 'itis', rows=1)

#══  Results  ═════════════════

#● Total: 1113 
#● Found: 1112 
#● Not Found: 

tax.storage <- matrix(0,length(classifications),2) %>% data.frame()
for(i in 1:length(classifications))
{
  tax.storage[i,1] <- paste(as.data.frame(classifications[i])[1], collapse = "; ")
  print(i)
}
rm(i)

colnames(tax.storage) <- c('tax', 'search_string')

#clean classification data and extract species names from the taxonomy 
tax.storage <- tax.storage %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(search_string = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+( [a-z]+)?'))%>% #match  on Genus  species followed by an optional sub  species  
  mutate(search_string = tolower(search_string))

#first, identify which species are not classified as bats
clean.up <- filter(tax.storage, !(grepl('Chiroptera', tax.storage$tax))) %>% filter(!is.na(search_string))
clean.up$search_string
# 4  species,as of November 11th,2020 
#[1] "trentepohlia iolithus"    "nycticeinops schlieffeni" "taphozous saccolaimus"    "anoura caudifera"    

#identify which of these bats are important in our dataset
#batphy %>% filter(species %in% clean.up$species) %>% select(species) %>% print()
batphy %>% filter(search_string %in% clean.up$search_string) %>% select(search_string) %>% print()
#only  two species in our actual data set, why the others  even  got  searched for is  beyond me. (as of November  22, 2020)
# species
# 1 nycticeinops schlieffeni
# 2  saccolaimus saccolaimus

#pull out the species in 'batphy' which were not identified in the original taxonomy search

batphy.missing <- left_join(batphy,  tax.storage, by  = c("search_string" = 'search_string')) %>%  filter(is.na(tax))

unique(batphy.missing$species)
unique(batphy.missing$search_string)

#11 in total on November 22th, 2019:
# [1] "hipposideros pygmaeus"  "ia io"                  "miniopterus manavi"     "myonycteris angolensis" "myotis capaccinii"     
# [6] "peropteryx kappleri"    "rhinolophus darlingi"   "rhinolophus mehelyi"    "anoura caudifer"        "nycteris major"        
# [11] "pteropus intermedius" 

#................................................................................................................................................
#................................................................................................................................................
#...........................................................round 2 of search....................................................................
#................................................................................................................................................
#................................................................................................................................................

#search by "species", instead of "search_string"
classifications_round2 <- classification((batphy.missing$species), db = 'itis')

#11.22.19 results: 
# ══  Results  ═════════════════
# 
# ● Total: 11 
# ● Found: 10 
# ● Not Found: 1

tax.storage_2 <- matrix(0,length(classifications_round2),2) %>% data.frame()
for(i in 1:length(classifications_round2))
{
  tax.storage_2[i,1] <- paste(as.data.frame(classifications_round2[i])[1], collapse = "; ")
  print(i)
}
rm(i)
colnames(tax.storage_2) <- c('tax', 'search_string')

#pull out the taxonomy
tax.storage_2 <- tax.storage_2 %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(search_string = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+( [a-z]+)?'))%>% #match  on Genus  species followed by an optional sub  species  
  mutate(search_string = tolower(search_string))

#first, identify which species are not classified as bats
clean.up.2 <- filter(tax.storage_2, !(grepl('Chiroptera', tax.storage_2$tax)))
clean.up.2$search_string

#filter our dataset to only the species where we are missing the taxonomy
batphy.missing <- batphy %>%
  filter(!(search_string %in% tax.storage$search_string)) %>% filter(!(species %in% tax.storage_2$search_string)) %>% mutate(search_string = as.character(search_string))

batphy.missing$species

#........................last issue:............................................................
#........................myonycteris angolensis.................................................
#........................round 3 of taxonomyfying...............................................

classifications_round3 <- classification('myonycteris angolensis', db = 'eol')[1]
#11.22.2019:
# 1: 1
# Input accepted, took eolid '10194347'.
# 
# ✔  Found:  myonycteris angolensis
# ══  Results  ═════════════════
# 
# ● Total: 1 
# ● Found: 1 
# ● Not Found: 0

tax.storage_3 <- matrix(0,length(classifications_round3),2) %>% data.frame()
for(i in 1:length(classifications_round3))
{
  tax.storage_3[i,1] <- paste(as.data.frame(classifications_round3[i])[1], collapse = "; ")
  print(i)
}

colnames(tax.storage_3) <- c('tax', 'search_string')

#pull out the taxonomy
tax.storage_3 <- tax.storage_3 %>%
  mutate(tax = gsub("c\\(", "", tax)) %>%
  mutate(tax = gsub("\\\"", "", tax)) %>%
  mutate(tax = gsub(",", ";", tax)) %>%
  mutate(search_string = stri_extract_last_regex(tax,'[A-Z][a-z]+ [a-z]+( [a-z]+)?'))%>% #match  on Genus  species followed by an optional sub  species  
  mutate(search_string = tolower(search_string))

#first, identify which species are not classified as bats
clean.up.3 <- filter(tax.storage_3, !(grepl('Chiroptera', tax.storage_3$tax)))
clean.up.3$search_string

#filter our dataset to only the species where we are missing the taxonomy
batphy.missing <- batphy %>%
  filter(!(search_string %in% tax.storage$search_string)) %>% 
  filter(!(species %in% tax.storage_2$search_string))  %>%
  filter(!(species %in% tax.storage_3$search_string)) %>% mutate(species = as.character(species))

#if true we can move on!
nrow(batphy.missing) == 0
#............................................................................................................................................
#............................................................................................................................................
#............................................................................................................................................
#............................................................................................................................................
#......................................................combine datasets......................................................................
#............................................................................................................................................
#............................................................................................................................................
#............................................................................................................................................
#............................................................................................................................................


#now combing then batphy data with the taxonomy data
sum(tax.storage$search_string %in% tax.storage_2$search_string)
sum(tax.storage$search_string %in% tax.storage_3$search_string)
#remove rows with na values and remove rows that are in the other tax storage systems. 
#for example, some of what is searched in tax.storage 2 is what didn't work in tax.storage 1, so remove that species from tax storage 1. 
tax.storage <- tax.storage %>% filter(!(is.na(search_string))) %>% 
  filter(!(search_string %in% tax.storage_2$search_string)) %>%  
  filter(!(search_string %in% tax.storage_3$search_string)) %>%
  filter(!(is.na(tax)))
#remove duplicate searches 
tax.storage <- unique(tax.storage)

#remove NA values from tax.storage_2
tax.storage_2 <- tax.storage_2 %>%
  filter(!(is.na(tax)))

nrow(rbind(tax.storage, tax.storage_2, tax.storage_3)) - nrow(batphy)
#so there are 6 extra entries in our 'tax' storage than there are in batphy, lets try and figure out which those are. 

left_join.1 <- left_join(tax.storage,    batphy, by = c("search_string" = "search_string"))
left_join.2 <- left_join(tax.storage_2 , batphy, by = c("search_string" = "species" ))
left_join.3 <- left_join(tax.storage_3 , batphy, by = c("search_string" = "species" ))

bat_phy_tax <- bind_rows(left_join.1, left_join.2, left_join.3)

rm(left_join.1, left_join.2, left_join.3, rank_ref, tax_storage_final)

#remove the samples that aren't bats
bat_phy_tax <- bat_phy_tax %>%
  filter((grepl('Chiroptera', bat_phy_tax$tax)))

save(bat_phy_tax, file = "/Users/dancrowley/phylofactor_paper/data/batphy_revised_with_tax.Rdata")




