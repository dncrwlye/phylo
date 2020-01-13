
#11-09-2019
#Dan Crowley

load("~/Dropbox/bat virus meta-analysis/seroprevalence_revised.Rdata")

#remove experimental studies
data <- seroprevalence %>%
  #filter(outcome == 'Prevalence_Seroprevalence') %>%
  filter(study_type == "Observational") 

#select only certain columsn

data <- data %>%
dplyr::select(title, last_name_of_first_author, virus, virus_specific, study_type, study_design, methodology, species, sex, age_class, sampling_location, sampling_location_two, sampling_location_three, sampling_location_four, sample_size, seroprevalence_percentage, number_positive, single_sampling_point, sampling_date_single_time_point, start_of_sampling, end_of_sampling, purpose_of_study, secondary_purpose_of_study)
  
#clean virus names and remove Tioman virus
unique(data$virus)
data <- data %>% 
  mutate(virus = ifelse(grepl('Ebola|Marburg|Sudan|Lloviu|Bombali Virus|ebola', virus), 'Filovirus',
                      ifelse(grepl('Henipa|Hendra|Nipah|cedar|Cedar|hendra|nipah', virus), 'Henipavirus',
                             ifelse(grepl('Tioman', virus), 'Tioman', virus))))  %>%
  filter(virus != 'Tioman')

unique(data$virus)


#clean methodology 

unique(data$methodology)

data <- data %>%
mutate(methodology_general = ifelse(grepl('PCR', methodology), 'PCR based method',
                                    ifelse(grepl('ELISA|Luminex|serology', methodology), 'non nAb based method',
                                           ifelse(grepl('VNT|SNT|Neutralizing', methodology), "nAb based method", methodology)))) 

unique(data$methodology_general)

#clean species names

data <- data %>%
  mutate(species = tolower(species)) %>%
  mutate(species = trimws(species))

## fix species binomials
data$species=plyr::revalue(data$species,c("rhinolophus refulgens"="rhinolophus lepidus",
                                    "hipposideros cf caffer"="hipposideros caffer",
                                    "hipposideros cf caffer/ruber"="hipposideros caffer",
                                    "hipposideros cf ruber"="hipposideros ruber",
                                    "natalus lanatus"="natalus mexicanus",
                                    "myonycteris leptodon"="myonycteris torquata",
                                    "tadarida plicata"="chaerephon plicatus",
                                    "pipistrellus cf nanus/nanulus"="pipistrellus nanus",
                                    "neoromicia nana"="neoromicia nanus"))
## other set of binomial fixes
data$species=plyr::revalue(data$species,c("hipposideros pygmeus"="hipposideros pygmaeus",
                                                "peroptery kappleri"="peropteryx kappleri",
                                                "myotis capaccini"="myotis capaccinii",
                                                "rhinolphus mehelyi"="rhinolophus mehelyi",
                                                "miniopterus magnate"="miniopterus magnater",
                                                "pipistrellus nanus"="neoromicia nanus",
                                                "miniopterus schreibersii"="miniopterus fuliginosus",
                                                "artibeus planirostris"="artibeus jamaicensis",
                                                "enchisthenes hartii"="artibeus hartii",
                                                "hypsygnathus monstrosus"="hypsignathus monstrosus",
                                                "miniopterus schrebersii"="miniopterus fuliginosus",
                                                "lissonycteris angolensis"="myonycteris angolensis"))
## fix multiple species (DC: might not want to do this)
data$species_final=plyr::revalue(data$species,c("pteropus alecto, pteropus poliocephalus"="pteropus alecto",
                                          "pteropus alecto, pteropus poliocephalus, pteropus scapulatus"="pteropus alecto",
                                          "pteropus alecto, pteropus scapulatus"="pteropus alecto",
                                          "pteropus conspicillatus, pteropus scapulatus"="pteropus conspicillatus")) 


hist(unique(data$seroprevalence_percentage))



seroprevalence <- seroprevalence %>%
  mutate(number_positive = as.numeric(number_positive)) %>%
  mutate(seroprevalence_percentage = as.numeric(seroprevalence_percentage)) %>%
  mutate(sample_size = as.numeric(sample_size)) %>%
  mutate(successes = (seroprevalence_percentage/100)*sample_size) %>%
  filter(!is.na(sample_size) & !is.na(successes))



