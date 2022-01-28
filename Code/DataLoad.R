###Libraries
library(tidyverse)

###Load Data
##Prices and sale volumes by city neighborhood and zone type
zones <- read.csv(url("https://data.integ.stadt-zuerich.ch/dataset/int_dwh_archiv_bau514od5141/download/BAU514OD5141.csv"), 
                  stringsAsFactors = FALSE, 
                  sep = ",",
                  encoding = "UTF-8")

##Price series and sales volumes by city neighborhood and zone type
series <- read.csv(url("https://data.integ.stadt-zuerich.ch/dataset/int_dwh_archiv_bau514od5142/download/BAU514OD5142.csv"), 
                   stringsAsFactors = FALSE, 
                   sep = ",",
                   encoding = "UTF-8")

##All Addresses
addresses <- read.csv(url("https://data.integ.stadt-zuerich.ch/dataset/int_dwh_archiv_bau_hae_lima_zuordnung_adr_quartier_bzo16_bzo99/download/BAU514OD5143.csv"), 
                      stringsAsFactors = FALSE, 
                      sep = ",",
                      encoding = "UTF-8")

###Data Transformation
##Zones
zones <- zones %>% 
  rename(Typ = X.U.FEFF.Typ)
  


##BZO16
zonesBZO16 <- zones %>% 
  filter(BZO == "BZO16") %>% 
  rename(Total = ALLE,
         Z = ZE,
         K = KE,
         Q = QU,
         W2 = W2,
         W3 = W23,
         W4 = W34,
         W5 = W45,
         W6 = W56)

##BZO99
zonesBZO99 <- zones %>% 
  filter(BZO == "BZO99") %>% 
  rename(Total = ALLE,
         Z = ZE,
         K = KE,
         Q = QU,
         ` ` = W2,
         W2 = W23,
         W3 = W34,
         W4 = W45,
         W5 = W56)

##Addresses
addresses <- addresses %>% 
  rename(Strasse = X.U.FEFF.Strasse) %>% 
  mutate(Zones = case_when(ZoneBZO16 == ZoneBZO99 ~ paste(ZoneBZO16),
                           TRUE ~ paste0(ZoneBZO16, " (bis 2018: ", ZoneBZO99,")")))

##Series
series <- series %>% 
  rename(Typ = X.U.FEFF.Typ)
