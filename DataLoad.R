### Required packages
packages <- c("tidyverse",
              "httr",
              "parallel",
	      "data.table")

### Load packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

### Load Data
## URLS
URLs <- c("https://data.stadt-zuerich.ch/dataset/bau_hae_lima_preise_anzahl_hae_art_gebiet_bzo_jahr/download/BAU514OD5141.csv",
          "https://data.stadt-zuerich.ch/dataset/bau_hae_lima_preise_anzahl_hae_art_gebiet_bzo_jahr_grpd/download/BAU514OD5142.csv",
          "https://data.stadt-zuerich.ch/dataset/bau_hae_lima_zuordnung_adr_quartier_bzo16_bzo99/download/BAU514OD5143.csv")

## Download function
dataDownload <- function(link) {
  data <- data.table::fread(link,
                            encoding = "UTF-8")
}

## Download
cl <- makeCluster(detectCores())
clusterExport(cl, "URLs")
data <- parLapply(cl, URLs, dataDownload)
stopCluster(cl)


## From list to data frame
zones <- data[[1]]
series <- data[[2]]
addresses <- data[[3]]

### Data Transformation

## BZO16
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
         W6 = W56) %>% 
  mutate_all(funs(replace(., .==".", "-"))) %>% 
  mutate_all(funs(replace(., .=="", "-")))

##B ZO99
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
         W5 = W56) %>% 
  mutate_all(funs(replace(., .==".", "-"))) %>% 
  mutate_all(funs(replace(., .=="", "-")))

## Series
series <- series %>% 
  mutate_all(funs(replace(., .==".", "-"))) %>% 
  mutate_at(vars(FrQmBodenGanzeLieg,
                         FrQmBodenStwE,
                         FrQmBodenAlleHA,
                         FrQmWohnflStwE),
            funs(replace(., .=="", "-")))

##Addresses
addresses <- addresses %>% 
  mutate(Zones = case_when(ZoneBZO16Lang == ZoneBZO99Lang ~ paste(ZoneBZO16Lang),
                           TRUE ~ paste0(ZoneBZO16Lang, " (bis 2018: ", ZoneBZO99Lang,")")))
