### Libraries
library(tidyverse)
library(httr)
library(furrr)

### Load Data
## Parallelisation

# By default the data frame is empty
data <- NULL

# Applying tryCatch
tryCatch(
  expr = { # Specifying expression

    ## URLS
    URLs <- c(
      "https://data.stadt-zuerich.ch/dataset/bau_hae_lima_preise_anzahl_hae_art_gebiet_bzo_jahr_od5141/download/BAU514OD5141.csv",
      "https://data.stadt-zuerich.ch/dataset/bau_hae_lima_preise_anzahl_hae_art_gebiet_bzo_jahr_grpd_od5142/download/BAU514OD5142.csv",
      "https://data.stadt-zuerich.ch/dataset/bau_hae_lima_zuordnung_adr_quartier_bzo16_bzo99_od5143/download/BAU514OD5143.csv"
    )

    ## Download
    dataDownload <- function(link) {
      data <- data.table::fread(link,
        encoding = "UTF-8"
      )
    }

    # Parallelisation
    data <- future_map(URLs, dataDownload)
  },
  error = function(e) { # Specifying error message
    message("Error in Data Load")
    return(NULL)
  },
  warning = function(w) { # Specifying warning message
    message("Warning in Data Load")
  }
)

if (!is.null(data)) {

  ## Data
  zones <- data[[1]]
  series <- data[[2]]
  addresses <- data[[3]]

  ### Data Transformation
  
  ## Zones
  zones <- zones %>% 
    mutate(PreisreiheLang = case_when(PreisreiheSort == 41 ~ "Preis pro m\u00B2 Grundstücksfläche",
                                      PreisreiheSort == 42 ~ "Preis pro m\u00B2 Grundstücksfläche, abzgl. Versicherungswert",
                                      PreisreiheSort == 49 ~ "Stockwerkeigentum pro m\u00B2 Wohnungsfläche")) %>% 
    mutate_all(funs(replace(., . == ".", "–"))) %>%
    mutate_all(funs(replace(., . == "", "–"))) 
  
  ## BZO16
  zonesBZO16 <- zones %>%
    filter(BZO == "BZO16") %>%
    rename(
      Total = ALLE,
      Z = ZE,
      K = KE,
      Q = QU,
      W2 = W2,
      W3 = W23,
      W4 = W34,
      W5 = W45,
      W6 = W56
    ) %>%
    mutate_all(funs(replace(., . == ".", "–"))) %>%
    mutate_all(funs(replace(., . == "", "–")))

  ## BZO99
  zonesBZO99 <- zones %>%
    filter(BZO == "BZO99") %>%
    rename(
      Total = ALLE,
      Z = ZE,
      K = KE,
      Q = QU,
      ` ` = W2,
      W2 = W23,
      W3 = W34,
      W4 = W45,
      W5 = W56
    ) %>%
    mutate_all(funs(replace(., . == ".", "–"))) %>%
    mutate_all(funs(replace(., . == "", "–")))

  ## Series
  series <- series %>%
    mutate_all(funs(replace(., . == ".", "–"))) %>%
    mutate_at(
      vars(
        FrQmBodenGanzeLieg,
        FrQmBodenStwE,
        FrQmBodenAlleHA,
        FrQmWohnflStwE
      ),
      funs(replace(., . == "", "–"))
    )

  ## Addresses
  addresses <- addresses %>%
    mutate(Zones = case_when(
      ZoneBZO16Lang == ZoneBZO99Lang ~ paste(ZoneBZO16Lang),
      TRUE ~ paste0(ZoneBZO16Lang, " (bis 2018: ", ZoneBZO99Lang, ")")
    ))
}
