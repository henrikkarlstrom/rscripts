## Script for å generere egenarkiveringslister for NTNUs fakulteter ##


# Nødvendige pakker
library(tidyverse)
library(writexl)


## Prosedyre ##

setwd("M:/Mellomlager/Div/R/egenarkivering") ## erstatt med foretrukket arbeidsmappe

# 1. last ned data fra DUCT fra datakilden "OA_data_sted_person" med følgende kolonner:

# AVDNR (STED) 
# STEDNAVN/ENHET 
# Nummer på posten 
# DOI 
# TITTEL på publikasjonen
# FORFATTERREKKEFØLGE
# ETTERNAVN 
# FORNAVN


# og følgende filter:

# Årstall: 2018
# AVDNR (STED): 31, 60, 61, 62, 63, 64, 65, 66, 67
# Open access status: ikke OA (ikke deponert)


# Disse lastes ned med funksjonen "Download > Data > Download all rows as a text file", 
# og lagres med navnet "OAliste[dagens dato].csv" i arbeidsmappa


# 2. Les inn masterfila, husk å endre filnavn til å passe aktuelle datoer:

OAliste <- read_csv2("oaliste010419.csv") ## erstatt med aktuell listenavn
gamalliste <- read_csv2("oaliste110319.csv") ## erstatt med forrige listenavn


# 3. Lag oversikt over arkiverte, ikke arkiverte og nye poster siden sist

gamalliste <- gamalliste %>%
  rename("ID" = "Nummer på posten") %>% 
  select(ID) %>% 
  distinct(ID)

nyliste <- OAliste %>%
  rename("ID" = "Nummer på posten") %>% 
  select(ID) %>% 
  distinct(ID)

arkivert <- setdiff(gamalliste, nyliste)
nytt <- setdiff(nyliste, gamalliste)
uarkivert <- intersect(gamalliste, nyliste)

oversikt <- data.frame("Arkivert" = nrow(arkivert), 
                       "Uarkivert" = nrow(uarkivert), 
                       "Nye" = nrow(nytt))


# 4. Omform lista for enkel filtrering

OAliste <- OAliste %>% 
  rename(
    "Fakultet" = `AVDNR (STED)`, 
    "Institutt" = `STEDNAVN / ENHET`
    ) %>% 
  mutate(Fakultet = recode(Fakultet,
                           "31" = "VM",
                           "60" = "ØK",
                           "61" = "AD",
                           "62" = "HF",
                           "63" = "IE",
                           "64" = "IV",
                           "65" = "MH",
                           "66" = "NV",
                           "67" = "SU")) %>%
  select(
    Fakultet, 
    Institutt, 
    `Nummer på posten`, 
    DOI, 
    `TITTEL på publikasjonen`,
    FORFATTERREKKEFØLGE,
    ETTERNAVN, 
    FORNAVN)


# 5. Generer liste for hvert fakultet og lagre listene

# lager ny mappe for dagens filer
dir.create(paste0("Filer ",Sys.Date()))

# deler opp ramma etter fakultet i en liste
faks <- split(OAliste, OAliste$Fakultet)

# oppretter en liste over navnene på hvert fakultet
paths <- file.path(paste0("Filer ", Sys.Date()), 
                   paste0(names(faks), " ", Sys.Date(), ".xlsx"))

# skriver hvert fakultet i lista til en Excel-fil med navn fra navnelista
walk2(faks, paths, write_xlsx)


# lager oversiktsliste til publiseringsgruppa ved UB
sammendrag <- OAliste %>%
  distinct(`Nummer på posten`, .keep_all = TRUE) %>%
  group_by(Fakultet, Institutt) %>%
  summarise(Antall = n()) %>%
  write_xlsx(paste0("Uarkiverte publikasjoner per institutt ", Sys.Date(),".xlsx"))

