## script for uthenting av siterende publikasjoner for en liste over publikasjoner ##

# dette scriptet leser inn en liste over publikasjoner, 
# slår de opp i siteringsdatabasen Dimensions, og:
#
# 1) henter ut referanselistene til disse publikasjonene
#
# 2) søker opp publikasjoner som siterer de samme publikasjonene
#
# 3) filtrerer på de publikasjonene som deler minst to av referansene med kildedokumentene


## oppsett av arbeidsområde
library(httr)
library(jsonlite)
library(tidyverse)

setwd("M:/Mellomlager/Div/R/sintef litteratur")


## login-prosedyre til Dimensions ##

# brukernavn og passord
login <- list(
  username = Sys.getenv("DIMENSIONS_USERNAME"),   ## lokalt lagret verdi
  password = Sys.getenv("DIMENSIONS_PASSWORD")    ## lokalt lagret verdi
)


# sender brukernavn og passord til serveren for å få autentiseringsvariabel
parsed <- content(POST("https://app.dimensions.ai/api/auth.json",
                       body = login,
                       encode = "json",
                       #har med verbose() for å sjekke at responskoden er 200, kan sløyfes
                       verbose()),
                  "parsed")



## oppslag i databasen

#  lager lister over doier som brukes til oppslag
reflist <- readxl::read_excel("doier.xlsx")


# formatterer doiene så de kan leses av serveren
reflist_quoted <- paste0('"', reflist$doi, '"') 
refers <- paste(reflist_quoted, collapse = ", ")

# formatterer forespørselen
query <- paste0('search publications 
where
doi in [', refers, 
                '] return publications[basics + reference_ids]
limit 1000 skip 0')


# sender forespørsel til serveren
uttrekk <- POST(
  "https://app.dimensions.ai/api/dsl.json",
  add_headers(Authorization = paste0("JWT ", parsed$token)),
  body = query,
  content_type_json(),
  encode = "raw",
  verbose())


# trekker ut innholdet av responsen
innhold <- fromJSON(content(uttrekk, "text"), flatten = TRUE)

# isolerer publikasjonene
pubframe <- innhold$publications

# fjerner NULL-elementer fra en variabel
pubframe$reference_ids[sapply(pubframe$reference_ids, is.null)] <- NA

# henter ut referanselistene for hver publikasjon
pubframe <- pubframe %>%
  filter(!is.na(reference_ids)) %>%
  select(id, title, journal.title, reference_ids) %>%
  unnest() %>%
  distinct(reference_ids, .keep_all = TRUE) %>%
  as_tibble()


# lager ny spørring som finner publikasjoner som siterer vårt nye referansesett
litteratur_quoted <- as.list(paste0('"', pubframe$reference_ids, '"'))

referanser <- as.list(paste0('search publications where (reference_ids in [', 
                             litteratur_quoted,
                             ']) return publications[id + doi + title + type + year + journal + FOR] limit 1000 skip 0'))

# funksjon for å sende ny forespørsel til serveren, siden denne må itereres over
uttrekk_funksjon <- function(body) {
  response <- POST(
    "https://app.dimensions.ai/api/dsl.json",
    add_headers(Authorization = paste0("JWT ", parsed$token)),
    body = body,
    content_type_json(),
    encode = "raw")
  
  if (status_code(response) != 200) {
    return(NA)
    Sys.sleep(1)
  } else {
    return(response)
    Sys.sleep(1)
  }

}

# kjøring av funksjonen
siterende_publikasjoner <- lapply(referanser, uttrekk_funksjon)

# funksjon for å hente ut det nye innholdet
uthenting <- function(respons) {
  if (is.na(respons)) {
    return(NULL)
  } else {
  innhold <- fromJSON(content(respons, "text", encoding = "UTF-8"), flatten = TRUE)
  return(innhold)
  }
}

# gjør om til tabulært format og filtrerer ut ikke siterte publikasjoner
siterende <- lapply(siterende_publikasjoner, uthenting)
names(siterende) <- pubframe$reference_ids
siterende <- compact(siterende)

oversikt <- map(siterende, "publications") %>% 
  bind_rows() %>%
  filter(year >= 2014)


# beholder bare publikasjoner med mer enn én felles referanse med kildegrunnlaget  
oversikt <- oversikt[oversikt$doi %in% oversikt$doi[duplicated(oversikt$doi)],] %>%
  distinct(doi, .keep_all = TRUE)

#fjerner overflødige kolonner
oversikt <- oversikt %>%
  filter(type != "preprint") %>%
  mutate(fagfelt = map_chr(FOR, pluck, "name", 1, .default = NA_character_)) %>%
  select(-FOR, -journal.id, -id)

#skriver resultatet til en excel-fil
writexl::write_xlsx(oversikt, "cscw-litteratur.xlsx")
