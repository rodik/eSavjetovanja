library(tidyverse)
library(conflicted)
library(here)
library(magrittr)
library(wrapr)

imena <- read_csv(here('analize', 'jezik', 'imena-prezimena',
                       'imena.csv'))

prezimena <- read_csv(here('analize', 'jezik', 'imena-prezimena',
                       'prezimena.csv'))

udruge <- readRDS(here('analize', 'jezik', 'udruge',
                       'registar_udruga_odd2019.RDS'))

komentari <- readRDS(here('export', 'uzorak_komentara.RDS'))

# normaliziranje korisničkih imena
komentari %<>% mutate_at(., .vars = vars(UserName), .funs = tolower) %>%
    mutate_at(., .vars = vars(UserName), .funs = str_trim) %>%
    mutate_at(., .vars = vars(UserName), .funs = str_replace_all,
    pattern = '[:punct:]', replacement = '') %>%
    mutate_at(., .vars = vars(UserName), .funs = str_squish)

komentari %>% select(UserName)

# normaliziranje imena, prezimena i naziva udruga
imena %<>% pull(., ime) %>%
    str_trim(.) %>% tolower(.)

prezimena %<>% pull(., ime) %>%
    str_trim(.) %>% tolower(.)

udruge %>% pull(., NAZIV) %>% tolower(.) %>% str_trim(.) %>% str_squish(.) %>%
    str_replace_all(., pattern = '[:punct:]', '') -> nazivi_udruga

# funkcija za kategoriziranje korsnika
categorize <- function(user) {
    if(user %in% nazivi_udruga) return('udruga')
    else {
        ime_split <- user %>% str_split(., ' ') %>% pluck(., 1)
        if(all(ime_split %in% imena | ime_split %in% prezimena))
            return('fizička osoba')
        else
            return('nepoznato')
    }
}

komentari$kategorijaKorisnika <- komentari %>% pull(., UserName) %>%
    sapply(., FUN = categorize)
