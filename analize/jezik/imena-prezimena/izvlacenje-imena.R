library(tidyverse)
library(conflicted)
library(here)
library(magrittr)
library(wrapr)

baza_imena <- read_csv(here('analize', 'jezik', 'imena-prezimena',
                            'names-database.csv'), col_names = F)
colnames(baza_imena) <- qc(ime, lema, postag)

imena <- baza_imena %>%
    dplyr::filter(., str_detect(.$postag, 'Np[mfn]sn'))

prezimena <- baza_imena %>%
    dplyr::filter(., str_detect(.$postag, 'Np-sn'))

write_csv(imena, here('analize', 'jezik', 'imena-prezimena', 'imena.csv'))
write_csv(prezimena, here('analize', 'jezik', 'imena-prezimena',
                          'prezimena.csv'))
