library(tidyverse)

# procitaj output iz skripte Glavna.R
komentari <- readRDS('data/svi_komentari.RDS')
clanci <- readRDS('data/clanci_20190303.RDS')
rasprave <- readRDS('data/headers_20190302.RDS')

# obradi dodatno komentare
komentari <- komentari %>% mutate(kategorijaKorisnika = 'unknown') # koristiti ovo ako nije napravljena stvarna kategorizacija
komentari <- komentari %>% mutate(
    kategorijaKorisnika = ifelse(!kategorijaKorisnika %in% c('udruga','nepoznato'), 'fizicka osoba', kategorijaKorisnika )
)


# na razini rasprave
rasprave_final <- rasprave %>% 
    select(Institucija, Naslov, DatumOtvaranja, DatumZatvaranja, OcekivanaObjavaIzvjesca, Stanje, ID) %>%
    inner_join(komentari, by = c("ID"="savjetovanje_id")) %>%
    group_by(Institucija, Naslov, DatumOtvaranja, DatumZatvaranja, OcekivanaObjavaIzvjesca, Stanje, ID) %>%
    summarize(
        broj_komentara = n(),
        broj_komentatora = n_distinct(UserName),
        likes_count = sum(NumOfLikes),
        dislikes_count = sum(NumOfDislikes),
        response_count = n_distinct(Response.ResponseId) ## ovo je krivo! null zbraja kao distinct vrijednost
    ) %>% rename(
        rasprava_id = ID
    )


# na razini komentatora
komentatori_final <- rasprave %>% 
    select(Institucija, Naslov, DatumOtvaranja, DatumZatvaranja, OcekivanaObjavaIzvjesca, Stanje, ID) %>%
    inner_join(komentari, by = c("ID"="savjetovanje_id")) %>%
    mutate(kategorijaKorisnika = enc2utf8(kategorijaKorisnika)) %>%
    group_by(UserName, kategorijaKorisnika) %>%
    summarize(
        broj_komentara = n(),
        broj_komentiranih_rasprava = n_distinct(ID),
        likes_count = sum(NumOfLikes),
        dislikes_count = sum(NumOfDislikes),
        response_count = n_distinct(Response.ResponseId) ## ovo je krivo! null zbraja kao distinct vrijednost
    ) %>% ungroup() %>% mutate(
        komentator_id = row_number()
    )

# na razini komentara
komentari_final <- komentari %>% 
    inner_join(komentatori_final %>% select(UserName, komentator_id), by=c('UserName'='UserName')) %>%
    mutate(broj_znakova_komentara = nchar(Text)) %>%
    mutate(omjer_reakcija = (NumOfLikes)/(NumOfLikes+NumOfDislikes))



saveRDS(rasprave_final, 'export/rasprave.RDS')
saveRDS(komentari_final, 'export/komentari.RDS')
saveRDS(komentatori_final, 'export/komentatori.RDS')

