library(rvest)
library(plyr)
library(dplyr)
library(RSelenium)

# pomocne funkcije

# ovo iterira po headerima i skrejpa jednu po jednu raspravu
Orkestracija <- function(obrada) {
    tryCatch({
        citava_rasprava <- data.frame()
        
        for(i in 1:nrow(obrada)) { # 
            
            h <- obrada[i,]
            
            base_url <- "https://esavjetovanja.gov.hr"
            
            url <- paste0(base_url, as.character(h$SavjetovanjeURL))
            
            
            
            citava_rasprava <- rbind(citava_rasprava, ProcitajRaspravuPoClancima(remDr, url, h$ID))
            
            obrada[i,"Scraped"] <- TRUE
        }
    }, error = function(e) {
        print(e)
    })
    list(
        komentari = citava_rasprava,
        obrada = obrada
    )
}

ProcitajTablicuHeadera <- function(stranica){
    # dohvati tablicu
    rows <- stranica %>%
        html_node("table.table-hover.table-striped-custom") %>% 
        html_table()
    
    # makni zadnju kolonu koja nema ime
    rows <- rows[,-c(10)]
    
    names(rows) <- c("Institucija", "Naslov", "IzvjescaURL", "DatumOtvaranja", "DatumZatvaranja", "OcekivanaObjavaIzvjesca", "Stanje", "DokumentiURL", "SavjetovanjeURL")
    
    # izvuci URL-ove
    rows$IzvjescaURL <- stranica %>% html_nodes("td:nth-child(3)") %>% html_children() %>% html_node("a") %>% html_attr("href")
    rows$DokumentiURL <- stranica %>% html_nodes("td:nth-child(8)") %>% html_node("a") %>% html_attr("href")
    rows$SavjetovanjeURL <- stranica %>% html_nodes("td:nth-child(9)") %>% html_node("a") %>% html_attr("href")
    # makni navodnike iz Naslova
    rows$Naslov <- gsub('"', "'", rows$Naslov)
    
    # kreiraj ID kolonu
    rows$ID <- sub(".*=", "", rows$SavjetovanjeURL)
    
    # pretvori sve kolone u string
    rows <- rows %>%
        mutate_all(as.character)
    
    # print max RowID
    print(max(rows$ID))
    
    # vrati tablicu kao data.frame
    rows
}


ProcitajHeadereRasprava <- function() {
    
    # priprema
    base_url <- "https://esavjetovanja.gov.hr"
    
    next_page_url <- "/ECon/Dashboard?page=1" # staviti drugi page po potrebi
    
    sve_rasprave <- data.frame()
    # iteriraj
    while(1==1){
        # slozi puni url za iducu stranicu
        url <- paste0(base_url, next_page_url)
        
        # pokupi html
        stranica <- read_html(url, encoding = "UTF-8")
        
        # procitaj podatke sa stranice
        sve_rasprave <- rbind(sve_rasprave, ProcitajTablicuHeadera(stranica))
        
        # uvjet zaustavljanja pri cupanju delte
        if (as.integer(min(sve_rasprave$ID)) <= 6758) {
            
            sve_rasprave <- sve_rasprave %>% filter(as.integer(ID) > 6758)
            
            break;
        }
        
        # odmori pet sekundi
        Sys.sleep(5)
        
        # dojavi di si
        print(paste0("Procitao ", next_page_url))
        
        # procitaj url iduce stranice
        next_page_url <- stranica %>% 
            html_node(xpath="//div[contains(@class, 'btnp-right')]/parent::*") %>%
            html_attr("href")
        
        if(next_page_url == "#")
            break # dosli smo do zadnje stranice
        
    }
    sve_rasprave
}

ProcitajRaspravuPoClancima <- function(remDr, savjetovanje_url, savjetovanje_id) {
    # navigate page
    remDr$navigate(savjetovanje_url)
    # pricekaj malo da se loada
    Sys.sleep(3)
    # ulovi sve buttone koji vode na komentirane clanke
    badges <- remDr$findElements(using = "css", value = ".title")
    
    svi_komentari <- data.frame()
    
    # citaj jedan po jedan clanak
    for(i in 1:length(badges)){
        
        b <- badges[[i]]
        
        print(paste0('i: ', i, ' badge text: ', b$getElementText()))

        # if(!is.na(as.numeric(unlist(b$getElementText())))){
        # klikni na badge
        b$clickElement()
        # pricekaj malo da se loada
        Sys.sleep(1)
        # procitaj ID clanka
        clanak_id <- unlist(remDr$findElement(using = "css", value = "#content-main .selected")$getElementAttribute(attrName = "id"))
        
        print(paste0('Parsam clanak: ', clanak_id))
            
        # novi_komentari <- ProcitajSveKomentareClanka(remDr, clanak_id, savjetovanje_id)
        novi_komentari <- tryCatch({
                ProcitajSveKomentareClanka(remDr, clanak_id, savjetovanje_id)
            }, warning = function(warning_condition) {
                # warning-handler-code
            }, error = function(error_condition) {
                sink('errors.txt', append = TRUE)
                cat(paste('Savjetovanje:', savjetovanje_id))
                sink()
            }, finally={
                # cleanup-code
            })
        
        if (!is.null(novi_komentari)) {
            svi_komentari <- rbind(svi_komentari, novi_komentari)
        }
        # }
    }
    if( nrow(svi_komentari) > 0 ) {
        svi_komentari$savjetovanje_id <- savjetovanje_id
        print(paste0("SAVJETOVANE: ", savjetovanje_id, " total comments: ", nrow(svi_komentari)))
        svi_komentari
    }
    else
        data.frame() # prazan frame
}

ProcitajSveKomentareClanka <- function(remDr, clanak_id, savjetovanje_id) {
    # procitaj koliko je stranica
    all_pagination <- remDr$findElement('class', 'red-pagination')
    pagination_divs <- all_pagination$findChildElements(using = 'tag name', value = "div")
    
    pages <- pagination_divs[sapply(pagination_divs, IsPagination)]
    # preventivno odi na prvu
    pages[[1]]$clickElement()
    # pricekaj malo da se loada
    Sys.sleep(2)
    # napravi cisti data.frame
    svi_komentari <- data.frame()
    
    for(i in 1:length(pages)) { 
        # pronadi sve potencijalne komentare
        potencijalni_komentari <- remDr$findElements(using = "class", value = "comment-content")
        # pronadi stvarne komentare medu njima
        komentari_stranice <- potencijalni_komentari %>% ldply(ProcitajKomentar)
        # ako si procitao prazan data frame izadi iz funkcije i vrati null
        if(nrow(komentari_stranice) == 0)
            return(NULL)
        # dodaj kolonu clanak_id
        komentari_stranice$clanak_id <- clanak_id
        
        if(i < length(pages)) { # ako nisi na zadnjoj stranici
            # klikni na iducu stranicu
            pages[[i + 1]]$clickElement()
            # pricekaj sekundu
            Sys.sleep(1)
        }
        # dodaj procitane komentare
        svi_komentari <- rbind(svi_komentari, komentari_stranice)
    }
    print(paste0("Clanak: ", clanak_id, " Savjetovanje ", savjetovanje_id , " comments: ", nrow(svi_komentari)))
    svi_komentari
}


ProcitajKomentar <- function(el){
    tryCatch({
        if(unlist(el$getElementAttribute(attrName = "class")) == 'comment-content') {
            
            data.frame(
                ime = unlist(el$findChildElement(using = 'class', value = 'comment-info-username')$getElementText()),
                datum = unlist(el$findChildElement(using = 'class', value = 'comment-info-datetime')$getElementText()),
                like = unlist(el$findChildElement(using = 'class', value = 'comment-like')$getElementText()),
                unlike = unlist(el$findChildElement(using = 'class', value = 'comment-unlike')$getElementText()),
                komentar = unlist(el$findChildElement(using = 'class', value = 'comment-text')$getElementText()),
                odgovor = unlist(el$findChildElement(using = 'class', value = 'response-text')$getElementText()),
                stringsAsFactors = FALSE
            )
        }
    }, warning = function(w) {
        #warning-handler-code
    }, error = function(e) {
        print(c)
        error(e)
    })
}


IsPagination <- function(el){
    grepl('btnp btnp-num ng-scope ng-binding', unlist(el$getElementAttribute(attrName = "class"))) 
}
