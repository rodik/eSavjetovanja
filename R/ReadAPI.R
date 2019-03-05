# curl ?

eSavjetovanja_Get_Comments_from_API <- function(clanak_id, cookie, token, elements_per_request = 100, page_num = 1) {
    response <- POST(
        url = 'https://esavjetovanja.gov.hr/ECon/MainScreen/GetSliceComments',
        body = list(
            eConsultationSliceId = as.character(clanak_id),
            paging = list(
                CurrentPageIndex = as.character(page_num),
                NumElementsPerPage = as.character(elements_per_request),
                OrderByColumnName = 'DateModified',
                SortingOrder = '1'
            )
        ),
        encode = "json",
        add_headers(
            `Origin` = 'https://esavjetovanja.gov.hr',
            `Accept-Encoding` = 'gzip, deflate, br',
            `Accept-Language` = 'en-US,en;q=0.9,hr;q=0.8,bs;q=0.7',
            `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/72.0.3626.119 Safari/537.36', 
            `Content-Type` = 'application/json;charset=UTF-8',
            `Accept` = 'application/json, text/plain, */*' ,
            `Referer` = 'https://esavjetovanja.gov.hr/ECon/MainScreen?entityId=5638' ,
            `Cookie` = cookie,
            `Connection` =  'keep-alive' ,
            `__RequestVerificationToken` = token,
            `DNT` = '1'
        )
    )
    
    parsed.content <- content(response, as = "text", encoding = 'UTF-8')
    list.content <- fromJSON(parsed.content)
    table.content <- list.content$Comments 
    
    if (is.data.frame(table.content)) {
        table.content <- table.content %>% jsonlite::flatten()
        
        # ako je komentar bez odgovora, onda manualno dodaj kolone koje bi postojale da odgovora ima
        if (!'Response.ResponseId' %in% colnames(table.content)) {
            table.content$Response.ResponseId <- as.integer(NA)
            table.content$Response.Text <- as.character(NA)
            table.content$Response.ResponseTypeId <- as.integer(NA)
            table.content$Response.IsStale <- as.logical(NA)
        }
        # makni nestanu kolonu ako postoji
        table.content$Response <- NULL
        
        if ('CanDelete' %in% colnames(table.content)) {
            table.content$CanDelete <- NULL
        }
    }
    
    # return
    list(
        comments = table.content %>% as_tibble(),
        total_comments = as.integer(list.content$NumOfComments)
    )
}


Get_all_comments_for_clanak <- function(clanak_id){
    
    cookie <- '_ga=GA1.2.968453555.1547744573; ASP.NET_SessionId=ulbegghdydsfhzxhnyuylrf5; __RequestVerificationToken=EdhVE7i1VD3BP5IAxUHYhambJFmc6dByPz'
    token <- 'aozGSpIu5ekxmI1db7t0CcnE1fQvFVCEocd5-BTQllikEp8qtgI-WRpuJt7GA_7o-6IPsMqVaanVSQ_etqbkVQH4Fu8D8S8wRDCDL6DBPbo1'
    
    all_comments_tbl <- tibble()
    
    i <- 1
    
    while (1 == 1) {
        # napravi upit
        comments_batch <- eSavjetovanja_Get_Comments_from_API(
            clanak_id, 
            cookie,
            token,
            page_num = i
        )
        # odmori
        # ako ima rezultata dodaj u globalnu kolekciju
        if (nrow(comments_batch$comments) > 0) {
            all_comments_tbl <- rbind(all_comments_tbl, comments_batch$comments)
            Sys.sleep(1)
        }
        # ako je ukupan broj komentara veci od prikupljenog, odi u iducu iteraciju
        if (nrow(all_comments_tbl) < comments_batch$total_comments) {
            i <- i + 1
        }
        else # u suprotnom zaustavi petlju
            break;
    }
    print(paste(
        'Clanak', clanak_id,
        '>>', 'komentara:', nrow(all_comments_tbl)
    ))
    # return
    all_comments_tbl
}

# primjer clanka s puno komentara (276)
# clanak_id <- 155091

# primjer clanka bez komentara 
# clanak_id <- 155104

# primjer normalnog komentara (5)
# clanak_id <- 155101

# primjer komentara s odgovorima (??)
# clanak_id <- 30971

Get_all_comments_for_all_rasprave <- function(clanak_ids) {
    
    sink('Get_all_comments_for_all_rasprave_errors.txt', append = TRUE)
    cat(paste('POCINJE ITERACIJA ::::', Sys.time(),'\n\n'))
    sink()
    
    svi_komentari_tbl <- tibble()
    
    for (i in 1:nrow(clanak_ids)) {
        # uzmi clanak
        c <- clanak_ids[i,]
        
        # posalji na obradu
        komentari_clanka <- tryCatch({
            Get_all_comments_for_clanak(c$clanak_id)
        }, warning = function(warning_condition) {
            # warning-handler-code
        }, error = function(error_condition) {
            print(error_condition)
            sink('Get_all_comments_for_all_rasprave_errors.txt', append = TRUE)
            cat(paste(c$clanak_id, 'vrijeme', Sys.time(),'\n'))
            sink()
            Sys.sleep(3)
            NULL
        }, finally={
            # message('All done, quitting.')
        })
        
        # provjeri sto je vraceno
        if (!is.null(komentari_clanka) && is.data.frame(komentari_clanka) && nrow(komentari_clanka) > 0) {
            komentari_clanka$clanak_id <- c$clanak_id
            komentari_clanka$savjetovanje_id <- c$savjetovanje_id
            svi_komentari_tbl <- rbind(svi_komentari_tbl, komentari_clanka)
        }
        if (i %% 10 == 0) {
            print(paste('i =', i))
        }
    }
    # return
    svi_komentari_tbl
}




# next.page <- list.content$`next` # TODO mozda treba i tu nakeljiti token!!
# total.count <- list.content$`count`
# 
# while (!is.null(next.page)) {
#     raw.result <- GET(next.page, config = login)
#     
#     parsed.content <- content(raw.result, as = "text", encoding = 'UTF-8')
#     list.content <- fromJSON(parsed.content)
#     
#     table.content <- rbind(table.content, list.content$results)
#     next.page <- list.content$`next`
#     
#     print(paste("Fetched", nrow(table.content), "of", total.count, "records"))
# }
# table.content


