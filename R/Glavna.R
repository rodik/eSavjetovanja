
# glavna
# start Selenium server
# startServer() #>java -jar selenium-server-standalone.jar 
# remDr <- remoteDriver(browser = "chrome", port=4444)
# remDr <- remoteDriver(browser = "firefox", port=4444)
# # open browser
# remDr$open()
# # hederi # imao sam ih pri ruci
# remDr$setTimeout(type = "implicit", milliseconds = 5000)
# remDr$setTimeout(type = "page load", milliseconds = 5000)


source('R/Pomocne funkcije.R', encoding = 'UTF-8')
source('R/ReadAPI.R', encoding = 'UTF-8')


# skini sve headere
rasprave_headers <- ProcitajHeadereRasprava()

# povuci za clanke svih rasprava
clanci_rasprava <- ProcitajSveClankeSvihRasprava(rasprave_headers)

# zatvoreni <- rasprave_headers %>% filter(Stanje != "Otvoren")

# povuci sve komentare svih rasprava -- ovo moze potrajati
komentari_clanaka <- Get_all_comments_for_all_rasprave(clanci_rasprava)


komentari_clanaka %>% saveRDS('data/komentari.RDS')
clanci_rasprava %>% saveRDS('data/clanci.RDS')
rasprave_headers %>% saveRDS('data/headers.RDS')