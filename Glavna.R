
# glavna
# start Selenium server
# startServer() #>java -jar selenium-server-standalone.jar // C:\Users\Administrator\Desktop\RSelenium
# remDr <- remoteDriver(browser = "chrome", port=4444)
remDr <- remoteDriver(browser = "firefox", port=4444)
# open browser
remDr$open()
# hederi # imao sam ih pri ruci
remDr$setTimeout(type = "implicit", milliseconds = 5000)
remDr$setTimeout(type = "page load", milliseconds = 5000)



zatvoreni <- hederi_delta %>% filter(Stanje != "Otvoren")


# sveee <- data.frame()
obrada <- zatvoreni %>% mutate(Scraped = FALSE)

# obrada <- zatvoreni %>% filter(Scraped == FALSE)

obrada <- obradeni2$obrada %>% filter(Scraped == FALSE & !is.na(ID))

obradeni3 <- Orkestracija(obrada)


View(obradeni$obrada)

komentari_MAIN <- rbind(obradeni$komentari, obradeni2$komentari)


# https://esavjetovanja.gov.hr/ECon/MainScreen?entityId=6492