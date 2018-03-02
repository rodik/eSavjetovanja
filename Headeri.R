
hederi <- ProcitajHeadereRasprava()


con<-file('headeri.csv',encoding="WINDOWS-1252")
# KORISTITI WRITE TABLE
write.table(hederi, file=con, na="NA", sep = ";", row.names = FALSE)



