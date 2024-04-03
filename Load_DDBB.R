
library(DBI)
library(readxl)

dbicon <-  DBI::dbConnect(RPostgres::Postgres(),
                          db = "Eponymous", 
                          host = "192.168.1.69", 
                          port = "5432", 
                          user = "postgres",
                          password = "1234$")
dbListTables(dbicon)

file <- "/Users/javiermartinez/Documents/CIBIO/Proyectos/Patricia_Eponymous/Eponymous/Amphibia.xlsx"
df = read_excel(file)
dbWriteTable(dbicon, "tblAmphibia", df, row.names=FALSE, overwrite=T) 

file <- "/Users/javiermartinez/Documents/CIBIO/Proyectos/Patricia_Eponymous/Eponymous/Aves.xlsx"
df = read_excel(file)
dbWriteTable(dbicon, "tblAves", df, row.names=FALSE, overwrite=T) 

file <- "/Users/javiermartinez/Documents/CIBIO/Proyectos/Patricia_Eponymous/Eponymous/Mammalia.xlsx"
df = read_excel(file)
dbWriteTable(dbicon, "tblMammalia", df, row.names=FALSE, overwrite=T) 

file <- "/Users/javiermartinez/Documents/CIBIO/Proyectos/Patricia_Eponymous/Eponymous/Reptilia.xlsx"
df = read_excel(file)
dbWriteTable(dbicon, "tblReptilia", df, row.names=FALSE, overwrite=T) 


sql <- 'SELECT * FROM "tblAmphibia"'
df_amphibia <- dbGetQuery(dbicon, sql)
sql <- 'SELECT * FROM "tblReptilia"'
df_reptilia <- dbGetQuery(dbicon, sql)
sql <- 'SELECT * FROM "tblAves"'
df_aves <- dbGetQuery(dbicon, sql)
sql <- 'SELECT * FROM "tblMammalia"'
df_mammalia <- dbGetQuery(dbicon, sql)

total <- rbind(df_amphibia, df_reptilia,df_aves,df_mammalia)


DBI::dbDisconnect(dbicon)
