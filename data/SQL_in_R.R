library(RSQLite)

myDB <- "portal_mammals.sqlite"
conn <- dbConnect(drv=SQLite(), dbname=myDB)

dbGetQuery(conn, "SELECT count(*) FROM surveys")

dbListTables(conn)
dbListFields(conn, "surveys")


q <- "SELECT DISTINCT year, species_id 
      FROM surveys"
result <- dbGetQuery(conn, q)
head(result)


yearRange <- dbGetQuery(conn, 
                        "SELECT min(year), max(year) 
                        FROM surveys")

years <- seq(yearRange[,1], yearRange[,2], by=2)


q <- paste("SELECT surveys.year, species.taxa, count(*) as count
FROM surveys
JOIN species
ON surveys.species_id=species.species_id
WHERE species.taxa='Rodent' AND surveys.year in (",
paste(years, collapse=','), 
") GROUP BY surveys.year, species.taxa", sep="")

rCount <- dbGetQuery(conn, q)


species <- read.csv("species.csv")
surveys <- read.csv("surveys.csv")
plots  <- read.csv("plots.csv")

newDB  <- "portalR.db"
myConn <- dbConnect(drv=SQLite(), dbname=newDB)
dbListTables(myConn)

dbWriteTable(myConn, "species", species)
dbWriteTable(myConn, "surveys", surveys)
dbWriteTable(myConn, "plots", plots)
dbListTables(myConn)

dbDisconnect(myConn)
