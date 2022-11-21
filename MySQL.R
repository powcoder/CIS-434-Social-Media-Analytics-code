rm(list=ls())
library(DBI)
library(RMySQL)

driver <- dbDriver("MySQL")
myhost <- "localhost"
mydb <- "studb"
myacct <- "cis434"
mypwd <- "LLhtFPbdwiJans8F@S207" 


###########################################
#############   Single File   #############
###########################################

# Tables: obgyn, dentist, pediatrician
mytable <- "dentist"

conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
mydata <- dbGetQuery(conn, paste("SELECT id, text FROM ", mytable))
dbDisconnect(conn)

myfile = paste(mytable, "txt", sep='.')
fileConn<-file( myfile, open="at" )
for(i in 1:nrow(mydata))
{
  write(mydata$text[i], fileConn, append=TRUE) # write all reviews into one file
}
close(fileConn)
file.rename(from=myfile, to=file.path("~/data", myfile))


# Tables: ratemds
mytable <- 'ratemds'
cmd <- paste('mkdir ~/data/', mytable, sep='')
system(cmd)
conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
setwd(file.path("~/data/", mytable))
fileConn0 <- file("../specialtycode.txt", open="at")
for( specialty in 1:49 )
{
  mydata <- dbGetQuery(conn, paste("SELECT id, specialty, text FROM ", mytable, "WHERE specialtycode=", specialty))
  fileConn<-file( paste(toString(specialty), '.txt', sep=''), open="at" )
  for(i in 1:nrow(mydata))
  {
    writeLines(mydata$text[i],fileConn)
  }
  close(fileConn)
  writeLines( paste(specialty, mydata$specialty[1]), fileConn0)
}
close(fileConn0)
dbDisconnect(conn)


###########################################
###########   Multiple Files   ############
###########################################

# Tables: yelp, obgyn, pediatrician, dentist, 13, 14, 15, 16
mytable <- 'yelp'
cmd <- paste('mkdir ~/data/', mytable, sep='')
system(cmd)
conn <- dbConnect(driver, host=myhost, dbname=mydb, myacct, mypwd)
mydata <- dbGetQuery(conn, paste("SELECT id, text FROM `", mytable, '`', sep=''))
dbDisconnect(conn)
setwd(file.path("~/data/", mytable))
for(i in 1:nrow(mydata))
{
  fileConn<-file(toString(mydata$id[i]))
  writeLines(mydata$text[i],fileConn)
  close(fileConn)
}