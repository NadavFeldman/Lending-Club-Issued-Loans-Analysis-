
require(dplyr)

if(!require(readxl)){install.packages("readxl"); require(readxl)}
# Load the Excel workbook
##path <- "D:/Google Drive/Final Project/"
##WorkPath
path <- paste0(getwd(),"/Data/Raw") 
excel_file <- paste0("/PovertyRate2007-2013.xlsx")
# see available tabs
excel_sheets(paste0(path, excel_file))

year2007 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 1 - Table 1")
year2008 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 2 - Table 1")
year2009 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 3 - Table 1")
year2010 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 4 - Table 1")
year2011 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 5 - Table 1")
year2012 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 6 - Table 1")
year2013 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 7 - Table 1")
excel_file <- paste0("/PovertyRate2014.xlsx")
year2014 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 1 - Table 1")
excel_file <- paste0("/PovertyRate2015.xlsx")
year2015 <- read_excel(paste0(path, excel_file), sheet = "Tabulate 1 - Table 1")


colnames(year2007)[colnames(year2007)=="CPS Data Collected in Year: 2008"] <- "state"
colnames(year2008)[colnames(year2008)=="CPS Data Collected in Year: 2009"] <- "state"
colnames(year2009)[colnames(year2009)=="CPS Data Collected in Year: 2010"] <- "state"
colnames(year2010)[colnames(year2010)=="CPS Data Collected in Year: 2011"] <- "state"
colnames(year2011)[colnames(year2011)=="CPS Data Collected in Year: 2012"] <- "state"
colnames(year2012)[colnames(year2012)=="CPS Data Collected in Year: 2013"] <- "state"
colnames(year2013)[colnames(year2013)=="CPS Data Collected in Year: 2014"] <- "state"
colnames(year2014)[colnames(year2014)=="CPS Data Collected in Year: 2015"] <- "state"
colnames(year2015)[colnames(year2015)=="CPS Data Collected in Year: 2016"] <- "state"

create_dsByYear <- function(data,year){
    percentColumn <-data %>% select("X__4") %>% filter(grepl("%",X__4))
    ##remove first row - it's a total value
    percentColumn <- percentColumn[-1,]
    statesColumn <-data %>% select("state") %>% filter(nchar(data$state)==2)
    if  (nrow(statesColumn) == nrow(percentColumn)){
      newData <- cbind(statesColumn,percentColumn,c(year))
      names(newData) <- c("state", "poverty_rate","year")
      return (newData)
     }
    else
      return (NULL)
  } 

povertyRate2007 <- create_dsByYear(year2007,2007)
povertyRate2008 <- create_dsByYear(year2008,2008)
povertyRate2009 <- create_dsByYear(year2009,2009)
povertyRate2010 <- create_dsByYear(year2010,2010)
povertyRate2011 <- create_dsByYear(year2011,2011)
povertyRate2012 <- create_dsByYear(year2012,2012)
povertyRate2013 <- create_dsByYear(year2013,2013)
povertyRate2014 <- create_dsByYear(year2014,2014)
povertyRate2015 <- create_dsByYear(year2015,2015)

povertyRate2007

poverty_rate_ds <- bind_rows(povertyRate2007,povertyRate2008,povertyRate2009,povertyRate2010,povertyRate2011,povertyRate2012,povertyRate2013,povertyRate2014,povertyRate2015)

dim(poverty_rate_ds)

create_dsByYearTranspose <- function(data){
    cat <- levels(factor(data[["state"]]))
    res <- data %>% group_by_("state") %>% select_("state") %>% distinct_()
    for (l in 1:length(cat)) {
        cname <- paste(cat[l])
        cc1 <- data %>% select("poverty_rate") %>% filter(povertyRate2007[["state"]]==cat[l]) 
        res[[cname]] <- as.character(cc1)
    }
    return (res)
}

try2007 <- create_dsByYearTranspose(povertyRate2007)
try2008 <- create_dsByYearTranspose(povertyRate2008)

try2007
try2008

tryj <- rbind(try2007,try2008)

tryj

t(tryj)

if(!require(openintro)){install.packages("openintro"); require(openintro)}

poverty_rate_ds <- poverty_rate_ds %>%select(everything())%>%mutate(state_name= abbr2state(poverty_rate_ds$state) , poverty_rate_p =  as.numeric(sub("%", "",poverty_rate_ds$poverty_rate,fixed=TRUE))/100)

DB_FILE = paste0(getwd(),"/Data/loans.db") 

##############################################################################
##########                DATABASE FUNCTIONS                     #############
##############################################################################
            
#### Read function to import data from the SQL to a pandas dataframe.
readSQL <- function(query, db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    df <- dbGetQuery(con, query)
    return(df)
}
#### Write a pandas dataframe into an SQL table. Use overwrite=True if you want to delete 
#### first a pre-existent table with the same name. Use append=True if you want to append
#### the data in the dataframe to a pre-existent table.
writeSQL <- function(df,tablename,overwrite=FALSE, append=FALSE,db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    dbWriteTable(con,tablename,df,overwrite,append)
}


writeSQL(poverty_rate_ds, "povertyRateByStates")
