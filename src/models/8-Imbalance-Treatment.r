
if(!require(imbalance)){install.packages("imbalance");require(imbalance)}
### ROSE = Random Over-Sampling Examples
if(!require(ROSE)){install.packages("ROSE");require(ROSE)}
if(!require(partykit)){install.packages("partykit");require(partykit)}
if(!require(dplyr)) {install.packages("dplyr");require(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}


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
####
listTables <- function(db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    ### list the tables on the DB
    res <- dbListTables(con)
    return(res)
} 

### load data
DB_FILE = (paste0(getwd(),"/Data/loans.db"))
listTables()
loansX = readSQL("SELECT * FROM X_train_scaled")
loansY = readSQL("SELECT [default] FROM Y_train")


train = cbind(loansX,loansY)

loansX = readSQL("SELECT * FROM X_dev_scaled")
loansY = readSQL("SELECT [default] FROM Y_dev")

dev = cbind(loansX,loansY)

train <- train%>%select(-one_of(c('index')))
dev <- dev%>%select(-one_of(c('index')))

table(train$default)
table(train$default)/nrow(train)*100

numPositive <- length(which(train$default == 1))
numNegative <- length(which(train$default == 0))
nInstances <- numNegative - numPositive
cbind(numPositive=numPositive,numNegative=numNegative,nInstances=nInstances)

mod1 <- ctree(default ~., data=train)
auc1 <- pROC::auc(dev$default, predict(mod1,newdata=dev,type="response"))
auc1

#under sampling
data_balanced_under <- ovun.sample(default ~ ., data = train, method = "under",N = numPositive*2)$data
table(data_balanced_under$default)

mod1 <- ctree(default ~., data=data_balanced_under)
auc1 <- pROC::auc(dev$default, predict(mod1,newdata=dev,type="response"))
auc1

#over sampling
data_balanced_over <- ovun.sample(default ~ ., data = train, method = "over",N = numNegative*2)$data
table(data_balanced_over$default)

mod1 <- ctree(default ~., data=data_balanced_over)
auc1 <- pROC::auc(dev$default, predict(mod1, newdata=dev,type="response"))
auc1

#over_under sampling
data_balanced_both <- ovun.sample(default ~ ., data = train, method = "both", p=0.5, seed = 1207)$data
table(data_balanced_both$default)

mod1 <- ctree(default ~., data=data_balanced_both)
auc1 <- pROC::auc(dev$default, predict(mod1, newdata=dev,type="response"))
auc1

# Rose: 
data.rose <- ROSE(default ~ ., data = train, seed = 1207)$data
table(data.rose$default)

mod1 <- ctree(default ~., data=data.rose)
auc1 <- pROC::auc(dev$default, predict(mod1, newdata=dev,type="response"))
auc1
