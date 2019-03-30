
if(!require(dplyr)) {install.packages("dplyr");require(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
if(!require(ggExtra)) {install.packages("ggExtra");require(ggExtra)}
if(!require(naniar)) {install.packages("naniar");require(naniar)}
if(!require(mvoutlier)) {install.packages("mvoutlier");require(mvoutlier)}
if(!require(MissMech)) {install.packages("MissMech");require(MissMech)}
if(!require(mice)) {install.packages("mice");require(mice)}
if(!require(MatchIt)) {install.packages("MatchIt");require(MatchIt)}
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}
if(!require(funModeling)){install.packages("funModeling"); require(funModeling)}
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}
if(!require(NbClust)) {install.packages("NbClust");require(NbClust)}

missingMatrix <- function(data) {
    vn <- names(data)
    missdata <- data.frame(row1=1:nrow(data))
    for(v in vn) {
        newn <- paste0(v,"_na")
        mv <- ifelse(is.na(data[[v]]),1,0)
        missdata[newn] <- mv
    }
    missdata$row1 <- NULL
    return(missdata)
}

minmax <- function(x) {
    return(((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))))
}

getMissingness <- function (data, getRows = FALSE) {
    require(dplyr)
    l <- nrow(data)
    vn <- names(data)
    nadf <- data
    cnt <- NULL
    miss <- function(x) return(sum(is.na(x)))
    for (n in vn) {
        nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
        cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
    }
    names(cnt) <- c("var", "na.count")
    cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
    nadf$na.cnt <- 0
    nadf$na.cnt <- rowSums(nadf)
    cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
        0)
    totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
    idx <- NULL
    msg <- (paste("This dataset has ", as.character(totmiss), 
        " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
        "%)", " complete rows. Original data has ", nrow(data), 
        " rows.", sep = ""))
    if (getRows == TRUE & totmiss != 0) {
        nadf$rn <- seq_len(nrow(data))
        idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
    }
    print(list(head(cnt, n = 10), msg))
    return(list(missingness = cnt, message = msg, rows = idx$rn))
}

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

### load data
DB_FILE = paste0(getwd(),"/Data/loans.db") 
loans = readSQL("SELECT * FROM loans_dataset_outliers")


dim(loans)
head(loans)

copy1<-loans

split(names(loans),sapply(loans, function(x) paste(class(x), collapse=" ")))

loans$verification_status <-as.numeric(as.factor(loans$verification_status))
loans$purpose <-as.numeric(as.factor(loans$purpose))
loans$emp_length <-as.numeric(as.factor(loans$emp_length))
loans$home_ownership <-as.numeric(as.factor(loans$home_ownership))
loans$term <-as.factor(loans$term)
loans$pymnt_plan <-as.factor(loans$pymnt_plan)
loans$initial_list_status <-as.factor(loans$initial_list_status)
loans$full_state <-as.numeric(as.factor(loans$full_state))
loans$issue_d <- as.Date(loans$issue_d)
loans$default <- factor(loans$default)
loans$cat_delinq_2yrs <-as.numeric(as.factor(loans$cat_delinq_2yrs))
loans$cat_inq_last_6mths <-as.numeric(as.factor(loans$cat_inq_last_6mths))
loans$cat_collections_12_mths_ex_med <-as.numeric(as.factor(loans$cat_collections_12_mths_ex_med))
loans$cat_pub_rec <-as.numeric(as.factor(loans$cat_pub_rec))
loans$inc_grp <-as.numeric(as.factor(loans$inc_grp))
loans$credit_hist_grp <-as.numeric(as.factor(loans$credit_hist_grp))

## numeric variables
num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()
num_vars

loans %>%
  sample_n(10000) %>%
  vis_miss()

getMissingness(loans)

options(repr.plot.width=6, repr.plot.height=8)
missing_data <- loans %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.1)+coord_flip()

missingcols_dummy <- missingMatrix(loans)

head(missingcols_dummy)

##function creates logistic Regression Model
createLogisticRegressionModel <- function (dt,predictCol,dependCols,family="binomial"){
  glmod <- glm(as.formula(paste(predictCol,paste("~", dependCols))), data=dt,family = family,control = list(maxit = 50))
  #print(summary(glmod))
  return(glmod)
}

## function to determine the missing mechanism
defineMissingMechanism<- function(mod){
    m<- coef(summary(mod))
    important<- subset(m, m[,4] <0.05)
    important_cnt <- nrow(important)
    #print(paste0("Number of Significant variables = ",important_cnt))
    mech<-NULL
    important_names<-''
    if (important_cnt > 1) {
       # print("MNAR")
        mech <- "MNAR"
    } else if (important_cnt == 0) {
        #print("MCAR")
        mech <- "MCAR"
        } else{
            #print("Need to check further if MCAR or MAR")
            mech <- "Need to check further if MCAR or MAR"
            important_names <- rownames(important)
            #print(important)
        }
    return(list(mech = mech, important_cnt = important_cnt,important_names = important_names))
}

missingVars <- colnames(loans)[colSums(is.na(loans)) > 0]
#missingVars <- missingVars[!missingVars %in% c("last_credit_pull_d","earliest_cr_line")]
missingVars

fullVars <- colnames(loans)[colSums(is.na(loans)) == 0]
fullVars <- fullVars[!fullVars %in% c("issue_d")]
fullVars

##Function That run through all the missing variables and check the missing mechanism and possible action
getMissingTreatment <- function (data,missingcols_dummy,missingVars,fullVars) {
missDF <- NULL
for (v in missingVars) {
    print(v)
    print(format(Sys.time(), "%X"))
    naName <- paste0(v,"_na")
    dfNames<-c(fullVars,naName)
    df <- cbind(loans[,fullVars],missingcols_dummy[[naName]])
    colnames(df) <- dfNames
    na.count <- sum(missingcols_dummy[[naName]])
    na.rate <- round((na.count/nrow(missingcols_dummy)) * 100, 1)
    mod <- createLogisticRegressionModel(df,naName,".")
    model_res <- defineMissingMechanism(mod)
    mech <- model_res$mech
    important_cnt <- model_res$important_cnt
    important_names <- model_res$important_names
    action <- NULL
    #print(na.count)
    #print(na.rate)
    if (mech =="MNAR" && na.rate>3){
        #print ("Drop Column")
        action <- "Drop Column"
        } else if (mech =="MNAR" && na.rate<3){
        #print ("Drop Rows")
         action <- "Drop Rows"
    }else{
        #print ("Can Do Imputation")
        action <- "Can Do Imputation"
        } 
        missDF <- rbind(missDF, data.frame(v,na.count,na.rate,mech,important_cnt,important_names,action))
    }
    names(missDF) <- c("var", "na.count", "na.rate","missing mechanism","siginificant vars count","siginificant vars","possible action")
return(missDF)
}

missDF <- getMissingTreatment(loans,missingcols_dummy,missingVars,fullVars)

missDF

missDF[missDF[,4]=='MCAR' & missDF[,3]==100,7] <- 'Drop Column'

missDF

missDF[missDF[,4]=='MCAR' & missDF[,3]==100,7] <- "Drop Column"

missDF

#function that treats the missing value in the data
treatMissing <- function (data,missDF) {
    misAction <- missDF%>%select("var","possible action")
    misAction$"possible action"<-as.character(misAction$"possible action")
    i <-1
for (v in misAction$var){
    action <- misAction[i,2]
    print(v)
    #print(action)
        if (action == "Drop Column"){
           # print("Drop Column")
            data <- data%>%select(-one_of(v))
        }
        else if (action == "Drop Rows"){
            #print ("drop rows")
        
            #print ( paste0("numrows before",nrow(data)) )
            data <- data[!is.na(data[[v]]) ,]
            #print ( paste0("numrows after",nrow(data)) )
        }
        else{
          print ("imputation")
        }
        i <- i + 1
    }
    return(data)
}

loans <- copy1
loans2 <- treatMissing(loans,missDF)
dim(loans)
dim(loans2)

##saving table
writeSQL(loans2, "loans_dataset_missing")
