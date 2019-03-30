
##Importing required packages 
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(funModeling)){install.packages("funModeling"); require(funModeling)}
if(!require(ggpubr)){install.packages("ggpubr"); require(ggpubr)}
if(!require(nortest)){install.packages("nortest"); require(nortest)}

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

DB_FILE = paste0(getwd(),"/Data/loans.db")

loans = readSQL("SELECT * FROM loans_dataset")

#make a copy - for restore if necessary 
loansdata <- loans

dim(loans)

split(names(loans),sapply(loans, function(x) paste(class(x), collapse=" ")))

loans$verification_status <-as.factor(loans$verification_status)
loans$purpose <-as.factor(loans$purpose)
loans$emp_length <-as.factor(loans$emp_length)
loans$home_ownership <-as.factor(loans$home_ownership)
loans$term <-as.factor(loans$term)
loans$pymnt_plan <-as.factor(loans$pymnt_plan)
loans$initial_list_status <-as.factor(loans$initial_list_status)
loans$full_state <-as.factor(loans$full_state)
loans$issue_d <- as.Date(loans$issue_d)
loans$default <- factor(loans$default)

## numeric variables
num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()
num_vars

length(num_vars)

## categorical variables
cat_vars <- 
  loans %>% select(-one_of("default")) %>% 
  sapply(is.factor) %>% 
  which() %>% 
  names()
cat_vars

## other
other_vars <- setdiff(names(loans),c(num_vars,cat_vars))
other_vars

# to caterorical
loans$cat_delinq_2yrs<-NULL
loans$cat_delinq_2yrs[loans$delinq_2yrs == 0] <- "zero"
loans$cat_delinq_2yrs[loans$delinq_2yrs == 1] <- 'one'
loans$cat_delinq_2yrs[loans$delinq_2yrs > 1] <- 'more than 1'
loans$cat_delinq_2yrs<-factor(loans$cat_delinq_2yrs)


summary(loans$cat_delinq_2yrs)


summary(factor(loans$inq_last_6mths))


# to caterorical
loans$cat_inq_last_6mths<-NULL
loans$cat_inq_last_6mths[loans$inq_last_6mths == 0] <- "zero"
loans$cat_inq_last_6mths[loans$inq_last_6mths == 1] <- 'one'
loans$cat_inq_last_6mths[loans$inq_last_6mths == 2] <- 'two'
loans$cat_inq_last_6mths[loans$inq_last_6mths > 2] <- 'more than 2'
loans$cat_inq_last_6mths<-factor(loans$cat_inq_last_6mths)


summary(loans$cat_inq_last_6mths)

summary(factor(loans$collections_12_mths_ex_med))

loans$cat_collections_12_mths_ex_med<-NULL
loans$cat_collections_12_mths_ex_med[loans$collections_12_mths_ex_med == 0] <- "zero"
loans$cat_collections_12_mths_ex_med[loans$collections_12_mths_ex_med >= 1] <- 'one or more'
loans$cat_collections_12_mths_ex_med<-factor(loans$cat_collections_12_mths_ex_med)
summary(loans$cat_collections_12_mths_ex_med)

summary(factor(loans$pub_rec))


# to caterorical
loans$cat_pub_rec<-NULL
loans$cat_pub_rec[loans$pub_rec == 0] <- "zero"
loans$cat_pub_rec[loans$pub_rec >= 1] <- 'one or more'
loans$cat_pub_rec<-factor(loans$cat_pub_rec)
summary(loans$cat_pub_rec)

loans$mths_since_last_delinq_rt<-loans$mths_since_last_delinq^0.5

hist(loans$mths_since_last_delinq_rt)
hist(loans$mths_since_last_delinq)

#trasform
loans$open_acc_rt<- loans$open_acc^0.5
hist(loans$open_acc_rt)
hist(loans$open_acc)
transform_var<-"open_acc_rt"

loans$total_acc_rt<- loans$total_acc^0.5
hist(loans$total_acc_rt)
hist(loans$total_acc)
transform_var<-c(transform_var,"total_acc_rt")

##transformations
loans$annual_inc_log<- log(loans$annual_inc)
loans$annual_inc_rt<-(loans$annual_inc)^0.5
hist(loans$annual_inc_log)
hist(loans$annual_inc_rt)
transform_var<-c(transform_var,"annual_inc_log","annual_inc_rt" )

summary(loans$annual_inc)


## make a categorical
loans$inc_grp<-NULL
loans$inc_grp[loans$annual_inc <= 45000] <- "verylow_inc"
loans$inc_grp[loans$annual_inc > 45000 & loans$annual_inc <= 60000] <- 'low_inc'
loans$inc_grp[loans$annual_inc > 60000 & loans$annual_inc <= 87000] <- 'middle_inc'
loans$inc_grp[loans$annual_inc > 87000 & loans$annual_inc <= 120000] <- 'high_inc'
loans$inc_grp[loans$annual_inc > 120000] <- 'veryhigh_inc'


loans$inc_grp <-factor(loans$inc_grp)

loans$revol_bal_rt <-loans$revol_bal^0.5

hist(loans$revol_bal)
hist(loans$revol_bal_rt)
transform_var<-c(transform_var,"revol_bal_rt" )

## transformations
loans$credit_hist_log <-log(loans$credit_hist)
loans$credit_hist_rt <-loans$credit_hist^0.5

hist(loans$credit_hist)
hist(loans$credit_hist_log)
hist(loans$credit_hist_rt)
transform_var<-c(transform_var,"credit_hist_log","credit_hist_rt"  )

summary(loans$credit_hist)


loans$credit_hist_grp<-NULL
loans$credit_hist_grp[loans$credit_hist <= 4] <- '<4_yrs'
loans$credit_hist_grp[loans$credit_hist > 4 & loans$credit_hist <= 13] <- '4-13_yrs'
loans$credit_hist_grp[loans$credit_hist > 13 & loans$credit_hist <= 17] <- '13-17_yrs'
loans$credit_hist_grp[loans$credit_hist > 17 & loans$credit_hist <= 21] <- '17-21_yrs'
loans$credit_hist_grp[loans$credit_hist > 21] <- '>21_yrs'
loans$credit_hist_grp<-factor(loans$credit_hist_grp)


head(loans)

dim(loans)

for (tv in transform_var)  {
    print(tv)
        print(ad.test(loans[,tv]))
}

for (tv in tranform_var)  {
        print(ggqqplot(loans[,tv],title = tv))
}

##SQLLite Converts the date to numeric so imlilictly apply as.character on date columns
loans$issue_d <- as.character(loans$issue_d)

writeSQL(loans,"loans_dataset_eng")

summary(loans)

meta_loans <- funModeling::df_status(loans, print_results = FALSE)
meta_loans%>%
  knitr::kable()
