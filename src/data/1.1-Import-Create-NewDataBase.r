
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}

DATA_PATH = paste0(getwd(),"/Data")
DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 

##############################################################################
##########                DATABASE FUNCTIONS                     #############
##############################################################################

#### List the name of the tables on the database 
listTables <- function(db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    ### list the tables on the DB
    res <- dbListTables(con)
    return(res)
}

#### Exucute function to execure queries on the SQL
runSQLCommand <- function(query, db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    rows_affected <- dbSendQuery(con, query)
    return(rows_affected)
}
            
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

#### Generate a view based on the provided SQL query
createView <- function(viewname, query) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    view = paste0('CREATE VIEW ',viewname,' AS \n', query)
    dbSendQuery(con, view)
}

loaners = readSQL("select member_id,emp_title,emp_length,zip_code,addr_state from loan")
# Write CSV in R
write.csv(loaners, file = "loaners.csv",row.names=FALSE)

mydata = read.csv("loaners.csv")  # read csv file 
DB_FILE = paste0(DATA_PATH,"/loans.db") 
db <- dbConnect(SQLite(), DB_FILE)
writeSQL(mydata,"loaners")

DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 
loaners_finance = readSQL("select member_id,home_ownership,annual_inc,dti,delinq_2yrs,annual_inc_joint,dti_joint,acc_now_delinq from loan")
# Write CSV in R
write.csv(loaners_finance, file = "loaners_finance.csv",row.names=FALSE)
mydata = read.csv("loaners_finance.csv")  # read csv file 
DB_FILE = paste0(DATA_PATH,"/loans.db") 
db <- dbConnect(SQLite(), DB_FILE)
writeSQL(mydata,"loaners_finance")

DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 
loaners_credit_history = readSQL("select member_id,earliest_cr_line,inq_last_6mths,mths_since_last_delinq,mths_since_last_record,open_acc,pub_rec,revol_bal,revol_util,total_acc,tot_coll_amt,
tot_cur_bal,open_acc_6m,open_il_6m,open_il_12m,open_il_24m,mths_since_rcnt_il,total_bal_il,il_util,
open_rv_12m,open_rv_24m,max_bal_bc,all_util,total_rev_hi_lim,inq_fi,total_cu_tl,inq_last_12m
from loan")
# Write CSV in R
write.csv(loaners_credit_history, file = "loaners_credit_history.csv",row.names=FALSE)
mydata = read.csv("loaners_credit_history.csv")  # read csv file 
DB_FILE = paste0(DATA_PATH,"/loans.db") 
db <- dbConnect(SQLite(), DB_FILE)
writeSQL(mydata,"loaners_credit_history")

DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 
loans = readSQL("select id as loan_id,member_id,verification_status,issue_d,loan_status,pymnt_plan,
url,[desc] as loanDesc,purpose,title,policy_code,
application_type,verification_status_joint
from loan")
# Write CSV in R
write.csv(loans, file = "loans.csv",row.names=FALSE)
#mydata = read.csv("loans.csv")  # read csv file 
#DB_FILE = paste0(DATA_PATH,"/loans.db") 
#db <- dbConnect(SQLite(), DB_FILE)
#writeSQL(mydata,"loans")

DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 
loans_finance = readSQL("select id as loan_id,loan_amnt,funded_amnt,funded_amnt_inv,term,int_rate,installment,out_prncp,out_prncp_inv,
total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,collection_recovery_fee,
last_pymnt_d,last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,mths_since_last_major_derog
from loan")
# Write CSV in R
write.csv(loans_finance, file = "loans_finance.csv",row.names=FALSE)
mydata = read.csv("loans_finance.csv")  # read csv file 
DB_FILE = paste0(DATA_PATH,"/loans.db") 
db <- dbConnect(SQLite(), DB_FILE)
writeSQL(mydata,"loans_finance")

DB_FILE = paste0(DATA_PATH,"/Raw/database.sqlite") 
loans_lc_rating = readSQL("select id as loan_id,grade,sub_grade,initial_list_status
from loan")
# Write CSV in R
write.csv(loans_lc_rating, file = "loans_lc_rating.csv",row.names=FALSE)
mydata = read.csv("loans_lc_rating.csv")  # read csv file 
DB_FILE = paste0(DATA_PATH,"/loans.db") 
db <- dbConnect(SQLite(), DB_FILE)
writeSQL(mydata,"loans_lc_rating")

##Eliminate Bad Row in the end
DB_FILE = paste0(DATA_PATH,"/loans.db") 
runSQLCommand("delete from loans where loan_id='Loans that do not meet the credit policy'")
runSQLCommand("delete from loans_lc_rating where loan_id='Loans that do not meet the credit policy'")
runSQLCommand("delete from loans_finance where loan_id='Loans that do not meet the credit policy'")
runSQLCommand("delete from loaners where member_id is null")
runSQLCommand("delete from loaners_finance where member_id is null")
runSQLCommand("delete from loaners_credit_history where member_id is null")


##change from text to number
runSQLCommand("update loaners_credit_history
set revol_util = replace(revol_util,'%','')
where revol_util is not null")

runSQLCommand("update loans_finance
set int_rate = replace(int_rate,'%','')
where int_rate is not null")

##change from text to boolean
runSQLCommand("update loans
set pymnt_plan=1
where pymnt_plan='y'")

runSQLCommand("update loans
set pymnt_plan=0
where pymnt_plan='n'")

runSQLCommand("update loans
set application_type=1
where application_type='JOINT'")

runSQLCommand("update loans
set application_type=0
where application_type='INDIVIDUAL'")

runSQLCommand("update loans_lc_rating
set initial_list_status=1
where initial_list_status='f'")

runSQLCommand("update loans_lc_rating
set initial_list_status=0
where initial_list_status='w'")

runSQLCommand("update loans_finance
set term=1
where term=' 60 months'")

runSQLCommand("update loans_finance
set term=0
where term=' 36 months'")

##Create View to pull all the data to dataset var
createView("v_loans"," 
select loans.loan_id,loans.member_id,verification_status,issue_d,loan_status,pymnt_plan,
url,loanDesc,purpose,title,policy_code,
application_type,verification_status_joint,
grade,sub_grade,initial_list_status,
emp_title,emp_length,zip_code,addr_state,
loan_amnt,funded_amnt,funded_amnt_inv,term,int_rate,installment,out_prncp,out_prncp_inv,
total_pymnt,total_pymnt_inv,total_rec_prncp,total_rec_int,total_rec_late_fee,recoveries,collection_recovery_fee,
last_pymnt_d,last_pymnt_amnt,next_pymnt_d,last_credit_pull_d,collections_12_mths_ex_med,mths_since_last_major_derog,
home_ownership,annual_inc,dti,delinq_2yrs,annual_inc_joint,dti_joint,acc_now_delinq,
earliest_cr_line,inq_last_6mths,mths_since_last_delinq,mths_since_last_record,open_acc,pub_rec,revol_bal,revol_util,total_acc,tot_coll_amt,
tot_cur_bal,open_acc_6m,open_il_6m,open_il_12m,open_il_24m,mths_since_rcnt_il,total_bal_il,il_util,
open_rv_12m,open_rv_24m,max_bal_bc,all_util,total_rev_hi_lim,inq_fi,total_cu_tl,inq_last_12m
from loans 
inner join loaners 
on loaners.member_id = loans.member_id
inner join loans_lc_rating
on loans.loan_id = loans_lc_rating.loan_id
inner join loans_finance
on loans.loan_id = loans_finance.loan_id
inner join loaners_credit_history
on loaners.member_id = loaners_credit_history.member_id
inner join loaners_finance
on loaners.member_id = loaners_finance.member_id")
