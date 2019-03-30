
##Importing required packages 
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(readxl)){install.packages("readxl"); require(readxl)}
if(!require(tableone)){install.packages("tableone"); require(tableone)}
if(!require(funModeling)){install.packages("funModeling"); require(funModeling)}
if(!require(openintro)){install.packages("openintro"); require(openintro)}
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
if(!require(gridExtra)) {install.packages("gridExtra");require(gridExtra)}
if(!require(choroplethr)) {install.packages("choroplethr");require(choroplethr)}
if(!require(choroplethrMaps)) {install.packages("choroplethrMaps");require(choroplethrMaps)}
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}
if(!require(ggcorrplot)){install.packages("ggcorrplot"); require(ggcorrplot)}
if(!require(nortest)){install.packages("nortest"); require(nortest)}
if(!require(ggpubr)){install.packages("ggpubr"); require(ggpubr)}

###Get Tomas Library
if(!require(devtools)){install.packages("devtools"); require(devtools)}
if(!require(mechkar)){install_github("karpatit/mechkar"); require(mechkar)}

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

meta_loans <- funModeling::df_status(loans, print_results = FALSE)
meta_loans%>%
  knitr::kable()

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

summary(loans)

meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(loans)) %>%
  mutate(uniq_rat = scales::percent(uniq_rat))

meta_loans %>%
  select(variable, p_zeros, p_na, unique,uniq_rat) %>% arrange(desc(unique))

meta_loans %>%
  select(variable, p_zeros, p_na, unique) %>% arrange(desc(p_na))

## numeric variables
num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()
num_vars

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

x_vars <- c(cat_vars,num_vars) 

tab1 <- Table1(data = loans,x = x_vars,y = "default")

##Export to txt to put in protocol
write.table(tab1,paste0(getwd(),"/table1New.txt"), append = FALSE, sep = ";", dec = ".",col.names = TRUE,row.names = FALSE)

tab1%>% arrange(desc(pval))

tab1%>% filter(grepl("Missing",V2) )%>% arrange(Pop)

tab1%>% filter(grepl("Mean",V2) | grepl("Median",V2))%>% arrange(V1)

exploreData(data = loans,x = x_vars,y = "default")

options(scipen = 999)
loans %>%
  select_(.dots = num_vars) %>%
  gather_("variable", "value", gather_cols = num_vars) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~variable,  scales = "free", ncol = 4)+
 geom_histogram(color="darkblue", fill="lightblue",na.rm = TRUE,bins = 10)

integer_vars <- 
  loans %>% 
  sapply(is.integer) %>% 
  which() %>% 
  names()

continuous_vars<-num_vars[!(num_vars%in%integer_vars)]
continuous_vars

for (v in continuous_vars) {
    print(v)
    print(ad.test(loans[,v]))
    print(ks.test(loans[,v], "pnorm",alternative='two.sided'))
}

options(repr.plot.width=4, repr.plot.height=4)
par(mfrow=c(2,2))
for (v in num_vars) {
    print(ggqqplot(loans[,v],title = v))
}

ver_df <- loans %>% 
  select(verification_status,default) %>% 
  group_by(verification_status,default) %>% 
  summarise(Count = n())%>% 
select(verification_status,Count,default) 

ver_df %>%
  ggplot(aes(verification_status,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by verification_status", x = "verification_status", y = "Loan Count \n")

pymnt_plan_df <- loans %>% 
  select(pymnt_plan,default) %>% 
  group_by(pymnt_plan,default) %>% 
  summarise(Count = n())%>% 
select(pymnt_plan,Count,default) 

pymnt_plan_df %>%
  ggplot(aes(pymnt_plan,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by pymnt_plan", x = "pymnt_plan", y = "Loan Count \n")

purposes_df <- loans %>% 
  select(purpose, loan_amnt,default) %>% 
  group_by(purpose,default) %>% 
  summarise(Count = n())
g <- ggplot(purposes_df, 
                  aes(x = purpose, y = Count))
g + geom_col() + xlab("Loan Purposes")+ coord_flip()+facet_wrap(~ default)

initial_list_status_df <- loans %>% 
  select(initial_list_status,default) %>% 
  group_by(initial_list_status,default) %>% 
  summarise(Count = n())%>% 
select(initial_list_status,Count,default) 

initial_list_status_df %>%
  ggplot(aes(initial_list_status,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by initial_list_status", x = "initial_list_status", y = "Loan Count \n")

emp_df <- loans %>% 
  select(emp_length,default) %>% 
  group_by(emp_length,default) %>% 
  summarise(Count = n())%>% 
select(emp_length,Count,default) 

emp_df %>%
  ggplot(aes(emp_length,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by emp_length", x = "emp_length", y = "Loan Count \n")+ coord_flip()

home_df <- loans %>% 
  select(home_ownership,default) %>% 
  group_by(home_ownership,default) %>% 
  summarise(Count = n())%>% 
select(home_ownership,Count,default) 

home_df %>%
  ggplot(aes(home_ownership,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  coord_flip() +
  labs(title="Loan Count by Home Ownership", x = "Home Ownership", y = "Loan Count \n")

loans$term_fac<- factor(loans$term,labels=c("36 Months", "60 Months"))

term_df <- loans %>% 
  select(term_fac, loan_amnt,default) %>% 
  group_by(term_fac,default) %>% 
  summarise(Count = n())%>% 
select(term_fac,Count,default) 

term_df %>%
  ggplot(aes(term_fac,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by Term", x = "Term", y = "Loan Count \n")

state_by_value <-
loans %>%rename(region = full_state) %>%mutate(region=tolower(region))%>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

state_choropleth(state_by_value, title = "Value by State")

state_by_volume <-
loans %>%rename(region = full_state) %>%mutate(region=tolower(region))%>% group_by(region) %>%
  summarise(value = n())

state_choropleth(state_by_volume, title = "Count by State")

amnt_df <- loans %>% 
  select(issue_d, loan_amnt,full_state,poverty_rate_p)%>% mutate(full_state=tolower(full_state))%>% mutate(issue_d=format(as.Date(issue_d, format="%d/%m/%Y"),"%Y")) %>%
  group_by(issue_d,full_state,poverty_rate_p) %>% 
  summarise(Count = n())

head(state_by_volume%>% arrange(desc(value)),10)

head(state_by_volume%>% arrange(value),10)

plotPovertyLoansByCountry <- function(data,states){
   i <- 1
   glist <- list();
   for (state in states){
      df <- data %>%filter(full_state==state)
      g2 <- ggplot(df, aes(issue_d, Count)) +
               geom_point(data=df, aes(x=issue_d, y=Count,  size=poverty_rate_p))
      g2 <-g2 +labs(title=paste0("Loans Count Over PovertyRate in ",state))
      glist[[i]] <- g2
      i <- i + 1
    }
  return(glist)
} 

par(mfrow = c(2,1))
#high volume states
statesv <- c("california","new york")
plist<- plotPovertyLoansByCountry(amnt_df,statesv)
for (i in 1:length(plist))
    print(plist[[i]])

par(mfrow = c(1,2))
#low volume states
statesv <- c("north dakota","nebraska")
plist<- plotPovertyLoansByCountry(amnt_df,statesv)
for (i in 1:length(plist))
    print(plist[[i]])

options(scipen = 999)
amnt_df <- loans %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt),Count = n())

ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")
ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Count))
ts_amnt + geom_line() + xlab("Date issued")

time_df <- loans %>% 
  select(issue_d, loan_amnt,default)

time_df$issue_month<-substr(time_df$issue_d,6,7)
time_df$issue_year<-substr(time_df$issue_d,1,4)

options(repr.plot.width=7, repr.plot.height=4)
time_df %>% group_by(issue_year,issue_month)%>%
summarise(lcnt=n())%>%
ggplot(aes(x=issue_month,y=lcnt,group=1,col=issue_month))+
geom_line()+
facet_wrap(~issue_year)+
geom_point()+
theme(legend.position="")+
labs(x="Loan Processed Month",y="Count",title="Loans processed in each month/Year")


options(repr.plot.width=5, repr.plot.height=4,scipen=10000)
time_df %>% group_by(issue_year)%>%
summarise(lycnt=n())%>%
ggplot(aes(x=issue_year,y=lycnt,group=1,col=issue_year))+
geom_line(size=1.5)+
geom_point(size=2)+
theme(legend.position="")+
labs(x="Year",y="Count",title="Loans processed in each Year")

options(repr.plot.width=5, repr.plot.height=4,scipen=999)
time_df %>% group_by(issue_year)%>%
summarise(lycnt=sum(loan_amnt))%>%
ggplot(aes(x=issue_year,y=lycnt,group=1,col=issue_year))+
geom_line(size=1.5)+
geom_point(size=2)+
theme(legend.position="")+
scale_y_log10()+
labs(x="Year",y="Amount",title="Loans processed in each Year")

options(repr.plot.width=5, repr.plot.height=4,scipen=9)
time_df2 <- time_df %>% 
  select(default, issue_year) %>% 
  group_by(default,issue_year) %>% 
  summarise(Count = n())
time_df2$default_fac<- factor(time_df2$default,labels=c("Paid Off", "Defaulted"))
time_df2 %>% ggplot(aes(default_fac,Count)) +
geom_bar(stat="identity",position="dodge")+
facet_wrap(~issue_year)



time_df3_0 <- time_df2 %>%select(default,issue_year,Count)%>%
filter(default==0)%>%mutate(PaidOff = Count)
time_df3_1 <- time_df2 %>%select(default,issue_year,Count)%>%
filter(default==1)%>%mutate(Defaulted = Count)


time_df3_sums <-inner_join(time_df3_0,time_df3_1,by ="issue_year" )%>%select(issue_year,PaidOff,Defaulted)


time_df3_sums%>%
gather(key=default,count,PaidOff,Defaulted)%>%
ggplot(aes(x=issue_year,y=count,group=default,fill=default))+
geom_bar(stat="identity",position="dodge")+

labs(title="PaidOff vs Defaulted Loans")



time_df3_sums  %>% select(issue_year,PaidOff,Defaulted)%>%mutate(ratio=Defaulted/(Defaulted+PaidOff))%>% ungroup()%>%select(issue_year,ratio)
  

loans$issue_year<-as.numeric(factor(time_df$issue_year))

#Chi-Square test
chisq.test(x = loans$issue_year,y = loans$default)

#exluding 2007 and 2008 issued loans
loans_ex_2008<-loans[ !loans$issue_year%in%c("2007","2008") ,]

chi2<-chisq.test(x = loans_ex_2008$issue_year,y = loans_ex_2008$default)
chi2


options(repr.plot.width=15, repr.plot.height=4,scipen=999)
p<-ggplot(loans, aes(x=purpose, y=loan_amnt, color=purpose)) +
  geom_boxplot()
p

options(repr.plot.width=10, repr.plot.height=4,scipen=999)
p<-ggplot(loans, aes(x=home_ownership, y=loan_amnt, color=home_ownership)) +
  geom_boxplot()
p

options(repr.plot.width=8, repr.plot.height=4,scipen=999)
p<-ggplot(loans, aes(x=verification_status, y=loan_amnt, color=verification_status)) +
  geom_boxplot()
p

options(repr.plot.width=15, repr.plot.height=4,scipen=999)
p<-ggplot(loans, aes(x=emp_length, y=total_acc, color=emp_length)) +
  geom_boxplot()
p

credithistory_df <- loans %>% 
  select(credit_hist,default) %>% 
  group_by(credit_hist,default) %>% 
  summarise(Count = n())%>% 
select(credit_hist,Count,default) 

credithistory_df %>%
  ggplot(aes(credit_hist,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by credit_hist", x = "credit_hist", y = "Loan Count \n")

loans %>%
  ggplot(aes(x = annual_inc, y = loan_amnt, color = default)) +
  geom_point()

options(repr.plot.width=6, repr.plot.height=4)
ggplot(loans, aes(x = annual_inc, y = dti, color = default)) + 
geom_point(alpha=0.5)

options(repr.plot.width=6, repr.plot.height=4)
ggplot(loans, aes(x = total_acc, y = open_acc, color = default )) + geom_point(alpha = 0.5)

options(repr.plot.width=6, repr.plot.height=4,scipen=9999)
ggplot(loans, aes(x = 1:nrow(loans), y =loan_installment, color = default )) + geom_point(alpha = 0.5)


#Important variables 
imv<-c('verification_status', 'loan_amnt', 'purpose' , 'term', 
'annual_inc', 'dti' , 'loan_installment', 'funded_amnt_inv','revol_util' )

#sample data 
set.seed(123)
loansemp<- loans[sample(nrow(loans), 20000), ]

#dfinne a panel.hist fuction
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}



#pairs plot 
options(repr.plot.width = 20, repr.plot.height = 20)
pairs(loansemp[imv], panel = panel.smooth,
      cex = 1.5,pch = 24,  bg = "light blue",  upper.panel = NULL,      
       diag.panel = panel.hist, cex.labels = 2, font.labels = 2)


nimv<-c( 'loan_amnt', 
'annual_inc', 'dti' , 'loan_installment', 'funded_amnt_inv','revol_util' )


corr <- round(cor(loans[nimv]), 3)
corr

## visualization
options(repr.plot.width = 11, repr.plot.height = 11)
ggcorrplot(corr, method = "circle")
