
##Importing required packages 
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}
if(!require(ggcorrplot)){install.packages("ggcorrplot"); require(ggcorrplot)}
if(!require(rcompanion)){install.packages("rcompanion"); require(rcompanion)}
if(!require(dbscan)){install.packages("dbscan");require(dbscan)}
if(!require(data.table)){install.packages("data.table");require(data.table)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
if(!require(mlr)) {install.packages("mlr");require(mlr)}
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}



##WorkPath
DB_FILE = (paste0(getwd(),"/Data/loans.db"))


###############################################################################
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

loans = readSQL("SELECT * From loans_dataset_eng")

dim(loans)

loans_coppy_1<-loans


loans$purpose<-factor(loans$purpose)
loans$pymnt_plan<-factor(loans$pymnt_plan)
loans$credit_hist_grp<-factor(loans$credit_hist_grp)
loans$verification_status<-factor(loans$verification_status)
loans$initial_list_status<-factor(loans$initial_list_status)
loans$term<-factor(loans$term)
loans$home_ownership<-factor(loans$home_ownership)
loans$cat_pub_rec<-factor(loans$cat_pub_rec)
loans$pymnt_plan<-factor(loans$pymnt_plan)
loans$emp_length<-factor(loans$emp_length)
loans$default<-factor(loans$default)
loans$cat_delinq_2yrs<-factor(loans$cat_delinq_2yrs)
loans$full_state<-factor(loans$full_state)
loans$inc_grp<-factor(loans$inc_grp)
loans$cat_inq_last_6mths <-factor(loans$cat_inq_last_6mths )
loans$cat_collections_12_mths_ex_med<-factor(loans$cat_collections_12_mths_ex_med)





cat_vars <- loans %>% sapply(is.factor) %>% which() %>% names()
cat_vars
length(cat_vars)

num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()
num_vars

options(repr.plot.width = 4, repr.plot.height = 4)
plt1 <- boxplot(loans$loan_amnt)

head(plt1$out,100)
length(plt1$out)

sum(plt1$out>35000)
sum(plt1$out==35000)
sum(plt1$out<35000)

loans$max_L<-(loans$loan_amnt==35000)*1
bin_vars<-"max_L"

options(repr.plot.width = 4, repr.plot.height = 4)
plt2 <- boxplot(loans$funded_amnt)




length(plt2$out)
sum(plt2$out>35000)
sum(plt2$out==35000)
sum(plt2$out<35000)

loans$max_fund_L=(loans$funded_amnt==35000)*1
bin_vars<-c(bin_vars,"max_fund_L")

options(repr.plot.width = 4, repr.plot.height = 4)
plt3 <- boxplot(loans$funded_amnt_inv)
head((plt2$out),70)
length(plt2$out)
sum(plt2$out>35000)
sum(plt2$out==35000)
sum(plt2$out<35000)



loans$MAX_fund_L_inv=(loans$funded_amnt==35000)*1
bin_vars<-c(bin_vars,"MAX_fund_L_inv")

head(loans$loan_installment, 40)

options(repr.plot.width = 4, repr.plot.height = 4)
plt4<- boxplot(loans$loan_installment)
head((plt4$out),70)
length(plt4$out)


options(repr.plot.width = 4, repr.plot.height = 4)
plt4.1<- boxplot(loans$loan_installment, range = 2)
head((plt4.1$out),70)
length(plt4.1$out)


sum(plt4.1$out==972.22)


plt4.2<-boxplot(loans$loan_amnt~loans$term) 


plt4.3<-boxplot(loans$loan_amnt~loans$term,range=2) 

out4.2<-plt4.2$out
length(out4.2)
head(out4.2)
sum(out4.2==35000)
min(out4.2)


options(repr.plot.width = 4, repr.plot.height = 4)
plt5<- boxplot(loans$mths_since_last_major_derog)

head((plt5$out),70)
length(plt5$out)

min(plt5$out)
plt5$stats
max(plt5$out)

159/12

hist(loans$mths_since_last_major_derog)

options(repr.plot.width = 4, repr.plot.height = 4)
plt6<- boxplot(loans$annual_inc)
head((plt6$out),70)
length(plt6$out)
plt6$stats

min(plt6$out)


quantile(x = loans$annual_inc, na.rm = TRUE)

e <- exp(1) 
log_annual_inc<-log(loans$annual_inc,base = e )
hist(loans$annual_inc,col = 20)
hist(log_annual_inc,col = 20)

plt6_l<- boxplot(log(loans$annual_inc,base = e))
length(plt6_l$out)

max(loans$annual_inc,na.rm = TRUE)


sum(loans$annual_inc>200000,na.rm = TRUE)/sum(loans$annual_inc<10000000000,na.rm = TRUE)

sum(loans$annual_inc>480930,na.rm = TRUE)/sum(loans$annual_inc<10000000000,na.rm = TRUE)

sum(loans$annual_inc>1000000,na.rm = TRUE) 
sum(loans$annual_inc>4000000,na.rm = TRUE)
sum(loans$annual_inc>6000000,na.rm = TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt7<- boxplot(loans$dti)

head((plt7$out),70)
length(plt7$out)

min(plt7$out)
quantile(x = loans$dti)

options(repr.plot.width = 4, repr.plot.height = 4)
plt16.1<- boxplot(loans$dti,range = 1.6)

head((plt16.1$out),70)
length(plt16.1$out)

plt16.1<- boxplot(loans$dti,range = 1.61)

head((plt16.1$out),70)
length(plt16.1$out)


options(repr.plot.width = 4, repr.plot.height = 4)
plt8<- boxplot(loans$delinq_2yrs)

head((plt8$out),70)
length(plt8$out)
plt8$stats

hist(loans$delinq_2yrs,col = 20)
hist(log(loans$delinq_2yrs,e),col = 20)

min(loans$delinq_2yrs,  na.rm=TRUE)
quantile(x = loans$delinq_2yrs, na.rm=TRUE)
sum(loans$delinq_2yrs==0,  na.rm=TRUE)
sum(loans$delinq_2yrs!=0,  na.rm=TRUE)

plt8<-boxplot(loans$acc_now_delinq)
length(plt8$out)


quantile(x = loans$acc_now_delinq, na.rm=TRUE)
sum(loans$acc_now_delinq==0,  na.rm=TRUE)
sum(loans$acc_now_delinq!=0,  na.rm=TRUE)


loans$is_acc_now_delinq_not_zero<-(loans$acc_now_delinq!=0)*1
bin_vars<-c(bin_vars,"is_acc_now_delinq_not_zero")

options(repr.plot.width = 4, repr.plot.height = 4)
plt9<- boxplot(loans$inq_last_6mths)

head((plt9$out),70)
length(plt9$out)

max((plt9$out))
sum(loans$inq_last_6mths==0,na.rm = TRUE)
sum(loans$inq_last_6mths!=0,na.rm = TRUE)

hist(loans$inq_last_6mths)
hist(log(loans$inq_last_6mths))


options(repr.plot.width = 4, repr.plot.height = 4)
plt10<- boxplot(loans$mths_since_last_delinq)

head((plt10$out),70)
length(plt10$out)

hist(loans$mths_since_last_delinq)
hist(loans$mths_since_last_delinq^0.5)


plt10<- boxplot(loans$mths_since_last_delinq^0.5)

head((plt10$out),70)
length(plt10$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt11<- boxplot(loans$mths_since_last_record)

head((plt11$out),70)
length(plt11$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt12<- boxplot(loans$open_acc)

head((plt12$out),70)
length(plt12$out)

max(plt12$out)

hist(loans$open_acc)
hist(loans$open_acc^0.5)
hist(log(loans$open_acc))

rootb9<-boxplot(loans$open_acc^0.5)
length(rootb9$out)
logb9<-boxplot(log(loans$open_acc))
length(logb9$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt13<- boxplot(loans$revol_bal)

head((plt13$out),70)
length(plt13$out)

hist(loans$revol_bal)

hist((loans$revol_bal)^0.5)

plt13<- boxplot(loans$revol_bal^0.5)

head((plt13$out),70)
length(plt13$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt14<- boxplot(loans$revol_util)

head((plt14$out),70)
length(plt14$out)

plt14$stats

max(loans$revol_util, na.rm=TRUE)

sum(100>loans$revol_util, na.rm=TRUE)
sum(100<loans$revol_util, na.rm=TRUE)

max(loans$revol_util[loans$revol_util!=max(loans$revol_util, na.rm=TRUE)], na.rm=TRUE)


Lmax<-loans[loans$revol_util==max(plt14$out),]
Lmax[!is.na(Lmax$verification_status),]

892.3/2677

2677/300

options(repr.plot.width = 4, repr.plot.height = 4)
plt15<- boxplot(loans$total_acc)

head((plt15$out),70)
length(plt15$out)

Lmax<-loans[loans$total_acc==max(plt15$out),]
Lmax[!is.na(Lmax$total_acc),]

options(repr.plot.width = 4, repr.plot.height = 4)
plt16<- boxplot(loans$tot_coll_amt)

head((plt16$out),70)
length(plt16$out)

m1<-max(plt16$out)
m2<-max( plt16$out[plt16$out!=max(plt16$out)] )
m1
m2
m1/m2


Lmax<-loans[loans$tot_coll_amt==max(plt16$out),]
print(Lmax[!is.na(Lmax$tot_coll_amt),])

loans$tot_coll_amt[loans$tot_coll_amt==max(plt16$out)]<-NA

options(repr.plot.width = 4, repr.plot.height = 4)
plt16<- boxplot(loans$tot_coll_amt)

head((plt16$out),70)
length(plt16$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt17<- boxplot(loans$tot_cur_bal)

head((plt17$out),70)
length(plt17$out)

min(loans$tot_cur_bal,na.rm=TRUE) 

hist(loans$tot_cur_bal)
hist(log(loans$tot_cur_bal))
hist((loans$tot_cur_bal)^0.5)

plt17.1<- boxplot(log(loans$tot_cur_bal))
length(plt17.1$out)

plt17.2<- boxplot(loans$tot_cur_bal^0.5)
length(plt17.2$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt28<- boxplot(loans$open_acc_6m)

head((plt28$out),70)
length(plt28$out)

sum(loans$open_acc_6m!=6,na.rm=TRUE)

sum(loans$open_acc_6m==0,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt19<- boxplot(loans$open_il_6m)

head((plt19$out),70)
length(plt19$out)

sum(loans$open_il_6m<50,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt20<- boxplot(loans$open_il_12m)


head((plt20$out),70)
length(plt20$out)

sum(loans$open_il_12m<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt31<- boxplot(loans$open_il_24m)

head((plt31$out),70)
length(plt31$out)

sum(loans$open_il_24m<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$mths_since_rcnt_il)

head((plt25$out),70)
length(plt25$out)

sum(loans$mths_since_rcnt_il<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$total_bal_il)

head((plt25$out),70)
length(plt25$out)

sum(loans$total_bal_il<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$il_util)

head((plt25$out),70)
length(plt25$out)

sum(loans$il_util<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$open_rv_12m)

head((plt25$out),70)
length(plt25$out)

sum(loans$open_rv_12m<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$open_rv_24m)

head((plt25$out),70)
length(plt25$out)

sum(loans$open_rv_24m<1000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$max_bal_bc)

head((plt25$out),70)
length(plt25$out)

sum(loans$max_bal_bc<100000,na.rm=TRUE)

hist(loans$all_util)

sum(loans$all_util<100000,na.rm=TRUE)

#IQR
options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$all_util)

head((plt25$out),70)
length(plt25$out)

#z
z_outlaiers<- function(x, outs_bound=2.7, na_rm=TRUE){
    z_x<-abs(scale(x))
    is_out<-z_x>outs_bound
    outs<-ifelse(sum(is_out,na.rm=TRUE)==0,0, x[is_out])
    
    outs<-ifelse(na_rm , outs[!is.na(outs)]  ,outs)
    outs<-ifelse(sum(is_out,na.rm=TRUE)==0,"There is no outliers!", outs)
    return (outs)
}

plt<-z_outlaiers(loans$all_util,2.7, na_rm=TRUE )
length(plt)
head(plt)

options(repr.plot.width = 4, repr.plot.height = 4)
plt29<- boxplot(loans$total_rev_hi_lim)

head((plt29$out),70)
length(plt29$out)

hist((loans$total_rev_hi_lim))
hist(log(loans$total_rev_hi_lim))
hist((loans$total_rev_hi_lim)^0.5)

plt29.1<- boxplot(log(loans$total_rev_hi_lim))

length(plt29.1$out)

plt29.2<- boxplot(loans$total_rev_hi_lim^0.5)

length(plt29.2$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt25<- boxplot(loans$inq_fi)

head((plt25$out),70)
length(plt25$out)

sum(loans$inq_fi<100000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt31<- boxplot(loans$total_cu_tl)

head((plt31$out),70)
length(plt31$out)

sum(loans$total_cu_tl<100000,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt32<- boxplot(loans$inq_last_12m)

head((plt32$out),70)
length(plt32$out)

sum(loans$inq_last_12m<100,na.rm=TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)
plt33<- boxplot(loans$acc_ratio)

head((plt33$out),70)
length(plt33$out)

sum(loans$acc_ratio<1,na.rm=TRUE)
sum(loans$acc_ratio==1,na.rm=TRUE)
sum(loans$acc_ratio>1,na.rm=TRUE)


Lmax<-loans[loans$acc_ratio>1,]
Lmax[!is.na(Lmax$total_acc),]

loans$total_acc [(loans$acc_ratio>1)]<-NA
loans$open_acc[(loans$acc_ratio>1)]<-NA

loans$acc_ratio[(loans$acc_ratio>1)]<-NA

options(repr.plot.width = 4, repr.plot.height = 4)
plt34<- boxplot(loans$credit_hist)

head((plt34$out),70)
length(plt34$out)

hist(loans$credit_hist)

hist(loans$credit_hist^0.5)
hist(log(loans$credit_hist))


plt34<- boxplot(loans$credit_hist^0.5)

length(plt34$out)

plt34<- boxplot(log(loans$credit_hist))

length(plt34$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt35<- boxplot(loans$poverty_rate_p)

head((plt35$out),70)
length(plt35$out)

options(repr.plot.width = 4, repr.plot.height = 4)
plt36<- boxplot(loans$collections_12_mths_ex_med)

head((plt36$out),70)
length(plt36$out)

sum(is.na(loans$collections_12_mths_ex_med))



summary(loans$cat_collections_12_mths_ex_med)

options(repr.plot.width = 4, repr.plot.height = 4)
plt37<- boxplot(loans$pub_rec)

head((plt37$out),70)
length(plt37$out)

sum(is.na(loans$pub_rec))

summary(loans$cat_pub_rec)

##sample 
k<-loans


k_num<-NULL
k_cat<-NULL
k_num<-k[num_vars]
k_cat<-k[,cat_vars]


sum(is.na(k_num),na.rm=TRUE)

ind <- is.na(k_num)
k_num[ind] <- rep(9999999, sum(ind))


head(k_num)

k.norm<-scale(k_num)
k.norm<-k.norm[, colSums(is.nan(k.norm)) != nrow(k.norm)]
head(k.norm)

k_dummy<-createDummyFeatures(k[,cat_vars], cols = cat_vars)
k_dummy[is.na(k_dummy)]<-0.4116079

sum(is.na(k_dummy))

k_bin_vars<-k[,bin_vars]


k_bin_vars[is.na(k_bin_vars)]<- -0.4116079

k_data<-cbind(k.norm,k_dummy,k_bin_vars)

dim(k_data)
head(k_data)

#sample data 
set.seed(123)
k_data_semp<- k_data[sample(nrow(k_data), 10000), ]

#distance_matrix <- as.matrix(dist(scale(k_data_semp)))
#pca <-prcomp(distance_matrix)
#saveRDS(object =pca ,file = "PCA.rds" )
pca <- readRDS(paste0(getwd(),"/Models/PCA.rds"))
embedding <- data.table(pca$x[, 1:2])
embedding[, loans := rownames(k)]
ggplot(embedding, aes(x = PC1, y = PC2)) +
    geom_point(size = 2, colour = "steelblue", alpha = 0.3) +
    geom_text(aes(label = loans), check_overlap = TRUE) +
    theme_minimal()

#:3

k_data[c(5852,740,8426,1621,2821),]

if(!require(knndist)){install.packages("knndist"); require(knndist)}
knnD<- kNNdist(k_data_semp, k=9, search="kd")
apply(knnD,2,min)
apply(knnD,2,max)
apply(knnD,2,mean)


## dbscan clustering
#mod <-dbscan(k_data, eps = 4 ,minPts =1)
#saveRDS(mod, "db_model.rds")

mod <- readRDS(paste0(getwd(),"/Models/db_model.rds"))
table(mod$cluster)

head(k_data)

k_data$cluster<-as.vector(mod$cluster)

##sample with clusters
set.seed(123)
k_data_semp<- k_data[sample(nrow(k_data), 10000), ]


embedding[, DClusters := k_data_semp$cluster]

options(repr.plot.width=10, repr.plot.height=10  )
ggplot(embedding, aes(x = PC1, y = PC2)) +
    geom_point(aes(colour = factor(k_data_semp$cluster)), size = 2, alpha = 0.9) +
geom_text(aes(label = loans), check_overlap = TRUE)
    

table(mod$cluster)[table(mod$cluster)>1]

k_data$is_out<-!(mod$cluster ==1)


k_data_semp$is_out<-!((k_data_semp$cluster ==1))

head(k_data)

sum(k_data$is_out)

k_data_outs<-k_data[k_data$is_out,]

k_outs<-k_data[k_data$is_out,]

k_data_in<-k_data[!k_data$is_out,]

k_in<-k_data[!k_data$is_out,]

#embedding[, DClusters := mod$k_data_semp]
ggplot(embedding, aes(x = PC1, y = PC2)) +
    geom_point(aes(colour = k_data_semp$is_out), size = 2, alpha = 0.8) 

#remove the support tag
k_data$cluster<-NULL

head(k[k_data$is_out,])

##chi-sqared test
chisq.test(k_data$is_out,k$default, correct=F )

#model <- glm(default.1~.,family=binomial,data=k_data_in)
#saveRDS(object = model, file = "log_data_in.rds")
model_in<-readRDS(file = paste0(getwd(),"/Models/log_data_in.rds"))

head(coef(summary(model_in)))
print(paste0("There is ",sum(coef(summary(model_in))[,4]<0.05)," significant variables"))

#model <- glm(default.1~.,family=binomial,data=k_data)
#saveRDS(object = model, file = "log_data_with_outs.rds")
model_with_out<-readRDS(file = paste0(getwd(),"/Models/log_data_with_outs.rds"))

head(coef(summary(model_with_out)))
print(paste0("There is ",sum(coef(summary(model_with_out))[,4]<0.05)," significant variables"))

#model <- glm(default.1~.,family=binomial,data=k_data_outs)
#saveRDS(object = model, file = "log_data_out.rds")
model_out<-readRDS(file = paste0(getwd(),"/Models/log_data_out.rds"))

head(coef(summary(model_out)))
print(paste0("There is ",sum(coef(summary(model_out))[,4]<0.05)," significant variables"))

dim(k)

sum(k_data$is_out)

k_ex<-k[!k_data$is_out,]

dim(k_ex)

loans<-k_ex

##saving table
writeSQL(loans, "loans_dataset_outliers")
