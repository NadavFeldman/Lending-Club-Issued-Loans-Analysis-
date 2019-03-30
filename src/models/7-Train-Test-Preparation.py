
# coding: utf-8

# # Train - Dev - Test Preparation

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


# In[2]:


##############################################################################
##########                DATABASE FUNCTIONS                     #############
##############################################################################
#### Read function to import data from the SQL to a pandas dataframe.
def readSQL(query):
    import pandas as pd
    import sqlite3 as sql3
    db = sql3.connect(DB_FILE)
    df = pd.read_sql_query(query, db)
    db.close()
    return(df)

#### Write a pandas dataframe into an SQL table. Use overwrite=True if you want to delete 
#### first a pre-existent table with the same name. Use append=True if you want to append
#### the data in the dataframe to a pre-existent table.
def writeSQL(df,tablename,overwrite=False, append=False):
    import pandas as pd
    import sqlite3 as sql
    db = sql.connect(DB_FILE)
    if (overwrite):
        action = "replace"
    elif (append):
        action = "append"
    else: 
        action = "fail"
    df.to_sql(tablename, db, if_exists=action)
    db.close()


# In[4]:


import os
cwd = os.getcwd()
DB_FILE = "%s\Data\loans.db" % cwd


# In[5]:


loans = readSQL('''SELECT * FROM full_dataset''')


# In[6]:


loans.head()


# In[7]:


loans = loans.drop(["index"],axis=1)


# In[52]:


df = loans.copy()


# In[53]:


## check the dtypes on the dataframe
df.dtypes


# We have to define some of the variables to be sure they are used as required by the models

# In[54]:


##Get Series of columns
cat_vars = list(df.select_dtypes(include=['object']).columns)
cat_vars
##Turn
for column in cat_vars:
    df[column] = df[column].astype('category')


# In[55]:


### check missings
df.isnull().sum()


# ## Preparing the partition to test, train and dev

# In[56]:


from random import seed, shuffle,randint
##Get Thomas Library
import pyMechkar as mechkar
import time


# In[57]:


def splittDF(df):
    p_value = 1
    vn = df.columns.tolist()
    while p_value > 0: 
        randomSeed = randint(100,2000)
        print (randomSeed)
        seed(randomSeed)
        xind = [i for i in range(len(df))]
        shuffle(xind,)
        ### We will split as follows: 10% test, 10% dev and 80% train
        ### test split
        split_1 = int(0.1 * len(xind))
        ### train-dev split
        split_2 = int(0.2 * len(xind))
        df['dataset'] = "train"
        df.iloc[xind[0:split_1],79] = "test"
        df.iloc[xind[split_1:split_2],79] = "dev"
        df['dataset'] = df['dataset'].astype('category')
        init = time.time()
        tab1 = mechkar.pyMechkar().Table1(x=vn,y="dataset",data=df,categorize=True,maxcat=7)
        print("---- %s seconds -----" % (time.time() - init))
        t = tab1.loc[(tab1['p_value']<0.05),]
        p_value = len(t.index)
    return(df)


# In[58]:


df = splittDF(df)


# In[59]:


##SEED=1207 Gave us the balanced split
df.head()


# In[61]:


### We will split as follows: 10% test, 10% dev and 80% train
X_test =  df.loc[(df["dataset"] =="test"),] #power.iloc[xind[0:split_1],]
y_test = df.loc[(df["dataset"] =="test"),"default"]#power.iloc[xind[0:split_1],29]

X_dev = df.loc[(df["dataset"] =="dev"),]#power.iloc[xind[split_1:split_2],]
y_dev = df.loc[(df["dataset"] =="dev"),"default"]#power.iloc[xind[split_1:split_2],29]

X_train = df.loc[(df["dataset"] =="train"),]#power.iloc[xind[split_2:],]
y_train = df.loc[(df["dataset"] =="train"),"default"]#power.iloc[xind[split_2:],29]


# In[62]:


print(X_train.shape)
print(y_train.shape)

print(X_dev.shape)
print(y_dev.shape)

print(X_test.shape)
print(y_test.shape)


# In[63]:


X_train.head()


# In[64]:


print(y_train.describe())
print(y_train.isnull().sum())


# In[65]:


## drop default from X_xxx
X_train = X_train.drop('default',axis=1)
X_dev = X_dev.drop('default',axis=1)
X_test = X_test.drop('default',axis=1)


# In[75]:


sns.distplot(y_train.astype(int),label="train")
sns.distplot(y_dev.astype(int),label="dev")
sns.distplot(y_test.astype(int),label="test")
plt.legend()
plt.show()


# In[76]:


X_train = X_train.drop('dataset',axis=1)
X_dev = X_dev.drop('dataset',axis=1)
X_test = X_test.drop('dataset',axis=1)


# In[140]:


writeSQL(X_train,tablename="X_train")
writeSQL(X_dev,tablename="X_dev")
writeSQL(X_test,tablename="X_test")
writeSQL(y_train,tablename="Y_train")
writeSQL(y_dev,tablename="Y_dev")
writeSQL(y_test,tablename="Y_test")


# <h3>Check Inbalance

# In[78]:


train = X_train.assign(default=y_train)


# In[79]:


train.head()


# In[82]:


target_count = train.default.value_counts()
print('Class 0:', target_count[0])
print('Class 1:', target_count[1])
print('Proportion:', round(target_count[0] / target_count[1], 2), ': 1')

target_count.plot(kind='bar', title='Count (target)');
plt.show()


# In[84]:


import imblearn
from imblearn.under_sampling import RandomUnderSampler
from imblearn.over_sampling import RandomOverSampler
from imblearn.over_sampling import SMOTE
from imblearn.combine import SMOTETomek
from imblearn.under_sampling import TomekLinks


# In[93]:


##Random Under Sampling
rus = RandomUnderSampler(return_indices=True)
X_rus, y_rus, id_rus = rus.fit_sample(X_train, y_train.astype(int))


# In[129]:


data_rus=pd.DataFrame(np.column_stack((X_rus,y_rus)))
target_count = data_rus[78].value_counts()
print('Class 0:', target_count[0])
print('Class 1:', target_count[1])


# In[94]:


##Random Over Sampling
ros = RandomOverSampler()
X_ros, y_ros = ros.fit_sample(X_train, y_train.astype(int))


# In[130]:


data_ros=pd.DataFrame(np.column_stack((X_ros,y_ros)))
target_count = data_ros[78].value_counts()
print('Class 0:', target_count[0])
print('Class 1:', target_count[1])


# In[95]:


## Under-sampling: Tomek links
tl = TomekLinks(return_indices=True, ratio='majority')
X_tl, y_tl, id_tl = tl.fit_sample(X_train, y_train.astype(int))


# In[131]:


data_tl=pd.DataFrame(np.column_stack((X_tl,y_tl)))
target_count = data_tl[78].value_counts()
print('Class 0:', target_count[0])
print('Class 1:', target_count[1])


# In[96]:


smote = SMOTE(ratio='minority')
X_sm, y_sm = smote.fit_sample(X_train, y_train.astype(int))


# In[132]:


data_smote=pd.DataFrame(np.column_stack((X_sm,y_sm)))
target_count = data_smote[78].value_counts()
print('Class 0:', target_count[0])
print('Class 1:', target_count[1])


# In[133]:


from sklearn.linear_model import LogisticRegression
model = LogisticRegression(solver="liblinear", penalty='l1',max_iter=1000)


# In[134]:


model.fit(X_train, y_train)


# In[135]:


model.score(X_train,y_train)


# In[136]:


model.fit(X_rus, y_rus)
model.score(X_rus, y_rus)


# In[137]:


model.fit(X_ros, y_ros)
model.score(X_ros, y_ros)


# In[138]:


model.fit(X_tl, y_tl)
model.score(X_tl, y_tl)


# In[139]:


model.fit(X_sm, y_sm)
model.score(X_sm, y_sm)

