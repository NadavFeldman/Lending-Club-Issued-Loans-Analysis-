
# coding: utf-8

# In[31]:


import os
cwd = os.getcwd()
DB_FILE = "%s\Data\loans.db" % cwd


# In[32]:


import numpy as np
import pandas as pd
import seaborn as sns
from sklearn import preprocessing


# In[33]:


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


# In[34]:


loans = readSQL('''SELECT * FROM loans_dataset_missing''')


# In[35]:


loans.head()


# In[36]:


loans.dtypes


# In[37]:


df = loans.copy()


# In[38]:


df.drop(['issue_d'],axis=1,inplace=True)


# In[39]:


##Get Series of columns
##cat_vars = list(df.select_dtypes(include=['object']).columns)
##cat_vars
##Turn
#for column in cat_vars:
 #   print(df[column])
 #   df[column] = df[column].astype('category')


# In[40]:


## check unique - df.select_dtypes('category').apply(pd.Series.nunique, axis = 0)


# <h3> Encoding the Categorical Variables

# <b> For categorical variables with only 2 unique values we will use LabelEncoder

# In[41]:


count = 0
for col in df:
    if df[col].dtype == 'object':
        if len(list(df[col].unique())) <= 2:     
            le = preprocessing.LabelEncoder()
            df[col] = le.fit_transform(df[col])
            count += 1
            print (col)
            
print('%d columns were label encoded.' % count)


# <b> For categorical variables with more than 2 unique values we will use dummies(onehotencoding)

# In[42]:


df = pd.get_dummies(df)
print(df.shape)
df.head()


# In[43]:


##check for NA
df.columns[df.isna().any()].tolist()


# In[44]:


##Turn Our Outcome To Category For TableOne


# In[45]:


df['default'] = df['default'].astype('category')


# # Feature Selection Strategy

# <p>After the dataset is ready we need to find what features are good for modeling.</p>
# <p>We will do it with Univariable and Multivariable methods.</p>
# <p>For Univariate we will check with <strong>tableOne</strong> what features have <strong>p-value&lt;0.05</strong></p>
# <p>For Multivariate we will check with numerous modeling:</p>
# <ol>
# <li>LASSO (L1 penalization)</li>
# <li>Random Forest</li>
# <li>Gradient Boosting classification</li>
# <li>SVM classification</li>
# </ol>

# In[46]:


##Get Thomas Library
import pyMechkar as mechkar


# <b>prepare the data for multiple checks

# In[47]:


##Get All Columns except default
df_columns = df.select_dtypes(exclude=['category']).columns


# In[48]:


X = df.loc[:,df_columns]
X.head()
X.shape


# In[49]:


y = df[['default']]
print(y.shape)


# Prepare table with all of our features and fill it with the result of each analysis method

# In[53]:


varSelection = pd.DataFrame({'Variable': df_columns})
varSelection.size


# <b>Univariable check

# In[55]:


tab1 = mechkar.pyMechkar().Table1(data=df, y='default')


# In[56]:


tab1[tab1['p_value']<0.05]


# In[57]:


vn1 = tab1.loc[tab1['p_value']<0.05,'Variables'].unique()
print(len(vn1))
vn1


# We will add these variables to our variable selection table

# In[58]:


varSelection['Univarable'] = 0
varSelection.loc[varSelection['Variable'].isin(vn1), 'Univarable'] = 1
varSelection


# <b>Multivariable Analysis checks

# ### Variable Selection using LASSO (L1 penalization)

# In[59]:


from sklearn.linear_model import Lasso
from sklearn.feature_selection import SelectFromModel

lassomod = Lasso(alpha=0.1,max_iter=10000).fit(X, y)


# In[60]:


model = SelectFromModel(lassomod, prefit=True)
#model.get_support()


# In[61]:


varSelection['Lasso'] = model.get_support().astype('int64')
#varSelection


# ### Variable Selection using Random Forest

# In[62]:


from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_selection import SelectFromModel

rfmod = RandomForestClassifier().fit(X, y.values.ravel())
#rfmod.feature_importances_ 


# In[63]:


model = SelectFromModel(rfmod, prefit=True)
#model.get_support()


# In[64]:


varSelection['RandomForest'] = model.get_support().astype('int64')
#varSelection


# ### Variable Selection using Gradient Boosting classification

# In[65]:


from sklearn.ensemble import GradientBoostingClassifier
from sklearn.feature_selection import SelectFromModel

gbmod = GradientBoostingClassifier().fit(X, y.values.ravel())


# In[66]:


model = SelectFromModel(gbmod, prefit=True)
#model.get_support()


# In[67]:


varSelection['GradientBoost'] = model.get_support().astype('int64')
#varSelection


# ### Variable Selection using SVM classification

# In[68]:


from sklearn.svm import LinearSVC
from sklearn.feature_selection import SelectFromModel

svmmod = LinearSVC(C=0.01, penalty="l1",dual=False).fit(X, y.values.ravel())


# In[69]:


model = SelectFromModel(svmmod, prefit=True)
#model.get_support()


# In[70]:


varSelection['SVM'] = model.get_support().astype('int64')
#varSelection


# ### Summarization and Selection of Variables 

# In[71]:


varSelection['Sum'] =  np.sum(varSelection,axis=1)
varSelection


# In[73]:


varSelection.groupby('Sum')['Variable'].count()


# We can now decide a threshold for selecting our variables!

# In[74]:


v=varSelection[varSelection['Sum']>1]


# In[77]:


cols = v["Variable"]


# In[80]:


cols = cols.append(pd.Series('default'))


# In[81]:


df2 = df.loc[:,cols]


# In[82]:


print(df2.shape)


# In[83]:


writeSQL(df=df2,tablename="full_dataset")

