
# coding: utf-8

# # Model - Hypertuning - Logistic Regression

# In[ ]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import metrics
from sklearn.linear_model import LogisticRegression
import pickle


# In[ ]:


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
def listTables():
    import sqlite3 as sql3
    db = sql3.connect(DB_FILE)
    cur = db.cursor()
    cur.execute("SELECT name FROM sqlite_master WHERE type='table';")
    res = cur.fetchall()
    cur.close()
    db.close()
    return(res)


# In[ ]:


import os
cwd = os.getcwd()
DB_FILE = "%s\Data\loans.db" % cwd


# In[ ]:


X_Train = readSQL('''SELECT * FROM X_train_scaled''')


# In[ ]:


Y_Train = readSQL('''SELECT * FROM Y_train''')


# In[ ]:


X_Train = X_Train.drop(["index"],axis=1)


# In[ ]:


Y_Train = Y_Train.drop(["index"],axis=1)


# In[ ]:


X_Dev = readSQL('''SELECT * FROM X_dev_scaled''')


# In[ ]:


X_Dev = X_Dev.drop(["index"],axis=1)


# In[ ]:


Y_Dev = readSQL('''SELECT * FROM Y_dev''')


# In[ ]:


Y_Dev = Y_Dev.drop(["index"],axis=1)


# In[ ]:


Y_Train = Y_Train.values.ravel()
Y_Dev = Y_Dev.values.ravel()


# ## Base Logistic Regression

# In[ ]:


# Initialize the predictive model object
#mod_logistic = LogisticRegression(random_state=1207,solver ="saga", max_iter=10000,C=1)
filename= "%s\Models\logistic_base_model.sav" % cwd
mod_logistic = pickle.load(open(filename, 'rb'))


# In[ ]:


# Train the model using the training sets
mod_logistic.fit(X_Train, Y_Train)


# In[ ]:


pred_train = mod_logistic.predict_proba(X_Train)


# In[ ]:


pred_dev = mod_logistic.predict_proba(X_Dev)


# In[ ]:


train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])


# In[ ]:


dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])


# In[ ]:


print(train_score)
print(dev_score)


# ## Checking Paramters with GridSearch

# In[ ]:


from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline


# In[ ]:


pipeline = Pipeline([('mdl',LogisticRegression(random_state=1207,solver ="saga", max_iter=10000))])
parameters = {"mdl__C":(0.001, 0.01, 0.1, 1, 10, 100), "mdl__penalty":["l1","l2"]}

grid_search = GridSearchCV(pipeline, parameters, n_jobs=2, cv=5,verbose=1, scoring='roc_auc')
grid_search.fit(X_Train, Y_Train)
print ('Best score: %0.3f' % grid_search.best_score_)
print ('Best parameters set:')
best_parameters = grid_search.best_estimator_.get_params()
for param_name in sorted(parameters.keys()):
    print ('\t%s: %r' % (param_name, best_parameters[param_name]))

predictions = grid_search.predict_proba(X_Dev)
print (metrics.classification_report(Y_Dev, predictions))


# In[ ]:


predictions = grid_search.predict_proba(X_Dev)
print (metrics.classification_report(Y_Dev, predictions))


# ## Checking the best model by GridSearch

# In[ ]:


mod_logistic_searchBest = LogisticRegression(random_state=1207,solver ="saga", max_iter=10000,C=0.01,penalty='l1')
mod_logistic_searchBest.fit(X_Train, Y_Train)


# In[ ]:


pred_train_searchBest = mod_logistic_searchBest.predict_proba(X_Train)


# In[ ]:


pred_dev_searchBest = mod_logistic_searchBest.predict_proba(X_Dev)


# In[ ]:


train_score_searchBest = metrics.roc_auc_score(Y_Train, pred_train_searchBest[:,1])


# In[ ]:


dev_score_searchBest = metrics.roc_auc_score(Y_Dev, pred_dev_searchBest[:,1])


# In[ ]:


print(train_score_searchBest)
print(dev_score_searchBest)


# ## Changing the parameters manually

# In[ ]:


c_param = [1e-07, 1e-06,1e-05,1e-04,0.001, 0.01, 0.1,5,10,100,1000]
l_param = ["l1","l2"]
res = []
for l in l_param:
    for i in c_param:
        model = LogisticRegression(solver="saga",max_iter=10000, C=i,penalty=l, random_state=1207,n_jobs=2)
        model = model.fit(X_Train, Y_Train)
        pred_train = model.predict_proba(X_Train)
        pred_dev = model.predict_proba(X_Dev)
        train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
        dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
        res.append([l,i,train_score,dev_score,model.score(X_Train, Y_Train), model.score(X_Dev,Y_Dev)])
        print("Penalty: %s | C: %s | Train AUC: %s | Dev AUC: %s | Train Score: %s | Dev Score: %s" % (l,i,train_score,dev_score, model.score(X_Train,Y_Train), model.score(X_Dev,Y_Dev)))


# In[ ]:


res = pd.DataFrame(res, columns=['L','C','Train_auc_score','Dev_auc_score','Train_Score','Dev_score'])


# In[ ]:


df=res[:-12]
df=df.drop(0,axis=1)


# In[ ]:


base = []
base.append(['l2',1,train_score,dev_score,mod_logistic.score(X_Train, Y_Train), mod_logistic.score(X_Dev,Y_Dev)])
base = pd.DataFrame(base, columns=['L','C','Train_auc_score','Dev_auc_score','Train_Score','Dev_score'])


# In[ ]:


df = df.append(base,ignore_index=True,sort=False)


# In[ ]:


df['AUC_Diff'] = df['Train_auc_score'] - df['Dev_auc_score']


# In[ ]:


df.sort_values(['Train_auc_score', 'Dev_auc_score','AUC_Diff'], ascending=[0, 0 ,0])


# The best models are l1/l2 penalty and c=100,1000

# In[ ]:


c_param = [500,1500,2000,5000,10000]
l_param = ["l1","l2"]
res2 = []
for l in l_param:
    for i in c_param:
        model = LogisticRegression(solver="saga",max_iter=10000, C=i,penalty=l, random_state=1207,n_jobs=2)
        model = model.fit(X_Train, Y_Train)
        pred_train = model.predict_proba(X_Train)
        pred_dev = model.predict_proba(X_Dev)
        train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
        dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
        res2.append([l,i,train_score,dev_score,model.score(X_Train, Y_Train), model.score(X_Dev,Y_Dev)])
        print("Penalty: %s | C: %s | Train AUC: %s | Dev AUC: %s | Train Score: %s | Dev Score: %s" % (l,i,train_score,dev_score, model.score(X_Train,Y_Train), model.score(X_Dev,Y_Dev)))


# In[ ]:


res2 = pd.DataFrame(res2, columns=['L','C','Train_auc_score','Dev_auc_score','Train_Score','Dev_score'])


# In[ ]:


res2['AUC_Diff'] = res2['Train_auc_score'] - res2['Dev_auc_score']


# In[ ]:


res2.sort_values(['Train_auc_score', 'Dev_auc_score','AUC_Diff'], ascending=[0, 0 ,0])


# We take the best model as l2 penalty and C=100

# In[ ]:


model = LogisticRegression(solver="saga",max_iter=10000, C=100,penalty='l2', random_state=1207,n_jobs=2)
model = model.fit(X_Train, Y_Train)


# In[ ]:


##Save the model
filename= "%s\Models\logistic_best_model.sav" % cwd
pickle.dump(model, open(filename, 'wb'))

