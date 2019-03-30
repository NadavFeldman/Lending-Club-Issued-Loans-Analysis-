
# coding: utf-8

# # XGB Model - Hypertuning

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn import metrics
import xgboost as xgb
import pickle
from sklearn.model_selection import GridSearchCV
from sklearn.calibration import CalibratedClassifierCV


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
def listTables():
    import sqlite3 as sql3
    db = sql3.connect(DB_FILE)
    cur = db.cursor()
    cur.execute("SELECT name FROM sqlite_master WHERE type='table';")
    res = cur.fetchall()
    cur.close()
    db.close()
    return(res)


# In[3]:


import os
cwd = os.getcwd()
DB_FILE = "%s\Data\loans.db" % cwd


# In[4]:


X_Train = readSQL('''SELECT * FROM X_train_scaled''')


# In[5]:


Y_Train = readSQL('''SELECT * FROM Y_train''')


# In[6]:


X_Train = X_Train.drop(["index"],axis=1)


# In[7]:


Y_Train = Y_Train.drop(["index"],axis=1)


# In[8]:


X_Dev = readSQL('''SELECT * FROM X_dev_scaled''')


# In[9]:


X_Dev = X_Dev.drop(["index"],axis=1)


# In[10]:


Y_Dev = readSQL('''SELECT * FROM Y_dev''')


# In[11]:


Y_Dev = Y_Dev.drop(["index"],axis=1)


# In[12]:


Y_Train = Y_Train.values.ravel()
Y_Dev = Y_Dev.values.ravel()


# ## Base XGBoost

# In[22]:


# Transform data to a DMatrix object
dtrain = xgb.DMatrix(np.array(X_Train), label=Y_Train)
ddev = xgb.DMatrix(np.array(X_Dev), Y_Dev)


# In[16]:


# Set the parameters for the xgboost
param = {'max_depth':5, 'eta': 1, 'silent': 1, 'objective': 'binary:logistic'}
#param['nthread'] = 10
param['eval_metric'] = 'auc'
# Train the model using the training sets
num_round = 10
mod_xgboost = xgb.train(param, dtrain, num_round)


# In[30]:


filename= "%s\Models\XGB_base_model.sav" % cwd
pickle.dump(mod_xgboost, open(filename, 'wb'))


# In[32]:


pred_train = mod_xgboost.predict(dtrain)


# In[33]:


pred_dev = mod_xgboost.predict(ddev)


# In[34]:


train_score = metrics.roc_auc_score(Y_Train, pred_train)


# In[35]:


dev_score = metrics.roc_auc_score(Y_Dev, pred_dev)


# In[36]:


print(train_score)
print(dev_score)


# ## Base XGB Classifier

# In[87]:


param_dist = {'objective':'binary:logistic','n_jobs':2}
clf = xgb.XGBClassifier(**param_dist)
clf.mod=clf.fit(X_Train, Y_Train,
        eval_set=[(X_Train, Y_Train), (X_Dev, Y_Dev)],
        eval_metric='auc',
        verbose=False)
evals_result = clf.mod.evals_result()


# In[88]:


clf


# XGBClassifier(base_score=0.5, booster='gbtree', colsample_bylevel=1,
#        colsample_bytree=1, gamma=0, learning_rate=0.1, max_delta_step=0,
#        max_depth=3, min_child_weight=1, missing=None, n_estimators=100,
#        n_jobs=2, nthread=None, objective='binary:logistic',
#        random_state=1207, reg_alpha=0, reg_lambda=1, scale_pos_weight=1,
#        seed=None, silent=True, subsample=1)

# In[89]:


pred_train = evals_result['validation_0']['auc'][-1]
pred_dev = evals_result['validation_1']['auc'][-1]
print(pred_train)
print(pred_dev)


# In[86]:


pred_train = clf.mod.predict_proba(X_Train)
pred_dev = clf.mod.predict_proba(X_Dev)

train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
print(train_score)
print(dev_score)


# <b>Checking model fit for hyperparamters tuning

# In[14]:


n_estimators = [100, 200, 300, 400, 500 , 1000]
learning_rate = [0.001, 0.01, 0.1, 0.2 , 0.3]
res = []
for n in n_estimators:
    for l in learning_rate:
        param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4}
        param_dist['n_estimators'] = n
        param_dist['learning_rate'] =l
        model = xgb.XGBClassifier(**param_dist)
        model = model.fit(X_Train, Y_Train)
        pred_train = model.predict_proba(X_Train)
        pred_dev = model.predict_proba(X_Dev)
        train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
        dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
        res.append([n,l,train_score,dev_score])
        print("n_estimators: %s | learning_rate: %s | Train AUC: %s | Dev AUC: %s" % (n,l,train_score,dev_score))


# In[15]:


res = pd.DataFrame(res, columns=['n_estimators','learning_rate','Train_auc_score','Dev_auc_score'])
res


# In[17]:


res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']


# In[19]:


res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# We see the high dev auc_score with lower difference between train and dev with learning rate =0.1 and n_estimators=500

# Tune max_depth and min_child_weight

# In[13]:


max_depth = [3, 4, 5, 6, 8]
min_child_weight = [1, 2, 3, 4]
res = []
for n in max_depth:
    for l in min_child_weight:
        param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4,'learning_rate': 0.1,'n_estimators': 500 }
        param_dist['max_depth'] = n
        param_dist['min_child_weight'] =l
        model = xgb.XGBClassifier(**param_dist)
        model = model.fit(X_Train, Y_Train)
        pred_train = model.predict_proba(X_Train)
        pred_dev = model.predict_proba(X_Dev)
        train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
        dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
        res.append([n,l,train_score,dev_score])
        print("max_depth: %s | min_child_weight: %s | Train AUC: %s | Dev AUC: %s" % (n,l,train_score,dev_score))


# In[14]:


res = pd.DataFrame(res, columns=['max_depth','min_child_weight','Train_auc_score','Dev_auc_score'])
res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']
res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# We use max_depth =4 and min_child_weight=3

# Tune subsample and colsample_bytree

# In[16]:


subsample = [i/10.0 for i in range(5,9)]
subsample.append(1)
colsample_bytree = [i/10.0 for i in range(5,11)]
res = []
for n in subsample:
    for l in colsample_bytree:
        param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4,'learning_rate': 0.1,'n_estimators': 500 ,
                      'max_depth': 4, 'min_child_weight': 3}
        param_dist['subsample'] = n
        param_dist['colsample_bytree'] =l
        model = xgb.XGBClassifier(**param_dist)
        model = model.fit(X_Train, Y_Train)
        pred_train = model.predict_proba(X_Train)
        pred_dev = model.predict_proba(X_Dev)
        train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
        dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
        res.append([n,l,train_score,dev_score])
        print("subsample: %s | colsample_bytree: %s | Train AUC: %s | Dev AUC: %s" % (n,l,train_score,dev_score))


# In[17]:


res = pd.DataFrame(res, columns=['subsample','colsample_bytree','Train_auc_score','Dev_auc_score'])
res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']
res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# we use subsample =0.8 and colsample_bytree=1

# tune gamma parameter

# In[24]:


gamma = [0, 0.1, 0.2, 0.5, 1, 5, 10]
res = []
for n in gamma:
    param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4,'learning_rate': 0.1,'n_estimators': 500 ,
                  'max_depth': 4, 'min_child_weight': 3,'colsample_bytree': 1, 'subsample': 0.8 ,
                  'reg_lambda': 1 ,'reg_alpha': 0}
    param_dist['gamma'] = n
    model = xgb.XGBClassifier(**param_dist)
    model = model.fit(X_Train, Y_Train)
    pred_train = model.predict_proba(X_Train)
    pred_dev = model.predict_proba(X_Dev)
    train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
    dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
    res.append([n,train_score,dev_score])
    print("gamma: %s | Train AUC: %s | Dev AUC: %s" % (n,train_score,dev_score))


# In[25]:


res = pd.DataFrame(res, columns=['gamma','Train_auc_score','Dev_auc_score'])
res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']
res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# we got good result with gamma = 0.2

# Tune alpha - regularization L1 Lasso

# In[18]:


reg_alpha = [1e-4, 1e-2, 0.1,0, 1, 5, 10]
res = []
for n in reg_alpha:
    param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4,'learning_rate': 0.1,'n_estimators': 500 ,
                  'max_depth': 4, 'min_child_weight': 3,'colsample_bytree': 1, 'subsample': 0.8}
    param_dist['reg_alpha'] = n
    model = xgb.XGBClassifier(**param_dist)
    model = model.fit(X_Train, Y_Train)
    pred_train = model.predict_proba(X_Train)
    pred_dev = model.predict_proba(X_Dev)
    train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
    dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
    res.append([n,train_score,dev_score])
    print("reg_alpha: %s | Train AUC: %s | Dev AUC: %s" % (n,train_score,dev_score))


# In[20]:


res = pd.DataFrame(res, columns=['reg_alpha','Train_auc_score','Dev_auc_score'])
res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']
res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# We prefer to choose reg_alpha = 0 because it has the lowest auc difference

# Tune lambda - regularization L2 Ridge

# In[22]:


reg_lambda = [1e-4, 1e-2, 0.1, 1, 5, 10]
res = []
for n in reg_lambda:
    param_dist = {'objective':'binary:logistic','random_state':1207,'n_jobs':4,'learning_rate': 0.1,'n_estimators': 500 ,
                  'max_depth': 4, 'min_child_weight': 3,'colsample_bytree': 1, 'subsample': 0.8}
    param_dist['reg_lambda'] = n
    model = xgb.XGBClassifier(**param_dist)
    model = model.fit(X_Train, Y_Train)
    pred_train = model.predict_proba(X_Train)
    pred_dev = model.predict_proba(X_Dev)
    train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
    dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
    res.append([n,train_score,dev_score])
    print("reg_lambda: %s | Train AUC: %s | Dev AUC: %s" % (n,train_score,dev_score))


# In[23]:


res = pd.DataFrame(res, columns=['reg_lambda','Train_auc_score','Dev_auc_score'])
res['AUC_Diff'] = res['Train_auc_score'] - res['Dev_auc_score']
res.sort_values(['Dev_auc_score','AUC_Diff'], ascending=[ 0 ,0])


# We prefer to choose reg_lambda = 1 because it has the lowest auc difference

# Combine the tuned parameters into final model

# In[33]:


clf = xgb.XGBClassifier(silent=True, 
                      scale_pos_weight=1,
                      learning_rate=0.1,  
                      colsample_bytree = 1,
                      subsample = 0.8,
                      objective='binary:logistic', 
                      n_estimators=500, 
                      max_depth=4, 
                      reg_lambda=1,
                      reg_alpha=0,
                     gamma=0.2,
                      min_child_weight=3,
                      random_state=1207,n_jobs=4)
clf.mod=clf.fit(X_Train, Y_Train,
        eval_set=[(X_Train, Y_Train), (X_Dev, Y_Dev)],
        eval_metric='auc',
        verbose=False)
evals_result = clf.mod.evals_result()


# In[34]:


clf


# In[35]:


pred_train = clf.mod.predict_proba(X_Train)
pred_dev = clf.mod.predict_proba(X_Dev)

train_score = metrics.roc_auc_score(Y_Train, pred_train[:,1])
dev_score = metrics.roc_auc_score(Y_Dev, pred_dev[:,1])
print(train_score)
print(dev_score)


# <b>Check for Calibration

# In[36]:


from sklearn.calibration import calibration_curve
from matplotlib import pyplot
from sklearn.metrics import brier_score_loss


# In[37]:


fop, mpv = calibration_curve(Y_Dev, pred_dev[:,1], n_bins=10, normalize=False)


# In[38]:


pyplot.plot([0, 1], [0, 1], linestyle='--')
# plot model reliability
pyplot.plot(mpv, fop, marker='.')
pyplot.show()


# In[39]:


brier_score_loss(Y_Dev,  pred_dev[:,1])


# In[40]:


##Save the model
filename= "%s\Models\XGB_best_model.sav" % cwd
pickle.dump(clf.mod, open(filename, 'wb'))

