#After R

import numpy as np
import pandas as pd
import scipy
import math
import statsmodels.api as sm
import matplotlib.pylab as plt
from sklearn.model_selection import KFold, TimeSeriesSplit, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import Lasso
from sklearn.metrics import mean_squared_error
from sklearn import metrics
from sklearn.pipeline import Pipeline
from itertools import chain
from sklearn import linear_model
#from tqdm.notebook import tqdm
from warnings import simplefilter
simplefilter(action='ignore', category=FutureWarning)

data =pd.read_csv("data2.csv")
industrylist=data['FFI49_desc'].unique().tolist()
data["date"]=pd.to_datetime(data["date"])
data.dropna(inplace=True)
lambda_list=pd.DataFrame(index= data.loc[data['FFI49_desc']==industrylist[0]].iloc[80:,0])
lasso_sse=pd.DataFrame(index= data.loc[data['FFI49_desc']==industrylist[0]].iloc[80:,0])
bestv=[]


# for m in range(len(industrylist)):


for m in range(49):
    data2=data.loc[data['FFI49_desc']==industrylist[m]] 
    data2.set_index(["date"], inplace=True)
    X=data2.iloc[:,2:]
    y=data2.iloc[:,1]
    ext_df=X
    XX=ext_df
    yy=y
    w=80
    pipe = Pipeline([
            ('scaler', StandardScaler()),
            ('lasso', Lasso(max_iter = 100000))
            ])
    param_grid = [{'lasso__alpha': np.logspace(-1, -4, 10)}]
    tscv = TimeSeriesSplit(max_train_size=None, n_splits=5)
    
    scores = []
    lengthList=[]
    coefList=[]
    lasso_y_pred_list=[]
    lamList=[]
    for i in range(len(yy)-w):
            cv_ess = []
            #Define the training and validation sets
            X_train= XX.iloc[i:w+i]
            y_train = yy.iloc[i:w+i]
            X_test = XX.iloc[w+i]
            y_test = yy.iloc[w+i]
            scaler = StandardScaler()
            X_train = scaler.fit_transform(X_train)
            X_test = scaler.transform([X_test])
            ##about lambda
            grid_cv = GridSearchCV(estimator=pipe, cv=tscv, param_grid=param_grid, n_jobs = -1)
            grid_cv.fit(X_train, y_train)
            #1
            coefIndex=np.nonzero(grid_cv.best_estimator_.steps[1][1].coef_)
            lengthList.append(len(coefIndex[0]))
            coefList=np.concatenate((coefList,coefIndex),axis=None)
            a=grid_cv.best_params_['lasso__alpha']
            lamList.append(a)
        
            model_type = Lasso(alpha =a, max_iter = 100000)
            model = model_type.fit(X_train, y_train) 
            #2
            y_pred=model.predict(X_test)
            lasso_y_pred_list.append(y_pred[0])
    co=pd.DataFrame(coefList)
    bestv[5*m:(5*(m+1)-1)]=co.iloc[:,0].value_counts().index[0:5]
    y_pred_lasso = lasso_y_pred_list
    se_lasso=(y_pred_lasso-y[80:])**2
    #    se_full=(y_pred_full-y[80:])**2
    lasso_sse[m] =[0]*(lambda_list.shape[0]-len(se_lasso))+np.cumsum(se_lasso).values.tolist()
    #    full_sse=np.cumsum(se_full)
    lambda_list[m] =[0]*(lambda_list.shape[0]-len(lamList))+lamList
lasso_sse.to_csv("lasso_sse.csv")
lambda_list.to_csv("lambda_list.csv")
variable=pd.DataFrame()
for i in range(49):
    variable[i]=bestv[5*i:(5*(i+1))]
variable.to_csv("variable.csv")
bestvariable=pd.DataFrame()
bestvariable['var']=list(set(variable.iloc[0,:].tolist()+variable.iloc[1,:].tolist()))
bestvariable.to_csv("bestvariable.csv")
bestvariable['var'].tolist()
best=bestvariable['var'].tolist()
data3=pd.DataFrame()
data3=data.iloc[:,0:(len(best)+3)]
for i in range(len(best)):
    data3.iloc[:,(i+3)]=data.iloc[:,(int(best[i])+3)]
data3.set_index(["date"], inplace=True)
data3.to_csv("data.csv")

