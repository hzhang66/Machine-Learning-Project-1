x# Clean workspace
rm(list = ls())
setwd("/Users/auroracappadocian/Downloads/431ML/Project")
# Load packages
library(foreign) 
library(data.table) 
library(lfe) 
library(glmnet) 
library(Metrics) 
library(ggplot2)
library(lubridate)
StockRetAcct_DT1<- as.data.table(read.csv("data2.csv"))
rf<- as.data.table(read.csv("rf.csv"))
StockRetAcct_DT1[, date:= ymd(floor(public_date/100), truncated=2)]
rf[, date := ymd(floor(caldt/100), truncated=2)]
StockRetAcct_DT1 <- StockRetAcct_DT1[indret_vw != "NA"]
StockRetAcct_DT1<-merge(StockRetAcct_DT1, rf,  by='date')
StockRetAcct_DT1[, ExRet := indret_vw-t30ret]
StockRetAcct_DT1 = StockRetAcct_DT1[,c(1,3,74,7:70)]
setkey(StockRetAcct_DT1, date)

#After Python
StockRetAcct_DT<- as.data.table(read.csv("data.csv"))
StockRetAcct_DT[, date:= ymd(date)]
setkey(StockRetAcct_DT, date)
#Lasso Result
lasso_sse<- as.data.table(read.csv("lasso_sse.csv"))
full_sse<-as.data.table(read.csv("full_sse.csv"))
sse<-c()
date=as.vector(unlist(lasso_sse[,c(1)]))
for (k in colnames(lasso_sse)[2:49]){
  sse=c(sse,unlist(lasso_sse[,get(k)]))
}
sse=c(sse,unlist(full_sse[,2]))
LassoSSE<-data.table(Date=ymd(rep(as.vector(unlist(lasso_sse[,c(1)])),49)),industry=c(as.character(ceiling(c(1:24960)/520)),rep('Full',520)),Lasso_SSE=sse)
ggplot(data = LassoSSE, mapping = aes(x =Date, y = Lasso_SSE, colour = industry)) + geom_line()

variable<- as.data.table(read.csv("variable.csv"))
var<-c()
for (k in colnames(variable)[2:49]){
  for (j in 1:5){
    var=c(var,rep(variable[j,get(k)],(6-j)))
  }
}
Variable<-data.table(Industry=as.character(ceiling(c(1:7200)/150)),Financial_Ratio=var)
p = ggplot(data=Variable, mapping=aes(x=Industry, y=Financial_Ratio, fill=Industry))
p + geom_violin(alpha=1, width=1)
p+ geom_jitter(alpha=0.7, width=2,shape=21)
var<-c()
for (k in colnames(variable)[2:49]){
  var=c(var,variable[,get(k)])
}
Variable2<-data.table(Frequency=rep(as.vector(1+unlist(variable[,c(1)])),48),Industry=as.character(ceiling(c(1:240)/5)),Financial_Ratio=var)
ggplot(data = Variable2, mapping = aes(x = Financial_Ratio, y = Frequency, fill = Industry)) + geom_bar(stat= 'identity', position = 'stack') + geom_text(mapping = aes(label = Frequency),size = 1, colour = 'black', vjust = 3.5, hjust = .5, position = position_stack())
library(reshape2)
standardized_StockRetAcct_DT1 = copy(StockRetAcct_DT1)
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3)]) {
  standardized_StockRetAcct_DT1 = standardized_StockRetAcct_DT1[!is.na(get(i))] 
}
# Create demeaned dataset
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3,68)]){
  standardized_StockRetAcct_DT1[, paste0(i) := (get(i) - mean(get(i))), by = date]
}
fina_ratio=standardized_StockRetAcct_DT1[,-c(1:3,68)] 
colnames(fina_ratio)<-as.character(c(1:64))
cormat <- round(cor(fina_ratio),2)
melted_cormat <- melt(cormat)
colnames(melted_cormat)<-c('Financial_Ratio','financial_ratio','value')
ggplot(data = melted_cormat, aes(x=Financial_Ratio, y=financial_ratio, fill = value)) +geom_tile(color = "white") +scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1, 1), space = "Lab",name="Pearson\nCorrelation") +
  theme_minimal() +theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed() 
ggpairs(StockRetAcct_DT1[-c(1:3,68)], diag = list("continuous"="blank"))

#heat map
library(reshape2)
standardized_StockRetAcct_DT1 = copy(StockRetAcct_DT1)
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3)]) {
  standardized_StockRetAcct_DT1 = standardized_StockRetAcct_DT1[!is.na(get(i))] 
}
# Create demeaned dataset
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3,68)]){
  standardized_StockRetAcct_DT1[, paste0(i) := (get(i) - mean(get(i))), by = date]
}
fina_ratio=standardized_StockRetAcct_DT1[,-c(1:3,68)] 
colnames(fina_ratio)<-as.character(c(1:64))
cormat <- round(cor(fina_ratio),2)
melted_cormat <- melt(cormat)
colnames(melted_cormat)<-c('Financial_Ratio','financial_ratio','value')
ggplot(data = melted_cormat, aes(x=Financial_Ratio, y=financial_ratio, fill = value)) +geom_tile(color = "white") +scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint = 0, limit = c(-1, 1), space = "Lab",name="Pearson\nCorrelation") +
  theme_minimal() +theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  coord_fixed() 
ggpairs(StockRetAcct_DT1[-c(1:3,68)], diag = list("continuous"="blank"))


#Elastic net
# Create a constant
StockRetAcct_DT[, Constant := 1]
standardized_StockRetAcct_DT = copy(StockRetAcct_DT)
for (i in colnames(standardized_StockRetAcct_DT)[-c(1:3)]) {
  standardized_StockRetAcct_DT = standardized_StockRetAcct_DT[!is.na(get(i))] 
}
# Create demeaned dataset
for (i in colnames(standardized_StockRetAcct_DT)[-c(1:3,41)]){
  standardized_StockRetAcct_DT[, paste0(i) := (get(i) - mean(get(i))), by = date]
}
standardized_StockRetAcct_DT[, Training_data := date <= "2004-01-01"]
N_factors = ncol(standardized_StockRetAcct_DT) - 4
Factor_matrix = matrix(NA, nrow = length(unique(standardized_StockRetAcct_DT$date)), ncol=N_factors)
monthlist=sort(unique(standardized_StockRetAcct_DT$date))
for (i in 1:length(monthlist)){
  Factor_matrix[i, 1:N_factors] = t(data.matrix(standardized_StockRetAcct_DT[date == monthlist[i], -c('date', 'FFI49_desc', 'ExRet','Training_data')])) %*% data.matrix(standardized_StockRetAcct_DT[date == monthlist[i], .(ExRet)])
}
colnames(Factor_matrix) = colnames(standardized_StockRetAcct_DT[,-c('date', 'FFI49_desc', 'ExRet','Training_data')])
rownames(Factor_matrix) = sort(unique(standardized_StockRetAcct_DT$date))

in_sample_Factor_matrix = Factor_matrix[1:420, 1:N_factors] 
for (i in 1:5){
  # Define sample
  Factor_sample_avg_ret = colMeans(in_sample_Factor_matrix[-((5*(i-1)+1):(5*i)),]) 
  Factor_sample_var_matrix =var(in_sample_Factor_matrix[-((5*(i-1)+1):(5*i)),])
  #Run elastic net
  assign(paste0('Elastic_net_', i), glmnet(Factor_sample_var_matrix, Factor_sample_avg_ret, family = 'gaussian', alpha = 0.5, standardize = TRUE))
  # Define a range of lambdas
  if (i == 1) {
    S_min = min(Elastic_net_1$lambda) 
    S_max = max(Elastic_net_1$lambda)
  } 
  else {
    S_min = min(S_min, get(paste0('Elastic_net_', i))$lambda) 
    S_max = max(S_max, get(paste0('Elastic_net_', i))$lambda)
  }
}

# Predict factor returns and compare
range_of_lambda = seq(S_min - 1, S_max + 1, 10^9)
MSE_lambda = matrix(NA, nrow = 6, ncol = length(range_of_lambda)) 
for (i in 1:5) {
  b_matrix = as.matrix(predict(get(paste0('Elastic_net_', i)),type = 'coef', s = range_of_lambda))[-1, ]
  for (j in 1:length(range_of_lambda)){ 
    MSE_lambda[i, j] = mse(b_matrix[, j] %*% var(in_sample_Factor_matrix[(5*(i-1)+1):(5*i),]), colMeans(in_sample_Factor_matrix[(5*(i-1)+1):(5*i),]))
  }
}
MSE_lambda[6,] = colMeans(MSE_lambda[1:5,])
Best_lambda = range_of_lambda[which(MSE_lambda[6,] == min(MSE_lambda[6,]), arr.ind=TRUE)] 
#0.9877450980392108*S_max
# Calculate b_vector for Best_lambda
Factor_avg_ret = colMeans(Factor_matrix[1:420, 1:N_factors])
Factor_var_matrix = var(Factor_matrix[1:420, 1:N_factors])
b_vector = glmnet(Factor_var_matrix, Factor_avg_ret, family = 'gaussian', alpha = 0.5, standardize = TRUE, lambda =Best_lambda)$beta
sum(b_vector!=0)
b_vector = data.matrix(b_vector)
Out_of_sample_ret = Factor_matrix[-(1:420),] %*% b_vector

#Calculate Market
CRSP_Stocks <- as.data.table(read.csv("crsp.csv"))
CRSP_Stocks[, date:= ymd(date)]
Mktpf <- function(CRSP_Stocks){
  
  # set keys, sort by date
  setkey(CRSP_Stocks, date)
  
  # set the year and month as integer
  CRSP_Stocks[,Year:= year(date)]
  CRSP_Stocks[,Month:= month(date)]
  
  #clean the data
  # Filter by sharecode 10 and 11  and Filter by EXCHCD 1 2 3
  CRSP_Stocks <- CRSP_Stocks[SHRCD %in% c(10,11)]
  CRSP_Stocks <- CRSP_Stocks[EXCHCD %in% c(1,2,3)]
  
  # convert the ret and delisting return into numeric type
  CRSP_Stocks[, RET := as.numeric(as.character(RET))]
  CRSP_Stocks[, DLRET := as.numeric(as.character(DLRET))]
  CRSP_Stocks[, `:=`(Return, ifelse(is.na(DLRET),RET,ifelse(is.na(RET), DLRET, (1+DLRET)*(1+RET)-1 )))]
  CRSP_Stocks <- CRSP_Stocks[Return != "NA"]
  
  # convert the price and number of shares into numeric type
  CRSP_Stocks[, PRC := as.numeric(as.character(PRC))]
  CRSP_Stocks <- CRSP_Stocks[PRC != "NA"]
  CRSP_Stocks[, SHROUT := as.numeric(as.character(SHROUT))]
  CRSP_Stocks <- CRSP_Stocks[SHROUT != "NA"]
  # calculate the marketcap
  CRSP_Stocks[, ME := abs(PRC) * abs(SHROUT)*1000]
  
  #lag the market cap of each firm
  CRSP_Stocks[, mktCapLagged := shift(ME), by=c("PERMNO")]
  CRSP_Stocks <- CRSP_Stocks[mktCapLagged != "NA"]
  
  # value weight portfolio
  
  valueweight <- CRSP_Stocks[,list(Stock_Vw_Ret = weighted.mean(Return, mktCapLagged)),
                             by=list(Year, Month)]
  Monthly_CRSP_Stocks <- valueweight[Year > 2004]
  return(Monthly_CRSP_Stocks)
}
MktRet <- Mktpf(CRSP_Stocks)


# Scale MVE portfolio to match market
Scaled_Out_of_sample_ret = data.table((Out_of_sample_ret-mean(Out_of_sample_ret))*sd(MktRet$Stock_Vw_Ret)/sd(Out_of_sample_ret)+mean(MktRet$Stock_Vw_Ret))
setnames(Scaled_Out_of_sample_ret, 's0', 'Elastic_Ret')
Scaled_Out_of_sample_ret[, Cum_Ret := cumprod(1 + Elastic_Ret)] 
MktRet[, Cum_Ret := cumprod(1 + Stock_Vw_Ret)]

#Calculate the full model
# Create a constant
StockRetAcct_DT1[, Constant := 1]
standardized_StockRetAcct_DT1 = copy(StockRetAcct_DT1)
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3)]) {
  standardized_StockRetAcct_DT1 = standardized_StockRetAcct_DT1[!is.na(get(i))] 
}
# Create demeaned dataset
for (i in colnames(standardized_StockRetAcct_DT1)[-c(1:3,68)]){
  standardized_StockRetAcct_DT1[, paste0(i) := (get(i) - mean(get(i))), by = date]
}
standardized_StockRetAcct_DT1[, Training_data := date <= "2004-01-01"]
N_factors1 = ncol(standardized_StockRetAcct_DT1) - 4
Factor_matrix1 = matrix(NA, nrow = length(unique(standardized_StockRetAcct_DT1$date)), ncol=N_factors1)
for (i in 1:length(monthlist)){
  Factor_matrix1[i, 1:N_factors1] = t(data.matrix(standardized_StockRetAcct_DT1[date == monthlist[i], -c('date', 'FFI49_desc', 'ExRet','Training_data')])) %*% data.matrix(standardized_StockRetAcct_DT1[date == monthlist[i], .(ExRet)])
}
colnames(Factor_matrix1) = colnames(standardized_StockRetAcct_DT1[,-c('date', 'FFI49_desc', 'ExRet','Training_data')])
rownames(Factor_matrix1) = sort(unique(standardized_StockRetAcct_DT1$date))

in_sample_Factor_matrix1 = Factor_matrix1[1:420, 1:N_factors1] 
for (i in 1:5){
  # Define sample
  Factor_sample_avg_ret1 = colMeans(in_sample_Factor_matrix1[-((5*(i-1)+1):(5*i)),]) 
  Factor_sample_var_matrix1 =var(in_sample_Factor_matrix1[-((5*(i-1)+1):(5*i)),])
  #Run elastic net
  assign(paste0('Elastic_net_', i), glmnet(Factor_sample_var_matrix1, Factor_sample_avg_ret1, family = 'gaussian', alpha = 0.5, standardize = TRUE))
  # Define a range of lambdas
  if (i == 1) {
    S_min = min(Elastic_net_1$lambda) 
    S_max = max(Elastic_net_1$lambda)
  } 
  else {
    S_min = min(S_min, get(paste0('Elastic_net_', i))$lambda) 
    S_max = max(S_max, get(paste0('Elastic_net_', i))$lambda)
  }
}

# Predict factor returns and compare
range_of_lambda = seq(S_min - 1, S_max + 1, 10^9)
MSE_lambda = matrix(NA, nrow = 6, ncol = length(range_of_lambda)) 
for (i in 1:5) {
  b_matrix = as.matrix(predict(get(paste0('Elastic_net_', i)),type = 'coef', s = range_of_lambda))[-1, ]
  for (j in 1:length(range_of_lambda)){ 
    MSE_lambda[i, j] = mse(b_matrix[, j] %*% var(in_sample_Factor_matrix1[(5*(i-1)+1):(5*i),]), colMeans(in_sample_Factor_matrix1[(5*(i-1)+1):(5*i),]))
  }
}
MSE_lambda[6,] = colMeans(MSE_lambda[1:5,])
Best_lambda = range_of_lambda[which(MSE_lambda[6,] == min(MSE_lambda[6,]), arr.ind=TRUE)] 
#0.9877450980392108*S_max
# Calculate b_vector for Best_lambda
Factor_avg_ret1 = colMeans(Factor_matrix1[1:420, 1:N_factors1])
Factor_var_matrix1 = var(Factor_matrix1[1:420, 1:N_factors1])
b_vector_full = glmnet(Factor_var_matrix1, Factor_avg_ret1, family = 'gaussian', alpha = 0.5, standardize = TRUE, lambda =Best_lambda)$beta
sum(b_vector_full!=0)
b_vector_full = data.matrix(b_vector_full)
Out_of_sample_ret_full = Factor_matrix1[-(1:420),] %*% b_vector_full

# Scale MVE portfolio to match market
Scaled_Out_of_sample_ret_full = data.table((Out_of_sample_ret_full-mean(Out_of_sample_ret_full))*sd(MktRet$Stock_Vw_Ret)/sd(Out_of_sample_ret_full)+mean(MktRet$Stock_Vw_Ret))
setnames(Scaled_Out_of_sample_ret_full, 's0', 'Elastic_Ret')
Scaled_Out_of_sample_ret_full[, Cum_Ret := cumprod(1 + Elastic_Ret)] 
# Calculate cumulative returns
RETURN=data.table(Date=ymd(rep(as.vector(unlist(as.character(monthlist[421:596]))),3)),Cumulative_Excess_Return=c(as.vector(unlist(MktRet$Cum_Ret[1:176])),as.vector(unlist(Scaled_Out_of_sample_ret$Cum_Ret[1:176])),as.vector(unlist(Scaled_Out_of_sample_ret_full$Cum_Ret[1:176]))),Porfolio=c(rep('Value-weighted Industry portfolio',176),rep('MVE portfolio after initial selection',176),rep('MVE portfolio without initial selection',176)))
#Plot
ggplot(data = RETURN, mapping = aes(x = Date, y = Cumulative_Excess_Return, colour = Porfolio))+ geom_line() 
