#### import package ####
library("caret")
library("tidyverse")
library("randomForest")
library("pdp")
library("iml")
library("ggfortify")
library("Hmisc")
library("gbm")
library("xgboost")
library("shapviz")
library("ggplot2")
library("ggtext")
library("car")
library("SuperLearner")
library("Rmisc")
library("fastshap")
library("ggpubr")
library("ggthemes")
library("ggsci")
library("ggbeeswarm")
library("LSD")
library("ranger")
####
#### 2.model tuning, 10-fold CV，best hyperparameter,
#### 3.fireinded model simulation, factor importance

graphics.off()
options(digits=10)


### Load data and pre-processing   combine belon  CZ-KrP DE-Kli DE-RuS DE-Geb FR-Aur FR-Lam
CAdataset<-read.csv("E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/predictors-combine_onlyWW-David_12factors-1201.csv")
### response parameter
CAdataset$albedo <-as.numeric(as.character(CAdataset$albedo))

#### continueous predictors
#CAdataset$ws <-as.numeric(as.character(CAdataset$ws))
CAdataset$ws <-as.numeric(as.character(CAdataset$ws))  # air temperature
CAdataset$Ta <-as.numeric(as.character(CAdataset$Ta))  # air temperature
CAdataset$Ts <-as.numeric(as.character(CAdataset$Ts))  # soil temperature
CAdataset$SWC <-as.numeric(as.character(CAdataset$SWC))   # soil water
CAdataset$VPD <-as.numeric(as.character(CAdataset$VPD)) # vpd

#CAdataset$GPP<-as.numeric(as.character(CAdataset$GPP)) # gpp
CAdataset$Bowenratio<-as.numeric(as.character(CAdataset$Bowenratio)) # bowen ratio
CAdataset$Kd<-as.numeric(as.character(CAdataset$Kd))  # diffused radiance
CAdataset$NEE<-as.numeric(as.character(CAdataset$NEE))  # NEE
CAdataset$Pre<-as.numeric(as.character(CAdataset$Pre))  # Precipitation
#CAdataset$Pre.label <-as.factor(as.character(CAdataset$Pre.label))
CAdataset$PA<-as.numeric(as.character(CAdataset$PA))  # PA
CAdataset$Clayratio<-as.numeric(as.character(CAdataset$Clayratio))  # clay
CAdataset$Sandratio<-as.numeric(as.character(CAdataset$Sandratio))  # sand
CAdataset$Siltratio<-as.numeric(as.character(CAdataset$Siltratio))  # silt

CAdataset$TCa <-as.numeric(as.character(CAdataset$TCa))
#CAdataset$CaFer. <-as.factor(as.character(CAdataset$CaFer.))

CAdataset$TN <-as.numeric(as.character(CAdataset$TN))
#CAdataset$NFer.<-as.factor(as.character(CAdataset$NFer.))

CAdataset$TMg <-as.numeric(as.character(CAdataset$TMg))
#CAdataset$MgFer.<-as.factor(as.character(CAdataset$MgFer.))

CAdataset$TS <-as.numeric(as.character(CAdataset$TS))

CAdataset$TFug <-as.numeric(as.character(CAdataset$TFug))
#CAdataset$Fpes. <-as.factor(as.character(CAdataset$Fpes.))

CAdataset$THer<-as.numeric(as.character(CAdataset$THer))
CAdataset$TIns<-as.numeric(as.character(CAdataset$TIns))
CAdataset$Tgr<-as.numeric(as.character(CAdataset$Tgr))
#CAdataset$GRpes. <-as.factor(as.character(CAdataset$GRpes.))

CAdataset$Tss<-as.numeric(as.character(CAdataset$Tss))
#CAdataset$SSpes. <-as.factor(as.character(CAdataset$SSpes.))

CAdataset$SiteIden<-as.factor(as.character(CAdataset$SiteIden))

CAdataset$TTil<-as.numeric(as.character(CAdataset$TTil))
CAdataset$DTil<-as.numeric(as.character(CAdataset$DTil))

CAdataset$TMan<-as.numeric(as.character(CAdataset$TMan))
#CAdataset$ManFer. <-as.factor(as.character(CAdataset$ManFer.))

#CAdataset$TPlo<-as.numeric(as.character(CAdataset$TPlo))
#CAdataset$DPlo<-as.numeric(as.character(CAdataset$DPlo))
#FEsow.
CAdataset$THar<-as.numeric(as.character(CAdataset$THar))
CAdataset$PartHar<-as.factor(as.character(CAdataset$PartHar))

CAdataset$Tsow<-as.numeric(as.character(CAdataset$Tsow))
#CAdataset$FEsow. <-as.factor(as.character(CAdataset$FEsow.))
#print(colnames(CAdataset))Pre.label
CAdataset1<-CAdataset[,c("albedo","ws","Ts","SWC","Bowenratio","Kd","Pre","PA",
                         "Clayratio","Sandratio","TCa","TN","TMg",
                         "TS","TFug","THer","TIns","Tgr","Tss","SiteIden","TTil","TMan","THar","Tsow")]

#==========================using R package RFE ========================================

#==========================using  RFE by myself ========================================


n<-nrow(CAdataset1)
set.seed(10)
# training and testing dataset
indextrain <- sample(1:n,round(0.8*n),replace=FALSE)  # 对每一次的模拟进行验证
#
CAdataset_train <- CAdataset1[indextrain,] # extract 80% X/Y to train
CAdataset_train00<-CAdataset_train
###print(CAdataset_train[,4:ncol(CAdataset_train)]) #
CAdataset_out_of_box<-CAdataset1[-indextrain,] # OOB tesing data


#print(rfe_varaibles)
# "gpp"   "sm"    "temp"  "vpd"   "bowen"

######### model simulation  #####################################################
n1<-ncol(CAdataset_train)
nam <- names(CAdataset_train)   # extract names of variables
#print(nam)
nam1<- paste(nam[2:n1], collapse = " + ") # 自变量的名字
#nam1<- paste(nam[3:5], collapse = " + ")
f <- paste("albedo ~", nam1) # connect strings of X and Y

f <- as.formula(f) # convert strings to formula

RMSEip<-matrix(0, nrow =40, ncol = 1)
R2ip<-matrix(0, nrow =40, ncol = 1)

#tempairip<-matrix(0, nrow =40, ncol = 1)
tempip<-matrix(0, nrow =40, ncol = 1)
smip<-matrix(0, nrow =40, ncol = 1)
ustarip<-matrix(0, nrow =40, ncol = 1)
nrip<-matrix(0, nrow =40, ncol = 1)
gip<-matrix(0, nrow =40, ncol = 1)
#vpdip<-matrix(0, nrow =40, ncol = 1)
wsip<-matrix(0, nrow =40, ncol = 1)
gppip<-matrix(0, nrow =40, ncol = 1)
bowenip<-matrix(0, nrow =40, ncol = 1)
diffuseradianceip<-matrix(0, nrow =40, ncol = 1)

Preciip<-matrix(0, nrow =40, ncol = 1)
Prelabelciip<-matrix(0, nrow =40, ncol = 1)
PAip<-matrix(0, nrow =40, ncol = 1)
clayip<-matrix(0, nrow =40, ncol = 1)
sandip<-matrix(0, nrow =40, ncol = 1)


nip<-matrix(0, nrow =40, ncol = 1)
caip<-matrix(0, nrow =40, ncol = 1)
mgip<-matrix(0, nrow =40, ncol = 1)
sip<-matrix(0, nrow =40, ncol = 1)
otherip<-matrix(0, nrow =40, ncol = 1)

nelement<-matrix(0, nrow =40, ncol = 1)
caelement<-matrix(0, nrow =40, ncol = 1)
mgelement<-matrix(0, nrow =40, ncol = 1)
selement<-matrix(0, nrow =40, ncol = 1)
otherelement<-matrix(0, nrow =40, ncol = 1)

funip<-matrix(0, nrow =40, ncol = 1)
herip<-matrix(0, nrow =40, ncol = 1)
insip<-matrix(0, nrow =40, ncol = 1)
grip<-matrix(0, nrow =40, ncol = 1)
ssip<-matrix(0, nrow =40, ncol = 1)

Fpes<-matrix(0, nrow =40, ncol = 1)
Hpes<-matrix(0, nrow =40, ncol = 1)
Inpes<-matrix(0, nrow =40, ncol = 1)
GRpes<-matrix(0, nrow =40, ncol = 1)
SSpes<-matrix(0, nrow =40, ncol = 1)


TManip<-matrix(0, nrow =40, ncol = 1)
manelement<-matrix(0, nrow =40, ncol = 1)

TSowip<-matrix(0, nrow =40, ncol = 1)
TSowid<-matrix(0, nrow =40, ncol = 1)
tillaip<-matrix(0, nrow =40, ncol = 1)
tillaid<-matrix(0, nrow =40, ncol = 1)
ploughip<-matrix(0, nrow =40, ncol = 1)
ploughid<-matrix(0, nrow =40, ncol = 1)
harvestip<-matrix(0, nrow =40, ncol = 1)
harvestpartip<-matrix(0, nrow =40, ncol = 1)

#FEsow.
sitelabel<-matrix(0, nrow =40, ncol = 1)
croptype<-matrix(0, nrow =40, ncol = 1)

################# RFE ######################################################################################
primary_importance<-matrix(0, nrow =n1-1, ncol = 2)
ij<-1
n11<-ncol(CAdataset_train00)
#"tempair","temp","sm","vpd","gpp","bowen","diffuseradiance","NEE","Precipitation","PA","clay","sand","silt","soiltexture","fertilizers","pesticides","tillage","ploughing","harvest"

for (l in 1:(n1-4)){
  print(l)
  # ,importance= 'impurity'
  # print(n11)
  # gbm  randomForest
  rf0<-ranger(f,data = CAdataset_train00,
                    num.trees=800, mtry=max(floor((n11-1)/3), 1), importance = "impurity", 
                    seed = 10)

  importance_rank <- data.frame(importance(rf0,type = "gini"))
 # print(importance_rank)
  importance_rankcopy<-importance_rank
  # print(rownames(importance_rank))
  importance_rankcopy$name<-row.names(importance_rankcopy)
#  print(importance_rankcopy)
  
  colnames(importance_rankcopy) <- c("gini","name")
 #  print(importance_rankcopy)
  importance_rankcopy <- importance_rankcopy[order(importance_rankcopy$gini,
                                           decreasing = TRUE), ]
  #
  imp_data <-as.data.frame(importance_rankcopy)
 # print(imp_data)
  # rf0<-ranger(f,data=CAdataset_train00,num.tree = 1000,mtry=3,keep.inbag=T,importance= 'impurity')
  
  #  print(rf0$variable.importance)
  #  importance_rank <- data.frame(rownames(rf0$variable.importance),rf0$variable.importance)
  
  
  # "albedo","Ts","SWC","GPP","Bowenratio","Kd","Pre.","PA","Clayratio","Sandratio","PartHar.","EFer.","TypePes.","TFer.","TPes.","TTil.","TPlo.","THar."
  if (l==1){
   # print(primary_importance)
   # print(imp_data)
    primary_importance[,1]<-rownames(imp_data)  # names of predictors
    primary_importance[,2]<-importance_rankcopy[,1]  # importance valuse of predictors
  }
 # print(primary_importance)
  # 对于每一次模拟，循环导入得到的每一个变量的importance得分
  for (j in 1:nrow(importance_rankcopy)){
    #if ("tempair" %in% rownames(imp_data)){
    #  tempairip[ij]<-importance_rank[which(rownames(imp_data)=="tempair"),2]
    #}
    if ("ws" %in% rownames(imp_data)){
      wsip[ij]<-importance_rankcopy[which(rownames(imp_data)=="ws"),1]
    }
    if ("Ts" %in% rownames(imp_data)){
      tempip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Ts"),1]
    }
    if ("SWC" %in% rownames(imp_data)){
      smip[ij]<-importance_rankcopy[which(rownames(imp_data)=="SWC"),1]
    }

    if ("GPP" %in% rownames(imp_data)){
      gppip[ij]<-importance_rankcopy[which(rownames(imp_data)=="GPP"),1]
    }
    if ("Bowenratio" %in% rownames(imp_data)){
      bowenip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Bowenratio"),1]
    }
    if ("Kd" %in% rownames(imp_data)){
      diffuseradianceip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Kd"),1]
    }

    if ("Pre" %in% rownames(imp_data)){
      Preciip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Pre"),1]
    }

    if ("PA" %in% rownames(imp_data)){
      PAip[ij]<-importance_rankcopy[which(rownames(imp_data)=="PA"),1]
    }
    if ("Clayratio" %in% rownames(imp_data)){
      clayip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Clayratio"),1]
    }
    if ("Sandratio" %in% rownames(imp_data)){
      sandip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Sandratio"),1]
    }

    if ("TCa" %in% rownames(imp_data)){
      caip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TCa"),1]
    }
    if ("TN" %in% rownames(imp_data)){
      nip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TN"),1]
    }
    if ("TMg" %in% rownames(imp_data)){
      mgip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TMg"),1]
    }
    if ("TS" %in% rownames(imp_data)){
      sip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TS"),1]
    }
   # if ("Tother" %in% rownames(imp_data)){
   #   otherip[ij]<-importance_rank[which(rownames(imp_data)=="Tother"),2]
   # }
    if ("TFug" %in% rownames(imp_data)){
      funip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TFug"),1]
    }
    if ("THer" %in% rownames(imp_data)){
      herip[ij]<-importance_rankcopy[which(rownames(imp_data)=="THer"),1]
    }
    if ("TIns" %in% rownames(imp_data)){
      insip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TIns"),1]
    }

    if ("Tgr" %in% rownames(imp_data)){
      grip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tgr"),1]
    }
    if ("Tss" %in% rownames(imp_data)){
      ssip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tss"),1]
    }

    if ("TMan" %in% rownames(imp_data)){
      TManip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TMan"),1]
    }
    
    if ("TTil" %in% rownames(imp_data)){
      tillaip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TTil"),1]
    }
    #if ("DTil" %in% rownames(imp_data)){
     # tillaid[ij]<-importance_rankcopy[which(rownames(imp_data)=="DTil"),1]
    #}
    #if ("TPlo" %in% rownames(imp_data)){
    #  ploughip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TPlo"),1]
   # }
   # if ("DPlo" %in% rownames(imp_data)){
  #    ploughid[ij]<-importance_rankcopy[which(rownames(imp_data)=="DPlo"),1]
  #  }
    if ("THar" %in% rownames(imp_data)){
      harvestip[ij]<-importance_rankcopy[which(rownames(imp_data)=="THar"),1]
    }
    #if ("PartHar" %in% rownames(imp_data)){
    #  harvestpartip[ij]<-importance_rankcopy[which(rownames(imp_data)=="PartHar"),1]
    #}

    if ("SiteIden" %in% rownames(imp_data)){
      sitelabel[ij]<-importance_rankcopy[which(rownames(imp_data)=="SiteIden"),1]
    }
    if ("Tsow" %in% rownames(imp_data)){
      TSowip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tsow"),1]
    }

    vali_oob<-select(CAdataset_out_of_box,rownames(imp_data))
    # print(ncol(vali_oob))
    pred222 <- predict(rf0,vali_oob) # 用testing的X部分进行预测
  #  print(pred222$predictions)
    RMSE<-sqrt(mean((pred222$predictions-CAdataset_out_of_box$albedo)^2))  # calculate RMSE of predicted Y and testing Y
    RMSEip[ij]<-RMSE
    ### calculate R2
    rss <- sum((pred222$predictions-CAdataset_out_of_box$albedo) ^ 2)  ## residual sum of squares
    tss <- sum((CAdataset_out_of_box$albedo - mean(CAdataset_out_of_box$albedo)) ^ 2)  ## total sum of squares
    rsq <- 1 - rss/tss
    R2ip[ij]<-rsq
  }
  #rownames(imp_data)[length(rownames(imp_data))]
  name_last<-rownames(imp_data)[length(rownames(imp_data))] #最不重要因子的名字
  CAdataset_train00<-select(CAdataset_train00,-name_last) #吧最不重要因子的行删除  CAdataset_train00[-nrow(importance_rank),]
  # print(CAdataset_train00)
  n11<-ncol(CAdataset_train00)
  nam <- names(CAdataset_train00)   # extract names of variables
  nam1<- paste(nam[2:n11], collapse = " + ") # 自变量的名字
  #nam1<- paste(nam[3:5], collapse = " + ")
  f <- paste("albedo ~", nam1) # connect strings of X and Y
  
  f <- as.formula(f) # convert strings to formula
  ij<-ij+1
}

name_list_dele<-setdiff(names(CAdataset_train),names(CAdataset_train00))
n2<-nrow(primary_importance)
#print(primary_importance)
for (l in 4:n2){
  
  nextip<-primary_importance[l,1]
  # print(names(CAdataset_train00))
  #print(paste(names(CAdataset_train00),nextip),sep = '')
  #name2<-paste(names(CAdataset_train00),nextip)
  #print(CAdataset_train)
  #print(CAdataset_train00)
  CAdataset_train3<-subset(CAdataset_train,select=names(CAdataset_train00))
  CAdataset_train3<-transform(CAdataset_train3,nextstep = CAdataset_train[,nextip])
  names(CAdataset_train3)<-c(names(CAdataset_train00),nextip)
  #CAdataset_train3$nextip<-CAdataset_train[,nextip]
  #  print(names(CAdataset_train3))
  nam22<- paste(names(CAdataset_train3)[2:ncol(CAdataset_train3)], collapse = " + ")
  #print(nam22)
  f <- paste("albedo ~", nam22) # connect strings of X and Y
  # print(f)
  f <- as.formula(f) # convert strings to formula
  # print(f)
  # randomForest
  # rf3<-randomForest(f,distribution = "gaussian",data = CAdataset_train3,
  #                 ntrees=500, mtry=max(floor((ncol(CAdataset_train3)-1)/3), 1), keep.data = TRUE,verbose = FALSE,importance = TRUE)
  rf3<-ranger(f,data = CAdataset_train3,
                    num.trees=800, mtry=max(floor((ncol(CAdataset_train3)-1)/3), 1), importance = "impurity", 
                    seed = 10)
  # print(rf0$variable.importance)
  # importance_rank <- rf3$variable.importance
  
  importance_rank <- data.frame(importance(rf3,type = "gini"))
  importance_rankcopy<-importance_rank
  # print(rownames(importance_rank))
  importance_rankcopy$name<-row.names(importance_rankcopy)
  #  print(importance_rankcopy)
  
  colnames(importance_rankcopy) <- c("gini","name")
  #  print(importance_rankcopy)
  importance_rankcopy <- importance_rankcopy[order(importance_rankcopy$gini,
                                                   decreasing = TRUE), ]
  #
  imp_data2 <-as.data.frame(importance_rankcopy)
  
  vali_oob2<-select(CAdataset_out_of_box,rownames(imp_data2))
  pred222 <- predict(rf3, vali_oob2) # 用testing的X部分进行预测
  RMSE<-sqrt(mean((pred222$predictions-CAdataset_out_of_box$albedo)^2))  # calculate RMSE of predicted Y and testing Y
  RMSEip[ij]<-RMSE
  ### calculate R2
  rss <- sum((pred222$predictions-CAdataset_out_of_box$albedo) ^ 2)  ## residual sum of squares
  tss <- sum((CAdataset_out_of_box$albedo - mean(CAdataset_out_of_box$albedo)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  R2ip[ij]<-rsq
  
  if (R2ip[ij]-R2ip[ij-1]>=0.005){
    CAdataset_train00<-CAdataset_train3
    #  print(names(CAdataset_train00))
    # names(CAdataset_train3)<-c(names(CAdataset_train00),nextip)
    # CAdataset_train00$nextip<-select(CAdataset_train,nextip)
    
    # CAdataset_train00<-CAdataset_train00+
  }
  ij<-ij+1
}


################# RFE finish ######################################################################################
##nam1<- paste(nam[2:ncol(CAdataset_rfe)], collapse = " + ") # select independent variables

# print(f)

################ random forest model tuning and 10-fold CV #########################################################

RMSE_temp<-matrix(0, nrow =48, ncol = 1)
R2_temp<-matrix(0, nrow =48, ncol = 1)
mtry1_temp<-matrix(0, nrow =48, ncol = 1)
ntree1_temp<-matrix(0, nrow =48, ncol = 1)
temp<-0


#for (mtry1_seq in c(2,3,4,5,6,7)){
#  for (ntree1_seq in c(300,400,500,600,700,800,900,1000)){
#for (mtry1_seq in c(2,3,4,5,6,7)){
for (mtry1_seq in c(3,4,5)){
  for (ntree1_seq in c(300,400,500,600,700)){
    
    temp<-temp+1
    cat("temp = ",temp,"\n")
    cat("mtry = ",mtry1_seq,"\n")
    cat("ntree = ",ntree1_seq,"\n")
    ### Dataset splitting, keep the data from the same data as train sample
    n<-nrow(CAdataset_train00)
    # K - fold cross validation, we set k = n
    kk<-10
    mmk<-floor(n/kk)
    
    
    # Define matrix used to save paramaters
    pred_rf<-matrix(0, nrow =n, ncol = 1)
    dataset_y_rf<-matrix(0, nrow =n, ncol = 1)
    count_total<-0
    
    # define sub-dataset for training and prediction
    for(k in 1:kk){
      cat(k)
      if(k==1){
        data_train<-0
        data_test<-0
        data_train<-CAdataset_train00[(mmk+1):n,]
        data_test<-CAdataset_train00[1:mmk,]
      }
      if(k==kk){
        data_train<-0
        data_test<-0
        data_train<-CAdataset_train00[1:((k-1)*mmk),]
        data_test<-CAdataset_train00[((k-1)*mmk+1):n,]
      }
      if(k>1 && k<kk){
        data_train<-0
        data_test<-0
        data_train1<-CAdataset_train00[1:((k-1)*mmk),]
        data_train2<-CAdataset_train00[(k*mmk+1):n,]
        data_train<-rbind(data_train1,data_train2)
        
        data_test<-CAdataset_train00[((k-1)*mmk+1):(k*mmk),]
      }
      
      
      # number of random features used to build one tree
      mtry1=mtry1_seq
      # number of trees
      ntree1=ntree1_seq
      
      n1<-ncol(data_train)
      nam <- names(data_train)
      nam1<- paste(nam[2:n1], collapse = " + ")
      #nam1<- paste(nam[c(2,3,9,11,12,13,14)], collapse = " + ")
      
      f <- paste("albedo ~", nam1)
      f <- as.formula(f)
      
      # model training
      #   rf3<-randomForest(f,distribution = "gaussian",data = CAdataset_train3,
      #                     ntrees=1000, mtry=max(floor((ncol(CAdataset_train3)-1)/3), 1), keep.data = TRUE,verbose = FALSE,importance = TRUE)
      rf <- ranger(f,data=data_train,num.trees=ntree1,mtry=mtry1, importance = "impurity", 
                   seed = 10)
      
      # data prediction
      pred222 <- predict(rf, data_test[,2:n1])
      pred1<-pred222$predictions
      # Save prediction
      for(hhh in 1:length(pred1)){
        count_total<-count_total+1
        pred_rf[count_total]<-as.numeric(as.character(pred1[hhh]))
        dataset_y_rf[count_total]<-as.numeric(as.character(data_test$albedo[hhh]))
      }
      # Start another loop with another k
    }
    
    result_pred<-matrix(0, nrow =n, ncol = 2)
    result_pred<-cbind(pred_rf,dataset_y_rf)
    colnames(result_pred)<-c("prediction","observation")
    result_pred<-data.frame(result_pred)
    
    
    ### Model RMSE - RF
    {
      lc_pred<-result_pred$prediction
      leng_total<-length(lc_pred)
      
      predict_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
      y_rf_temp<-matrix(0, nrow = leng_total, ncol = 1)
      predict_rf_temp<-result_pred$prediction
      y_rf_temp<-result_pred$observation
      
      
      
      RMSE<-sqrt(mean((predict_rf_temp-y_rf_temp)^2))
      RMSE_temp[temp]<-RMSE
      
      rss <- sum((predict_rf_temp - y_rf_temp) ^ 2)  ## residual sum of squares
      tss <- sum((y_rf_temp - mean(y_rf_temp)) ^ 2)  ## total sum of squares
      rsq <- 1 - rss/tss
      
      R2_temp[temp]<-rsq
      
    }
    
    cat("R2 of rf is",R2_temp[temp],"\n")
    cat("RMSE of rf is",RMSE_temp[temp],"\n")
    
    mtry1_temp[temp]<-mtry1_seq
    ntree1_temp[temp]<-ntree1_seq
  }
}

#tiff("1 R2-layers rf.tiff", width = 700, height = 700, units = 'mm', res = 300)
#par(mfrow=c(2,3))
#plot(mtry1_temp,RMSE_temp)
#plot(ntree1_temp,RMSE_temp)
#scatterplot3d(mtry1_temp,ntree1_temp,RMSE_temp)

#plot(mtry1_temp,R2_temp)
#plot(ntree1_temp,R2_temp)
#scatterplot3d(mtry1_temp,ntree1_temp,R2_temp)


#plot(mtry1_temp,R2_temp)
#plot(ntree1_temp,R2_temp)

nam33<- paste(names(CAdataset_train00)[2:ncol(CAdataset_train00)], collapse = " + ")
# print(nam22)
f4 <- paste("albedo ~", nam33) # connect strings of X and Y
# print(f)
f4 <- as.formula(f4) # convert strings to formula

ind<-which(R2_temp==max(R2_temp))
mtry1_temp[ind]
ntree1_temp[ind]

### final model
{
  # number of random features used to build one tree
  mtry1=mtry1_temp[ind]
  # number of trees
  ntree1=ntree1_temp[ind]
  
  # model training - probability approach, with proximity=true
  # rf4<-ranger(f,data=CAdataset_train00,num.tree = min(ntree1_temp[ind]) ,mtry=min(mtry1_temp[ind]),keep.inbag=T,importance= 'impurity')
  
  rf4<-ranger(f4,data = CAdataset_train00,
              num.trees=min(ntree1_temp[ind]),mtry=min(mtry1_temp[ind]), importance = "impurity", 
              seed = 10)
  # model training
  nam5<-names(CAdataset_train00)[2:ncol(CAdataset_train00)]
  y_pred<-predict(rf4,CAdataset_out_of_box[,nam5])
  #y_pred<-predict(rf_final,data = dataset_out_of_box[,c(2,3,4,5,7,9,11,12,13,14)])
  pred1_rf_oob<-y_pred$predictions
  
  
  ## performance
  
  ### Model accuracy - RF
  {
    RMSE<-sqrt(mean((pred1_rf_oob-CAdataset_out_of_box$albedo)^2))
    RMSE_rf_final<-RMSE
    
    
    rss <- sum((pred1_rf_oob - CAdataset_out_of_box$albedo) ^ 2)  ## residual sum of squares
    tss <- sum((CAdataset_out_of_box$albedo - mean(CAdataset_out_of_box$albedo)) ^ 2)  ## total sum of squares
    rsq <- 1 - rss/tss
    
    R2_rf_final<-rsq
    
    hebing<-data.frame(CAdataset_out_of_box$albedo,pred1_rf_oob)
    write.table(hebing,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-observation-noGPP-wholeperiods.csv")
    
    
  }
  
}


######################################## model tuning finish ###########################################################

############################## ranking ###########################################################
#importance_rank<- rf2$importance

importance_rank4 <- data.frame(importance(rf4,type = "gini"))
importance_rankcopy4<-importance_rank4
# print(rownames(importance_rank))
importance_rankcopy4$name<-row.names(importance_rankcopy4)
#  print(importance_rankcopy)

colnames(importance_rankcopy4) <- c("gini","name")
#  print(importance_rankcopy)
importance_rankcopy4 <- importance_rankcopy4[order(importance_rankcopy4$gini,
                                                 decreasing = TRUE), ]

write.table(importance_rank,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_importance-new-combinetillage-noGPP-wholeperiods.csv")

print("========finish importance ====================")

# ################### importance fininsh #####################################################################

# #################################计算shapley values ############################################################
x=CAdataset_train00[,2:ncol(CAdataset_train00)]

predictor <- Predictor$new(rf4, data = x, y = CAdataset_train00$albedo)
nn<-nrow(x)
ss<-matrix(0, nrow =nn, ncol = 12)
#shapley <- Shapley$new(predictor, x.interest = x[6,])
#print(shapley$results)
#print(shapley$results[2,2])
for(i in 1:nn){
  print(i)
  shapley <- Shapley$new(predictor, x.interest = x[i,])
 # print((shapley$results))
 # print((shapley$results)[1,2])
  s_result<-shapley$results
#  print(which(s_result$feature=="gpp"))
  ss[i,1] <- s_result[which(s_result$feature=="GPP"),2]
  ss[i,2] <- s_result[which(s_result$feature=="Kd"),2]
  ss[i,3] <- s_result[which(s_result$feature=="TN"),2]
  ss[i,4] <- s_result[which(s_result$feature=="THar"),2]
  ss[i,5] <- s_result[which(s_result$feature=="TFug"),2]
  ss[i,6] <- s_result[which(s_result$feature=="Tss"),2]
  ss[i,7] <- s_result[which(s_result$feature=="TIns"),2]
  ss[i,8] <- s_result[which(s_result$feature=="Tgr"),2]
  ss[i,9] <- s_result[which(s_result$feature=="TMan"),2]
  ss[i,10] <- s_result[which(s_result$feature=="SWC"),2]
  ss[i,11] <- s_result[which(s_result$feature=="ws"),2]
  ss[i,12] <- s_result[which(s_result$feature=="SiteIden"),2]


}

#write.table(ss,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues.csv")
print("========shapley values ====================")



####################################### shapley values from fastshap package #######################################################################


# Create the feature matrix
#X <- model.matrix(f4, data = CAdataset_train00)
x=CAdataset_train00[,2:ncol(CAdataset_train00)]

pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

#"gpp","diffuseradiance","fertilizers","temp","harvest","ploughing","tillage","sm","pesticidestype","clay","sand","soiltexture"
set.seed(10)
shap <- fastshap::explain(rf4,X=x, pred_wrapper=pfun, nsim = 10)

autoplot(shap,type="dependence", feature = "SWC",X=x, color_by = "TN")
## "GPP","TFer.","TPes.","Ts","TPlo.","THar.","SWC","TTil.","TypePes."
shap_imp <- data.frame(
  Variable = names(shap),
  Importance = apply(shap, MARGIN = 2, FUN = function(xx) sum(abs(xx)))
)

ggplot(shap_imp, aes(reorder(Variable, Importance), Importance)) +
  geom_col(color = "black", fill = "orange") +
  
  coord_flip() +
  xlab("") +
  ylab("mean(|Shapley value|)") +
  theme_classic()+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
        legend.direction = "horizontal",
        #   strip.text = ggtext::element_textbox(),
        #legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size =18),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))

gppname<-rep("GPP", times=length(x[,1]))
shap_dep_gpp <- data.frame(xx = x[["GPP"]], shap = shap[["GPP"]],name =gppname)
kdname<-rep("Kd", times=length(x[,1]))
shap_dep_kd <- data.frame(xx = x[["Kd"]], shap = shap[["Kd"]],name =kdname)
SWCname<-rep("SWC", times=length(x[,1]))
shap_dep_SWC <- data.frame(xx = x[["SWC"]], shap = shap[["SWC"]],name =SWCname)
wsname<-rep("ws", times=length(x[,1]))
shap_dep_ws <- data.frame(xx = x[["ws"]], shap = shap[["ws"]],name =wsname)


fername<-rep("TN", times=length(x[,1]))
shap_dep_fertilizers <- data.frame(xx = x[["TN"]], shap = shap[["TN"]],name =fername)
harname<-rep("THar", times=length(x[,1]))
shap_dep_har <- data.frame(xx = x[["THar"]], shap = shap[["THar"]],name =harname)
fugname<-rep("TFug", times=length(x[,1]))
shap_dep_fug <- data.frame(xx = x[["TFug"]], shap = shap[["TFug"]],name =fugname)
ssname<-rep("Tss", times=length(x[,1]))
shap_dep_ss <- data.frame(xx = x[["Tss"]], shap = shap[["Tss"]],name =ssname)
Insname<-rep("TIns", times=length(x[,1]))
shap_dep_Ins <- data.frame(xx = x[["TIns"]], shap = shap[["TIns"]],name =Insname)
grname<-rep("Tgr", times=length(x[,1]))
shap_dep_gr <- data.frame(xx = x[["Tgr"]], shap = shap[["Tgr"]],name =grname)
Manname<-rep("TMan", times=length(x[,1]))
shap_dep_Man <- data.frame(xx = x[["TMan"]], shap = shap[["TMan"]],name =Manname)

#bb<-bind_rows(shap_dep_fertilizers,shap_dep_pesticide,shap_dep_ploughing,shap_dep_tillage,shap_dep_harvest)

sitename<-rep("SiteIden", times=length(x[,1]))
shap_dep_site <- data.frame(xx = x[["SiteIden"]], shap = shap[["SiteIden"]],name =sitename)
#pestypename<-rep("TypePes.", times=length(x[,1]))
#shap_dep_pestype <- data.frame(xx = x[["TypePes."]], shap = shap[["TypePes."]],name =pestypename)
#efername<-rep("EFer.", times=length(x[,1]))
#shap_dep_efer <- data.frame(xx = x[["EFer."]], shap = shap[["EFer."]],name =efername)

write.table(shap_dep_gpp,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-gpp.csv")
write.table(shap_dep_kd,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-kd.csv")
write.table(shap_dep_SWC,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-swc.csv")
write.table(shap_dep_ws,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-ws.csv")

write.table(shap_dep_fertilizers,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-tn.csv")
write.table(shap_dep_har,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-har.csv")
write.table(shap_dep_fug,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-fug.csv")
write.table(shap_dep_ss,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-ss.csv")

write.table(shap_dep_Ins,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-ins.csv")
write.table(shap_dep_gr,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-gr.csv")
write.table(shap_dep_Man,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-man.csv")

write.table(shap_dep_site,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/rsults/predictors-combine_onlyWW1_shapleyvalues-fastshap-sitelabel.csv")
#write.table(shap_dep_fug,file ="D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/8/results/predictors-combine_onlyWW1_shapleyvalues-fastshap-fug.csv")
#write.table(shap_dep_efer,file ="D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/5/bestreults/predictors-combine_onlyWW1_shapleyvalues-fastshap-efer.csv")

a<-shap_dep_site
ggplot(data = a, aes(x = a[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a[,1],y =a[,2]),color="blue",size=4,alpha = 0.3)  +
#  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a[,1],y =a[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
 # xlim(0,60)+
 # xlim(0,60)+
  xlim(0,8)+  
  # Site Labels          "Precipitation"    expression("Temp"['s'])                                           Ploughing Tillage  Days after Manure fertilizer
  #Types of pesticldes  Elements of fertilizers   Days after tillage  harvest manure ploughing GPP SWC  Stalk stabilizer  Growth regulator Ca fertilizer
  #expression("T"["N"])
  xlab('Site identity')+ylab("Shaply value of surface albedo")+
 # scale_x_discrete(breaks=c('Ca','Mg', 'Mo', 'Nitrogen','S'),labels=c('Ca','Mg', 'Mo', 'N','S'))+
  #scale_x_discrete(breaks=c("Fungicide","Growth regulator","Herbicide","Insecticide","Stalk Stabilizer"),labels=c("Fug.","GR","Her.","Ins.","SS"))+
  scale_x_discrete(breaks=c('1','2', '3', '4','5','6', '7', '8'),labels=c("BL","CK","DG","DK","DR","FA","FG","FL"))+
  #ggtitle("Soil Temperature")+
  #scale_x_continuous(limits = c(min(listdraw[,iii-1]), max(listdraw[,iii-1])), breaks = seq(0, 60,5))+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
        legend.direction = "horizontal",
        #   strip.text = ggtext::element_textbox(),
        #legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = "none",
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(size = 20, colour = "black", angle = 0, hjust = 1),
        
        
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size =20),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))



#ggplot(bb,aes(x=bb[,1],y = bb[,2],group=bb[,3],color=bb[,3])) +
#  geom_point(aes(x=bb[,1],y = bb[,2],colour = bb[,3]),size=4)  +
#  #geom_bar(aes(x=as.data.frame(CAdataset)[,1],colour = CAdataset[,1]),stat="identity",width = 0.5, position = position_dodge(width = 0.9),size=5)  +
#  scale_color_npg()+
#  geom_point(size=3,alpha = 0.3) +
# # geom_smooth(color="#C0C0C0",size=1) +
# # geom_text(aes(label = c("Fungicide","Growth Regulator","Herbicide","Insecticide","Stalk Stabilizer")), "Fug.","GR","Her.","Inc.","SS" angle = 45)+
#  # breaks=c("Clay-Loam","Loam","Silt-Loam"),labels=c("Clay-Loam","Loam","Silt-Loam")v
#  ylab("Shapley value")+
#  ylim(-0.05,0.05)+
#  xlim(1,60)+
# # scale_x_discrete(breaks=c("Clay-Loam","Loam","Silt-Loam"),labels=c("Clay-Loam","Loam","Silt-Loam"))+
#  xlab("Days after management events")+
#  #ggtitle("Soil Temperature")+
#  #scale_x_continuous(limits = c(min(listdraw[,iii-1]), max(listdraw[,iii-1])), breaks = seq(0, 60,5))+
#  theme_bw()+
#  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#        axis.line.y=element_line(linetype=1,color="black",size=1),
#        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#        legend.direction = "horizontal",
#        #   strip.text = ggtext::element_textbox(),
#        #legend.background = element_rect(fill = "transparent", colour = NA),
#        legend.position = c(0.4, 0.99),
 #       legend.justification = c(0.4, 0.99),
 #       legend.title = element_blank(),
 #       legend.text = element_text(size = 18),
#        axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
#        axis.text.y = element_text(size = 18, colour = "black"),
#        axis.title = element_text(size =18),
#        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
 #       panel.grid = element_blank(),
 #       panel.background = element_rect(fill = NA, colour = "black",
#                                        size = 1.5))

data1<-shap %>%
  as.data.frame() %>%
  mutate(id=1:n()) %>%
  pivot_longer(cols = -(ncol(x)+1), values_to = "shap")




xx<-x
xx$SiteIden <- as.numeric(as.character(xx$SiteIden))

#'Fungicide','Stalk Stabilizer', 'Herbicide', 'Insecticide', 'Growth regulator'
#xx$TypePes. <- as.character(xx$TypePes.)

#xx$TypePes.[which(xx$TypePes.=="Fungicide")] <-"1"
#xx$TypePes.[which(xx$TypePes.=='Stalk Stabilizer')] <-"2"
#xx$TypePes.[which(xx$TypePes.=='Herbicide')] <-"3"
#xx$TypePes.[which(xx$TypePes.=='Insecticide')] <-"4"
#xx$TypePes.[which(xx$TypePes.=='Growth regulator')] <-"5"

#xx$TypePes. <- as.numeric(as.character(xx$TypePes.))

#xx$MgFer. <- as.character(xx$MgFer.)

#xx$MgFer.[which(xx$MgFer.=="yes")] <-"1"
#xx$MgFer.[which(xx$MgFer.=='no')] <-"2"

#xx$MgFer. <- as.numeric(as.character(xx$MgFer.))

#xx$EFer. <- as.character(xx$EFer.)
#xx$EFer.[which(xx$EFer.=='Ca')] <-"1"
#xx$EFer.[which(xx$EFer.=='Mg')] <-"2"
#xx$EFer.[which(xx$EFer.=='Mo')] <-"3"
#xx$EFer.[which(xx$EFer.=='Nitrogen')] <-"4"
#xx$EFer.[which(xx$EFer.=='S')] <-"5"

#xx$EFer. <- as.numeric(as.character(xx$EFer.))

#xx$albedo<-CAdataset_train00[,1]

data2<-xx %>%
  mutate(id=1:n()) %>%
  pivot_longer(cols = -(ncol(xx)+1))





################################# plot shape figure with color of predictors#################################################################
data1 %>%
  left_join(data2) %>%
 # rename("name"="feature") %>%
 # data1$name
  group_by(name) %>%
  mutate(value = (value - min(value)) / (max(value)-min(value))) %>%
  #ggplot(aes(x=shap,y=value)) +
  ggplot(aes(x=shap,y=name,color=value)) +
 # geom_beeswarm(groupOnX = FALSE)+
  scale_y_discrete(labels=c("GPP","Kd",expression("T"["N"]),
                            expression("T"["Har"]),expression("T"["Fug"]),expression("T"["SS"]),
                            expression("T"["Ins"]),
                            expression("T"["gr"]),expression("T"["Man"]),"SWC","ws",
                            "SiteIden"))+
  geom_vline(aes(xintercept=0),colour = "lightgray",size=2)+
  geom_jitter(size=2,height=0.1,width=0)+
  scale_color_gradient(
    low = "#FFCC33",
    high = "#6600CC",
    breaks = c(0,1),
    labels = c("Low","High"),
    guide = guide_colorbar(barwidth =1,barheight=10)
  )+
  labs(x="SHAP value",color = "Feature value")+
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
        legend.direction = "vertical",
        #   strip.text = ggtext::element_textbox(),
        #legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.19, 0.99),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.title = element_text(size =18),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))
################################# plot shape figure with color of predictors#################################################################


################################# plot shape values caused by #################################################################

#CAdataset1<-CAdataset[,c("albedo","ws","Ts","SWC","GPP","Bowenratio","Kd","Pre.","Pre.label","PA",
#                         "Clayratio","Sandratio","TCa","CaFer.","TN","NFer.","TMg",
#                         "MgFer.","TS","SFer.",	"Tother","otherFer.","TFug.","Fpes.","THer.","Hpes.","TIns.",
#                         "Inpes.","Tgr.","GRpes.","Tss.","SSpes.","SiteIden.","TTil.","DTil.","TMan.","ManFer.","TPlo.","DPlo.","THar.","PartHar.","Tsow.")]


feature<-list("TN","THar","TFug","Tss","TIns","Tgr","TMan","GPP","Kd","SWC","ws")
data_hebing<-left_join(data1,data2)
data_hebing_adjust<-matrix(0, nrow =2120, ncol =23)
#nnn<-as.character(feature[1])
#data_hebing_adjust[,1] <- as.numeric(unlist(data_hebing[which(data_hebing$name==nnn),3]))
for (j in 1:length(feature)){
  nnn<-as.character(feature[j])
  print(nnn)
 # print(nnn)
 # print(i)
  data_hebing_adjust[,(2*j)] <- as.numeric(unlist(data_hebing[which(data_hebing$name==nnn),3]))
  data_hebing_adjust[,(2*j-1)] <- as.numeric(unlist(data_hebing[which(data_hebing$name==nnn),4]))
}

for (jj in 1:2120){
  data_hebing_adjust[jj,ncol(data_hebing_adjust)] <- as.numeric(CAdataset_train00[jj,1])
}
#feature<-list("TFer.","TMan.","GPP","Kd","THar.","Ts","TPlo.","SWC","TTil.","sitelabel","TypePes.")
#data_hebing_adjust$albedo<-CAdataset_train00[,1]
colnames(data_hebing_adjust)<-c("TN_value","TN_shap","THar_value","THar_shap","TFug_value","TFug_shap",
                                "Tss_value","Tss_shap","TIns_value","TIns_shap","Tgr_value","Tgr_shap",
                                "TMan_value","TMan_shap",
                                "GPP_value","GPP_shap","Kd_value","Kd_shap","SWC_value","SWC_shap",
                                "ws_value","ws_shap","albedo")

#write.table(data_hebing_adjust,file ="D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/5/bestnew/shapvalue-matchpredictors.csv")

aaaa<-data_hebing_adjust[,c("TMan_value","TMan_shap","GPP_shap","Kd_shap","SWC_shap","ws_shap")]

xx <- 0
yy <- -0.02
ggplot(data = as.data.frame(aaaa), aes(x = aaaa[,1],y = aaaa[,2:ncol(aaaa)]))+
  geom_hline(aes(yintercept=0),colour = "black",size=1)  +
 # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  #geom_point(aes(x = aaaa[,1],y = aaaa[,2],color="gray30"),size=4,alpha=0.3)  +   # FER SHAP
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,2]),colour="gray30",method="loess",se=TRUE,size=3,linetype = "dashed")+
  
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,2]),color="orange",size=4,alpha=0.3)  +    # GPP SHAP
  
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,6]),color="orchid3",size=4,alpha=0.3)  +   # Kd SHAP
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,10]),color="tomato2",size=4,alpha=0.3)  +   # Ts SHAP
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,14]),color="steelblue3",size=4,alpha=0.3)  +   # SWC SHAP
  
 # geom_smooth()+
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,3]),colour="orange",method="loess",se=TRUE,size=3)+
  
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,4]),colour="orchid3",method="loess",se=TRUE,size=3)+
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,5]),colour="tomato2",method="loess",se=TRUE,size=3)+
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,6]),colour="steelblue3",method="loess",se=TRUE,size=3)+
  
 #guides(color=guide_legend(title = "ABC"))+
#  geom_line(aes(x = listdraw[,iii-1],y = listdraw[,iii]),color="#F8766D",size=2)  +
 # geom_col(color = "black", fill = "orange") +
 # coord_flip() +
  #xlim(-0.05,0.05)+
  # xlim(0,60)+
#  ylim(-0.05,0.05)+
  xlim(0,60)+
  ylim(-0.03,0.01)+
 # ylim(0,30)+ N fertilizer"  sowing  fungicide"  herbicide and insecticide stalk stabilizer  growth regulator
  xlab("Days after manure") +
  ylab("Shaply value of surface albedo") +
  #Days after fertilizers  manure harvest ploughing
 # theme_classic()+
 # scale_shape_discrete(name  ="Payer",
 #                      breaks=c("gray30","orange", "orchid3","tomato2","steelblue3"),
 #                      labels=c("FER","GPP", "Kd","Ts","SWC"))+
  
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
        legend.direction = "horizontal",
        #   strip.text = ggtext::element_textbox(),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.6, 0.95),
        legend.justification = c(1, 1),
       # legend.key.height = unit(100, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(size = 20, colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size =20),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))+
  annotate("segment", x = xx, xend = xx+5, y = yy, yend = yy,size = 3,color = "gray30")+
  annotate("text",x=xx+9,y=yy, label=expression("T"["gr"]),parse=T,size = 7, colour = "black")+
  annotate("segment", x = xx+16, xend = xx+21, y = yy, yend =yy,size = 3,color = "orange")+
  annotate("text",x=xx+25,y=yy, label="GPP",parse=T,size = 7, colour = "black")+
  annotate("segment", x = xx+30, xend = xx+35, y =yy, yend = yy,size = 3,color = "orchid3")+
  annotate("text",x=xx+39,y=yy, label="Kd",parse=T,size = 7, colour = "black")+
  
  annotate("segment", x =xx, xend = xx+5, y = yy-0.005, yend =yy- 0.005,size = 3,color = "tomato2")+
  annotate("text",x=xx+10,y=yy-0.005, label="SWC",parse=T,size = 7, colour = "black")+
  annotate("segment", x =xx+16, xend = xx+21, y =yy- 0.005, yend = yy-0.005,size = 3,color = "steelblue3")+
  annotate("text",x=xx+25,y=yy-0.005, label="ws",parse=T,size = 7, colour = "black")


#colnames(data_hebing_adjust)<-c("TN_value","TN_shap","Tss_value","Tss_shap","THer_value","THer_shap",
#                                "Tgr_value","Tgr_shap","Tca_value","Tca_shap","TTil_value","TTil_shap",
 #                               "TPlo_value","TPlo_shap","TMan_value","TMan_shap",
 #                               "GPP_value","GPP_shap","Kd_value","Kd_shap","SWC_value","SWC_shap",
 #                               "Clayratio_value","Clayratio_shap","albedo")
# y=19   0.95   33    2.5
#    GPP  KD    SWC   sand
### "TN","Tsow.","TFug.","THerIns.","Tss.","Tgr.","TMg","GPP","Kd","Ts","Pre."
aaaa<-data_hebing_adjust[,c("TN_value","THar_value","TFug_value","Tss_value","TIns_value","Tgr_value","TMan_value","GPP_value")]
xx <- 0
yy<-33
ggplot(data = as.data.frame(aaaa), aes(x = aaaa[,8],y = aaaa[,1:ncol(aaaa)-1]))+
 # geom_hline(aes(yintercept=0),colour = "black",size=1)  +
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  #geom_point(aes(x = aaaa[,1],y = aaaa[,2],color="gray30"),size=4,alpha=0.3)  +   # FER SHAP
 # geom_smooth(aes(x = aaaa[,1],y = aaaa[,6]),colour="gray30",method="loess",se=TRUE,size=3,linetype = "dashed")+
  
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,2]),color="orange",size=4,alpha=0.3)  +    # GPP SHAP
  
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,6]),color="orchid3",size=4,alpha=0.3)  +   # Kd SHAP
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,10]),color="tomato2",size=4,alpha=0.3)  +   # Ts SHAP
  #geom_point(aes(x = data_hebing_adjust[,3],y = data_hebing_adjust[,14]),color="steelblue3",size=4,alpha=0.3)  +   # SWC SHAP
  #geom_smooth(aes(x = aaaa[,8],y = aaaa[,8]),colour="seagreen1",method="loess",se=FALSE,size=3)+
  # geom_smooth()+
  geom_smooth(aes(x = aaaa[,1],y = aaaa[,8]),colour="gray30",method="loess",se=FALSE,size=3)+

  geom_smooth(aes(x = aaaa[,2],y = aaaa[,8]),colour="orange",method="loess",se=FALSE,size=3)+

  geom_smooth(aes(x = aaaa[,3],y = aaaa[,8]),colour="orchid3",method="loess",se=FALSE,size=3)+

  geom_smooth(aes(x = aaaa[,4],y = aaaa[,8]),colour="tomato2",method="loess",se=FALSE,size=3)+

  geom_smooth(aes(x = aaaa[,5],y = aaaa[,8]),colour="steelblue3",method="loess",se=FALSE,size=3)+
  
  geom_smooth(aes(x = aaaa[,6],y = aaaa[,8]),colour="forestgreen",method="loess",se=FALSE,size=3)+
  
  geom_smooth(aes(x = aaaa[,7],y = aaaa[,8]),colour="seagreen1",method="loess",se=FALSE,size=3)+
  
  
  
  #guides(color=guide_legend(title = "ABC"))+
  #  geom_line(aes(x = listdraw[,iii-1],y = listdraw[,iii]),color="#F8766D",size=2)  +
  # geom_col(color = "black", fill = "orange") +
  # coord_flip() +
  #xlim(-0.05,0.05)+
  # xlim(0,60)+
  #  ylim(-0.05,0.05)+
#  20,0.6 1, 0 35, 0,3
  xlim(0,60)+ 
  ylim(0,35)+
  # ylim(0,30)+
  xlab("Days after management practices") +
  ylab(expression('GPP')) +
#  ylab(expression('Pre.'~'(mm'*' d'^-1*')')) +
#  ylab(expression('Temp'['s']~'('*degree*'C)')) +
#  ylab('Kd') +
 # ylab(expression('GPP'~'(gC'*'m'^-2*'d'^-1*')')) +
  #xlab=expression("kg/m"^"2")
  #Days after fertilizers  manure harvest ploughing
  # theme_classic()+
  # scale_shape_discrete(name  ="Payer",
  #                      breaks=c("gray30","orange", "orchid3","tomato2","steelblue3"),
  #                      labels=c("FER","GPP", "Kd","Ts","SWC"))+
  
  theme_bw()+
  theme(axis.line.x=element_line(linetype=1,color="black",size=1),
        axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
        axis.line.y=element_line(linetype=1,color="black",size=1),
        axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
        legend.direction = "horizontal",
        legend.key.height = unit(100, "cm"),
        #   strip.text = ggtext::element_textbox(),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.position = c(0.6, 0.95),
        legend.justification = c(2, 2),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(size = 20, colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title = element_text(size =20),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))+
  annotate("segment", x = xx, xend = xx+5, y = yy, yend = yy,size = 3,color = "gray30")+
  annotate("text",x=xx+7,y=yy, label=expression("T"["N"]),parse=T,size = 7, colour = "black")+
  
  annotate("segment", x = xx+12, xend = xx+16, y = yy, yend = yy,size = 3,color = "orange")+
  annotate("text",x=xx+20,y=yy, label=expression("T"["Har"]),parse=T,size = 7, colour = "black")+
  
  annotate("segment", x = xx+26, xend = xx+30, y = yy, yend = yy,size = 3,color = "orchid3")+
  annotate("text",x=xx+33,y=yy, label=expression("T"["Fug"]),parse=T,size = 7, colour = "black")+
  
  annotate("segment", x = xx+38, xend = xx+42, y = yy, yend = yy,size = 3,color = "tomato2")+
  annotate("text",x=xx+46,y=yy, label=expression("T"["ss"]),parse=T,size = 7, colour = "black")+
  ### 2 0.05  2  0.5
  #YYYYYY <- 2
 # print(YYYYYY)
  annotate("segment", x = xx, xend = xx+5, y = yy-2, yend = yy-2,size = 3,color = "steelblue3")+
  annotate("text",x=xx+8,y=yy-2, label=expression("T"["Ins"]),parse=T,size = 7, colour = "black") +

  annotate("segment", x = xx+12, xend = xx+16, y = yy-2, yend = yy-2,size = 3,color = "forestgreen")+
  annotate("text",x=xx+20,y=yy-2, label=expression("T"["gr"]),parse=T,size = 7, colour = "black") +
  
  annotate("segment", x = xx+26, xend = xx+30, y = yy-2, yend = yy-2,size = 3,color = "seagreen1")+
  annotate("text",x=xx+34,y=yy-2, label=expression("T"["Man"]),parse=T,size = 7, colour = "black")

# "TN_value","Tss_value","THer_value","Tgr_value","Tca_value","TTil_value","TPlo_value","TMan_value"
