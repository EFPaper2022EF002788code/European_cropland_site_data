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

CAdataset<-read.csv("E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant181/predictors-fallow-181.csv")
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

CAdataset$DCa <-as.numeric(as.character(CAdataset$DCa))
#CAdataset$CaFer. <-as.factor(as.character(CAdataset$CaFer.))

CAdataset$DN <-as.numeric(as.character(CAdataset$DN))
#CAdataset$NFer.<-as.factor(as.character(CAdataset$NFer.))

CAdataset$DMg <-as.numeric(as.character(CAdataset$DMg))
#CAdataset$MgFer.<-as.factor(as.character(CAdataset$MgFer.))

CAdataset$DS <-as.numeric(as.character(CAdataset$DS))

CAdataset$DFug <-as.numeric(as.character(CAdataset$DFug))
#CAdataset$Fpes. <-as.factor(as.character(CAdataset$Fpes.))

CAdataset$DHer<-as.numeric(as.character(CAdataset$DHer))
CAdataset$DIns<-as.numeric(as.character(CAdataset$DIns))
CAdataset$Dgr<-as.numeric(as.character(CAdataset$Dgr))
#CAdataset$GRpes. <-as.factor(as.character(CAdataset$GRpes.))

CAdataset$Dss<-as.numeric(as.character(CAdataset$Dss))
#CAdataset$SSpes. <-as.factor(as.character(CAdataset$SSpes.))

CAdataset$SiteIden<-as.factor(as.character(CAdataset$SiteIden))

CAdataset$DTil<-as.numeric(as.character(CAdataset$DTil))
CAdataset$DepthTil<-as.numeric(as.character(CAdataset$DepthTil))

CAdataset$DMan<-as.numeric(as.character(CAdataset$DMan))
#CAdataset$ManFer. <-as.factor(as.character(CAdataset$ManFer.))

#CAdataset$TPlo<-as.numeric(as.character(CAdataset$TPlo))
#CAdataset$DPlo<-as.numeric(as.character(CAdataset$DPlo))
#FEsow.
#CAdataset$DHar<-as.numeric(as.character(CAdataset$DHar))
#CAdataset$PartHar<-as.factor(as.character(CAdataset$PartHar))

CAdataset$Dsow<-as.numeric(as.character(CAdataset$Dsow))

CAdataset$Dres<-as.numeric(as.character(CAdataset$Dres))
# CAdataset$Tmustard<-as.numeric(as.character(CAdataset$Tmustard))
# CAdataset$Tfababean<-as.numeric(as.character(CAdataset$Tfababean))
# CAdataset$Toilradish<-as.numeric(as.character(CAdataset$Toilradish))
# CAdataset$Toilmus<-as.numeric(as.character(CAdataset$Toilmus))
# CAdataset$Tsoftwheat<-as.numeric(as.character(CAdataset$Tsoftwheat))

#CAdataset$GS <-as.factor(as.character(CAdataset$GS))
CAdataset$cctype <-as.factor(as.character(CAdataset$cctype))

#print(colnames(CAdataset))Pre.label
#"TCa","TN","TMg","TS","TFug","THer","TIns","Tgr","Tss","TMan",
# "TTil","DTil","TPlo","DPlo","THar",
#"ws","Ts","SWC","Bowenratio","GPP","Kd","Pre","PA", "Clayratio","Sandratio",

CAdataset1<-CAdataset[,c("albedo","ws","Ts","SWC","Bowenratio","Kd","Pre",
                         "PA", "Clayratio","Sandratio",
                         "DCa","DN","DMg","DS","DFug","DHer","DIns","Dgr","Dss","DMan",
                         "DTil","DepthTil","SiteIden","Dsow","Dres")]
                     #   "Tmustard", "Tfababean","Toilradish","Toilmus","Tsoftwheat","cctype")]
                         

#==========================using R package RFE ========================================

#==========================using  RFE by myself ========================================


n<-nrow(CAdataset1)
set.seed(1000)
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
resip<-matrix(0, nrow =40, ncol = 1)
mustardip<-matrix(0, nrow =40, ncol = 1)
fababeanip<-matrix(0, nrow =40, ncol = 1) #"Tmustard","Tfababean","Toilradish","Toilmus","Tsoftwheat"
oilradiship<-matrix(0, nrow =40, ncol = 1)
oilmusip<-matrix(0, nrow =40, ncol = 1)
softwheatip<-matrix(0, nrow =40, ncol = 1)

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

harvestip<-matrix(0, nrow =40, ncol = 1)
harvestpartip<-matrix(0, nrow =40, ncol = 1)
gsip<-matrix(0, nrow =40, ncol = 1)
#FEsow.
sitelabel<-matrix(0, nrow =40, ncol = 1)
cctype<-matrix(0, nrow =40, ncol = 1)

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
                    seed = 1000)

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

    #if ("GPP" %in% rownames(imp_data)){
    #  gppip[ij]<-importance_rankcopy[which(rownames(imp_data)=="GPP"),1]
    #}
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

    if ("DCa" %in% rownames(imp_data)){
      caip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DCa"),1]
    }
    if ("DN" %in% rownames(imp_data)){
      nip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DN"),1]
    }
    if ("DMg" %in% rownames(imp_data)){
      mgip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DMg"),1]
    }
    if ("DS" %in% rownames(imp_data)){
      sip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DS"),1]
    }
   # if ("Tother" %in% rownames(imp_data)){
   #   otherip[ij]<-importance_rank[which(rownames(imp_data)=="Tother"),2]
   # }
    if ("DFug" %in% rownames(imp_data)){
      funip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DFug"),1]
    }
    if ("DHer" %in% rownames(imp_data)){
      herip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DHer"),1]
    }
    if ("DIns" %in% rownames(imp_data)){
      insip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DIns"),1]
    }

    if ("Dgr" %in% rownames(imp_data)){
      grip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Dgr"),1]
    }
    if ("Dss" %in% rownames(imp_data)){
      ssip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Dss"),1]
    }

    if ("DMan" %in% rownames(imp_data)){
     TManip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DMan"),1]
    }
    
    if ("DTil" %in% rownames(imp_data)){
      tillaip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DTil"),1]
    }
    if ("DepthTil" %in% rownames(imp_data)){
      tillaid[ij]<-importance_rankcopy[which(rownames(imp_data)=="DepthTil"),1]
    }
    #if ("TPlo" %in% rownames(imp_data)){
    #  ploip[ij]<-importance_rankcopy[which(rownames(imp_data)=="TPlo"),1]
    #}
    #if ("DPlo" %in% rownames(imp_data)){
    #  ploid[ij]<-importance_rankcopy[which(rownames(imp_data)=="DPlo"),1]
    #}
    #if ("DHar" %in% rownames(imp_data)){
    #  harvestip[ij]<-importance_rankcopy[which(rownames(imp_data)=="DHar"),1]
    #}
    #if ("PartHar" %in% rownames(imp_data)){
    #  harvestpartip[ij]<-importance_rankcopy[which(rownames(imp_data)=="PartHar"),1]
    #}

    if ("SiteIden" %in% rownames(imp_data)){
      sitelabel[ij]<-importance_rankcopy[which(rownames(imp_data)=="SiteIden"),1]
    }
    if ("Dsow" %in% rownames(imp_data)){
      TSowip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Dsow"),1]
    }
    if ("Dres" %in% rownames(imp_data)){
      resip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Dres"),1]
    }
    # if ("Tmustard" %in% rownames(imp_data)){
    #   mustardip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tmustard"),1]
    # }
    # if ("Tfababean" %in% rownames(imp_data)){
    #   fababeanip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tfababean"),1]
    # }
    # if ("Toilradish" %in% rownames(imp_data)){
    #   oilradiship[ij]<-importance_rankcopy[which(rownames(imp_data)=="Toilradish"),1]
    # }
    # if ("Toilmus" %in% rownames(imp_data)){
    #   oilmusip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Toilmus"),1]
    # }
    # if ("Tsoftwheat" %in% rownames(imp_data)){
    #   softwheatip[ij]<-importance_rankcopy[which(rownames(imp_data)=="Tsoftwheat"),1]
    # }
    #if ("GS" %in% rownames(imp_data)){
    #  gsip[ij]<-importance_rankcopy[which(rownames(imp_data)=="GS"),1]
    #}
    if ("cctype" %in% rownames(imp_data)){
      cctype[ij]<-importance_rankcopy[which(rownames(imp_data)=="cctype"),1]
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
                    seed = 1000)
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
  for (ntree1_seq in c(300,400,500,600,700,800,900,1000)){
    
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
                   seed = 1000)
      
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
              seed = 1000)
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
   # write.table(hebing,file ="D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant151/nogpp-up/predictors-observation-fallow-151-nogpp.csv")
    
    
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
                                                                                                                                                                                                                       #-noGPP                
write.table(importance_rank,file ="E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant181/nogpp-up/predictors-fallow-181-noGPP-importance.csv")

print("========finish importance ====================")

# ################### importance fininsh #####################################################################


#===============only change days pdp figure for continuous variables ================================

#liepre<-list("GPP","Kd","SWC")
#liepre<-list("GPP","Kd","Ts","SWC","PA","Clayratio","Sandratio")
liepre<-list("Kd","Ts","SWC","ws","Sandratio","Clayratio")
#print(liepre[1])
# gpp, diffuseradiance,temp, sm,clay, sand
num<-798
listdrawnew<-list()
listdraw <-matrix(0, nrow = num, ncol = 3*length(liepre))
listname<-list()
#print(length(liepre))
iijj<-1

for (ll in 1:length(liepre)){
  na<-as.character(liepre[ll])
  if (na %in% colnames(CAdataset_train00)){
    
    print(na)
    pre_max<-max(CAdataset_train00[,na])  # gpp max
    pre_min<-min(CAdataset_train00[,na])   # gpp min
    
    
    albedo_prediction_zong<-matrix(0, nrow = num, ncol = 4)
    pre_list<-seq(from = pre_min, to = pre_max, length.out = num) # set the gpp sequence with fixed interval of num
    for (n in 1:num){
      # print(n)
      subdataset<-CAdataset_train00   # copy the entile dataset
      subdataset[,na]<-pre_list[n]  # set the gpp as a fixed value in each loop
      #  aaa<-matrix(0, nrow = num, ncol = 1)
      albedo_prediction_zong[n,1]<-pre_list[n]
      #print(1)
      pred <- predict(rf4, subdataset[,2:ncol(subdataset)])  # using new setting to run rf to see in each gpp how the fertilizer at fixed number will effect
      # print(pred$predictions)
      albedo_prediction_zong[n,2]<-mean(pred$predictions) # get average of prediction in each temp with fixed fertilizer label.
      # print(n)
      albedo_prediction_zong[n,3]<-sd(pred$predictions)
      albedo_prediction_zong[n,4]<-na
      # print(n)
      #  albedo_prediction_zong[n,4]<-length(pred)
      
      # colnames(albedo_prediction_zong)<-c(na,"albedo","std")
      
    }
    filename<-paste("D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant151/nogpp-up/predictors_pdp-",liepre[ll],"-151-nogpp-average.csv",sep="")
    #write.csv(albedo_prediction_zong,file=filename)
    #3*ll-2
    listdraw[,iijj]<-albedo_prediction_zong[,1]
    listdraw[,iijj+1]<-albedo_prediction_zong[,2]
    listdraw[,iijj+2]<-albedo_prediction_zong[,3]
    # listdraw[,iijj+3]<-albedo_prediction_zong[,4]
    listname<-append(listname,c(na,"albedo","std"))
    # colnames(listdraw[,iijj])<-na
    listdrawnew<-append(listdrawnew,liepre[ll])
    iijj<-iijj+3
  }else{
    listdraw<-listdraw
    listdrawnew<-listdrawnew
    # listname<-append(listname,c("no","no","no"))
    
  }
}
#print(names(as.data.frame(listdraw)))
zong<-3*length(listdrawnew)
#print(names(as.data.frame(listdraw))[1:zong])
#listdraw<-as.data.frame(listdraw)
listdraw<-as.data.frame(lapply(listdraw, function(x) as.numeric(x)))
names(listdraw)[1:zong]<-listname[1:length(listname)]
#names(listdraw)=c('na',"albedo","std",'na',"albedo","std",'na',"albedo","std",'na',"albedo","std",'na',"albedo","std")
#2,5,8,11,14,17

#for (iii in list(2,5,8,11,14,17)){
iii1<-2
nnaamm<-names(listdraw)[iii1-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}



wz1<-((max(listdraw[,iii1-1])-min(listdraw[,iii1-1]))/10)+min(listdraw[,iii1-1])

p1<-ggplot(data = as.data.frame(listdraw[,iii1-1:iii1+1]), aes(x = listdraw[,iii1-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii1-1],y = listdraw[,iii1]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii1-1],y = listdraw[,iii1]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x = wz1, y = 0.11,label = "(b)",size = 10,face="bold") + 
  # geom_text(aes(label = "(a)"),nudge_x=0.1,nudge_y=0.1)+
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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
#albedo_prediction<-seq(from = gpp_min, to = gpp_max, length.out = num)
iii2<-5
nnaamm<-names(listdraw)[iii2-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}
# print(nnaamm)
#paste("Soil Temperature (",expression("℃"),ssesp"")
#aaaaa<-"Soil Temperature&#176;C"
#print(listdraw[,iii-1])
wz2<-((max(listdraw[,iii2-1])-min(listdraw[,iii2-1]))/10)+min(listdraw[,iii2-1])
p2<-ggplot(data = as.data.frame(listdraw[,iii2-1:iii2+1]), aes(x = listdraw[,iii2-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii2-1],y = listdraw[,iii2]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii2-1],y = listdraw[,iii2]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x = wz2, y = 0.11,label = "(c)",size = 10,face="bold") + 
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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
iii3<-8
nnaamm<-names(listdraw)[iii3-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}
wz3<-((max(listdraw[,iii3-1])-min(listdraw[,iii3-1]))/10)+min(listdraw[,iii3-1])
p3<-ggplot(data = as.data.frame(listdraw[,iii3-1:iii3+1]), aes(x = listdraw[,iii3-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii3-1],y = listdraw[,iii3]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii3-1],y = listdraw[,iii3]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x = wz3, y = 0.11,label = "(d)",size = 10,face="bold") + 
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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

iii4<-11
nnaamm<-names(listdraw)[iii4-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}
wz4<-((max(listdraw[,iii4-1])-min(listdraw[,iii4-1]))/10)+min(listdraw[,iii4-1])
p4<-ggplot(data = as.data.frame(listdraw[,iii4-1:iii4+1]), aes(x = listdraw[,iii4-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii4-1],y = listdraw[,iii4]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii4-1],y = listdraw[,iii4]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x = wz4, y = 0.11,label = "(e)",size = 10,face="bold") + 
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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

iii5<-14
nnaamm<-names(listdraw)[iii5-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}
# print(nnaamm)
#paste("Soil Temperature (",expression("℃"),ssesp"")
#aaaaa<-"Soil Temperature&#176;C"
#print(listdraw[,iii-1])
wz5<-((max(listdraw[,iii5-1])-min(listdraw[,iii5-1]))/10)+min(listdraw[,iii5-1])
p5<-ggplot(data = as.data.frame(listdraw[,iii5-1:iii5+1]), aes(x = listdraw[,iii5-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii5-1],y = listdraw[,iii5]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii5-1],y = listdraw[,iii5]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x =wz5, y = 0.11,label = "(f)",size = 10,face="bold") + 
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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

iii6<-17
nnaamm<-names(listdraw)[iii6-1]
if (nnaamm=="SWC"){
  nnaamm<-"SWC"# (%)
} else if (nnaamm=="GPP"){
  nnaamm<-"GPP" # (gC m^-2 d^-1)
} else if (nnaamm=="Kd"){
  nnaamm<-"Kd" # (gC m^-2 d^-1)
} else if (nnaamm=="Ts"){
  nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
} else if (nnaamm=="PA"){
  nnaamm<-expression("PA") # (gC m^-2 d^-1)
} else if (nnaamm=="Clayratio"){
  nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Sandratio"){
  nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Bowenratio"){
  nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
} else if (nnaamm=="Pre"){
  nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
} else if (nnaamm=="ws"){
  nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
}
# print(nnaamm)
#paste("Soil Temperature (",expression("℃"),ssesp"")
#aaaaa<-"Soil Temperature&#176;C"
#print(listdraw[,iii-1])
wz6<-((max(listdraw[,iii6-1])-min(listdraw[,iii6-1]))/10)+min(listdraw[,iii6-1])
p6<-ggplot(data = as.data.frame(listdraw[,iii6-1:iii6+1]), aes(x = listdraw[,iii6-1]))+
  # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
  geom_point(aes(x = listdraw[,iii6-1],y = listdraw[,iii6]),color="#000000",size=4)  +
  geom_line(aes(x = listdraw[,iii6-1],y = listdraw[,iii6]),color="#000000",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.13,0.19)+
  xlab(nnaamm)+ylab("Surface albedo")+
  annotate("text", x = wz6, y = 0.11,label = "(g)",size = 7,face="bold") +
  #xlab(expression("Temp"['s']))+ylab("albedo")+
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
#
# iii7<-20
# nnaamm<-names(listdraw)[iii7-1]
# if (nnaamm=="SWC"){
#   nnaamm<-"SWC"# (%)
# } else if (nnaamm=="GPP"){
#   nnaamm<-"GPP" # (gC m^-2 d^-1)
# } else if (nnaamm=="Kd"){
#   nnaamm<-"Kd" # (gC m^-2 d^-1)
# } else if (nnaamm=="Ts"){
#   nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
# } else if (nnaamm=="PA"){
#   nnaamm<-expression("PA") # (gC m^-2 d^-1)
# } else if (nnaamm=="Clayratio"){
#   nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Sandratio"){
#   nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Bowenratio"){
#   nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Pre"){
#   nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
# } else if (nnaamm=="ws"){
#   nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
# }
# # print(nnaamm)
# #paste("Soil Temperature (",expression("℃"),ssesp"")
# #aaaaa<-"Soil Temperature&#176;C"
# #print(listdraw[,iii-1])
# wz7<-((max(listdraw[,iii7-1])-min(listdraw[,iii7-1]))/10)+min(listdraw[,iii7-1])
# p7<-ggplot(data = as.data.frame(listdraw[,iii7-1:iii7+1]), aes(x = listdraw[,iii7-1]))+
#   # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
#   geom_point(aes(x = listdraw[,iii7-1],y = listdraw[,iii7]),color="#F8766D",size=4)  +
#   geom_line(aes(x = listdraw[,iii7-1],y = listdraw[,iii7]),color="#F8766D",size=2)  +
#   #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#   ylim(0.10,0.17)+
#   xlab(nnaamm)+ylab("Surface albedo")+
#   annotate("text", x = wz7, y =0.161,label = "(g)",size = 7,face="bold") +
#   #xlab(expression("Temp"['s']))+ylab("albedo")+
#   #ggtitle("Soil Temperature")+
#   #scale_x_continuous(limits = c(min(listdraw[,iii-1]), max(listdraw[,iii-1])), breaks = seq(0, 60,5))+
#   theme_bw()+
#   theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#         axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#         axis.line.y=element_line(linetype=1,color="black",size=1),
#         axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#         legend.direction = "horizontal",
#         #   strip.text = ggtext::element_textbox(),
#         #legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.position = "none",
#         legend.justification = c(1, 1),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 20),
#         axis.text.x = element_text(size = 20, colour = "black", angle = 0, hjust = 1),
#         axis.text.y = element_text(size = 20, colour = "black"),
#         axis.title = element_text(size =20),
#         #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill = NA, colour = "black",
#                                         size = 1.5))
# 
# iii8<-23
# nnaamm<-names(listdraw)[iii8-1]
# if (nnaamm=="SWC"){
#   nnaamm<-"SWC"# (%)
# } else if (nnaamm=="GPP"){
#   nnaamm<-"GPP" # (gC m^-2 d^-1)
# } else if (nnaamm=="Kd"){
#   nnaamm<-"Kd" # (gC m^-2 d^-1)
# } else if (nnaamm=="Ts"){
#   nnaamm<-expression("Temp"['s']) # (gC m^-2 d^-1)
# } else if (nnaamm=="PA"){
#   nnaamm<-expression("PA") # (gC m^-2 d^-1)
# } else if (nnaamm=="Clayratio"){
#   nnaamm<-expression("Clay ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Sandratio"){
#   nnaamm<-expression("Sand ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Bowenratio"){
#   nnaamm<-expression("Bowen ratio") # (gC m^-2 d^-1)
# } else if (nnaamm=="Pre"){
#   nnaamm<-expression("Precipitation") # (gC m^-2 d^-1)
# } else if (nnaamm=="ws"){
#   nnaamm<-expression("Wind speed") # (gC m^-2 d^-1)
# }
# # print(nnaamm)
# #paste("Soil Temperature (",expression("℃"),ssesp"")
# #aaaaa<-"Soil Temperature&#176;C"
# #print(listdraw[,iii-1])
# wz8<-((max(listdraw[,iii8-1])-min(listdraw[,iii8-1]))/10)+min(listdraw[,iii8-1])
# p8<-ggplot(data = as.data.frame(listdraw[,iii8-1:iii8+1]), aes(x = listdraw[,iii8-1]))+
#   # geom_errorbar(aes(ymin =listdraw[,iii]-listdraw[,iii+1],ymax =listdraw[,iii]+listdraw[,iii+1]),width=2)+
#   geom_point(aes(x = listdraw[,iii8-1],y = listdraw[,iii8]),color="#F8766D",size=4)  +
#   geom_line(aes(x = listdraw[,iii8-1],y = listdraw[,iii8]),color="#F8766D",size=2)  +
#   #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#   ylim(0.10,0.17)+
#   xlab(nnaamm)+ylab("Surface albedo")+
#   annotate("text", x = wz8, y = 0.161,label = "(h)",size = 7,face="bold") +
#   #xlab(expression("Temp"['s']))+ylab("albedo")+
#   #ggtitle("Soil Temperature")+
#   #scale_x_continuous(limits = c(min(listdraw[,iii-1]), max(listdraw[,iii-1])), breaks = seq(0, 60,5))+
#   theme_bw()+
#   theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#         axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#         axis.line.y=element_line(linetype=1,color="black",size=1),
#         axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#         legend.direction = "horizontal",
#         #   strip.text = ggtext::element_textbox(),
#         #legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.position = "none",
#         legend.justification = c(1, 1),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 20),
#         axis.text.x = element_text(size = 20, colour = "black", angle = 0, hjust = 1),
#         axis.text.y = element_text(size = 20, colour = "black"),
#         axis.title = element_text(size =20),
#         #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill = NA, colour = "black",
#                                         size = 1.5))
library(gridExtra)

grid.arrange(p1, p2, p3, p4, p5,p6, nrow=3,ncol=2)

print("========finish pdp ====================")
#===============only change days pdp figure for continuous variables finish  ================================



#liepre<-list("GPP","Kd","SWC") # ,"Tres","THar","TTil","TS","Tsow","Tfababean" "TN","TFug","THer" ,"THar","THer"
#liepre<-list("DHar","DTil","DN","DFug","DHer","Dss")
# 300 "DFug","DN","DHer","Dres","DS","DCa","DIns"
#270  "DFug","DN","DHer","Dres","DS","Dsow"
# 240" DN", "Dres", "Dss","Dsow"
# 210  "Dres", "DFug","DN", "Dss","DHer","Dgr","DIns"
# 180 "Dres", "DN", "Dss", "DS"
liepre<-list("Dres", "DN", "Dss", "DS")
#print(liepre[1])
# gpp, diffuseradiance,temp, sm,clay, sand
num<-181
listdrawnew<-list()
listdraw <-matrix(0, nrow = num, ncol = 3*length(liepre))
listname<-list()
#print(length(liepre))
iijj<-1

for (ll in 1:length(liepre)){
  na<-as.character(liepre[ll])
  if (na %in% colnames(CAdataset_train00)){
    
    print(na)
    pre_max<-max(CAdataset_train00[,na])  # gpp max
    pre_min<-min(CAdataset_train00[,na])   # gpp min
    
    
    albedo_prediction_zong<-matrix(0, nrow = num, ncol = 4)
    pre_list<-seq(from = pre_min, to = pre_max, length.out = num) # set the gpp sequence with fixed interval of num
    for (n in 1:num){
      # print(n)
      subdataset<-CAdataset_train00   # copy the entile dataset
      subdataset[,na]<-pre_list[n]  # set the gpp as a fixed value in each loop
      #  aaa<-matrix(0, nrow = num, ncol = 1)
      albedo_prediction_zong[n,1]<-pre_list[n]
      #print(1)
      pred <- predict(rf4, subdataset[,2:ncol(subdataset)])  # using new setting to run rf to see in each gpp how the fertilizer at fixed number will effect
      # print(pred$predictions)
      albedo_prediction_zong[n,2]<-mean(pred$predictions) # get average of prediction in each temp with fixed fertilizer label.
      # print(n)
      albedo_prediction_zong[n,3]<-sd(pred$predictions)
      albedo_prediction_zong[n,4]<-na
      # print(n)
      #  albedo_prediction_zong[n,4]<-length(pred)
      
      # colnames(albedo_prediction_zong)<-c(na,"albedo","std")
      
    }
    filename<-paste("E:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant181/nogpp-up/predictors_pdp-",liepre[ll],"-181-nogpp-average.csv",sep="")
    write.csv(albedo_prediction_zong,file=filename)
    #3*ll-2
    listdraw[,iijj]<-albedo_prediction_zong[,1]
    listdraw[,iijj+1]<-albedo_prediction_zong[,2]
    listdraw[,iijj+2]<-albedo_prediction_zong[,3]
    # listdraw[,iijj+3]<-albedo_prediction_zong[,4]
    listname<-append(listname,c(na,"albedo","std"))
    # colnames(listdraw[,iijj])<-na
    listdrawnew<-append(listdrawnew,liepre[ll])
    iijj<-iijj+3
  }else{
    listdraw<-listdraw
    listdrawnew<-listdrawnew
    # listname<-append(listname,c("no","no","no"))
    
  }
}

zong<-3*length(listdrawnew)
#print(names(as.data.frame(listdraw))[1:zong])
listdraw<-as.data.frame(listdraw)
names(listdraw)[1:zong]<-listname[1:length(listname)]
#names(listdraw)=c('na',"albedo","std",'na',"albedo","std",'na',"albedo","std",'na',"albedo","std",'na',"albedo","std")
#2,5,8,11,14,17

library("ggthemes")
library("ggsci")
CAdataset<-read.csv("D:/PHD/OECHIDEE/preprocess/WINTER2020/randomforest_robustlinear/extractpredictorsRF20220921/david/test20221029/9/test/fallow/constant/constant151/nogpp-up/predictors_pdp-Dres-151-nogpp-average.csv")
### response parameter

ggplot(data = as.data.frame(CAdataset), aes(x = CAdataset[,2],y = CAdataset[,3],group=CAdataset[,5],color=CAdataset[,5]))+
  geom_point(size=4)  +
  geom_line(size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  ylim(0.10,0.17)+
  xlab("Days after management practices")+ylab("Surface albedo")+
  scale_color_npg(labels=c(expression("D"["Her"]),expression("D"["N"]),
                          expression("D"["res"]),expression("D"["ss"]),expression("D"["Til"])                                                  )
                           
  )+
  
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
        legend.position = c(0.4, 0.99),
        legend.justification = c(0.4, 0.99),
        legend.title = element_blank(),
        legend.key.size = unit(1,'cm'),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size = 24, colour = "black", angle = 0, hjust = 1),
        axis.text.y = element_text(size = 24, colour = "black"),
        axis.title = element_text(size =24),
        #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = "black",
                                        size = 1.5))




print("========finish pdp ====================")

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

#gppname<-rep("GPP", times=length(x[,1]))
#shap_dep_gpp <- data.frame(xx = x[["GPP"]], shap = shap[["GPP"]],name =gppname)
kdname<-rep("Kd", times=length(x[,1]))
shap_dep_kd <- data.frame(xx = x[["Kd"]], shap = shap[["Kd"]],name =kdname)
SWCname<-rep("SWC", times=length(x[,1]))
shap_dep_SWC <- data.frame(xx = x[["SWC"]], shap = shap[["SWC"]],name =SWCname)
Tsname<-rep("Ts", times=length(x[,1]))
shap_dep_ts <- data.frame(xx = x[["Ts"]], shap = shap[["Ts"]],name =Tsname)
#bwname<-rep("Bowenratio", times=length(x[,1]))
#shap_dep_bw <- data.frame(xx = x[["Bowenratio"]], shap = shap[["Bowenratio"]],name =bwname)
wsname<-rep("ws", times=length(x[,1]))
shap_dep_ws <- data.frame(xx = x[["ws"]], shap = shap[["ws"]],name =wsname)
#paname<-rep("PA", times=length(x[,1]))
#shap_dep_pa <- data.frame(xx = x[["PA"]], shap = shap[["PA"]],name =paname)
#clayname<-rep("Clayratio", times=length(x[,1]))
#shap_dep_clay <- data.frame(xx = x[["Clayratio"]], shap = shap[["Clayratio"]],name =clayname)

sandname<-rep("Sandratio", times=length(x[,1]))
shap_dep_sand <- data.frame(xx = x[["Sandratio"]], shap = shap[["Sandratio"]],name =sandname)


prename<-rep("Pre", times=length(x[,1]))
shap_dep_pre <- data.frame(xx = x[["Pre"]], shap = shap[["Pre"]],name =prename)

#resname<-rep("Tres", times=length(x[,1]))
#hap_dep_res <- data.frame(xx = x[["Tres"]], shap = shap[["Tres"]],name =resname)
#hername<-rep("THer", times=length(x[,1]))
#shap_dep_her <- data.frame(xx = x[["THer"]], shap = shap[["THer"]],name =hername)
#tilname<-rep("TTil", times=length(x[,1]))
#shap_dep_til <- data.frame(xx = x[["TTil"]], shap = shap[["TTil"]],name =tilname)

#fername<-rep("TN", times=length(x[,1]))
#shap_dep_fertilizers <- data.frame(xx = x[["TN"]], shap = shap[["TN"]],name =fername)
#harname<-rep("THar", times=length(x[,1]))
#shap_dep_har <- data.frame(xx = x[["THar"]], shap = shap[["THar"]],name =harname)
#fugname<-rep("TFug", times=length(x[,1]))
#shap_dep_fug <- data.frame(xx = x[["TFug"]], shap = shap[["TFug"]],name =fugname)
#ssname<-rep("Tss", times=length(x[,1]))
#shap_dep_ss <- data.frame(xx = x[["Tss"]], shap = shap[["Tss"]],name =ssname)
#Insname<-rep("TIns", times=length(x[,1]))
#shap_dep_Ins <- data.frame(xx = x[["TIns"]], shap = shap[["TIns"]],name =Insname)
#grname<-rep("Tgr", times=length(x[,1]))
#shap_dep_gr <- data.frame(xx = x[["Tgr"]], shap = shap[["Tgr"]],name =grname)
Manname<-rep("TMan", times=length(x[,1]))
shap_dep_man <- data.frame(xx = x[["TMan"]], shap = shap[["TMan"]],name =Manname)

typename<-rep("cctype", times=length(x[,1]))
shap_dep_type <- data.frame(xx = x[["cctype"]], shap = shap[["cctype"]],name =typename)

dtilname<-rep("DTil", times=length(x[,1]))
shap_dep_dtil <- data.frame(xx = x[["DTil"]], shap = shap[["DTil"]],name =dtilname)

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


a1<-shap_dep_kd
wz1<-((max(a1[,1])-min(a1[,1]))/10)+min(a1[,1])

p11<-ggplot(data = a1, aes(x = a1[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a1[,1],y =a1[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a1[,1],y =a1[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  #xlim(-1,61)+
  xlab('Kd')+ylab("Shaply value")+
  annotate("text", x = wz1, y = 0.046,label = "(a)",size = 7,face="bold") +
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

a2<-shap_dep_ts
wz2<-((max(a2[,1])-min(a2[,1]))/10)+min(a2[,1])

p22<-ggplot(data = a2, aes(x = a2[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a2[,1],y =a2[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a2[,1],y =a2[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  
  #xlim(-1,61)+
  xlab(expression("Temp"['s']))+ylab("Shaply value")+
  annotate("text", x = wz2, y = 0.046,label = "(b)",size = 7,face="bold") +
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

a3<-shap_dep_SWC
wz3<-((max(a3[,1])-min(a3[,1]))/10)+min(a3[,1])

p33<-ggplot(data = a3, aes(x = a3[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a3[,1],y =a3[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a3[,1],y =a3[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
 # xlim(-1,61)+
  xlab('SWC')+ylab("Shaply value")+
  annotate("text", x = wz3, y = 0.046,label = "(c)",size = 7,face="bold") +
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


a5<-shap_dep_ws
wz5<-((max(a5[,1])-min(a5[,1]))/10)+min(a5[,1])

p55<-ggplot(data = a5, aes(x = a5[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a5[,1],y =a5[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a5[,1],y =a5[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  # xlim(-1,61)+
  xlab('Wind speed')+ylab("Shaply value")+
  annotate("text", x = wz5, y = 0.046,label = "(d)",size = 7,face="bold") +
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

a4<-shap_dep_sand
wz4<-((max(a4[,1])-min(a4[,1]))/10)+min(a4[,1])

p44<-ggplot(data = a4, aes(x = a4[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a4[,1],y =a4[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a4[,1],y =a4[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  # xlim(-1,61)+
  xlab("Sand ratio")+ylab("Shaply value")+
  annotate("text", x = wz4, y = 0.046,label = "(e)",size = 7,face="bold") +
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



a6<-shap_dep_pre
wz6<-((max(a6[,1])-min(a6[,1]))/10)+min(a6[,1])

p66<-ggplot(data = a6, aes(x = a6[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a6[,1],y =a6[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a6[,1],y =a6[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  # xlim(-1,61)+
  xlab('Precipitation')+ylab("Shaply value")+
  annotate("text", x = wz6, y = 0.046,label = "(f)",size = 7,face="bold") +
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


# a7<-shap_dep_type
# wz7<-((max(a7[,1])-min(a7[,1]))/10)+min(a7[,1])
# 
# p77<-ggplot(data = a7, aes(x = a7[,1]))+
#   geom_hline(aes(yintercept=0),colour = "black",size=2)+
#   geom_point(aes(x = a7[,1],y =a7[,2]),color="blue",size=4,alpha = 0.3)  +
#   #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
#   #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#   geom_smooth(aes(x = a7[,1],y =a7[,2]),colour="red",se=TRUE,size=3,method="loess") +
#   ylim(-0.05,0.05)+
#   # xlim(-1,61)+
#   xlab("Type"['cc'])+ylab("Shaply value")+
#   annotate("text", x = wz7, y = 0.046,label = "(g)",size = 7,face="bold") +
#   theme_bw()+
#   theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#         axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#         axis.line.y=element_line(linetype=1,color="black",size=1),
#         axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#         legend.direction = "horizontal",
#         #   strip.text = ggtext::element_textbox(),
#         #legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.position = "none",
#         legend.justification = c(1, 1),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 18),
#         axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
# 
# 
#         axis.text.y = element_text(size = 18, colour = "black"),
#         axis.title = element_text(size =18),
#         #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill = NA, colour = "black",
#                                         size = 1.5))


a8<-shap_dep_dtil
wz8<-((max(a8[,1])-min(a8[,1]))/10)+min(a8[,1])

p88<-ggplot(data = a8, aes(x = a8[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a8[,1],y =a8[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a8[,1],y =a8[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
 #xlim(-1,61)+
  xlab('Depth of tillage')+ylab("Shaply value")+
  annotate("text", x = wz8, y = 0.046,label = "(g)",size = 7,face="bold") +
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



a9<-shap_dep_man
wz9<-((max(a9[,1])-min(a9[,1]))/10)+min(a9[,1])

p99<-ggplot(data = a9, aes(x = a9[,1]))+
  geom_hline(aes(yintercept=0),colour = "black",size=2)+
  geom_point(aes(x = a9[,1],y =a9[,2]),color="blue",size=4,alpha = 0.3)  +
  #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
  #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
  geom_smooth(aes(x = a9[,1],y =a9[,2]),colour="red",se=TRUE,size=3,method="loess") +
  ylim(-0.05,0.05)+
  xlim(-1,61)+
  xlab('Days after manure')+ylab("Shaply value")+
  annotate("text", x = wz9, y = 0.046,label = "(h)",size = 7,face="bold") +
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
# 
# a10<-shap_dep_res
# wz10<-((max(a10[,1])-min(a10[,1]))/10)+min(a10[,1])
# 
# p100<-ggplot(data = a10, aes(x = a10[,1]))+
#   geom_hline(aes(yintercept=0),colour = "black",size=2)+
#   geom_point(aes(x = a10[,1],y =a10[,2]),color="blue",size=4,alpha = 0.3)  +
# #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
# #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#   geom_smooth(aes(x = a10[,1],y =a10[,2]),colour="red",se=TRUE,size=3,method="loess") +
#   ylim(-0.05,0.05)+
#   xlim(-1,61)+
#   xlab('days after crop residues')+ylab("Shaply value")+
#   annotate("text", x = wz10, y = 0.046,label = "(i)",size = 7,face="bold") +
#   theme_bw()+
#   theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#         axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#         axis.line.y=element_line(linetype=1,color="black",size=1),
#         axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#         legend.direction = "horizontal",
# #   strip.text = ggtext::element_textbox(),
# #legend.background = element_rect(fill = "transparent", colour = NA),
#         legend.position = "none",
#         legend.justification = c(1, 1),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 18),
#         axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
#         axis.text.y = element_text(size = 18, colour = "black"),
#         axis.title = element_text(size =18),
#         #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill = NA, colour = "black",
#                                         size = 1.5))
# 
# a11<-shap_dep_har
# wz11<-((max(a11[,1])-min(a11[,1]))/10)+min(a11[,1])
# # 
# p111<-ggplot(data = a11, aes(x = a11[,1]))+
#    geom_hline(aes(yintercept=0),colour = "black",size=2)+
#    geom_point(aes(x = a11[,1],y =a11[,2]),color="blue",size=4,alpha = 0.3)  +
# #   #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
# #   #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#    geom_smooth(aes(x = a11[,1],y =a11[,2]),colour="red",se=TRUE,size=3,method="loess") +
#    ylim(-0.05,0.05)+
#    xlim(-1,61)+
#    xlab('days after harvest')+ylab("Shaply value")+
#    annotate("text", x = wz11, y = 0.046,label = "(j)",size = 7,face="bold") +
#    theme_bw()+
#    theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#          axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#          axis.line.y=element_line(linetype=1,color="black",size=1),
#          axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#          legend.direction = "horizontal",
#          #   strip.text = ggtext::element_textbox(),
#          #legend.background = element_rect(fill = "transparent", colour = NA),
#          legend.position = "none",
#          legend.justification = c(1, 1),
#          legend.title = element_blank(),
#          legend.text = element_text(size = 18),
#          axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
#          axis.text.y = element_text(size = 18, colour = "black"),
#          axis.title = element_text(size =18),
#          #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#          panel.grid = element_blank(),
#          panel.background = element_rect(fill = NA, colour = "black",
#                                          size = 1.5))
# 
# a12<-shap_dep_her
# wz12<-((max(a12[,1])-min(a12[,1]))/10)+min(a12[,1])
# # 
# p122<-ggplot(data = a12, aes(x = a12[,1]))+
#    geom_hline(aes(yintercept=0),colour = "black",size=2)+
#    geom_point(aes(x = a12[,1],y =a12[,2]),color="blue",size=4,alpha = 0.3)  +
# #   #  geom_line(aes(x = shap_dep_gpp[,1],y = shap_dep_gpp[,2]),color="#F8766D",size=2)  +
# #   #geom_errorbar(data=as.data.frame(listdraw[,iii-1:iii+1]),aes(x=listdraw[,iii-1],ymin=listdraw[,iii]-1.96*listdraw[,iii+1],ymax=listdraw[,iii]+1.96*listdraw[,iii+1],color="dodgerblue2"),width=2)+
#    geom_smooth(aes(x = a12[,1],y =a12[,2]),colour="red",se=TRUE,size=3,method="loess") +
#    ylim(-0.05,0.05)+
#    xlim(-1,61)+
#    xlab('Days after herbicide')+ylab("Shaply value")+
#    annotate("text", x = wz12, y = 0.046,label = "(i)",size = 7,face="bold") +
#    theme_bw()+
#    theme(axis.line.x=element_line(linetype=1,color="black",size=1),
#          axis.ticks.x=element_line(color="black",size=1.2,lineend = 1),
#          axis.line.y=element_line(linetype=1,color="black",size=1),
#          axis.ticks.y=element_line(color="black",size=1.2,lineend = 1),
#          legend.direction = "horizontal",
# #         #   strip.text = ggtext::element_textbox(),
#          #legend.background = element_rect(fill = "transparent", colour = NA),
#          legend.position = "none",
#          legend.justification = c(1, 1),
#          legend.title = element_blank(),
#          legend.text = element_text(size = 18),
#          axis.text.x = element_text(size = 18, colour = "black", angle = 0, hjust = 1),
#          axis.text.y = element_text(size = 18, colour = "black"),
#          axis.title = element_text(size =18),
#          #  plot.title = element_text(colour = "black",size = 18,face = "bold"),
#          panel.grid = element_blank(),
#          panel.background = element_rect(fill = NA, colour = "black",
#                                          size = 1.5))


library(gridExtra)
grid.arrange(p11, p22, p33, p55, p44, p66, p88, p99,nrow=3,ncol=3)

#####################################################################################
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
  scale_y_discrete(labels=c("Ts","SWC","kd","Bowenratio","ws","PA","Sandratio",
                            "SiteIden","Pre",
                            expression("T"["res"]),expression("T"["Her"])
                            ))+
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
