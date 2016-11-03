ensemLRSVMRF<- function(train, test, dep_var_ind, run){
  #tr = train data with target variable
  #te = test data for prediction
  #dep_var_ind = index of the target column in train data
  #run = how many iterations of each algorithm (linear regression, random forest and SVM) should be run
  
  tr = train
  te = test
  
  if(missing(tr)){
    stop("Must give train data frame.")
  }
  if(missing(te)){
    stop("Must give test data frame.")
  }  
  
  if(ncol(te)!=(ncol(tr)-1)){
    stop("Test data should not contain Dependent Variable")
  }
  
  if(missing(dep_var_ind)){
    stop("Must give train data column name of Dependent Variable.")
  }
  if(missing(run)){
    stop("Must give number of runs of Random Forests and SVMs")
  }
  
  library(e1071, quietly = T, warn.conflicts = F)
  library(randomForest, quietly = T, warn.conflicts = F)
  
  LMds<-data.frame(rep(0,nrow(te)))
  RFds<-data.frame(rep(0,nrow(te)))
  SVMds<-data.frame(rep(0,nrow(te)))
  colnames(tr)[dep_var_ind]<- "target"
  
  #'m' runs 
  for(m in 1:run){
    
    #models
    lmfit<-lm(formula= target~., data=tr)
    svmfit<-svm(formula= target~., data=tr, kernel='linear')
    rffit<- randomForest(formula= target~., data=tr)
    
    lmpred<-as.numeric(predict(lmfit, newdata=te))
    rfpred<-as.numeric(predict(rffit, newdata=te))
    svmpred<-as.numeric(predict(svmfit, newdata= te))
    
    LMds[,m]<-as.numeric(lmpred)
    RFds[,m]<-as.numeric(rfpred)
    SVMds[,m]<-as.numeric(svmpred)
  }
  
  bigtemp<-data.frame(LMds)
  bigtemp<-cbind(bigtemp,RFds,SVMds)
  
  smalltemp<-0
  
  FinalScores<-data.frame(rep(0,nrow(te)))
  remove(svmfit, rffit, LMds, RFds, SVMds, m, tr, te)
  noco<-ncol(bigtemp)
  for(i in 1:nrow(bigtemp)){
    temp<-0
    temp.ind1st<-0
    temp.ind2nd<-0
    
    for(j in 1:noco){
      if(j != noco){ 
        temp.ind1st<-append(temp.ind1st, seq(j,noco,1))
        temp.ind2nd<-append(temp.ind2nd,rep(j,noco-j+1))
      }
    }
    temp.ind2nd<-temp.ind2nd[-1]
    temp.ind1st<-temp.ind1st[-1]
    
    temp<-as.numeric(bigtemp[i,temp.ind2nd]-bigtemp[i,temp.ind1st])
    
    temp<- data.frame(temp.ind2nd,temp.ind1st,as.numeric(temp))
    temp<-temp[-which(temp[,1]==temp[,2],arr.ind = T),]
    xez1<-as.numeric(temp[as.numeric(which(min(abs(temp[,3]))==abs(temp[,3])),arr.ind=T),1])
    xez1<-append(xez1,as.numeric(temp[as.numeric(which(min(abs(temp[,3]))==abs(temp[,3])),arr.ind=T),2]))
    
    smalltemp<-(bigtemp[i,xez1])  
    
    FinalScores[i,1]<-ifelse(length(smalltemp)>1,(sum(smalltemp[1:length(smalltemp)]))/length(smalltemp),smalltemp)
  }
  remove(bigtemp,smalltemp,temp,temp.ind1st,temp.ind2nd)
  return(FinalScores)
  detach("package:e1071",unload= T)
  detach("package:randomForest",unload= T)
}

ensem<-ensemLRSVMRF(train = train, test= test, dep_var_ind = 2, run=1)

train = data.frame(Numbers=rnorm(10000)*100)
train$Codes= rnorm(10000)
str(train)
test = data.frame(Numbers = train[(sample(nrow(train),500)),1])
test$predict<- ensem
test$or<-
#################

rmse1<-data.frame(run=1:10,rmse=rep(0,10))

for(i in 1:10){
  ensem<-regrensem(train = traing,test=testg1, dep_var_ind = 3, run=i)
  rmse1[i,2]<-rmse(g[,3],ensem)
}

ensem

rmse(g[,3],ensem)
str(test)
tr<-train[,6]
test<-test[,-6]
traing<-g
testg1<-g1
colnames(train)<-c("Business","Status","Risk.Category","Risk.Sub.Category","Net.Loss","Time_Diff","Recovery.Amount")
test<-test[,-6]
head(train)
class(test)



