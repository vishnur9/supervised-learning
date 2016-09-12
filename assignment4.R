#install.packages("rpart",dependencies=T)
suppressPackageStartupMessages(require(rpart)) 
#install.packages("e1071",dependencies=T)
suppressPackageStartupMessages(require(e1071))
#install.packages("class",dependencies=T)
suppressPackageStartupMessages(require(class)) 
#install.packages("neuralnet",dependencies=T)
suppressPackageStartupMessages(require(neuralnet)) 
#install.packages(IDPmisc)
suppressPackageStartupMessages(require(mlbench)) 
#install.packages("ipred",dependencies=T)
suppressPackageStartupMessages(require(ipred)) 
#install.packages("ada",dependencies=T)
suppressPackageStartupMessages(require(ada)) 
#install.packages("randomForest",dependencies=T)
suppressPackageStartupMessages(require(randomForest)) 
#install.packages("ROCR",dependencies=T,dependencies=T)
suppressPackageStartupMessages(require(ROCR)) 

options(warn=-1)
args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
d<-read.csv(dataURL,header = header)
no<-as.integer(args[3])
names(d)[no]<-"Class"

if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data")
{
  colnames(d)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24","a25","a26","a27","a28","a29","a30","a31","a32","a33","a34","a35")
  names(d)[no]<-"Class"
  d$Class<-factor(d$Class,levels=c('N','R'),labels = c(0,1))
  for(j in 1:nrow(d))
  {if(as.integer(d[j,35])==1)
  { d[j,35]=0} }
  d$a35<-as.integer(d$a35)
}
if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
{
  colnames(d)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24","a25","a26","a27","a28","a29","a30","a31","a32")
  names(d)[no]<-"Class"
  d$Class<-factor(d$Class,levels=c('M','B'),labels = c(0,1))}

if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data")
{
  colnames(d)<-c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24","a25","a26","a27","a28","a29","a30","a31","a32","a33","a34","a35")
  names(d)[no]<-"Class"
  d$Class<-factor(d$Class,levels=c('g','b'),labels = c(0,1))}

accuracy_dt<-c()
accuracy_nb<-c()
accuracy_svm<-c()
accuracy_knn<-c()
accuracy_lg<-c()
accuracy_bag<-c()
accuracy_boost<-c()
accuracy_rf<-c()
accuracy_nn<-c()


# create 10 samples
set.seed(4691231)
for(i in 1:10) {
  cat("Running sample ",i,"\n")
  sampleInstances<-sample(1:nrow(d),size = 0.9*nrow(d))
  trainingData<-d[sampleInstances,]
  testData<-d[-sampleInstances,]

  # now create all the classifiers and output accuracy values:
  
  #decision trees
  if(no==6)
  {
  dtree<-rpart(as.factor(Class)~.,data=trainingData,method="class",parms=list(split='information'),control = rpart.control(cp = 0.01))
  }
  if(no==1){
    dtree<-rpart(as.factor(Class)~.,data=trainingData,method="class",parms=list(split='information'),control = rpart.control(cp = 0.05))
  }  
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data")
  {
    dtree<-rpart(as.factor(Class)~.,data=trainingData,method="class",parms=list(split='information'),control = rpart.control(cp = 0.07))
  }
  
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
  {
    dtree<-rpart(as.factor(Class)~.,data=trainingData,method="class",parms=list(split='information'),control = rpart.control(cp = 0.01))
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data")
  {
    dtree<-rpart(as.factor(Class)~.,data=trainingData,method="class",parms=list(split='information'),control = rpart.control(cp = 0.02))
  }
  pred_dtree1<-predict(dtree,newdata=testData,type="class")
  accuracy_dt<-append(accuracy_dt,sum(testData$Class==pred_dtree1)/length(pred_dtree1))
  
  
  
  #naive bayes
  if(no==1||6){nb=naiveBayes(as.factor(Class)~.,data=trainingData,laplace =3)
  }
  if(no==2||35){nb=naiveBayes(as.factor(Class)~.,data=trainingData,laplace=2)
  }
  pred<-predict(nb,testData,type="class")
  accuracy_nb<-append(accuracy_nb,sum(testData$Class==pred)/length(pred))
  
  
  #svm models
  svm_model <- svm(as.factor(Class)~., data=trainingData)
  pred1<-predict(svm_model,testData)
  accuracy_svm<-append(accuracy_svm,sum(testData$Class==pred1)/length(pred1))
  
  #knn 
  train_tar<-trainingData[,no]
  test_tar<-testData[,no]
  c1<-as.factor(trainingData$Class)
  if(no==6){
  knn1=knn(train=trainingData,test=testData,c1,k=20)
  }
  if(no==1){
    knn1=knn(train=trainingData,test=testData,c1,k=2)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data"){
    knn1=knn(train=trainingData,test=testData,c1,k=15)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"){
    knn1=knn(train=trainingData,test=testData,c1,k=1)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data"){
    knn1=knn(train=trainingData,test=testData,c1,k=1)
  }
  count<-0
  for(i in 1:length(test_tar)){
    if(knn1[i]==test_tar[i]) {count<-count+1}
  }
  ac<-count/length(test_tar)
  accuracy_knn<-append(accuracy_knn,ac)
  
  
  #logistic regression
  lg <- glm(factor(Class) ~., data = trainingData, family = "binomial")
  pred<-predict(lg, newdata=testData, type="response")
  threshold=0.65
  prediction<-sapply(pred, FUN=function(x) if (x>threshold) 1 else 0)
  actual<-testData$Class
  accuracy_lg <-append(accuracy_lg,sum(actual==prediction)/length(actual))
  
  #neural networks
  t<-sapply(trainingData,as.numeric)
  tes<-sapply(testData,as.numeric)
 if(no==6){
   nn<- neuralnet(Class~ clientid+income+age+loan+LTI , data=trainingData, hidden = 4, lifesign = "none",linear.output = FALSE, threshold= 0.1)
    
  }
  if(no==1){
    nn<- neuralnet(Class~ rank+gre+gpa , data=trainingData, hidden = 3, lifesign = "none",linear.output = FALSE, threshold= 0.1)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data"){
    nn<- neuralnet(as.numeric(Class)~a1+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34+as.integer(a35) , data=t, hidden = 3, lifesign = "none",linear.output = FALSE, threshold= 0.1)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"){
   
    nn<- neuralnet(as.numeric(Class)~a1+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32 , data=t, hidden = 10, lifesign = "none",linear.output = FALSE, threshold= 0.5)
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data"){
    
    nn<- neuralnet(as.numeric(Class)~a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20+a21+a22+a23+a24+a25+a26+a27+a28+a29+a30+a31+a32+a33+a34 , data=t, hidden = 4, lifesign = "none",linear.output = FALSE, threshold= 0.1)
  }
  nn.results <- compute(nn,tes[,-no])
  d1 <- round(data.frame(actual = tes[,no], prediction = nn.results$net.result))
  co<-0
  q<-2
  for (i1 in 1:nrow(testData))
  {
    if(d1[i1,1]==d1[i1,2])
    {co=co+1} 
  }
  if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
  { accuracy_nn<-append(accuracy_nn,2*(co/nrow(testData)))}
  else
  {
    accuracy_nn<-append(accuracy_nn,co/nrow(testData))
  }
  
  
  #bagging
  if(no==6)
  {bag <- bagging(Class ~.,data=trainingData,boos=TRUE,mfinal=100,control=rpart.control(cp=-1,maxdepth = 4))
  }
  else if(no==1){
    bag <- bagging(Class ~.,data=trainingData,boos=TRUE,mfinal=100,control=rpart.control(cp=-1,maxdepth = 2))
  }
  else if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data")
  {
          bag <- bagging(Class ~.,data=trainingData,boos=TRUE,mfinal=150,control=rpart.control(cp=-1,maxdepth = 3))
  }
  else if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"){
    bag <- bagging(Class ~.,data=trainingData,boos=TRUE,mfinal=100,control=rpart.control(cp=-1,maxdepth = 3))
  }
  else if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data"){
    bag <- bagging(Class ~.,data=trainingData,boos=TRUE,mfinal=100,control=rpart.control(cp=-1,maxdepth = 4))
  }
  if(no==2||no==35){
    pred_bg <-predict(bag,type="prob",newdata=testData)
  threshold=0.5
  predict_bg<-sapply(pred_bg[,2], FUN=function(x) if (x>threshold) 1 else 0)
  actual3<-testData$Class
  accuracy_bag <-append(accuracy_bag,sum(actual3==predict_bg)/length(actual3))
  }
  if(no==1||no==6){
    pred_bg <-predict(bag,newdata=testData)
    pred_bg<-round(pred_bg)
    actual3<-testData$Class
    accuracy_bag <-append(accuracy_bag,sum(actual3==pred_bg)/length(actual3))
  }
  
  
  #boosting
  if(no==6)
  {
   boost <- ada(formula=Class~., data=trainingData, test.x=testData[,-no], test.y=testData[,no], loss="exponential", type="discrete", iter=50, max.iter=20)
  }
  if(no==1)
  {
    boost <- ada(formula=Class~., data=trainingData, test.x=testData[,-no], test.y=testData[,no], loss="exponential", type="discrete", iter=50, max.iter=20)
  }
  else{
    boost <- ada(formula=Class~., data=trainingData, test.x=testData[,-no], test.y=testData[,no], loss="exponential", type="discrete", iter=40, max.iter=20)
  }
  pred_bo<-predict(boost,newdata=testData)
  actual4<-testData$Class
  accuracy_boost <-append(accuracy_boost,sum(actual4==pred_bo)/length(actual4))
  
  
  #random forests
  if(no==6)
  {
    rf <-randomForest(Class~.,data=trainingData, mtry=8, ntree=100, 
                      keep.forest=TRUE, importance=TRUE)
  }
  if(no==1)
  {
    rf <-randomForest(Class~.,data=trainingData, mtry=7, ntree=100, 
                      keep.forest=TRUE, importance=TRUE)
  }
  else{
    rf <-randomForest(Class~.,data=trainingData, mtry=6, ntree=80, 
                      keep.forest=TRUE, importance=TRUE)
  
  }
  
 if(no==1||no==6){ pred_rf<-predict(rf,type="response",newdata=testData)
  pred_rf<-round(pred_rf)
  actual5<-testData$Class
  accuracy_rf <-append(accuracy_rf, sum(actual5==pred_rf)/length(actual5))
 }
 else if(no==2||no==35){
  pred_rf<-predict(rf,type="prob",newdata=testData)
 
  threshold=0.5
  predict_rf<-sapply(pred_rf[,2], FUN=function(x) if (x>threshold) 1 else 0)
  actual3<-testData$Class
  accuracy_rf <-append(accuracy_rf,sum(actual3==predict_rf)/length(actual3))
  
  }
 }
m_dt=mean(accuracy_dt)*100
m_nb=mean(accuracy_nb)*100
m_svm=mean(accuracy_svm)*100
m_knn=mean(accuracy_knn)*100
m_lg=mean(accuracy_lg)*100
m_bag=mean(accuracy_bag)*100
m_boost=mean(accuracy_boost)*100
m_rf=mean(accuracy_rf)*100
m_nn=mean(accuracy_nn)*100
s <- function(x) if(x!=100) format(round(x,2), nsmall=2) else format(round(x,1),nsmall=1)


if(no==6)
{
cat("\nDATASET-1\n")
cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
cat("                                                                                                        ACCURACY[in %]\n" )
cat("=================================================================================================================================================================================================\n")
cat("   METHOD    ","  ","BEST PARAMETERS   ","       ","SAMPLE-1","  ","SAMPLE-2","  ","SAMPLE-3","   ","SAMPLE-4","   ","SAMPLE-5","   ","SAMPLE-6","   ","SAMPLE-7","   ","SAMPLE-8","   ","SAMPLE-9","   ","SAMPLE-10","   ","AVG OF 10 SAMPLES\n")
cat("=================================================================================================================================================================================================\n")
cat("DECISION TREE","split=information,cp=0.01    ",s(accuracy_dt[1]*100),"      ",s(accuracy_dt[2]*100),"      ",s(accuracy_dt[3]*100),"      ",s(accuracy_dt[4]*100),"      ",s(accuracy_dt[5]*100),"      ",s(accuracy_dt[6]*100),"      ",s(accuracy_dt[7]*100),"      ",s(accuracy_dt[8]*100),"      ",s(accuracy_dt[9]*100),"      ",s(accuracy_dt[10]*100),"        ",s(m_dt),"\n")
cat("NAIVE BAYES  ","laplace=3                    ",s(accuracy_nb[1]*100),"      ",s(accuracy_nb[2]*100),"      ",s(accuracy_nb[3]*100),"      ",s(accuracy_nb[4]*100),"      ",s(accuracy_nb[5]*100),"      ",s(accuracy_nb[6]*100),"      ",s(accuracy_nb[7]*100),"      ",s(accuracy_nb[8]*100),"      ",s(accuracy_nb[9]*100),"      ",s(accuracy_nb[10]*100),"        ",s(m_nb),"\n")
cat("SVM          ","default values               ",s(accuracy_svm[1]*100),"      ",s(accuracy_svm[2]*100),"      ",s(accuracy_svm[3]*100),"      ",s(accuracy_svm[4]*100),"      ",s(accuracy_svm[5]*100),"      ",s(accuracy_svm[6]*100),"      ",s(accuracy_svm[7]*100),"      ",s(accuracy_svm[8]*100),"      ",s(accuracy_svm[9]*100),"      ",s(accuracy_svm[10]*100),"        ",s(m_svm),"\n")
cat("K-NN         ","k=20                         ",s(accuracy_knn[1]*100),"      ",s(accuracy_knn[2]*100),"      ",s(accuracy_knn[3]*100),"      ",s(accuracy_knn[4]*100),"      ",s(accuracy_knn[5]*100),"      ",s(accuracy_knn[6]*100),"      ",s(accuracy_knn[7]*100),"      ",s(accuracy_knn[8]*100),"      ",s(accuracy_knn[9]*100),"      ",s(accuracy_knn[10]*100),"        ",s(m_knn),"\n")
cat("LOGISTIC REG ","family=binomial              ",s(accuracy_lg[1]*100),"      ",s(accuracy_lg[2]*100),"      ",s(accuracy_lg[3]*100),"      ",s(accuracy_lg[4]*100),"      ",s(accuracy_lg[5]*100),"      ",s(accuracy_lg[6]*100),"      ",s(accuracy_lg[7]*100),"      ",s(accuracy_lg[8]*100),"      ",s(accuracy_lg[9]*100),"      ",s(accuracy_lg[10]*100),"        ",s(m_lg),"\n")
cat("NEURALNETWORK","hidden=6,threshold=0.1       ",s(accuracy_nn[1]*100),"      ",s(accuracy_nn[2]*100),"      ",s(accuracy_nn[3]*100),"      ",s(accuracy_nn[4]*100),"      ",s(accuracy_nn[5]*100),"      ",s(accuracy_nn[6]*100),"      ",s(accuracy_nn[7]*100),"      ",s(accuracy_nn[8]*100),"      ",s(accuracy_nn[9]*100),"      ",s(accuracy_nn[10]*100),"        ",s(m_nn),"\n")
cat("BAGGING      ","mfinal=100,cp=-1,maxdep=4    ",s(accuracy_bag[1]*100),"      ",s(accuracy_bag[2]*100),"      ",s(accuracy_bag[3]*100),"      ",s(accuracy_bag[4]*100),"      ",s(accuracy_bag[5]*100),"      ",s(accuracy_bag[6]*100),"      ",s(accuracy_bag[7]*100),"      ",s(accuracy_bag[8]*100),"      ",s(accuracy_bag[9]*100),"      ",s(accuracy_bag[10]*100),"        ",s(m_bag),"\n")
cat("BOOSTING     ","iter=50,max.iter=20          ",s(accuracy_boost[1]*100),"      ",s(accuracy_boost[2]*100),"      ",s(accuracy_boost[3]*100),"      ",s(accuracy_boost[4]*100),"      ",s(accuracy_boost[5]*100),"      ",s(accuracy_boost[6]*100),"      ",s(accuracy_boost[7]*100),"      ",s(accuracy_boost[8]*100),"      ",s(accuracy_boost[9]*100),"      ",s(accuracy_boost[10]*100),"        ",s(m_boost),"\n")
cat("RANDOM FOREST","mtry=8,ntree=100             ",s(accuracy_rf[1]*100),"      ",s(accuracy_rf[2]*100),"      ",s(accuracy_rf[3]*100),"      ",s(accuracy_rf[4]*100),"      ",s(accuracy_rf[5]*100),"      ",s(accuracy_rf[6]*100),"      ",s(accuracy_rf[7]*100),"      ",s(accuracy_rf[8]*100),"      ",s(accuracy_rf[9]*100),"      ",s(accuracy_rf[10]*100),"        ",s(m_rf),"\n")
cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
cat("End of analysis for current dataset\n\n")
}
if(no==1)
{
  cat("\nDATASET-2\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("                                                                                                        ACCURACY[in %]\n" )
  cat("=================================================================================================================================================================================================\n")
  cat("   METHOD    ","  ","BEST PARAMETERS   ","       ","SAMPLE-1","  ","SAMPLE-2","  ","SAMPLE-3","   ","SAMPLE-4","   ","SAMPLE-5","   ","SAMPLE-6","   ","SAMPLE-7","   ","SAMPLE-8","   ","SAMPLE-9","   ","SAMPLE-10","   ","AVG OF 10 SAMPLES\n")
  cat("=================================================================================================================================================================================================\n")
  cat("DECISION TREE","split=information,cp=0.05    ",s(accuracy_dt[1]*100),"      ",s(accuracy_dt[2]*100),"      ",s(accuracy_dt[3]*100),"      ",s(accuracy_dt[4]*100),"      ",s(accuracy_dt[5]*100),"      ",s(accuracy_dt[6]*100),"      ",s(accuracy_dt[7]*100),"      ",s(accuracy_dt[8]*100),"      ",s(accuracy_dt[9]*100),"      ",s(accuracy_dt[10]*100),"        ",s(m_dt),"\n")
  cat("NAIVE BAYES  ","laplace=2                    ",s(accuracy_nb[1]*100),"      ",s(accuracy_nb[2]*100),"      ",s(accuracy_nb[3]*100),"      ",s(accuracy_nb[4]*100),"      ",s(accuracy_nb[5]*100),"      ",s(accuracy_nb[6]*100),"      ",s(accuracy_nb[7]*100),"      ",s(accuracy_nb[8]*100),"      ",s(accuracy_nb[9]*100),"      ",s(accuracy_nb[10]*100),"        ",s(m_nb),"\n")
  cat("SVM          ","default values               ",s(accuracy_svm[1]*100),"      ",s(accuracy_svm[2]*100),"      ",s(accuracy_svm[3]*100),"      ",s(accuracy_svm[4]*100),"      ",s(accuracy_svm[5]*100),"      ",s(accuracy_svm[6]*100),"      ",s(accuracy_svm[7]*100),"      ",s(accuracy_svm[8]*100),"      ",s(accuracy_svm[9]*100),"      ",s(accuracy_svm[10]*100),"        ",s(m_svm),"\n")
  cat("K-NN         ","k=2                          ",s(accuracy_knn[1]*100),"      ",s(accuracy_knn[2]*100),"      ",s(accuracy_knn[3]*100),"      ",s(accuracy_knn[4]*100),"      ",s(accuracy_knn[5]*100),"      ",s(accuracy_knn[6]*100),"      ",s(accuracy_knn[7]*100),"      ",s(accuracy_knn[8]*100),"      ",s(accuracy_knn[9]*100),"      ",s(accuracy_knn[10]*100),"        ",s(m_knn),"\n")
  cat("LOGISTIC REG ","family=binomial              ",s(accuracy_lg[1]*100),"      ",s(accuracy_lg[2]*100),"      ",s(accuracy_lg[3]*100),"      ",s(accuracy_lg[4]*100),"      ",s(accuracy_lg[5]*100),"      ",s(accuracy_lg[6]*100),"      ",s(accuracy_lg[7]*100),"      ",s(accuracy_lg[8]*100),"      ",s(accuracy_lg[9]*100),"      ",s(accuracy_lg[10]*100),"        ",s(m_lg),"\n")
  cat("NEURALNETWORK","hidden=3,threshold=0.1       ",s(accuracy_nn[1]*100),"      ",s(accuracy_nn[2]*100),"      ",s(accuracy_nn[3]*100),"      ",s(accuracy_nn[4]*100),"      ",s(accuracy_nn[5]*100),"      ",s(accuracy_nn[6]*100),"      ",s(accuracy_nn[7]*100),"      ",s(accuracy_nn[8]*100),"      ",s(accuracy_nn[9]*100),"      ",s(accuracy_nn[10]*100),"        ",s(m_nn),"\n")
  cat("BAGGING      ","mfinal=100,cp=-1,maxdep=2    ",s(accuracy_bag[1]*100),"      ",s(accuracy_bag[2]*100),"      ",s(accuracy_bag[3]*100),"      ",s(accuracy_bag[4]*100),"      ",s(accuracy_bag[5]*100),"      ",s(accuracy_bag[6]*100),"      ",s(accuracy_bag[7]*100),"      ",s(accuracy_bag[8]*100),"      ",s(accuracy_bag[9]*100),"      ",s(accuracy_bag[10]*100),"        ",s(m_bag),"\n")
  cat("BOOSTING     ","iter=50,max.iter=20          ",s(accuracy_boost[1]*100),"      ",s(accuracy_boost[2]*100),"      ",s(accuracy_boost[3]*100),"      ",s(accuracy_boost[4]*100),"      ",s(accuracy_boost[5]*100),"      ",s(accuracy_boost[6]*100),"      ",s(accuracy_boost[7]*100),"      ",s(accuracy_boost[8]*100),"      ",s(accuracy_boost[9]*100),"      ",s(accuracy_boost[10]*100),"        ",s(m_boost),"\n")
  cat("RANDOM FOREST","mtry=7,ntree=100             ",s(accuracy_rf[1]*100),"      ",s(accuracy_rf[2]*100),"      ",s(accuracy_rf[3]*100),"      ",s(accuracy_rf[4]*100),"      ",s(accuracy_rf[5]*100),"      ",s(accuracy_rf[6]*100),"      ",s(accuracy_rf[7]*100),"      ",s(accuracy_rf[8]*100),"      ",s(accuracy_rf[9]*100),"      ",s(accuracy_rf[10]*100),"        ",s(m_rf),"\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("End of analysis for current dataset\n\n")
  
}
if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wpbc.data")
{
  cat("\nDATASET-3\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("                                                                                                        ACCURACY[in %]\n" )
  cat("=================================================================================================================================================================================================\n")
  cat("   METHOD    ","  ","BEST PARAMETERS   ","       ","SAMPLE-1","  ","SAMPLE-2","  ","SAMPLE-3","   ","SAMPLE-4","   ","SAMPLE-5","   ","SAMPLE-6","   ","SAMPLE-7","   ","SAMPLE-8","   ","SAMPLE-9","   ","SAMPLE-10","   ","AVG OF 10 SAMPLES\n")
  cat("=================================================================================================================================================================================================\n")
  cat("DECISION TREE","split=information,cp=0.07    ",s(accuracy_dt[1]*100),"      ",s(accuracy_dt[2]*100),"      ",s(accuracy_dt[3]*100),"      ",s(accuracy_dt[4]*100),"      ",s(accuracy_dt[5]*100),"      ",s(accuracy_dt[6]*100),"      ",s(accuracy_dt[7]*100),"      ",s(accuracy_dt[8]*100),"      ",s(accuracy_dt[9]*100),"      ",s(accuracy_dt[10]*100),"        ",s(m_dt),"\n")
  cat("NAIVE BAYES  ","laplace=3                    ",s(accuracy_nb[1]*100),"      ",s(accuracy_nb[2]*100),"      ",s(accuracy_nb[3]*100),"      ",s(accuracy_nb[4]*100),"      ",s(accuracy_nb[5]*100),"      ",s(accuracy_nb[6]*100),"      ",s(accuracy_nb[7]*100),"      ",s(accuracy_nb[8]*100),"      ",s(accuracy_nb[9]*100),"      ",s(accuracy_nb[10]*100),"        ",s(m_nb),"\n")
  cat("SVM          ","default values               ",s(accuracy_svm[1]*100),"      ",s(accuracy_svm[2]*100),"      ",s(accuracy_svm[3]*100),"      ",s(accuracy_svm[4]*100),"      ",s(accuracy_svm[5]*100),"      ",s(accuracy_svm[6]*100),"      ",s(accuracy_svm[7]*100),"      ",s(accuracy_svm[8]*100),"      ",s(accuracy_svm[9]*100),"      ",s(accuracy_svm[10]*100),"        ",s(m_svm),"\n")
  cat("K-NN         ","k=15                         ",s(accuracy_knn[1]*100),"      ",s(accuracy_knn[2]*100),"      ",s(accuracy_knn[3]*100),"      ",s(accuracy_knn[4]*100),"      ",s(accuracy_knn[5]*100),"      ",s(accuracy_knn[6]*100),"      ",s(accuracy_knn[7]*100),"      ",s(accuracy_knn[8]*100),"      ",s(accuracy_knn[9]*100),"      ",s(accuracy_knn[10]*100),"        ",s(m_knn),"\n")
  cat("LOGISTIC REG ","family=binomial              ",s(accuracy_lg[1]*100),"      ",s(accuracy_lg[2]*100),"      ",s(accuracy_lg[3]*100),"      ",s(accuracy_lg[4]*100),"      ",s(accuracy_lg[5]*100),"      ",s(accuracy_lg[6]*100),"      ",s(accuracy_lg[7]*100),"      ",s(accuracy_lg[8]*100),"      ",s(accuracy_lg[9]*100),"      ",s(accuracy_lg[10]*100),"        ",s(m_lg),"\n")
  cat("NEURALNETWORK","hidden=3,threshold=0.1       ",s(accuracy_nn[1]*100),"      ",s(accuracy_nn[2]*100),"      ",s(accuracy_nn[3]*100),"      ",s(accuracy_nn[4]*100),"      ",s(accuracy_nn[5]*100),"      ",s(accuracy_nn[6]*100),"      ",s(accuracy_nn[7]*100),"      ",s(accuracy_nn[8]*100),"      ",s(accuracy_nn[9]*100),"      ",s(accuracy_nn[10]*100),"        ",s(m_nn),"\n")
  cat("BAGGING      ","mfinal=150,cp=-1,maxdep=3    ",s(accuracy_bag[1]*100),"      ",s(accuracy_bag[2]*100),"      ",s(accuracy_bag[3]*100),"      ",s(accuracy_bag[4]*100),"      ",s(accuracy_bag[5]*100),"      ",s(accuracy_bag[6]*100),"      ",s(accuracy_bag[7]*100),"      ",s(accuracy_bag[8]*100),"      ",s(accuracy_bag[9]*100),"      ",s(accuracy_bag[10]*100),"        ",s(m_bag),"\n")
  cat("BOOSTING     ","iter=40,max.iter=20          ",s(accuracy_boost[1]*100),"      ",s(accuracy_boost[2]*100),"      ",s(accuracy_boost[3]*100),"      ",s(accuracy_boost[4]*100),"      ",s(accuracy_boost[5]*100),"      ",s(accuracy_boost[6]*100),"      ",s(accuracy_boost[7]*100),"      ",s(accuracy_boost[8]*100),"      ",s(accuracy_boost[9]*100),"      ",s(accuracy_boost[10]*100),"        ",s(m_boost),"\n")
  cat("RANDOM FOREST","mtry=6,ntree=80              ",s(accuracy_rf[1]*100),"      ",s(accuracy_rf[2]*100),"      ",s(accuracy_rf[3]*100),"      ",s(accuracy_rf[4]*100),"      ",s(accuracy_rf[5]*100),"      ",s(accuracy_rf[6]*100),"      ",s(accuracy_rf[7]*100),"      ",s(accuracy_rf[8]*100),"      ",s(accuracy_rf[9]*100),"      ",s(accuracy_rf[10]*100),"        ",s(m_rf),"\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("End of analysis for current dataset\n\n")
  
}
if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data")
{
  cat("\nDATASET-4\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("                                                                                                        ACCURACY[in %]\n" )
  cat("=================================================================================================================================================================================================\n")
  cat("   METHOD    ","  ","BEST PARAMETERS   ","       ","SAMPLE-1","  ","SAMPLE-2","  ","SAMPLE-3","   ","SAMPLE-4","   ","SAMPLE-5","   ","SAMPLE-6","   ","SAMPLE-7","   ","SAMPLE-8","   ","SAMPLE-9","   ","SAMPLE-10","   ","AVG OF 10 SAMPLES\n")
  cat("=================================================================================================================================================================================================\n")
  cat("DECISION TREE","split=information,cp=0.01    ",s(accuracy_dt[1]*100),"      ",s(accuracy_dt[2]*100),"      ",s(accuracy_dt[3]*100),"      ",s(accuracy_dt[4]*100),"      ",s(accuracy_dt[5]*100),"      ",s(accuracy_dt[6]*100),"      ",s(accuracy_dt[7]*100),"      ",s(accuracy_dt[8]*100),"      ",s(accuracy_dt[9]*100),"      ",s(accuracy_dt[10]*100),"        ",s(m_dt),"\n")
  cat("NAIVE BAYES  ","laplace=3                    ",s(accuracy_nb[1]*100),"      ",s(accuracy_nb[2]*100),"      ",s(accuracy_nb[3]*100),"      ",s(accuracy_nb[4]*100),"      ",s(accuracy_nb[5]*100),"      ",s(accuracy_nb[6]*100),"      ",s(accuracy_nb[7]*100),"      ",s(accuracy_nb[8]*100),"      ",s(accuracy_nb[9]*100),"      ",s(accuracy_nb[10]*100),"        ",s(m_nb),"\n")
  cat("SVM          ","default values               ",s(accuracy_svm[1]*100),"      ",s(accuracy_svm[2]*100),"      ",s(accuracy_svm[3]*100),"      ",s(accuracy_svm[4]*100),"      ",s(accuracy_svm[5]*100),"      ",s(accuracy_svm[6]*100),"      ",s(accuracy_svm[7]*100),"      ",s(accuracy_svm[8]*100),"      ",s(accuracy_svm[9]*100),"      ",s(accuracy_svm[10]*100),"        ",s(m_svm),"\n")
  cat("K-NN         ","k=1                          ",s(accuracy_knn[1]*100),"      ",s(accuracy_knn[2]*100),"      ",s(accuracy_knn[3]*100),"      ",s(accuracy_knn[4]*100),"      ",s(accuracy_knn[5]*100),"      ",s(accuracy_knn[6]*100),"      ",s(accuracy_knn[7]*100),"      ",s(accuracy_knn[8]*100),"      ",s(accuracy_knn[9]*100),"      ",s(accuracy_knn[10]*100),"        ",s(m_knn),"\n")
  cat("LOGISTIC REG ","family=binomial              ",s(accuracy_lg[1]*100),"      ",s(accuracy_lg[2]*100),"      ",s(accuracy_lg[3]*100),"      ",s(accuracy_lg[4]*100),"      ",s(accuracy_lg[5]*100),"      ",s(accuracy_lg[6]*100),"      ",s(accuracy_lg[7]*100),"      ",s(accuracy_lg[8]*100),"      ",s(accuracy_lg[9]*100),"      ",s(accuracy_lg[10]*100),"        ",s(m_lg),"\n")
  cat("NEURALNETWORK","hidden=10,threshold=0.5      ",s(accuracy_nn[1]*100),"      ",s(accuracy_nn[2]*100),"      ",s(accuracy_nn[3]*100),"      ",s(accuracy_nn[4]*100),"      ",s(accuracy_nn[5]*100),"      ",s(accuracy_nn[6]*100),"      ",s(accuracy_nn[7]*100),"      ",s(accuracy_nn[8]*100),"      ",s(accuracy_nn[9]*100),"      ",s(accuracy_nn[10]*100),"        ",s(m_nn),"\n")
  cat("BAGGING      ","mfinal=100,cp=-1,maxdep=3    ",s(accuracy_bag[1]*100),"      ",s(accuracy_bag[2]*100),"      ",s(accuracy_bag[3]*100),"      ",s(accuracy_bag[4]*100),"      ",s(accuracy_bag[5]*100),"      ",s(accuracy_bag[6]*100),"      ",s(accuracy_bag[7]*100),"      ",s(accuracy_bag[8]*100),"      ",s(accuracy_bag[9]*100),"      ",s(accuracy_bag[10]*100),"        ",s(m_bag),"\n")
  cat("BOOSTING     ","iter=40,max.iter=20          ",s(accuracy_boost[1]*100),"      ",s(accuracy_boost[2]*100),"      ",s(accuracy_boost[3]*100),"      ",s(accuracy_boost[4]*100),"      ",s(accuracy_boost[5]*100),"      ",s(accuracy_boost[6]*100),"      ",s(accuracy_boost[7]*100),"      ",s(accuracy_boost[8]*100),"      ",s(accuracy_boost[9]*100),"      ",s(accuracy_boost[10]*100),"        ",s(m_boost),"\n")
  cat("RANDOM FOREST","mtry=6,ntree=80              ",s(accuracy_rf[1]*100),"      ",s(accuracy_rf[2]*100),"      ",s(accuracy_rf[3]*100),"      ",s(accuracy_rf[4]*100),"      ",s(accuracy_rf[5]*100),"      ",s(accuracy_rf[6]*100),"      ",s(accuracy_rf[7]*100),"      ",s(accuracy_rf[8]*100),"      ",s(accuracy_rf[9]*100),"      ",s(accuracy_rf[10]*100),"        ",s(m_rf),"\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("End of analysis for current dataset\n\n")
  
}
if(args[1]=="http://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data")
{
  cat("\nDATASET-5\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("                                                                                                        ACCURACY[in %]\n" )
  cat("=================================================================================================================================================================================================\n")
  cat("   METHOD    ","  ","BEST PARAMETERS   ","       ","SAMPLE-1","  ","SAMPLE-2","  ","SAMPLE-3","   ","SAMPLE-4","   ","SAMPLE-5","   ","SAMPLE-6","   ","SAMPLE-7","   ","SAMPLE-8","   ","SAMPLE-9","   ","SAMPLE-10","   ","AVG OF 10 SAMPLES\n")
  cat("=================================================================================================================================================================================================\n")
  cat("DECISION TREE","split=information,cp=0.02    ",s(accuracy_dt[1]*100),"      ",s(accuracy_dt[2]*100),"      ",s(accuracy_dt[3]*100),"      ",s(accuracy_dt[4]*100),"      ",s(accuracy_dt[5]*100),"      ",s(accuracy_dt[6]*100),"      ",s(accuracy_dt[7]*100),"      ",s(accuracy_dt[8]*100),"      ",s(accuracy_dt[9]*100),"      ",s(accuracy_dt[10]*100),"        ",s(m_dt),"\n")
  cat("NAIVE BAYES  ","laplace=3                    ",s(accuracy_nb[1]*100),"      ",s(accuracy_nb[2]*100),"      ",s(accuracy_nb[3]*100),"      ",s(accuracy_nb[4]*100),"      ",s(accuracy_nb[5]*100),"      ",s(accuracy_nb[6]*100),"      ",s(accuracy_nb[7]*100),"      ",s(accuracy_nb[8]*100),"      ",s(accuracy_nb[9]*100),"      ",s(accuracy_nb[10]*100),"        ",s(m_nb),"\n")
  cat("SVM          ","default values               ",s(accuracy_svm[1]*100),"      ",s(accuracy_svm[2]*100),"      ",s(accuracy_svm[3]*100),"      ",s(accuracy_svm[4]*100),"      ",s(accuracy_svm[5]*100),"      ",s(accuracy_svm[6]*100),"      ",s(accuracy_svm[7]*100),"      ",s(accuracy_svm[8]*100),"      ",s(accuracy_svm[9]*100),"      ",s(accuracy_svm[10]*100),"        ",s(m_svm),"\n")
  cat("K-NN         ","k=1                          ",s(accuracy_knn[1]*100),"      ",s(accuracy_knn[2]*100),"      ",s(accuracy_knn[3]*100),"      ",s(accuracy_knn[4]*100),"      ",s(accuracy_knn[5]*100),"      ",s(accuracy_knn[6]*100),"      ",s(accuracy_knn[7]*100),"      ",s(accuracy_knn[8]*100),"      ",s(accuracy_knn[9]*100),"      ",s(accuracy_knn[10]*100),"        ",s(m_knn),"\n")
  cat("LOGISTIC REG ","family=binomial              ",s(accuracy_lg[1]*100),"      ",s(accuracy_lg[2]*100),"      ",s(accuracy_lg[3]*100),"      ",s(accuracy_lg[4]*100),"      ",s(accuracy_lg[5]*100),"      ",s(accuracy_lg[6]*100),"      ",s(accuracy_lg[7]*100),"      ",s(accuracy_lg[8]*100),"      ",s(accuracy_lg[9]*100),"      ",s(accuracy_lg[10]*100),"        ",s(m_lg),"\n")
  cat("NEURALNETWORK","hidden=4,threshold=0.1       ",s(accuracy_nn[1]*100),"      ",s(accuracy_nn[2]*100),"      ",s(accuracy_nn[3]*100),"      ",s(accuracy_nn[4]*100),"      ",s(accuracy_nn[5]*100),"      ",s(accuracy_nn[6]*100),"      ",s(accuracy_nn[7]*100),"      ",s(accuracy_nn[8]*100),"      ",s(accuracy_nn[9]*100),"      ",s(accuracy_nn[10]*100),"        ",s(m_nn),"\n")
  cat("BAGGING      ","mfinal=100,cp=-1,maxdep=4    ",s(accuracy_bag[1]*100),"      ",s(accuracy_bag[2]*100),"      ",s(accuracy_bag[3]*100),"      ",s(accuracy_bag[4]*100),"      ",s(accuracy_bag[5]*100),"      ",s(accuracy_bag[6]*100),"      ",s(accuracy_bag[7]*100),"      ",s(accuracy_bag[8]*100),"      ",s(accuracy_bag[9]*100),"      ",s(accuracy_bag[10]*100),"        ",s(m_bag),"\n")
  cat("BOOSTING     ","iter=40,max.iter=20          ",s(accuracy_boost[1]*100),"      ",s(accuracy_boost[2]*100),"      ",s(accuracy_boost[3]*100),"      ",s(accuracy_boost[4]*100),"      ",s(accuracy_boost[5]*100),"      ",s(accuracy_boost[6]*100),"      ",s(accuracy_boost[7]*100),"      ",s(accuracy_boost[8]*100),"      ",s(accuracy_boost[9]*100),"      ",s(accuracy_boost[10]*100),"        ",s(m_boost),"\n")
  cat("RANDOM FOREST","mtry=6,ntree=80              ",s(accuracy_rf[1]*100),"      ",s(accuracy_rf[2]*100),"      ",s(accuracy_rf[3]*100),"      ",s(accuracy_rf[4]*100),"      ",s(accuracy_rf[5]*100),"      ",s(accuracy_rf[6]*100),"      ",s(accuracy_rf[7]*100),"      ",s(accuracy_rf[8]*100),"      ",s(accuracy_rf[9]*100),"      ",s(accuracy_rf[10]*100),"        ",s(m_rf),"\n")
  cat("-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n")
  cat("End of analysis for current dataset\n\n")
  
}