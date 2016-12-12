# optimize two values to match pi and sqrt(50)
system.time({
library(caret)
library(genalg)
library(nnet)
library("e1071")
  
library(doMC)
registerDoMC(cores = 4)
    
set.seed(1234)


neural_net <- function(x,y){
  
  data(iris)
  
  ctrl <- trainControl(method = "cv", number=10, repeats = 5) 
  #nnet_grid <- expand.grid(.decay = 10^seq(-4, -1, 1), .size = c(8))
  nn_fit <- train(Species ~ ., iris, method = "nnet", 
                  maxit = 1000, 
                  trControl = ctrl, 
                  tuneGrid = data.frame(.decay=x, .size=y), 
                  preProcess = c("range"))

  return (max(nn_fit$results[,4]))
}

## tisti, ki ima layerje!


############################################################################3
evaluate.nn <- function(string=c()) {
  ## here comes the accuracy calculation!
  returnVal = NA;
  if (length(string) == 2) {
    #returnVal = string[1]+string[2]
    p1<-eval(parse(text=paste("10e-",round(as.numeric(string[1]),0),sep="")))
    p2<-round(as.numeric(string[2]),0)
      returnVal<-neural_net(p1,p2)
    print(returnVal)
  } else {
    stop("Expecting a chromosome of length 2!");
  }
  -returnVal
  
}
monitor.nn <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  ylim = c(obj$stringMin[2], obj$stringMax[2]);
  print(obj$iters)
  #dev.copy(png,'myplot.png')
  #print(ls(obj))
  plot(obj$population, xlim=xlim, ylim=ylim, xlab="x", ylab="y", main="Paramater monitor NN");
  #dev.off()
}
rbga.results.nnet = rbga(c(1, 1), c(12, 50),popSize = 100,iters = 100,
                    evalFunc=evaluate.nn, verbose=TRUE, mutationChance=0.01, monitorFunc=monitor.nn)
rbga.results.nnet
plot(rbga.results.nnet)


################# SVM ##################################

svm_model <- function(x,y){
  
  data(iris)
  
  ctrl <- trainControl(method = "cv", number=10, repeats = 5) 
  grid <- expand.grid(sigma = x,
                      C = y)
  
  svm_fit <- train(Species ~.,data=iris,
                    method = "svmRadial",
                    preProc = c("center","scale"),
                    metric="Accuracy",
                    tuneGrid = grid,
                    trControl=ctrl)


  return (max(svm_fit$results[,4]))
}

evaluate.svm <- function(string=c()) {
  ## here comes the accuracy calculation!
  returnVal = NA;
  if (length(string) == 2) {
    #returnVal = string[1]+string[2]
    p1<- as.numeric(string[1]) #eval(parse(text=paste("10e-",round(as.numeric(string[1]),0),sep="")))
    p2<-as.numeric(string[2])
    #print(p1)
    #print(p2)
    returnVal<-svm_model(p1,p2)
    print(returnVal)
  } else {
    stop("Expecting a chromosome of length 2!");
  }
  -returnVal
  
}
monitor.svm <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  ylim = c(obj$stringMin[2], obj$stringMax[2]);
  print(obj$iters)
  #dev.copy(png,'myplot.png')
  #print(ls(obj))
  plot(obj$population, xlim=xlim, ylim=ylim, xlab="x", ylab="y", main="Paramater monitor SVM");
  #dev.off()
}
rbga.results.svm = rbga(c(0, 0), c(5, 5),popSize = 100,iters = 100,
                         evalFunc=evaluate.svm, verbose=TRUE, mutationChance=0.01, monitorFunc=monitor.svm)
rbga.results.svm
plot(rbga.results.svm)


################# RF ###################################

rf_model <- function(x,y){
  
  data(iris)
  
  ctrl <- trainControl(method = "cv", number=10, repeats = 5) 
  grid <- expand.grid(mtry = x,
                      maxdepth = y)
  
  rf_fit <- train(Species ~.,data=iris,
                   method = "rfRules",
                   preProc = c("center","scale"),
                   metric="Accuracy",
                  # tuneGrid = grid,
                   trControl=ctrl)
  
  
  return (max(rf_fit$results[,3]))
}

evaluate.rf <- function(string=c()) {
  ## here comes the accuracy calculation!
  returnVal = NA;
  if (length(string) == 2) {
    #returnVal = string[1]+string[2]
    p1<- as.numeric(string[1]) #eval(parse(text=paste("10e-",round(as.numeric(string[1]),0),sep="")))
    p2<-as.numeric(string[2])
    returnVal<-rf_model(p1,p2)
    print(returnVal)
  } else {
    stop("Expecting a chromosome of length 2!");
  }
  -returnVal
  
}
monitor.svm <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  ylim = c(obj$stringMin[2], obj$stringMax[2]);
  print(obj$iters)
  #dev.copy(png,'myplot.png')
  #print(ls(obj))
  plot(obj$population, xlim=xlim, ylim=ylim, xlab="x", ylab="y", main="Paramater monitor rf");
  #dev.off()
}
rbga.results.rf = rbga(c(1, 1), c(5, 5),popSize = 100,iters = 10,
                        evalFunc=evaluate.rf, verbose=TRUE, mutationChance=0.01, monitorFunc=monitor.rf)
rbga.results.rf
plot(rbga.results.rf)




################# DTREES ###############################


tree_model <- function(x){
  
  data(iris)
  
  ctrl <- trainControl(method = "cv", number=10, repeats = 5) 
  grid <- expand.grid(.maxdepth = x)
  
  tree_fit <- train(Species ~.,data=iris,
                   method = "rpart2",
                   preProc = c("center","scale"),
                   metric="Accuracy",
                   tuneGrid = grid,
                   trControl=ctrl)

  return (max(tree_fit$results[,2]))
}

evaluate.tree <- function(string=c()) {
  ## here comes the accuracy calculation!
  returnVal = NA;
  if (length(string) == 1) {
    #returnVal = string[1]+string[2]
    p1<- round(as.numeric(string[1]),0) #eval(parse(text=paste("10e-",round(as.numeric(string[1]),0),sep="")))
    #print (p1)
    returnVal<-tree_model(p1)
    print(returnVal)

  } else {
    stop("Expecting a chromosome of length 1!");
  }
  -returnVal
  
}
monitor.tree <- function(obj) {
  # plot the population
  xlim = c(obj$stringMin[1], obj$stringMax[1]);
  print(obj$iters)
  #dev.copy(png,'myplot.png')
  #print(ls(obj))
  plot(obj$population, xlim=xlim, xlab="x", ylab="State", main="Paramater monitor tree");
  #dev.off()
}
rbga.results.tree = rbga(c(1),c(10),popSize = 100,iters = 100,
                        evalFunc=evaluate.tree, verbose=TRUE, mutationChance=0.01)
rbga.results.tree
plot(rbga.results.tree)



## end0time
})




