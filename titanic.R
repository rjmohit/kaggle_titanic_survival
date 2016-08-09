data <- read.csv(file="data.csv",head=TRUE,sep=",")

data$surv <- ifelse(data$Survived ==1, "Y", "N")
data = subset(data,select= -c(Survived,Age,Ticket,Lname))

nzv = nearZeroVar(data,saveMetrics= TRUE)
highCorr = findCorrelation(cor(data[,sapply(data, is.numeric)]), cutoff = .85)
#data[,sapply(data, is.numeric)][,highCorr]

# preProc = preProcess(data,method=c("center", "scale", "YeoJohnson", "nzv"))
# trainBC = predict(preProc,data)




fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2,
                           classProbs = TRUE)

gbmFit <- train(surv ~ ., data = data,
                 method = "gbm",
                 trControl = fitControl,
                 metric = 'ROC',
                 preProcess = c("center","scale","nzv"),
                 verbose = F)
gbmFit




fit <- rpart(surv ~ .,
             method="class",
             data=data)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Who survived?")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

prp(fit)

fancyRpartPlot(fit)	
