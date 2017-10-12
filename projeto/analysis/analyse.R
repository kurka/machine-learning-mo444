mo444LinearRegression <- function(use.grouped=FALSE, min.retweets.count=1, max.retweets.count=1000000, time.stamps.from=1, time.stamps.to=28) {
  library('MASS')

  if (use.grouped) {
    local.feature.vectors <- feature.vectors.norm.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- feature.vectors.norm
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data >= min.retweets.count & local.observation.data <= max.retweets.count)
  local.feature.vectors <- local.feature.vectors[indexes,c(time.stamps.from:time.stamps.to, (28+time.stamps.from):(28+time.stamps.to), 57:70)]
  local.observation.data <- local.observation.data[indexes]

  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  feature.vectors.train <- local.feature.vectors[1:local.train.size,]
  feature.vectors.validation <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  observation.data.train <- local.observation.data[1:local.train.size]
  observation.data.validation <- local.observation.data[(local.train.size+1):local.num.tweets]

  #theta - parameters
  trained.params <- ginv(feature.vectors.train) %*% observation.data.train
  estimated.results.train <- as.vector(feature.vectors.train %*% trained.params)
  estimated.results.validation <- as.vector(feature.vectors.validation %*% trained.params)

  # calculate errors
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  minmax.train <- data.frame(min=0.85*observation.data.train, max=1.15*observation.data.train)
  minmax.validation <- data.frame(min=0.85*observation.data.validation, max=1.15*observation.data.validation)
  hit.rate.train <- sapply(1:length(estimated.results.train), function(x) {
    return(ifelse(estimated.results.train[x]>=minmax.train[x, 'min'] && estimated.results.train[x]<=minmax.train[x, 'max'], 1, 0))
  })
  hit.rate.train <- mean(hit.rate.train)
  hit.rate.validation <- sapply(1:length(estimated.results.validation), function(x) {
    return(ifelse(estimated.results.validation[x]>=minmax.validation[x, 'min'] && estimated.results.validation[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)

  ret <- list()
  ret[['trained.params']] <- trained.params
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  ret[['hit.rate.train']] <- hit.rate.train
  ret[['hit.rate.validation']] <- hit.rate.validation
  ret[['local.num.tweets']] <- local.num.tweets

  return(ret)
}

mo444PolynomialRegression <- function(use.grouped=FALSE, min.retweets.count=1, time.stamps.from=1) {
  if (use.grouped) {
    local.feature.vectors <- feature.vectors.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- feature.vectors
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data > min.retweets.count)
  local.feature.vectors <- local.feature.vectors[indexes,time.stamps.from:27]
  local.observation.data <- local.observation.data[indexes]

  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  times <- c(
    10, 20,30, 40,50, # 10 to 50 seconds
    1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
    15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
    1*60*60, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
  )

  feature.vectors.train <- local.feature.vectors[1:local.train.size,]
  feature.vectors.validation <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  observation.data.train <- local.observation.data[1:local.train.size]
  observation.data.validation <- local.observation.data[(local.train.size+1):local.num.tweets]

  poly.X <- times[time.stamps.from:27]
  poly.Y <- colMeans(feature.vectors.train)
  fit <- lm(poly.Y~poly(poly.X, 2, raw=TRUE))
  #plot(poly.X,poly.Y)
  #lines(poly.X, predict(fit, data.frame(x=poly.X)), col='red')

  estimated.results.train <- as.vector(predict(fit, newdata=data.frame(feature.vectors.train)))
  estimated.results.validation <- as.vector(predict(fit, newdata=feature.vectors.validation))
  
  # calculate errors
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  minmax.train <- data.frame(min=0.85*observation.data.train, max=1.15*observation.data.train)
  minmax.validation <- data.frame(min=0.85*observation.data.validation, max=1.15*observation.data.validation)
  hit.rate.train <- sapply(1:length(estimated.results.train), function(x) {
    return(ifelse(estimated.results.train[x]>=minmax.train[x, 'min'] && estimated.results.train[x]<=minmax.train[x, 'max'], 1, 0))
  })
  hit.rate.train <- mean(hit.rate.train)
  hit.rate.validation <- sapply(1:length(estimated.results.validation), function(x) {
    return(ifelse(estimated.results.validation[x]>=minmax.validation[x, 'min'] && estimated.results.validation[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)
  
  ret <- list()
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  ret[['hit.rate.train']] <- hit.rate.train
  ret[['hit.rate.validation']] <- hit.rate.validation
  return(ret)
}

mo444EuclidianDistanceToKMeans <- function(use.grouped=FALSE, min.retweets.count=1, max.retweets.count=1000000, time.stamps=28, num.clusters=100) {
  if (use.grouped) {
    local.feature.vectors <- feature.vectors.norm.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- feature.vectors.norm
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data >= min.retweets.count & local.observation.data <= max.retweets.count)

  local.feature.vectors <- local.feature.vectors[indexes,c(1:time.stamps, 29:(28+time.stamps), 57:70)]
  local.observation.data <- local.observation.data[indexes]
  
  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  feature.vectors.train <- local.feature.vectors[1:local.train.size,]
  feature.vectors.validation <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  observation.data.train <- local.observation.data[1:local.train.size]
  observation.data.validation <- local.observation.data[(local.train.size+1):local.num.tweets]

  #num.clusters <- ifelse(use.grouped, 50, 500)
  km <- kmeans(feature.vectors.train, centers=num.clusters, iter.max=500)
  km.retweets.means <- sapply(1:num.clusters, function(x) {
    return(mean(observation.data.train[which(km$cluster == x)]))
  })
  estimated.results.validation <- apply(feature.vectors.validation, 1, function(validation.feature.vector) {
    distances <- apply(km$centers, 1, function(centroid) {
      return(sqrt(sum((validation.feature.vector - centroid)^2)))
    })
    min.index <- which.min(distances)
    return(km.retweets.means[min.index])
  })

  # calculate errors
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  minmax.validation <- data.frame(min=0.85*observation.data.validation, max=1.15*observation.data.validation)
  hit.rate.validation <- sapply(1:length(estimated.results.validation), function(x) {
    return(ifelse(estimated.results.validation[x]>=minmax.validation[x, 'min'] && estimated.results.validation[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)

  ret <- list()
  ret[['observation.data.train']] <- observation.data.train
  #ret[['estimated.results.train']] <- estimated.results.train
  #ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  ret[['hit.rate.validation']] <- hit.rate.validation
  ret[['local.num.tweets']] <- local.num.tweets
  
  return(ret)
}

mo444DecisionTree <- function(use.grouped=FALSE, min.retweets.count=1, max.retweets.count=1000000, time.stamps=28) {
  library('rpart')

  if (use.grouped) {
    local.feature.vectors <- data.frame.norm.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- data.frame.norm
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data >= min.retweets.count & local.observation.data <= max.retweets.count)
  local.feature.vectors <- local.feature.vectors[indexes,c(1:time.stamps, 29:(28+time.stamps), 57:71)]
  local.observation.data <- local.observation.data[indexes]

  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  feature.vectors.train <- local.feature.vectors[1:local.train.size,]
  feature.vectors.validation <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  observation.data.train <- local.observation.data[1:local.train.size]
  observation.data.validation <- local.observation.data[(local.train.size+1):local.num.tweets]
  
  # Build decision tree and make predictions
  control <- rpart.control(xval=10, minsplit=4, minbucket=2, cp=0)
  #fit <- rpart(observation.data~retweets10sec+retweets20sec+retweets30sec+retweets40sec+retweets50sec+retweets60sec+retweets90sec+retweets02min+retweets03min+retweets04min+retweets05min+retweets06min+retweets07min+retweets08min+retweets09min+retweets10min+retweets15min+retweets20min+retweets25min+retweets30min+retweets60min+retweets90min+retweets2h+retweets3h+retweets4h+retweets5h+retweets6h+retweets10h+followers10sec+followers20sec+followers30sec+followers40sec+followers50sec+followers60sec+followers90sec+followers02min+followers03min+followers04min+followers05min+followers10min+followers15min+followers20min+followers25min+followers30min+followers60min+followers90min+followers2h+followers3h+followers4h+followers5h+followers6h+followers10h+week.sunday+week.monday+week.tuesday+week.wednesday+week.thursday+week.friday+week.saturday+hour.0to4+hour.4to8+hour.8to12+hour.12to16+hour.16to20+hour.20to24, data=feature.vectors.train,observation.data.train, method='anova', control=control)
  fit <- rpart(as.formula(paste('observation.data~', paste(colnames(feature.vectors)[c(1:time.stamps, 29:(28+time.stamps), 57:69)], collapse='+'), sep='')), data=feature.vectors.train, observation.data.train, method='anova', control=control)
  #fit.cp <- printcp(fit)
  #min.cp <- fit.cp[which.min(fit.cp[,'xerror']),'CP']
  #fit <- prune(fit, min.cp)

  #plot(fit, uniform=T)
  #text(fit, use.n = TRUE, cex = 0.75)

  estimated.results.train <- as.vector(predict(fit, newdata=feature.vectors.train))
  estimated.results.validation <- as.vector(predict(fit, newdata=feature.vectors.validation))

  # calculate error
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)
  minmax.train <- data.frame(min=0.85*observation.data.train, max=1.15*observation.data.train)
  minmax.validation <- data.frame(min=0.85*observation.data.validation, max=1.15*observation.data.validation)
  hit.rate.train <- sapply(1:length(estimated.results.train), function(x) {
    return(ifelse(estimated.results.train[x]>=minmax.train[x, 'min'] && estimated.results.train[x]<=minmax.train[x, 'max'], 1, 0))
  })
  hit.rate.train <- mean(hit.rate.train)
  hit.rate.validation <- sapply(1:length(estimated.results.validation), function(x) {
    return(ifelse(estimated.results.validation[x]>=minmax.validation[x, 'min'] && estimated.results.validation[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)

  ret <- list()
  ret[['fit']] <- fit
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation
  ret[['hit.rate.train']] <- hit.rate.train
  ret[['hit.rate.validation']] <- hit.rate.validation
  ret[['local.num.tweets']] <- local.num.tweets
  
  return(ret)
}

mo444SVM <- function(use.grouped=FALSE) {
  library('e1071')
  
  if (use.grouped) {
    feature.vectors.train <- feature.vectors.norm.grouped[1:train.size.grouped,,drop=F]
    feature.vectors.validation <- feature.vectors.norm.grouped[(train.size.grouped+1):num.tweets.grouped,,drop=F]
    observation.data.train <- observation.data.grouped[1:train.size.grouped]
    observation.data.validation <- observation.data.grouped[(train.size.grouped+1):num.tweets.grouped]
  }
  else {
    feature.vectors.train <- feature.vectors.norm[1:train.size,,drop=F]
    feature.vectors.validation <- feature.vectors.norm[(train.size+1):num.tweets,,drop=F]
    observation.data.train <- observation.data[1:train.size]
    observation.data.validation <- observation.data[(train.size+1):num.tweets]
  }

  # perform fit
  fit <- svm(x=feature.vectors.train, y=observation.data.train, type="eps-regression", decision.values=TRUE)
  estimated.results.train <- as.vector(predict(fit, newdata=feature.vectors.train))
  estimated.results.validation <- as.vector(predict(fit, newdata=feature.vectors.validation))

  # calculate error
  J.train <- mean((estimated.results.train - observation.data.train)^2)
  J.validation <- mean((estimated.results.validation - observation.data.validation)^2)

  ret <- list()
  ret[['fit']] <- fit
  ret[['observation.data.train']] <- observation.data.train
  ret[['estimated.results.train']] <- estimated.results.train
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation
  ret[['estimated.results.validation']] <- estimated.results.validation
  ret[['J.validation']] <- J.validation

  return(ret)
}

mo444NeuralNetworkELM <- function(use.grouped=FALSE, min.retweets.count=1, time.stamps=27, n.hid=3000) {
  library('MASS')

  if (use.grouped) {
    local.feature.vectors <- feature.vectors.norm.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- feature.vectors.norm
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data > min.retweets.count)
  local.feature.vectors <- local.feature.vectors[indexes,c(1:time.stamps, 29:(28+time.stamps), 57:70)]
  local.observation.data <- local.observation.data[indexes]

  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  #separate training and test data
  fv <- local.feature.vectors[1:local.train.size,]
  fv.test <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  Y <- as.matrix(local.observation.data[1:local.train.size])
  Y.test <- as.matrix(local.observation.data[(local.train.size+1):local.num.tweets])
  
  #separate training and validation data
  split <- round(0.8*nrow(fv))
  X.train <- fv[1:split, ]
  X.train <- as.matrix(cbind(X.train, 1))
  Y.train <- as.matrix(Y[1:split, ])
  X.val <- fv[(split+1):nrow(fv),]
  X.val <- as.matrix(cbind(X.val, 1))
  Y.val <- as.matrix(Y[(split+1):nrow(fv),])
  
  #get the number of inputs and outputs
  n.elem <- nrow(X.train)
  n.inputs <- ncol(X.train)
  n.outputs <- ncol(Y.train)
  
  ###ELM
  #1 - set weights of hidden layer randomly, in a normal distribution with mean=0 and std=1
  W1 <- matrix(rnorm(n.hid*n.inputs), nrow=n.hid, ncol=n.inputs)
  hidden.layer <- cbind(tanh(X.train%*%t(W1)), 1)
  hidden.layer.val <- cbind(tanh(X.val%*%t(W1)), 1) #output from hidden layer, for validation data
  
  #2 - define weights of exit layer, as a linear combination of hidden neurons, minimizing it norm.
  
  #using golden search to define C
  k <- (sqrt(5) - 1) / 2;
  a <- 0.001
  b <- 20 #a and c are the current bounds; the minimum is between them.
  
  xL <- b - k * (b - a);
  xR <- a + k * (b - a);
  
  while(abs(b - a) > 0.0001){
    #evaluate the function in xL and xR, where:
    
    #xL:
    #solving system:
    #I/C + Hw2 = Y.train
    #w2 = pinv(I/C + H) Y.train
    
    if(n.hid <= n.elem){ #less neurons than samples
      W2.xL <- ginv((diag(1, n.hid+1)/xL) + t(hidden.layer)%*%hidden.layer) %*% t(hidden.layer) %*% Y.train
    }
    else {
      W2.xL <- t(hidden.layer) %*% ginv((diag(1, n.elem)/xL) + hidden.layer%*%t(hidden.layer)) %*% Y.train
    }
    
    #use validation set to calculate error
    Y.xL <- hidden.layer.val %*% W2.xL;
    MSE.xL <- mean((Y.xL - Y.val)^2) #value of the function in point xL
    
    #the same for xR:
    if(n.hid <= n.elem){ #less neurons than samples
      W2.xR <- ginv((diag(1, n.hid+1)/xL) + t(hidden.layer)%*%hidden.layer) %*% t(hidden.layer) %*% Y.train
    }
    else {
      W2.xR <- t(hidden.layer) %*% ginv((diag(1, n.elem)/xR) + hidden.layer%*%t(hidden.layer)) %*% Y.train
    }
    
    Y.xR <- hidden.layer.val %*% W2.xR;
    MSE.xR <- mean((Y.xR - Y.val)^2) #value of the function in point xR
    
    
    if(MSE.xL < MSE.xR){
      b <- xR
      xR <- xL
      xL <- b - k * (b - a)
    }
    else{
      a <- xL
      xL <- xR
      xR <- a + k * (b - a)
    }
  }
  C = (a + b) / 2;
  
  #use C to calculate final errors
  if(n.hid <= n.elem){ #less neurons than samples
    W2 <- ginv((diag(1, n.hid+1)/C) + t(hidden.layer)%*%hidden.layer) %*% t(hidden.layer) %*% Y.train
  }
  if (n.hid > n.elem) {
    W2 <- t(hidden.layer) %*% ginv((diag(1, n.elem)/C) + hidden.layer%*%t(hidden.layer)) %*% Y.train
  }
  
  Y.train.est = hidden.layer %*% W2
  MSE.train <- mean((Y.train.est-Y.train)^2) #training error
  
  Y.val.est = hidden.layer.val %*% W2
  MSE.val <- mean((Y.val.est-Y.val)^2) #validation error
  
  #do the prediction!
  X.test <- as.matrix(cbind(fv.test, 1))

  hidden.layer.test <- cbind(tanh(X.test%*%t(W1)), 1)
  Y.test.est = hidden.layer.test %*% W2
  MSE.test <- mean((Y.test.est - Y.test)^2)

  minmax.validation <- data.frame(min=0.85*Y.test, max=1.15*Y.test)
  hit.rate.validation <- sapply(1:length(Y.test.est), function(x) {
    return(ifelse(Y.test.est[x]>=minmax.validation[x, 'min'] && Y.test.est[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)
  
  ret <- list()
  ret[['observation.data.validation']] <- Y.test
  ret[['estimated.results.validation']] <- Y.test.est
  ret[['MSE.test']] <- MSE.test
  ret[['hit.rate.validation']] <- hit.rate.validation
  return(ret)
}

mo444NeuralNetworkMLP <- function(use.grouped=FALSE, min.retweets.count=1, max.retweets.count=1000000, time.stamps=27, hidden.neurons=800, n.epochs=5000) {
  if (use.grouped) {
    local.feature.vectors <- feature.vectors.norm.grouped
    local.observation.data <- observation.data.grouped
  }
  else {
    local.feature.vectors <- feature.vectors.norm
    local.observation.data <- observation.data
  }
  indexes <- which(local.observation.data >= min.retweets.count & local.observation.data <= max.retweets.count)
  local.feature.vectors <- local.feature.vectors[indexes,c(1:time.stamps, 29:(28+time.stamps), 57:70)]
  local.observation.data <- local.observation.data[indexes]

  local.num.tweets <- length(local.observation.data)
  local.train.size <- 0.8 * local.num.tweets

  #separate training and test data
  fv <- local.feature.vectors[1:local.train.size,]
  X.test <- local.feature.vectors[(local.train.size+1):local.num.tweets,]
  Y <- as.matrix(local.observation.data[1:local.train.size])
  Y.test <- as.matrix(local.observation.data[(local.train.size+1):local.num.tweets])

  #Neural network
  #ML

  #separate train and validation data
  #fv <- cbind(fv, 1) #add bias
  split1 <- round(0.8*nrow(fv))
  X.train <- fv[1:split1, ]
  Y.train <- as.matrix(Y[1:split1, ])
  X.val <- fv[(split1+1):nrow(fv),]
  Y.val <- as.matrix(Y[(split1+1):nrow(fv),])

  #get the number of inputs and outputs
  n.elem <- nrow(X.train)
  n.inputs <- ncol(X.train)
  n.outputs <- ncol(Y.train)

  #Random Initialize weights matrices
  W1 <- matrix(rnorm(hidden.neurons*n.inputs), nrow=hidden.neurons, ncol=n.inputs)
  W2 <- matrix(rnorm(n.outputs*(hidden.neurons+1)), nrow=n.outputs, ncol=(hidden.neurons+1))
  
  #Define minimum error (stop condition)
  threshold = 1e-5
  MSE <- Inf
  best.MSE.val = Inf

  #save progression to analyse plot
  MSE.train.prog <- c()
  MSE.val.prog <- c()

  #Define initial learning rate
  alpha <- 1#/ncol(X.train)
  current.epoch <- 1
  while((current.epoch <= n.epochs)){# && ( MSE > threshold )){
    #print(current.epoch)
    #find the gradient
    
    #propagation
    #forward propagation
    hidden.layer <- tanh(X.train%*%t(W1))
    Y.train.est <- cbind(hidden.layer, 1) %*% t(W2)
    
    #backpropagation
    error <- Y.train.est - Y.train
    dJ.dW2 <- t(error)%*%cbind(hidden.layer, 1)
    
    sig <- (error%*%W2[,1:hidden.neurons]) * ((1-hidden.layer)* hidden.layer)
    dJ.dW1 <- t(sig) %*% X.train
    
    #normalize
    gradient <- c(dJ.dW1, dJ.dW2)
    dJ.dW2 <- dJ.dW2 / norm(matrix(gradient), "2")
    dJ.dW1 <- dJ.dW1 / norm(matrix(gradient), "2")
    
    #update weights
    W1.temp <- W1 - alpha*dJ.dW1
    W2.temp <- W2 - alpha*dJ.dW2
    
    #calculate error
    hidden.layer <- tanh(X.train%*%t(W1.temp))
    Y.train.est <- cbind(hidden.layer, 1) %*% t(W2.temp)
    
    error <- Y.train.est - Y.train
    MSE.temp <- mean(error^2)
    
    #update attributes
    W1 <- W1.temp
    W2 <- W2.temp
    MSE <- MSE.temp

    #get validation error
    #calculate error
    hidden.layer <- tanh(X.val%*%t(W1))
    Y.val.est <- cbind(hidden.layer, 1) %*% t(W2)
    
    error <- Y.val.est - Y.val
    MSE.val <- mean(error^2)
    
    if(MSE.val < best.MSE.val){
      best.MSE.val <- MSE.val
      best.W1 <- W1;
      best.W2 <- W2;
      best.epoch <- current.epoch
    }
    
    #register progress
    MSE.train.prog <- c(MSE.train.prog, MSE)
    MSE.val.prog <- c(MSE.val.prog, MSE.val)
    
    current.epoch <- current.epoch + 1
  }
  
  #plot(MSE.val.prog, type="l")
  #plot(MSE.train.prog, type="l")

  #do the prediction
  hidden.layer <- tanh(X.test%*%t(best.W1))
  Y.test.est <- cbind(hidden.layer, 1) %*% t(best.W2)

  error <- Y.test.est - Y.test
  MSE.test <- mean(error^2)
  
  minmax.validation <- data.frame(min=0.85*Y.test, max=1.15*Y.test)
  hit.rate.validation <- sapply(1:length(Y.test.est), function(x) {
    return(ifelse(Y.test.est[x]>=minmax.validation[x, 'min'] && Y.test.est[x]<=minmax.validation[x, 'max'], 1, 0))
  })
  hit.rate.validation <- mean(hit.rate.validation)
  
  ret <- list()
  ret[['observation.data.validation']] <- Y.test
  ret[['estimated.results.validation']] <- Y.test.est
  ret[['MSE.test']] <- MSE.test
  ret[['hit.rate.validation']] <- hit.rate.validation
  ret[['local.num.tweets']] <- local.num.tweets
  return(ret)
}

load("/home/felipe/2s2013/mo444/mo444/analysis/data.Rdata")
# Build feature.vectors
rand.data <<- data[sample(41514),]
feature.vectors <<- data.matrix(rand.data[,2:70])
observation.data <<- rand.data[,'observed.result']

# Group feature vectors with same Y (observed.result)
library('plyr')
feature.vectors.grouped.df <<- ddply(rand.data, ~observed.result, summarise,retweets10sec=mean(retweets10sec),retweets20sec=mean(retweets20sec),retweets30sec=mean(retweets30sec),retweets40sec=mean(retweets40sec),retweets50sec=mean(retweets50sec),retweets60sec=mean(retweets60sec),retweets90sec=mean(retweets90sec),retweets02min=mean(retweets02min),retweets03min=mean(retweets03min),retweets04min=mean(retweets04min),retweets05min=mean(retweets05min),retweets06min=mean(retweets06min),retweets07min=mean(retweets07min),retweets08min=mean(retweets08min),retweets09min=mean(retweets09min),retweets10min=mean(retweets10min),retweets15min=mean(retweets15min),retweets20min=mean(retweets20min),retweets25min=mean(retweets25min),retweets30min=mean(retweets30min),retweets60min=mean(retweets60min),retweets90min=mean(retweets90min),retweets2h=mean(retweets2h),retweets2h=mean(retweets2h),retweets3h=mean(retweets3h),retweets4h=mean(retweets4h),retweets5h=mean(retweets5h),retweets6h=mean(retweets6h),retweets10h=mean(retweets10h),followers10sec=mean(followers10sec),followers20sec=mean(followers20sec),followers30sec=mean(followers30sec),followers40sec=mean(followers40sec),followers50sec=mean(followers50sec),followers60sec=mean(followers60sec),followers90sec=mean(followers90sec),followers02min=mean(followers02min),followers03min=mean(followers03min),followers04min=mean(followers04min),followers05min=mean(followers05min),followers06min=mean(followers06min),followers07min=mean(followers07min),followers08min=mean(followers08min),followers09min=mean(followers09min),followers10min=mean(followers10min),followers15min=mean(followers15min),followers20min=mean(followers20min),followers25min=mean(followers25min),followers30min=mean(followers30min),followers60min=mean(followers60min),followers90min=mean(followers90min),followers2h=mean(followers2h),followers2h=mean(followers2h),followers3h=mean(followers3h),followers4h=mean(followers4h),followers5h=mean(followers5h),followers6h=mean(followers6h),followers10h=mean(followers10h),week.sunday=mean(week.sunday),week.monday=mean(week.monday),week.tuesday=mean(week.tuesday),week.wednesday=mean(week.wednesday),week.thursday=mean(week.thursday),week.friday=mean(week.friday),week.saturday=mean(week.saturday),hour.0to4=mean(hour.0to4),hour.4to8=mean(hour.4to8),hour.8to12=mean(hour.8to12),hour.12to16=mean(hour.12to16),hour.16to20=mean(hour.16to20),hour.20to24=mean(hour.20to24))
rand.sample <<- sample(nrow(feature.vectors.grouped.df))
feature.vectors.grouped.df <<- feature.vectors.grouped.df[rand.sample,]
feature.vectors.grouped <<- data.matrix(feature.vectors.grouped.df[,2:70])
observation.data.grouped <<- feature.vectors.grouped.df[,1]
observation.data.grouped <<- observation.data.grouped[rand.sample]

# Normalization
num.tweets <<- nrow(feature.vectors)
train.size <<- floor(0.8 * num.tweets)
means.train <<- apply(feature.vectors[1:train.size,],2,mean)
sds.train <<- apply(feature.vectors[1:train.size,], 2, sd)
feature.vectors.norm <<- t(apply(feature.vectors, 1, function(x) { x-means.train })) # subtract each feature value by feature mean
feature.vectors.norm <<- t(apply(feature.vectors.norm, 1, function(x) { x/sds.train }))  # divide each row by its standard deviation
feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias
data.frame.norm <<- data.frame(feature.vectors.norm, observation.data)

num.tweets.grouped <<- nrow(feature.vectors.grouped)
train.size.grouped <<- floor(0.8 * num.tweets.grouped)
means.train.grouped <<- apply(feature.vectors.grouped[1:train.size.grouped,],2,mean)
sds.train.grouped <<- apply(feature.vectors.grouped[1:train.size.grouped,], 2, sd)
feature.vectors.norm.grouped <<- t(apply(feature.vectors.grouped, 1, function(x) { x-means.train.grouped })) # subtract each feature value by feature mean
feature.vectors.norm.grouped <<- t(apply(feature.vectors.norm.grouped, 1, function(x) { x/sds.train.grouped }))  # divide each row by its standard deviation
feature.vectors.norm.grouped <<- cbind(feature.vectors.norm.grouped, 1) # add bias
data.frame.norm.grouped <<- data.frame(feature.vectors.norm.grouped, observation.data=observation.data.grouped)

# Execute analysis
#regression.results <- mo444LinearRegression(use.grouped=F, min.retweets.count=1, max.retweets.count=1000000, time.stamps.from=1, time.stamps.to=27)
#decision.tree.results <- mo444DecisionTree(use.grouped=F, min.retweets.count=1, time.stamps=27)
#euclidian.distance.results <- mo444EuclidianDistanceToKMeans()
#svm.results <- mo444SVM(use.grouped=T)
#elm.results <- mo444NeuralNetworkELM(use.grouped=T)
#mlp.results <- mo444NeuralNetworkMLP(use.grouped=T,hidden.neurons=100, n.epochs=100)

# Kmeans test
#for (clust in c(100, 200, 500)) {
#  for (stamps in c(27, 21, 11)) {
#    kmeans.results <- mo444EuclidianDistanceToKMeans(use.grouped=F, time.stamps=stamps, num.clusters=clust)
#    print(paste("time.stamps=",stamps, ", num.clusters=", clust))
#    print(data.frame(J.validation=kmeans.results$J.validation, hit.rate.validation=kmeans.results$hit.rate.validation))
#  }
#}

# Decision tree
#minmaxes <- list(c(1,2000000), c(1, 100), c(100, 2000), c(2000, 2000000))
#for (minmax in minmaxes) {
#  for (stamps in c(27, 21, 11)) {
#    decision.tree.results <- mo444DecisionTree(min.retweets.count=minmax[1], max.retweets.count=minmax[2], time.stamps=stamps)
#    print(paste("time.stamps=",stamps, ", min=", minmax[1], ", max=", minmax[2]))
#    print(data.frame(J.validation=decision.tree.results$J.validation, hit.rate.validation=decision.tree.results$hit.rate.validation))
#  }
#}
