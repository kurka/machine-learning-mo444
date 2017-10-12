# data <- read.csv("../../wine.data")
# 
# 
# Y.data <- matrix(0, 177, 3)
# Y.data[1:58,1] <- 1
# Y.data[59:129,2] <- 1
# Y.data[130:177,3] <- 1
# 
# 
# rand.ind <- sample(1:nrow(data), nrow(data))
# split1 <- round(0.8*nrow(data))
# fv.test <- as.matrix(data[rand.ind[split1:nrow(data)], 2:13])
# Y.test <- as.matrix(Y.data[rand.ind[split1:nrow(data)],])
# fv <- as.matrix(data[rand.ind[1:split1],2:13])
# Y <- as.matrix(Y.data[rand.ind[1:split1],])
# 
# #normalizar fv
# mean.train <- apply(fv, 2, mean)
# sd.train <- apply(fv,2,sd)
# fv <- t(apply(fv,1,function(x){x-mean.train}))
# fv <- t(apply(fv,1,function(x){x/sd.train}))
# 
# 
# fv.test <- t(apply(fv.test,1,function(x){x-mean.train}))
# fv.test <- t(apply(fv.test,1,function(x){x/sd.train}))
# X.test <- cbind(fv.test, 1) #add bias!


#################### novo comeco

load("/run/media/kurka/dados/davidold/unicamp/mestrado/MO444 - aprendizado de maquina/projeto/analysis/data.Rdata")


# Build feature.vectors
rand.data <- data[sample(nrow(data)),]
feature.vectors <- data.matrix(rand.data[,2:70])
observed.data <- data.matrix(rand.data[,'observed.result'])

# eltermann esta aqui
rand.sample <<- sample(nrow(feature.vectors.grouped))
feature.vectors <- feature.vectors.grouped[rand.sample,]
observed.data <- as.matrix(observation.data.grouped)
observed.data <- observed.data[rand.sample,,drop=F]

feature.vectors <- feature.vectors[,1:56]
# fim do eltermann


# Normalization
num.tweets <- nrow(feature.vectors)
train.size <- floor(0.8 * num.tweets)
means.train <- apply(feature.vectors[1:train.size,],2,mean)
sds.train <- apply(feature.vectors[1:train.size,], 2, sd)
feature.vectors.norm <<- t(apply(feature.vectors, 1, function(x) { x-means.train })) # subtract each feature value by feature mean
feature.vectors.norm <<- t(apply(feature.vectors.norm, 1, function(x) { x/sds.train }))  # divide each row by its standard deviation
feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias

#normalize output
#mean.Y <- mean(observed.data[1:train.size])
#sd.Y <- sd(observed.data[1:train.size])
#observed.data.norm <- (observed.data - mean.y) / sd.y
observed.data.norm <- observed.data

#separate training and test data
fv <- as.matrix(feature.vectors.norm[1:train.size,])
Y <- as.matrix(observed.data.norm[1:train.size,])

X.test <- as.matrix(feature.vectors.norm[(train.size+1):num.tweets,])
Y.test <- as.matrix(observed.data.norm[(train.size+1):num.tweets,])



#Neural network
#ML

hidden.neurons <- 800
n.epochs <- 5000

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
  print(current.epoch)
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
  
  #find optimal alpha
#  alpha <- alpha*2
#   while(MSE.temp > MSE){
#     alpha <- alpha / 2
#     #update weights
#     W1.temp <- W1 - alpha*dJ.dW1
#     W2.temp <- W2 - alpha*dJ.dW2
#     
#     
#     #calculate error
#     hidden.layer <- tanh(X.train%*%t(W1.temp))
#     Y.train.est <- cbind(hidden.layer, 1) %*% t(W2.temp)
#     
#     error <- Y.train.est - Y.train
#     MSE.temp <- 1/length(error) * sum(error^2)
#   }
#   print(alpha)
  
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
  print(MSE)
}

plot(MSE.val.prog, type="l")
plot(MSE.train.prog, type="l")


#do the prediction


hidden.layer <- tanh(X.test%*%t(best.W1))
Y.test.est <- cbind(hidden.layer, 1) %*% t(best.W2)

error <- Y.test.est - Y.test
MSE.test <- mean(error^2)