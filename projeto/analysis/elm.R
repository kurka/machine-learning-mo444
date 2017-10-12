library('MASS')

#load("/run/media/kurka/dados/davidold/unicamp/mestrado/MO444 - aprendizado de maquina/projeto/analysis/data.Rdata")
load("/home/felipe/2s2013/mo444/mo444/analysis/data.Rdata")


# Build feature.vectors
rand.data <- data[sample(nrow(data)),]
feature.vectors <- data.matrix(rand.data[,2:70])
observed.data <- data.matrix(rand.data[,'observed.result'])

# eltermann esta aqui
rand.sample <<- sample(nrow(feature.vectors.grouped))
feature.vectors <- feature.vectors.grouped[rand.sample,]
observed.data <- as.matrix(observation.data.grouped)
observed.data <- observed.data[rand.sample,,drop=F]
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
#observed.data.norm <- (observed.data - mean(observed.data[1:train.size])) / sd(observed.data[1:train.size])
observed.data.norm <- observed.data

#separate training and test data
fv <- as.matrix(feature.vectors.norm[1:train.size,])
Y <- as.matrix(observed.data.norm[1:train.size,])

fv.test <- as.matrix(feature.vectors.norm[(train.size+1):num.tweets,])
Y.test <- as.matrix(observed.data.norm[(train.size+1):num.tweets,])



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
n.hid <- 3000


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