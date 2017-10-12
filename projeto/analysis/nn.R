library('RMySQL')
library('MASS')

#Input
#elapsed time <-vector of time elapsed in seconds
#count - number of retweets
mo444GetFeatureVector <- function(elapsed_time, count) {
  feature.vector <- array(0, dim=length(times))
  i <- 1
  index <- 1
  for (index in 1:length(times)) {
    for (j in i:length(elapsed_time)) {
      if (times[index] < elapsed_time[j]) {
        break
      }
    }

    i <- j
    if (i == 1) {
      # (left boundary exception) no retweets until this "time"
      feature.vector[index] <- 0
    }
    else if (j == length(elapsed_time)) {
      # (right boundary exception) "time" larger than last retweet time
      feature.vector[index] <- count[i]
    }
    else {
      feature.vector[index] <- ifelse(i == 1, 0, count[i-1])
    }
  }

  return(feature.vector)
}




con <- dbConnect(MySQL(), user="root", dbname="mo444-twitter")

times <<- c(
  10, 20,30, 40,50, # 10 to 50 seconds
  1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
  15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
  1*60*60#, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
)

#followed.users <- read.csv('./followed_user_ids.csv', header=FALSE)
#for (u in 1:nrow(followed.users)) {

  #user.name <- as.character(followed.users[u,'V1'])
  #user.id <- as.character(followed.users[u,'V2'])


  #print(paste(user.name, user.id, sep=': '))

  #q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id FROM tweet_with_created_at2 t WHERE t.user_id = %s AND t.tweet_created_at > 1379827567;", user.id))
  #q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id FROM tweet_with_created_at2 t WHERE t.tweet_created_at > 1379827567;"))
  q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id FROM tweet_with_created_at2 t WHERE t.user_id IN (27260086,21447363,14230524,813286,17919972,10228272,16409683,79293791,180505807,26565946,101402940,783214,85603854,44409004,15846407,155659213,19397785,28706024,25365536,21111883,184910040,1311017701,35787166,100220864,60865434,23375688,35094637,181561712,22940219,209708391,19058681,105119490,815322103,31927467,268414482,116362700,19248106,428333,73992972,119509520,85426644,158314798,84279963,1311017701,50393960,23976386,27195114,24929621,31239408,2425151,52551600,20322929,16190898,18863815,218571680,3004231,21425125,18091904,53153263,176566242,43152482,259379883,759251) AND t.tweet_created_at > 1379827567;"))
  if (!'tweet_id' %in% names(q0) || length(q0[['tweet_id']]) < 50) {
    print('not enough tweets')
#   next
  }
  user_tweets <- q0[['tweet_id']]

  feature.vectors <<- matrix(0, nrow=length(user_tweets), ncol=length(times))
  observation.data <<- array(0, dim=length(user_tweets))
  tweet_counter <- 1
  for (tweet_id in as.character(user_tweets)) {
    q <- dbGetQuery(con, sprintf("SELECT * FROM retweets_history2 WHERE original_tweet_id = %s;", tweet_id))
    q2 <- dbGetQuery(con, sprintf("SELECT t.tweet_text, u.user_screen_name FROM tweet t INNER JOIN user u ON t.user_id = u.user_id where t.tweet_id = %s;", tweet_id))

    if ('elapsed_time' %in% names(q) && length(q[['elapsed_time']]) > 0) {
      observation.data[tweet_counter] <- length(q[['elapsed_time']])
      feature.vector <- mo444GetFeatureVector(q[['elapsed_time']], q[['count']])
      feature.vectors[tweet_counter,] <- feature.vector
      tweet_counter <- tweet_counter + 1
    }
  }
  num.tweets <- tweet_counter - 1
  feature.vectors <<- feature.vectors[1:num.tweets,]
  observation.data <<- observation.data[1:num.tweets]

  if (!is.matrix(feature.vectors) || nrow(feature.vectors) <= 1) {
    print('no tweets found.')
    next
  }
  print(paste('num posts: ', num.tweets, sep=''))

  num.tweets <- nrow(feature.vectors)
  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)

  # normalization
  means.train <- apply(feature.vectors[1:train.size,,drop=F],2,mean)
  sds.train <- apply(feature.vectors[1:train.size,,drop=F], 2, sd)
  feature.vectors.norm <<- t(apply(feature.vectors, 1, function(x) { x-means.train })) # subtract each feature value by feature mean
  feature.vectors.norm <<- t(apply(feature.vectors.norm, 1, function(x) { x/sds.train }))  # divide each row by its standard deviation
  #feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias

  mean.y <<- mean(observation.data[1:train.size])
  sd.y <<- sd(observation.data[1:train.size])
  observation.data.norm <<- (observation.data - mean.y) / sd.y

#}

dbDisconnect(con)


#fv <- matrix(rnorm(50*20), 50, 20) #feature vector already normalized
#Y <- matrix(rnorm(50), 50, 1) #results column
fv <- as.matrix(feature.vectors.norm[1:train.size,])
Y <- as.matrix(observation.data.norm[1:train.size])

#Neural network
#ML

hidden.neurons <- 100
n.epochs <- 1000




#function [ best_A , best_B , nerro, epoch_stop ] = gradiente_simples( Xtr, Ytr, Xval, Yval, W, h, no_epochs )
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

#Random Initialize weights matrices
W1 <- matrix(rnorm(hidden.neurons*n.inputs), nrow=hidden.neurons, ncol=n.inputs)
W2 <- matrix(rnorm(n.outputs*(hidden.neurons+1)), nrow=n.outputs, ncol=(hidden.neurons+1))


#Define minimum error (stop condition)
threshold = 1e-5;
MSE <- Inf
best.MSE.val = Inf;


#Define initial learning rate
alpha = 1;

#save progression to analyse plot
MSE.train.prog <- c()
MSE.val.prog <- c()



#gradient descent
current.epoch <- 1
while((current.epoch <= n.epochs) && ( MSE > threshold )){
  #find the gradient
  print(current.epoch)
  #propagation
  #forward propagation
  hidden.layer <- tanh(X.train%*%t(W1))
  Y.train.est <- cbind(hidden.layer, 1) %*% t(W2)

  #backpropagation
  error <- Y.train.est - Y.train
  dJ.dW2 <- t(error)%*%cbind(hidden.layer, 1)
  
  sig <- (error%*%W2[,1:hidden.neurons]) * ((1-hidden.layer)* hidden.layer)
  dJ.dW1 <- t(sig) %*% X.train

  #normalize (optional?)
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
  MSE.temp <- 1/length(error) * sum(error^2)
  
  #if error is too big, find a better alpha
  #find optimal alpha

  while(MSE.temp > MSE){
    alpha <- alpha / 2
    #update weights
    W1.temp <- W1 - alpha*dJ.dW1
    W2.temp <- W2 - alpha*dJ.dW2
    
    
    #calculate error
    hidden.layer <- tanh(X.train%*%t(W1.temp))
    Y.train.est <- cbind(hidden.layer, 1) %*% t(W2.temp)
    
    error <- Y.train.est - Y.train
    MSE.temp <- 1/length(error) * sum(error^2)
  }

  #increase learning rate
  alpha <- alpha * 2
  
  #update attributes
  W1 <- W1.temp
  W2 <- W2.temp
  MSE <- MSE.temp
  
  
  
  #get validation error
  #calculate error
  hidden.layer <- tanh(X.val%*%t(W1.temp))
  Y.val.est <- cbind(hidden.layer, 1) %*% t(W2.temp)
  
  error <- Y.val.est - Y.val
  MSE.val <- 1/length(error) * sum(error^2)
  
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


#do the prediction
X.test <- as.matrix(feature.vectors.norm[(train.size+1):num.tweets,])
X.test <- as.matrix(cbind(X.test, 1))
Y.test <- as.matrix(observation.data.norm[(train.size+1):num.tweets])

hidden.layer <- tanh(X.test%*%t(best.W1))
Y.test.est <- cbind(hidden.layer, 1) %*% t(best.W2)

#unormalize
Y.test.est.un <- (Y.test.est * sd.y) + mean.y
Y.test.un <- (Y.test * sd.y) + mean.y

error <- Y.test.est.un - Y.test.un
MSE.test <- 1/length(error) * sum(error^2)
