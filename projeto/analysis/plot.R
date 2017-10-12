library('RMySQL')
library('MASS')

#data.set <<- 'small'
data.set <<- 'medium'
#data.set <<- 'large'
max.date <<- ifelse(data.set == 'small', 1379847960, ifelse(data.set == 'medium', 1379979180, 1380627240))
tweets.table <<- ifelse(data.set == 'small', 'tweet_with_created_at2', ifelse(data.set == 'medium', 'tweet_with_created_at3', 'tweet_with_created_at4'))
history.table <<- ifelse(data.set == 'small', 'retweets_history4', ifelse(data.set == 'medium', 'retweets_history3_2', 'retweets_history4_2'))


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


#Input
#feature vectors
#observation.data - the data we gathered
#mean.y and sd.y are related to the normalization of observation.
#mean is mean
#sd is standard deviation
mo444LinearRegression <- function() {
  num.tweets <- nrow(feature.vectors)

  #TODO - select random subset for training
  train.size <- floor(0.8 * num.tweets)

  feature.vectors.train <- feature.vectors.norm[1:train.size,,drop=F]
  feature.vectors.validation <- feature.vectors.norm[(train.size+1):num.tweets,,drop=F]

  observation.data.train <- observation.data.norm[1:train.size]
  observation.data.validation <- observation.data.norm[(train.size+1):num.tweets]

  #theta - parameters
  trained.params <- ginv(feature.vectors.train) %*% observation.data.train
  estimated.results.train <- as.vector(feature.vectors.train %*% trained.params)
  estimated.results.validation <- as.vector(feature.vectors.validation %*% trained.params)

  # unnormalize output and calculate error
  observation.data.train.un <- (observation.data.train * sd.y) + mean.y
  observation.data.validation.un <- (observation.data.validation * sd.y) + mean.y
  estimated.results.train.un <- (estimated.results.train * sd.y) + mean.y
  estimated.results.validation.un <- (estimated.results.validation * sd.y) + mean.y

  J.train <- mean((estimated.results.train.un - observation.data.train.un)^2)
  J.validation <- mean((estimated.results.validation.un - observation.data.validation.un)^2)

  ret <- list()
  ret[['trained.params']] <- trained.params
  ret[['observation.data.train']] <- observation.data.train.un
  ret[['estimated.results.train']] <- estimated.results.train.un
  ret[['J.train']] <- J.train
  ret[['observation.data.validation']] <- observation.data.validation.un
  ret[['estimated.results.validation']] <- estimated.results.validation.un
  ret[['J.validation']] <- J.validation

  return(ret)
}

mo444DecisionTree <- function() {
  library('rpart')

  num.tweets <- nrow(feature.vectors)

  train.size <- floor(0.8 * num.tweets)

  feature.vectors.train <- feature.vectors.df[1:train.size,,drop=F]
  feature.vectors.validation <- feature.vectors.df[(train.size+1):num.tweets,,drop=F]

  observation.data.train <- observation.data[1:train.size]
  observation.data.validation <- observation.data[(train.size+1):num.tweets]

  # Build decision tree and make predictions
  control <- rpart.control(xval=10, minsplit=4, minbucket=2, cp=0)
  #fit <- rpart(observation.data~retweets10sec+retweets20sec+retweets30sec+retweets40sec+retweets50sec+retweets60sec+retweets90sec+retweets02min+retweets03min+retweets04min+retweets05min+retweets06min+retweets07min+retweets08min+retweets09min+retweets10min+retweets15min+retweets20min+retweets25min+retweets30min+retweets60min+retweets90min+retweets2h+retweets3h+retweets4h+retweets5h+retweets6h+retweets10h+followers10sec+followers20sec+followers30sec+followers40sec+followers50sec+followers60sec+followers90sec+followers02min+followers03min+followers04min+followers05min+followers06min+followers07min+followers08min+followers09min+followers10min+followers15min+followers20min+followers25min+followers30min+followers60min+followers90min+followers2h+followers3h+followers4h+followers5h+followers6h+followers10h+week.sunday+week.monday+week.tuesday+week.wednesday+week.thursday+week.friday+week.saturday+hour.0to4+hour.4to8+hour.8to12+hour.12to16+hour.16to20+hour.20to24, data=feature.vectors.train, method='anova', control=control)
  fit <- rpart(observation.data~retweets10sec+retweets20sec+retweets30sec+retweets40sec+retweets50sec+retweets60sec+retweets90sec+retweets02min+retweets03min+retweets04min+retweets05min+followers10sec+followers20sec+followers30sec+followers40sec+followers50sec+followers60sec+followers90sec+followers02min+followers03min+followers04min+followers05min+week.sunday+week.monday+week.tuesday+week.wednesday+week.thursday+week.friday+week.saturday+hour.0to4+hour.4to8+hour.8to12+hour.12to16+hour.16to20+hour.20to24, data=feature.vectors.train, method='anova', control=control)
  #plot(fit, uniform=T)
  #text(fit, use.n = TRUE, cex = 0.75)

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

mo444BuildFeatureVectorsDataframe <- function() {
  return(data.frame(
    retweets10sec=feature.vectors[,1],
    retweets20sec=feature.vectors[,2],
    retweets30sec=feature.vectors[,3],
    retweets40sec=feature.vectors[,4],
    retweets50sec=feature.vectors[,5],
    retweets60sec=feature.vectors[,6],
    retweets90sec=feature.vectors[,7],
    retweets02min=feature.vectors[,8],
    retweets03min=feature.vectors[,9],
    retweets04min=feature.vectors[,10],
    retweets05min=feature.vectors[,11],
    retweets06min=feature.vectors[,12],
    retweets07min=feature.vectors[,13],
    retweets08min=feature.vectors[,14],
    retweets09min=feature.vectors[,15],
    retweets10min=feature.vectors[,16],
    retweets15min=feature.vectors[,17],
    retweets20min=feature.vectors[,18],
    retweets25min=feature.vectors[,19],
    retweets30min=feature.vectors[,20],
    retweets60min=feature.vectors[,21],
    retweets90min=feature.vectors[,22],
    retweets2h=feature.vectors[,23],
    retweets3h=feature.vectors[,24],
    retweets4h=feature.vectors[,25],
    retweets5h=feature.vectors[,26],
    retweets6h=feature.vectors[,27],
    retweets10h=feature.vectors[,28],
    followers10sec=feature.vectors[,29],
    followers20sec=feature.vectors[,30],
    followers30sec=feature.vectors[,31],
    followers40sec=feature.vectors[,32],
    followers50sec=feature.vectors[,33],
    followers60sec=feature.vectors[,34],
    followers90sec=feature.vectors[,35],
    followers02min=feature.vectors[,36],
    followers03min=feature.vectors[,37],
    followers04min=feature.vectors[,38],
    followers05min=feature.vectors[,39],
    followers06min=feature.vectors[,40],
    followers07min=feature.vectors[,41],
    followers08min=feature.vectors[,42],
    followers09min=feature.vectors[,43],
    followers10min=feature.vectors[,44],
    followers15min=feature.vectors[,45],
    followers20min=feature.vectors[,46],
    followers25min=feature.vectors[,47],
    followers30min=feature.vectors[,48],
    followers60min=feature.vectors[,49],
    followers90min=feature.vectors[,50],
    followers2h=feature.vectors[,51],
    followers3h=feature.vectors[,52],
    followers4h=feature.vectors[,53],
    followers5h=feature.vectors[,54],
    followers6h=feature.vectors[,55],
    followers10h=feature.vectors[,56],
    week.sunday=categoric.feature.vectors[,1],
    week.monday=categoric.feature.vectors[,2],
    week.tuesday=categoric.feature.vectors[,3],
    week.wednesday=categoric.feature.vectors[,4],
    week.thursday=categoric.feature.vectors[,5],
    week.friday=categoric.feature.vectors[,6],
    week.saturday=categoric.feature.vectors[,7],
    hour.0to4=categoric.feature.vectors[,8],
    hour.4to8=categoric.feature.vectors[,9],
    hour.8to12=categoric.feature.vectors[,10],
    hour.12to16=categoric.feature.vectors[,11],
    hour.16to20=categoric.feature.vectors[,12],
    hour.20to24=categoric.feature.vectors[,13],
    observation.data=as.vector(observation.data)
  ))
}

con <- dbConnect(MySQL(), user="root", dbname="mo444-twitter")

times <<- c(
  10, 20,30, 40,50, # 10 to 50 seconds
  1*60, 1.5*60, 2*60, 3*60, 4*60, 5*60, 6*60, 7*60, 8*60, 9*60, 10*60, # 1 to 10 minutes
  15*60, 20*60, 25*60, 30*60, # 15 to 30 minutes
  1*60*60, 1.5*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60, 6*60*60, 10*60*60 # 1 to 10 hours
)

#followed.users <- read.csv('/home/felipe/2s2013/mo444/mo444/analysis/followed_user_ids.csv', header=FALSE)
#for (u in 1:nrow(followed.users)) {

  #user.name <- as.character(followed.users[u,'V1'])
  #user.id <- as.character(followed.users[u,'V2'])


  #print(paste(user.name, user.id, sep=': '))

  print('fetching tweets...')
  #q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id FROM tweet_with_created_at2 t WHERE t.user_id = %s AND t.tweet_created_at > 1379827567 AND t.tweet_created_at < 1379937600;", user.id))
  q0 <- dbGetQuery(con, sprintf("SELECT t.tweet_id, FROM_UNIXTIME(t.tweet_created_at, '%%w;%%H') AS week_and_hour FROM %s t WHERE t.user_id IN (27260086,21447363,14230524,813286,17919972,10228272,16409683,79293791,180505807,26565946,101402940,783214,85603854,44409004,15846407,155659213,19397785,28706024,25365536,21111883,184910040,1311017701,35787166,100220864,60865434,23375688,35094637,181561712,22940219,209708391,19058681,105119490,815322103,31927467,268414482,116362700,19248106,428333,73992972,119509520,85426644,158314798,84279963,1311017701,50393960,23976386,27195114,24929621,31239408,2425151,52551600,20322929,16190898,18863815,218571680,3004231,21425125,18091904,53153263,176566242,43152482,259379883,759251) AND t.tweet_created_at > 1379827567 AND t.tweet_created_at < %s ORDER BY RAND();", tweets.table, max.date))
  if (!'tweet_id' %in% names(q0)) {
    print('not enough tweets')
    next
  }
  user_tweets <- q0[['tweet_id']]

  feature.vectors <<- matrix(0, nrow=length(user_tweets), ncol=2*length(times))
  categoric.feature.vectors <<- matrix(0, nrow=length(user_tweets), ncol=13)
  observation.data <<- array(0, dim=length(user_tweets))
  tweet_counter <- 1
  print('fetching retweets history...')
  for (tweet_index in 1:length(user_tweets)) {
    tweet_id <- as.character(q0[tweet_index,'tweet_id'])
    tweet_week <- as.integer(substr(q0[tweet_index,'week_and_hour'], 1, 1))
    tweet_hour <- as.integer(substr(q0[tweet_index,'week_and_hour'], 3, 4))
    q <- dbGetQuery(con, sprintf("SELECT * FROM %s WHERE original_tweet_id = %s AND elapsed_time < 86400;", history.table, tweet_id))
    q2 <- dbGetQuery(con, sprintf("SELECT t.tweet_text, u.user_screen_name FROM %s t INNER JOIN user u ON t.user_id = u.user_id where t.tweet_id = %s;", tweets.table, tweet_id))

    if ('elapsed_time' %in% names(q) && length(q[['elapsed_time']]) > 0) {
      observation.data[tweet_counter] <- length(q[['elapsed_time']])
      feature.vectors[tweet_counter,] <- c(
         mo444GetFeatureVector(q[['elapsed_time']], q[['count']]),
         mo444GetFeatureVector(q[['elapsed_time']], q[['reached_followers_count']])
      )
      categoric.feature.vectors[tweet_counter,] <- c(
        ifelse(tweet_week==0, 1, 0), # sunday
        ifelse(tweet_week==1, 1, 0), # monday
        ifelse(tweet_week==2, 1, 0), # ...
        ifelse(tweet_week==3, 1, 0),
        ifelse(tweet_week==4, 1, 0),
        ifelse(tweet_week==5, 1, 0),
        ifelse(tweet_week==6, 1, 0),
        ifelse(tweet_hour>=0 && tweet_hour< 4, 1, 0), # 0AM to 4AM
        ifelse(tweet_hour>=4 && tweet_hour< 8, 1, 0), # 4AM to 8AM
        ifelse(tweet_hour>=8 && tweet_hour< 12, 1, 0), # ...
        ifelse(tweet_hour>=12 && tweet_hour< 16, 1, 0),
        ifelse(tweet_hour>=16 && tweet_hour< 20, 1, 0),
        ifelse(tweet_hour>=20 && tweet_hour< 24, 1, 0)
      )
      tweet_counter <- tweet_counter + 1
    }
  }
  num.tweets <- tweet_counter - 1
  feature.vectors <<- feature.vectors[1:num.tweets,]
  categoric.feature.vectors <<- categoric.feature.vectors[1:num.tweets,]
  observation.data <<- observation.data[1:num.tweets]
  feature.vectors.df <<- mo444BuildFeatureVectorsDataframe()

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
  feature.vectors.norm <<- cbind(feature.vectors.norm, 1) # add bias

  mean.y <<- mean(observation.data[1:train.size])
  sd.y <<- sd(observation.data[1:train.size])
  observation.data.norm <<- (observation.data - mean.y) / sd.y

  #regression.results <- mo444LinearRegression()
  decision.tree.results <- mo444DecisionTree()
#}

dbDisconnect(con)