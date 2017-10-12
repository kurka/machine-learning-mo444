class Fetcher():
   def __init__(self, api=None, db=None):
      self.api = api
      self.db = db

   def fetchTweetRecursive(self, tweetId):
      retweeters = self.api.GetRetweeters(tweetId)



   def visitTweet(self, tweet):
      print "visiting tweet %d" % (tweet.id)
      '''
      TODO check whether tweet is already persisted
      TODO if not, persist it
      '''
