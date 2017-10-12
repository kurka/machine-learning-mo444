import csv
import json
import os
import PersistencyLayer
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream

basepath = os.path.dirname(os.path.realpath(__file__))
json_fp = open(basepath + '/twitter-api-credentials.json')
credentials = json.load(json_fp)

reader = csv.reader(open(basepath + "/followed_user_ids.csv", "rb"))
followedUserIds = []
for row in reader:
   followedUserIds.append(row[1])

persister = PersistencyLayer.TweetsPersister()

class MainStreamingListener(StreamListener):
   """ A listener handles tweets are the received from the stream.
   This is a basic listener that just prints received tweets to stdout.

   """

   def on_data(self, data):
      parsed = json.loads(data)

      # we are only interested on user's tweets or direct retweets
      if ('user' in parsed and parsed['user']['id_str'] in followedUserIds) or ('retweeted_status' in parsed and parsed['retweeted_status']['user']['id_str'] in followedUserIds):
         print "tweetid: %d" % parsed['id']
         persister.insertRawTweet(data)

      return True

   def on_error(self, status):
      print "error: %d" % status
      return

if __name__ == '__main__':
   l = MainStreamingListener()
   auth = OAuthHandler(credentials['consumer_key'], credentials['consumer_secret'])
   auth.set_access_token(credentials['access_token_key'], credentials['access_token_secret'])

   stream = Stream(auth, l)
   stream.filter(follow=[','.join(followedUserIds)])
