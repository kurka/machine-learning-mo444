import json
import twitter
import TweetFetcher
import PersistentLayer

json_fp = open('./twitter-api-credentials.json')
data = json.load(json_fp)
api = twitter.Api(consumer_key=data['consumer_key'], consumer_secret=data['consumer_secret'], access_token_key=data['access_token_key'], access_token_secret=data['access_token_secret'])
db = PersistentLayer.Persister()

fetcher = TweetFetcher.Fetcher(api, db)
fetcher.fetchTweetRecursive("374623252179472385")
