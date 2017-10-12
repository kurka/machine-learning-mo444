import json
import MySQLdb
import os

class TweetsPersister():
   def __init__(self):
      basepath = os.path.dirname(os.path.realpath(__file__))
      json_fp = open(basepath + '/db-credentials.json')
      credentials = json.load(json_fp)
      self.db_raw = MySQLdb.connect(host = credentials['raw-tweets']['host'],
                                    db = credentials['raw-tweets']['db'],
                                    user = credentials['raw-tweets']['user'],
                                    passwd = credentials['raw-tweets']['passwd'])

      self.db = MySQLdb.connect(host = credentials['structured-tweets']['host'],
                                db = credentials['structured-tweets']['db'],
                                user = credentials['structured-tweets']['user'],
                                passwd = credentials['structured-tweets']['passwd'],
                                charset = 'utf8')


   def insertRawTweet(self, string):
      c = self.db_raw.cursor()
      c.execute("INSERT INTO raw_tweets (tweet) VALUES (%s)", string)
      self.db_raw.commit()
      return


   def loadTweet(self, tweet_id):
      """
      Tweet loader.
      @param tweet_id
      @return Populated tweet dictionary, if tweet found. "None" otherwise.
      """
      c = self.db.cursor()
      c.execute("SELECT tweet_text, tweet_source, user_id, tweet_retweeted_status_id FROM tweet WHERE tweet_id = %s", tweet_id)
      row = c.fetchone()
      if row is None:
         return None

      tweet = {
         'id': tweet_id,
         'text': row[0],
         'source': row[1],
         'user_id': row[2],
         'retweeted_status_id': row[3],
      }

      return tweet


   def loadUser(self, user_id):
      """
      User loader.
      @param user_id
      @return Populated user dictionary, if found. "None" otherwise.
      """
      c = self.db.cursor()
      c.execute("SELECT user_name, user_screen_name, user_location, user_description, user_url, user_followers_count, user_friends_count, user_favourites_count FROM user WHERE user_id = %s", user_id)
      row = c.fetchone()
      if row is None:
         return None

      user = {
         'id': user_id,
         'name': row[0],
         'screen_name': row[1],
         'location': row[2],
         'description': row[3],
         'url': row[4],
         'followers_count': row[5],
         'friends_count': row[6],
         'favorited_count': row[7],
      }

      return user

   def saveTweet(self, tweet):
      """
      Create/update tweet on database.
      @param Populated tweet.
      """
      existing = self.loadTweet(tweet['id'])
      if not existing is None:
         #TODO - update tweet
         return
      else:
         user = self.loadUser(tweet['user']['id'])
         if user is None:
            # create user
            user = self.saveUser(tweet['user'])

         tweet['user_id'] = tweet['user']['id']

         # create tweet
         c = self.db.cursor()
         data = (
            tweet['id'],
            tweet['text'],
            tweet['source'],
            (1 if tweet['truncated'] else 0),
            tweet['in_reply_to_status_id'],
            tweet['in_reply_to_user_id'],
            tweet['in_reply_to_screen_name'],
            tweet['user_id'],
            tweet['retweet_count'],
            tweet['favorite_count'],
            (1 if tweet['favorited'] else 0),
            (1 if tweet['retweeted'] else 0),
            (1 if ('possibly_sensitive' in tweet and tweet['possibly_sensitive']) else 0),
            tweet['lang'],
            (tweet['retweeted_status']['id'] if 'retweeted_status' in tweet else 0)
         )
         c.execute("INSERT INTO tweet (tweet_id, tweet_text, tweet_source, tweet_truncated, tweet_in_reply_to_status_id, tweet_in_reply_to_user_id, tweet_in_reply_to_screen_name, user_id, tweet_retweet_count, tweet_favorite_count, tweet_favorited, tweet_retweeted, tweet_possibly_sensitive, tweet_lang, tweet_retweeted_status_id) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", data)

         if 'entities' in tweet:
            if 'urls' in tweet['entities']:
               for url in tweet['entities']['urls']:
                  url_data = (
                     tweet['id'],
                     url['url'],
                     url['display_url'],
                     url['expanded_url'],
                     '%d;%d' % (url['indices'][0], url['indices'][1])
                  )
                  c.execute("INSERT INTO tweet_url (tweet_id, tweet_url_url, tweet_url_display_url, tweet_url_expanded_url, tweet_url_indices) VALUES (%s, %s, %s, %s, %s)", url_data)

            if 'user_mentions' in tweet['entities']:
               for user_mention in tweet['entities']['user_mentions']:
                  user_mention_data = (
                     tweet['id'],
                     user_mention['id'],
                     user_mention['screen_name'],
                     user_mention['name'],
                     '%d;%d' % (user_mention['indices'][0], user_mention['indices'][1])
                  )
                  c.execute("INSERT INTO tweet_usermention (tweet_id, user_id, user_screen_name, user_name, tweet_usermention_indices) VALUES (%s, %s, %s, %s, %s)", user_mention_data)

            if 'hashtags' in tweet['entities']:
               for hashtag in tweet['entities']['hashtags']:
                  hashtag_data = (
                     tweet['id'],
                     hashtag['text'],
                     '%d;%d' % (hashtag['indices'][0], hashtag['indices'][1])
                  )
                  c.execute("INSERT INTO tweet_hashtag (tweet_id, hashtag_text, hashtag_indices) VALUES (%s, %s, %s)", hashtag_data)

         self.db.commit()
      return

   def saveUser(self, user):
      existing = self.loadUser(user['id'])
      if not existing is None:
         # TODO - update user
         return
      else:
         # create user
         c = self.db.cursor()
         data = (
            user['id'],
            user['name'],
            user['screen_name'],
            user['location'],
            user['description'],
            user['url'],
            user['followers_count'],
            user['friends_count'],
            user['listed_count'],
            user['created_at'],
            user['favourites_count'],
            user['utc_offset'],
            user['time_zone'],
            (1 if user['geo_enabled'] else 0),
            (1 if user['verified'] else 0),
            user['statuses_count'],
            user['lang'],
            (1 if user['contributors_enabled'] else 0)
         )
         c.execute("INSERT INTO user (user_id, user_name, user_screen_name, user_location, user_description, user_url, user_followers_count, user_friends_count, user_listed_count, user_created_at, user_favourites_count, user_utc_offset, user_time_zone, user_geo_enabled, user_verified, user_statuses_count, user_lang, user_contributors_enabled) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", data)
         self.db.commit()
      return

   def updateTweetCreatedAt(self, tweet):
      c = self.db.cursor()
      data = (
         tweet['created_at'],
         tweet['id']
      )
      c.execute("UPDATE tweet SET tweet_created_at = %s WHERE tweet_id = %s", data)
      self.db.commit()
      return
