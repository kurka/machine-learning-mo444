#import csv
import PersistencyLayer

persister = PersistencyLayer.TweetsPersister()
collectStartedAt = 1379827567
c1 = persister.db.cursor()
c2 = persister.db.cursor()

#reader = csv.reader(open("./followed_user_ids.csv", "rb"))
#followedUserIds = []
#for row in reader:
#   followedUserIds.append(row[1])

last_original_tweet_id = 381639673019641856

params = (
   collectStartedAt,
   last_original_tweet_id
   #','.join(followedUserIds)
)

c2.execute("SELECT tweet_id, tweet_created_at FROM tweet_with_created_at4 WHERE tweet_created_at > %s AND tweet_retweeted_status_id = 0 AND tweet_id >= %s", params)

original_rows = c2.fetchall()
for original_row in original_rows:
   original_tweet_id = original_row[0]
   original_created_at = original_row[1]
   print "original_tweet: %s" % (original_tweet_id)

   c1.execute("SELECT tweet_id, tweet_created_at, user_id FROM tweet_with_created_at4 WHERE tweet_retweeted_status_id = %s", (original_tweet_id))

   rows = c1.fetchall()
   count = 0
   reached_followers_count = 0
   reached_friends_count = 0
   reached_favorited_count = 0
   for row in rows:
      retweet_id = row[0]
      retweet_created_at = row[1]
      user_id = row[2]

      user = persister.loadUser(user_id)

      count += 1
      reached_followers_count += user["followers_count"]
      reached_friends_count += user["friends_count"]
      reached_favorited_count += user["favorited_count"]
      data = (
         original_tweet_id,
         retweet_id,
         int(retweet_created_at) - int(original_created_at),
         count,
         reached_followers_count,
         reached_friends_count,
         reached_favorited_count
      )
      c1.execute("INSERT INTO retweets_history4_2 (original_tweet_id, retweet_id, elapsed_time, count, reached_followers_count, reached_friends_count, reached_favorited_count) VALUES (%s, %s, %s, %s, %s, %s, %s)", data)
      persister.db.commit()


   original_row = c2.fetchone()
