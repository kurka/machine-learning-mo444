-- MySQL dump 10.13  Distrib 5.5.31, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: mo444-proj
-- ------------------------------------------------------
-- Server version	5.5.31-0ubuntu0.12.10.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `tweet`
--

DROP TABLE IF EXISTS `tweet`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tweet` (
  `tweet_id` bigint(20) unsigned NOT NULL,
  `tweet_text` varchar(140) DEFAULT NULL,
  `tweet_source` varchar(45) DEFAULT NULL,
  `tweet_truncated` smallint(6) DEFAULT NULL,
  `tweet_in_reply_to_status_id` bigint(20) unsigned DEFAULT NULL,
  `tweet_in_reply_to_user_id` bigint(20) unsigned DEFAULT NULL,
  `tweet_in_reply_to_screen_name` varchar(128) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `tweet_retweet_count` int(10) unsigned DEFAULT NULL,
  `tweet_favorite_count` int(10) unsigned DEFAULT NULL,
  `tweet_favorited` smallint(6) DEFAULT NULL,
  `tweet_retweeted` smallint(6) DEFAULT NULL,
  `tweet_possibly_sensitive` smallint(6) DEFAULT NULL,
  `tweet_lang` varchar(8) DEFAULT NULL,
  `tweet_retweeted_status_id` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`tweet_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tweet`
--

LOCK TABLES `tweet` WRITE;
/*!40000 ALTER TABLE `tweet` DISABLE KEYS */;
/*!40000 ALTER TABLE `tweet` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tweet_hashtag`
--

DROP TABLE IF EXISTS `tweet_hashtag`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tweet_hashtag` (
  `tweet_hashtag_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `tweet_id` bigint(20) unsigned DEFAULT NULL,
  `hashtag_text` varchar(140) DEFAULT NULL,
  `hashtag_indices` varchar(128) DEFAULT NULL,
  PRIMARY KEY (`tweet_hashtag_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tweet_hashtag`
--

LOCK TABLES `tweet_hashtag` WRITE;
/*!40000 ALTER TABLE `tweet_hashtag` DISABLE KEYS */;
/*!40000 ALTER TABLE `tweet_hashtag` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tweet_url`
--

DROP TABLE IF EXISTS `tweet_url`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tweet_url` (
  `tweet_url_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `tweet_id` bigint(20) unsigned DEFAULT NULL,
  `tweet_url_url` varchar(128) DEFAULT NULL,
  `tweet_url_display_url` varchar(128) DEFAULT NULL,
  `tweet_url_expanded_url` varchar(128) DEFAULT NULL,
  `tweet_url_indices` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`tweet_url_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tweet_url`
--

LOCK TABLES `tweet_url` WRITE;
/*!40000 ALTER TABLE `tweet_url` DISABLE KEYS */;
/*!40000 ALTER TABLE `tweet_url` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tweet_usermention`
--

DROP TABLE IF EXISTS `tweet_usermention`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tweet_usermention` (
  `tweet_usermention_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `tweet_id` bigint(20) unsigned DEFAULT NULL,
  `user_id` bigint(20) unsigned DEFAULT NULL,
  `user_screen_name` varchar(256) DEFAULT NULL,
  `user_name` varchar(256) DEFAULT NULL,
  `tweet_usermention_indices` varchar(45) DEFAULT NULL,
  PRIMARY KEY (`tweet_usermention_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tweet_usermention`
--

LOCK TABLES `tweet_usermention` WRITE;
/*!40000 ALTER TABLE `tweet_usermention` DISABLE KEYS */;
/*!40000 ALTER TABLE `tweet_usermention` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `user_id` bigint(20) unsigned NOT NULL,
  `user_name` varchar(256) NOT NULL,
  `user_screen_name` varchar(256) NOT NULL,
  `user_location` varchar(256) DEFAULT NULL,
  `user_description` varchar(1024) DEFAULT NULL,
  `user_url` varchar(256) DEFAULT NULL,
  `user_followers_count` int(10) unsigned DEFAULT NULL,
  `user_friends_count` int(10) unsigned DEFAULT NULL,
  `user_listed_count` int(10) unsigned DEFAULT NULL,
  `user_created_at` varchar(128) DEFAULT NULL,
  `user_favourites_count` int(10) unsigned DEFAULT NULL,
  `user_utc_offset` int(11) DEFAULT NULL,
  `user_time_zone` varchar(128) DEFAULT NULL,
  `user_geo_enabled` smallint(6) DEFAULT NULL,
  `user_verified` smallint(6) DEFAULT NULL,
  `user_statuses_count` int(10) unsigned DEFAULT NULL,
  `user_lang` varchar(10) DEFAULT NULL,
  `user_contributors_enabled` smallint(6) DEFAULT NULL,
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2013-10-09 15:49:46
