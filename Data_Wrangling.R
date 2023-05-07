library(dplyr)
library(stringr)
library(testthat)



subscribed_df <- read.csv("H:/INFO201/FINAL/topSubscribed.csv")
videos_df <- read.csv("H:/INFO201/FINAL/CAvideos.csv")


videos_df <- select(videos_df, channel_title, views, category_id)
subscribed_df <- select(subscribed_df, Youtube.Channel, Video.Views, Category, Video.Count)

#Since there are some lines contain "0", we first clean these lines in subscribed_df
subscribed_df <- subset(subscribed_df, Video.Count != 0)

# After cleaning these line, we face the second question. 
# One dataset is containing the channel as whole, while another is aboue each 
# single video. So we decide to add a new column called "performance" to 
# Find the average number of play of each video for each Youtuber.

performance <- vector("numeric", length = nrow(subscribed_df))
subscribed_df$views <- as.numeric(subscribed_df$Video.Views)

# There are also datas contain the mark "," in the numbers, to deal with it,
# We use the gsub function to remove the "," within the column
# We also use as.numeric to deal with the strings...

subscribed_df[,2] <- gsub(",", "", subscribed_df[,2])
subscribed_df[,4] <- gsub(",", "", subscribed_df[,4])

subscribed_df[,2] <- as.numeric(subscribed_df[,2])
subscribed_df[,4] <- as.numeric(subscribed_df[,4])

i <- 1

while(i < nrow(subscribed_df)){
  
  performance[i] = subscribed_df[i,2] / subscribed_df[i,4]
  
  i <- i + 1
  
}

subscribed_df$views <- performance

# Yet, in the most popular videos, we may also see Youtuber with high subscribed
# numbers also in the list of top subscribers, so we add a new feature 
subscribed_df$Channel.Video <- "Channel"
videos_df$Channel.Video <- "Video"

# Converting the categories in video_df into the characters
# The meaning behind the numbers is provided in the json file of the uploader

videos_df <- videos_df %>% mutate(category_id = case_when(
  
  category_id == 2 ~ "Autos & Vehicles",
  category_id == 10 ~ "Music",
  category_id == 15 ~ "Pets & Animals",
  category_id == 17 ~ "Sports",
  category_id == 18 ~ "Short Movies",
  category_id == 19 ~ "Travel & Events",
  category_id == 20 ~ "Gaming",
  category_id == 21 ~ "Videoblogging",
  category_id == 22 ~ "People & Blogs",
  category_id == 23 ~ "Comedy",
  category_id == 24 ~ "Entertainment",
  category_id == 25 ~ "News & Politics",
  category_id == 26 ~ "Howto & Style",
  category_id == 27 ~ "Education",
  category_id == 28 ~ "Science & Technology",
  category_id == 30 ~ "Movies",
  category_id == 31 ~ "Anime/Animation",
  category_id == 32 ~ "Action/Adventure",
  category_id == 33 ~ "Classics",
  category_id == 34 ~ "Comedy",
  category_id == 35 ~ "Documentary",
  category_id == 36 ~ "Drama",
  category_id == 37 ~ "Family",
  category_id == 38 ~ "Foreign",
  category_id == 39 ~ "Horror",
  category_id == 40 ~ "Sci-Fi/Fantasy",
  category_id == 41 ~ "Thriller",
  category_id == 42 ~ "Shorts",
  category_id == 43 ~ "Shows",
  category_id == 44 ~ "Trailers",
  
  ))

# Yet, we see see many NAs in this column
# So we would remove these lines with NA

videos_df <- filter(videos_df, !is.na(videos_df$category_id))

# So far, the videos_df is cleaned and ready to be merge
# But we need to remove the irrelevent line in subscribed_df 

subscribed_df <- select(subscribed_df,Youtube.Channel,Category,views,Channel.Video)

#Now they have same number of roles
#But we still need to change name of each column to use Rbind

names(subscribed_df)[1] <- "channel_title"
names(subscribed_df)[2] <- "category"

youtube_df <- rbind(videos_df, subscribed_df)
