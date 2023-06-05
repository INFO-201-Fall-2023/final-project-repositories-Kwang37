library(dplyr)
library(stringr)
library(ggplot2)

# 这一段是载入csv数据包 工作路径要根据实际情况更改
# 视频相关的数据储存在videos_df里 订阅相关的数据储存在subscribed_df里
# This part will load a csv file, change the working directory as you need.
# Video data are storageed in videos_df, subscribe data in subscribed_df.

subscribed_df <- read.csv("topSubscribed.csv")
videos_df <- read.csv("CAvideos.csv")

# 这一段用来剔除我们不需要的数据 如描述 视频id等等
# This part will delete the data we dont need, such as descrption and video ID.
videos_df <- select(videos_df, channel_title, views, category_id,likes,dislikes,comment_count)
subscribed_df <- select(subscribed_df, Youtube.Channel, Video.Views, Category, Video.Count,Subscribers)

# Since there are some lines contain "0", we first clean these lines in subscribed_df
subscribed_df <- subset(subscribed_df, Video.Count != 0)

# There are also datas contain the mark "," in the numbers, to deal with it,
# We use the gsub function to remove the "," within the column
# We also use as.numeric to deal with the strings...

# 这一部分会把原来的带逗号数据转化为可以读取和处理的数字
# This part will change numbers with , into processable numbers.
subscribed_df[,2] <- gsub(",", "", subscribed_df[,2])
subscribed_df[,4] <- gsub(",", "", subscribed_df[,4])
subscribed_df[,5] <- gsub(",", "", subscribed_df[,5])

subscribed_df[,2] <- as.numeric(subscribed_df[,2])
subscribed_df[,4] <- as.numeric(subscribed_df[,4])
subscribed_df[,5] <- as.numeric(subscribed_df[,5])

# 同样的，这段关于整合的部分也被删除了
# 由于其中有一个数据集的类型 Categories是用数字表示的
# 所以我们根据注释中的文案将其assign到具体文字
# Converting the categories in video_df into the characters
# The meaning behind the numbers is provided in the json file of the csv uploader

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
# 文件中存在一些NA 删除掉

videos_df <- filter(videos_df, !is.na(videos_df$category_id))

# 此处和整合相关的code已经删除
# 这里作用是统一命名
# Rename the column

names(subscribed_df)[1] <- "channel_title"
names(videos_df)[3] <- "category"

# Now, I will try to find what is the most popular topic 
# To do so, we need to find the rate of transform and the total number of subscriber
# These two line of codes will remove an error line, and the ones with zero videos.
# The error line contains a link, idk why it appears. Maybe some kind of watermark.
# 有的频道没有数据 所以我们把它删掉
# 此外 有一个类别的数据引导到了一个链接 所以我们也删掉
subscribed_df <- subset(subscribed_df, Video.Count != 0)
subscribed_df <- filter(subscribed_df, subscribed_df$Category != "https://us.youtubers.me/global/all/top-1000-most_subscribed-youtube-channels")


# This line of code will calculate the rate of transform, which can used to represent how attractive a topic is.
# 这一部分算的是“转化率”也就是有多少观看视频的人成为了该频道的订阅者
subscribed_df$Transform <- subscribed_df$Subscribers / subscribed_df$Video.Views

# Now we get the subscribed_df we need, containing trasform rate. 
# But we also found some accounts have errors in the transfrom rate extremely high. 
# So we use the classic statistical way to remove them (outliers)
# 有一些频道的转化率太高了 比如一个视频几百万关注 这明显是不正常的 所以我们删掉

sd_trans <- sd(subscribed_df$Transform)

Q1 <- quantile(subscribed_df$Transform, 0.25)
Q3 <- quantile(subscribed_df$Transform, 0.75)
IQR <- Q3 - Q1
lower_fence <- Q1 - 1.5 * IQR
upper_fence <- Q3 + 1.5 * IQR

# Identify outliers and remove them...
subscribed_df <- filter(subscribed_df,subscribed_df$Transform < upper_fence)
subscribed_df <- filter(subscribed_df,subscribed_df$Transform > lower_fence)

# 这样一来我们就移除了那些数据异常的账号
# 接下来我们来计算每个类别下的订阅数量
# Now we have a dataset with transfrom rate
# Now we are going to calculate the average subscriber of each categories

subscribers_by_category <- aggregate(Subscribers ~ Category, data = subscribed_df, FUN = mean)

# Now we are going to calculate the average transform rate of each categories

subscribers_by_transform <- aggregate(Transform ~ Category, data = subscribed_df, FUN = mean)

#By now, we merge the data about transform rate and subscrption together
category_avg <- merge(subscribers_by_category,subscribers_by_transform,by = "Category")

# Now we can find the relationship between topic and subscribers
# 这段会生成分区和平均粉丝数之间的图表 用于part3的分析
category_avg$Category <- reorder(category_avg$Category , category_avg$Subscribers)

cat_sub_plot <- ggplot(data = category_avg, aes(x = Subscribers, y = Category, color = Category)) +
  geom_point() + ylab("Category of the video") + xlab("Number of subscribers")

# cat_sub_plot 输入这段来观看图表
# type this one to see the plot

# 这一段则是不同类别的分区和转化率之间的图表 我不记得这段是干啥的了反正先留着
# This line shows the transform rate between different categories.
category_avg$Category<- reorder(category_avg$Category, category_avg$Transform)

transform_plot <- ggplot(data = category_avg, aes(x = Transform , y = Category, color = Category)) +
  geom_point() + ylab("Category of the video") + xlab("Rate of transform")

# transform_plot
# 这个图用来让我们看 不同分区之间 转化率和粉丝数之间的关系 比如说 某某分区粉丝数多 转化率高
# 某某分区粉丝数少 转化率低 暂时也不确定是干嘛的
plot <- ggplot(data = category_avg, aes(x = Subscribers, y = Transform, color = Category)) + geom_point()

# Yet, all these above only shows the top subscibed channels, what if we take a look in real day data ?
# The following part is to calculate "percentage of total play"
# For example, if the total view on Youtube is 1000 on today, and video a has 200 views
# We say "This video contributed to 20% of total play"
# 接下来的部分则是着重于每天的数据 也就是CAvideos，需要的话可以在最开始的地方更改数据集的名字
# play_ratio是用来算“这个视频的播放量占据了当天总播放量的百分之多少”
# 比如说今天youtube全平台的用户看了1000次视频 然后某个视频的播放量为200次
# 那么这个视频就占据了当天总播放量的百分之二十
videos_df$play_ratio <- videos_df$views / sum(videos_df$views)
videos_df$like_dis_ratio <- videos_df$dislikes / videos_df$likes

# 这段用于计算平均每个分区的视频播放量 并且画出图标
# This part will calculate the average views of different categories
views_by_category <- aggregate(views ~ category, data = videos_df, FUN = mean)

views_by_category$category<- reorder(views_by_category$category, views_by_category$views)

  video_plot <- ggplot(data = views_by_category, aes(x = views , y = category, color = category)) +
    geom_point() + xlab("AVG views of the video") + ylab("Categories of video")

#video_plot 

# The following part will displays the pie chart based on the video data
# We also have the version without the music and movie data since they are too high
# 这个则是把不同分区的视频播放占总播放数量的比例用饼状图画出来
# 因为music和movie比较特殊 所以我们在后面剔除掉
views_by_category$ratio <- views_by_category$views / sum(views_by_category$views)

pie(views_by_category$ratio, labels = views_by_category$category, col = rainbow(15))

# 这一段就是没有music的pie chart了
views_no_music <- views_by_category
views_no_music <- filter(views_no_music,views_no_music$category != "Music")
views_no_music <- filter(views_no_music,views_no_music$category != "Movies")

pie(views_no_music$ratio, labels = views_no_music$category, col = rainbow(13))

# We also want to know whether the number of comment and ratio of upvote and downvote
# would affects the number of view.
# 这段对应part 1 也就是点赞点踩数量和视频播放量之间的关系
# Start from this part, we will discuss the relationship between like, dislike, comments, and the number of plays

# 这里可以建立点赞点踩之比和播放量以及评论数量之间的线性回归关系
view_model <- lm(views ~ comment_count + like_dis_ratio, data = videos_df)
summary(view_model)

# 然后可以写成一个式子

# 这里就是不同分区的平均点赞/点踩比和评论数了
# 如果需要的话 我们可以计算出当天最具争议的视频都来自哪些分区

# 20230604更新 重新制作了like和dislike的比例

#这两行是用于计算每个视频的点赞/点踩比
videos_df$like_ratio <- videos_df$likes / (videos_df$likes + videos_df$dislikes)
videos_df$dislike_ratio <- 1 - videos_df$like_ratio

#这两行是用于计算每个分区视频的平均点赞/点踩比
like_ratio_by_category <- aggregate(like_ratio ~ category, data = videos_df, FUN = mean)
like_ratio_by_category$dislike_ratio <- 1 - like_ratio_by_category$like_ratio

# 这一段用于画图 所以创造了很多没用的df
category_df_1 <- data.frame(category =like_ratio_by_category$category, ratio = like_ratio_by_category$like_ratio)
category_df_1$type <- "like"

category_df_2 <- data.frame(category =like_ratio_by_category$category, ratio = like_ratio_by_category$dislike_ratio)
category_df_2$type <- "dislike"

df_merged_1 <- merge(category_df_1,category_df_2, by = c("category", "type"), all = TRUE)

ratio <- vector("numeric", length = nrow(df_merged_1))

i <- 1

while(i < nrow(df_merged_1) + 1){
  
  if(is.na(df_merged_1[i,4])){
    ratio[i] <- df_merged_1[i,3]    
  }else(
    ratio[i] <- df_merged_1[i,4]
  )
  
  i <- i + 1
  
}

df_merged_1$ratio <- ratio

p <- ggplot(df_merged_1, aes(fill=type, y=category, x = ratio)) + 
  geom_bar(position='stack', stat='identity') + scale_fill_manual(values = c("green2","tomato"))

plot(p)

# Comment
comment_by_category <- aggregate(comment_count ~ category, data = videos_df, FUN = mean)
category_df <- merge(comment_by_category,like_ratio_by_category)

p <- ggplot(data = category_df, aes(x = like_ratio, y = comment_count, text = category)) +
  geom_point(aes(color = category)) + labs(x = "Like Ratio", y = "Number of comment") +
  geom_hline(yintercept = mean(category_df$comment_count), color = "red2")

plot(p)

