library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("final.R")

subscribed_df <- read.csv("topSubscribed.csv")
videos_df <- read.csv("CAvideos.csv")

ui <- navbarPage(
  title = "Video Analysis",
  
  tabPanel("Introduction",
           fluidPage(
             titlePanel("Public Interest based on viewers' interest in video content on the Internet"),
             tabsetPanel(
               tabPanel("Background Notes",
                        h3("Background Information"),
                        h4("Nowadays, video has become an important communication
                           medium. Platforms such as TikTok and Youtube brought a
                           chance for the user to share and create content, making
                           the communication environment today become more diversified.
                           For content creators, the popularity of video platforms also
                           led to more opportunities to gain benefits from their influence.
                           Therefore, this study will use the data from subscribers and 
                           top-trended videos on Youtube to seek the relationship between
                           the audience, creator, and categories."),
                        h3("Inspiration"),
                        h4("The project is inspired by the Youtuber experience of one 
                           of our group members, Ziyi Li, who began working as a video content 
                           creator at Bilibili and Youtube in 2021, primarily 
                           targeting Mandarin-speaking audiences in East and Southeast Asia."),
                        tags$a(href = "https://www.youtube.com/channel/UCqstTrpgvYJ1Tk2Ji-zn01Q", "Ziyi Li's Youtube Channel"),
                        br(),
                        tags$a(href = "https://space.bilibili.com/11037021", "Ziyi Li's Bilibili Channel"),
                        h3("Finding Data"),
                        h4("The datasets were collected by MITCHELL J and MRITYUNJAY PATHAK on Kaggle.
                           the source of the data of this database is from the backend data of Youtube 
                           users, the data is collected by using Youtube program data statistics. 
                           Viewers and creators are involved in the data collection."),
                        tags$a(href = "https://www.kaggle.com/datasets/themrityunjaypathak/most-subscribed-1000-youtube-channels", "Most Subscribed 1000 Youtube Channels"),
                        br(),
                        tags$a(href = "https://www.kaggle.com/datasets/datasnaek/youtube-new?select=USvideos.csv", "Trending YouTube Video Statistics"),
               )
             )
           )
  ),
  
  tabPanel("Views Analysis",
         fluidPage(
           titlePanel("What kind of video attracts most attention?"),
           tabsetPanel(
             tabPanel("Video Views by Category",
                      fluidRow(
                        column(width = 6,
                               h3('The idea of "Accessibility"'),
                               tags$p('The first thing we need to discuss is the idea of',tags$b('"Accessibility"'),'. Which means',tags$b('"How easy an audience can enjoy the content"'), 'or', tags$b('"how much knowlege or experience a audience needs to understand and enjoy the video"'),'. For example, a video about a football game has higher accessibility than a video about statistics in Rstudio.',tags$b("Not everyone studied informatics and statistic, but most people know how football competition works.")),
                               tags$p("From the data, we found that", tags$b("Music, Movies, Science, and Sports"), "are the most interesting types of videos. "),
                               tags$p('One thing to mention is, the "Science and Techonolgy" category here is more related to digital products (such as the unboxing of a phone or computer) instead of academic research. This conclusion is fitting our prediction, that video requires', tags$b("less knowledge"), 'and can bring more views. Many users watch them in', tags$b("seek of entertainment"), 'rather than', tags$b("getting information"),'. In contrast, videos in the education, news, and travel genres often require more time and attention to comprehend, resulting in relatively fewer views on video platforms.'),
                               tags$p('According to the data,', tags$b('"Music"'), 'and', tags$b('"Movies"'), 'are the two most popular categories on Youtube, followed by Science, Sports, and entertainment.'),
                               tags$p("Yet, this does not mean music creator is the most welcoming type. People may add the video of music to a playlist. A single video could be played hundreds of times a day. Similar bug issues may also be applied to the movie category; the life period of a movie is longer than a video."),
                               tags$p("Although music and movies are said to account for a total percentage of 39.2%, this is not a authentic reflection of public interest."),
                               tags$p("In contrast, videos in the education, news, and travel genres often require", tags$b("more time and attention to comprehend"), ", resulting in relatively fewer views on video platforms."),
                               tags$p("This outcome is not surprising as these types of videos demand more effort and time from viewers, whereas short video platforms are better suited for content that can quickly capture viewers' attention and evoke strong emotional experiences in a short period of time.")
                        ),
                        column(width = 6,
                               plotlyOutput("pieChart")
                        )
                      )
             )
           )
         )
  ),
  
  tabPanel("Followers Analysis",
         fluidPage(
           titlePanel("What kind of video attracts followers?"),
           tabsetPanel(
             tabPanel("Transformation Rates",
                      h3("Background Notes"),
                      tags$p("This part will focus on the idea of the", tags$b("transform rate"),". Which represents the relationship between subscribers and the number of views. For example, a Youtuber uploaded a new video with 10000 times views. Among these 10000 times views, 100 users decide to subscribe to the channel. At this time, the transformation rate here is 100 / 10000 = 0.01."),
                      tags$p("Yet, the data of this part came from the", tags$b('TOP 1000 subscribed YouTubers on Youtube'), ", so it can", tags$b("only reflect a small part"), "of the creator environment of Youtube. Most content creators may", tags$b("not be able to get to this level"), ". "),
                      tags$p("The transform rate is important for creators since it can tell the", tags$b("uniqueness"), "of their channel and content.  A channel with a", tags$b("high transformation rate"), "means this creator's video is", tags$b("attractive and unique"), ", and people who watched their video decided to stay and", tags$b("wait for more future content"),". It is a variant of the", tags$b("free market theory"), "in economics: the resource (time) is limited, but the demand (for entertainment) is not. Videos today are", tags$b("substitute goods"), "for each other. When a user subscribed to a channel, he or she is more likely to spend time on this creator's video rather than others."),
                      br(),
                      h4("In this section, we made a scattorplot for you to explorethe rate of transform among the TOP 1000 subscribed Youtubers."),
                      
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("transformInput", label = "Select Transform Input", min = 0, max = 0.009, value = 0.003, step = 0.003)
                        ),
                        mainPanel(
                          plotlyOutput("interactivePlot")
                        )
                      ),
                      br(),
                      h3("Takeaway Message"),
                      
                      tags$p('From the data, we found that', tags$b("Nonprofits and Activism, Science and technology, and Howto and Style"), 'are the ones with the', tags$b("highest transformation rate"),'. It is very interesting that nonprofits and how-to are having a', tags$b("high transformation rate"), 'rather to games and entertainment. But it is also reasonable. For the content creator, making a video about activism or science may have a higher cost rather than topics like games or blogs. Therefore, when the cost of a specific category of video is high, people would have', tags$b("fewer choices"), 'when consuming these kinds of videos. ')
             )
           )
         )
  ),
  
  tabPanel("Ratio Analysis",
  tabsetPanel(
    tabPanel("Comparing Like and Dislike Ratios in Various Video Categories",
             h3("Background Notes"),
             tags$p('Finally, this part of data will focus on the like and dislike. When a user clicked a like or dislike button, we count this behavior as one reflection. Differences in ratio can represent different features of video. Based on the sum of like and dislike, we can calculate the "like ratio" of different categories. This is why we created a "ratio" column.'),
             tags$p("When a type video is having a high ratio of", tags$b("dislike"), ", it means this category of video is", tags$b("controversial"),". By controversial, it", tags$b("does not"), "mean the video must having something offensive or incorrect content, but maybe led to the", tags$b("argument between viewers"),". The most controversial categories are: Shows, News, and Blogs. Compared to the Pet, Howto, Travels, and Gaming. Thinking of the idea of accessibility in the previous chapter, it is possible that, topics like game, pet and travel may contain", tags$b("much less information than"), "topics like shows, news and blogs. But another way to think of this phenomenon is that, topics like shows and news may contain more", tags$b("subjective emotion"), "of the creator. "),
             plotlyOutput("barPlot")
    ),
    tabPanel("Like/Dislike Ratio vs. Number of Comments",
             tags$p("But we also find an interesting relationship that there is a strong positive relationship between the number of comments and the like ratio. This means that, Youtube users are more likely to leave a comment under the video that they enjoy. For the video that they dislike, they would prefer to click a dislike rather than point it out. "),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("xAxis", "Choose X-axis:",
                              choices = c("Like Ratio", "Dislike Ratio"),
                              selected = "Like Ratio")
               ),
               mainPanel(
                 plotOutput("scatterPlot")
               )
             )
    )
  )
  ),
  
  tabPanel("Summary",
        fluidPage(
           titlePanel("Public Interest based on viewers' interest in video content on the Internet"),
           tabsetPanel(
             tabPanel("TAKEAWAYS",
                      tags$p(style = "font-size: 18px; color:black; font-weight: bold;", "In conclusion, we find many interesting features of Youtube."),
                      tags$p(style = "font-size: 14px; color:black;","First, the viewers on Youtube are likely to choose content that is easy to understand (such as sports or entertainment) or relates to daily life (such as science)."),
                      tags$p(style = "font-size: 14px; color:black;", "Second, we found that the transform rate of the channel is related to the uniqueness of the content and the cost of producing it."),
                      tags$p(style = "font-size: 14px; color:black;", "Finally, we also discovered that, when facing a controversial topic, Youtube users tend to click a dislike button as the response."),
                      tags$p(style = "font-size: 18px; color:black; font-weight: bold;", "However, there are also many constraints to this project."), 
                      tags$p(style = "font-size: 14px; color:black;",  "First, it failed to deal with the outliers, including the Music and Movie categories of video. They are a part of the Youtube videos but have a totally different cultural value to consume. A person may watch a movie twice, but not a video about the game news."),
                      tags$p(style = "font-size: 14px; color:black;", "Second, the date of data is outdated. The one about the subscriber came from 2018, while the one about the video came from 2017. In the past 5 years, new video platforms like Tiktok also started to appear as new media. Many people tend to spend their time on short-video platforms on phones, and long-video platforms on PC or iPad. We failed to examine how would this change the favor of users."),
                      tags$p(style = "font-size: 14px; color:black;",  "Third, all these data only considered the habit of the English-speaking community. There are also other regions in which Youtube is popular, such as Japan, German, and East Asian countries. The conclusion we make has a limited meaning. "),
             ),
             tabPanel("ABOUT",
                      h4("Authors"),
                      tags$p("Kaibo Wang: kwang37@uw.edu"),
                      tags$p("Ziyi Li: ziyil9@uw.edu"),
                      h4("Acknowledgements"),
                      tags$p("We would like to say thank you to our TA, the instructor and everyone on our team. In addition, we appreciate the source from Kaggle."),
             )
                      
           )
         )
  )
)


server <- function(input, output) {
  
  output$pieChart <- renderPlotly({
    plot_ly(data = views_by_category, labels = views_by_category$category, values = views_by_category$ratio, type = "pie")
  })
  
  output$interactivePlot <- renderPlotly({
    transform_plot <- ggplot(data = category_avg, aes(x = Transform, y = Category, color = Category, text = paste("Category: ", Category))) +
      geom_point() +
      ylab("Category of the video") +
      xlab("Rate of transform")
    
    if (input$transformInput > 0) {
      transform_plot <- transform_plot + xlim(0, input$transformInput)
    }
    
    interactive_plot <- plotly::ggplotly(transform_plot, tooltip = "text")
    
    interactive_plot
  })
  
  output$barPlot <- renderPlotly({
    
    ratio_p <- ggplot(df_merged_1, aes(fill = type, y = category, x = ratio)) +
      geom_bar(position = 'stack', stat = 'identity') +
      scale_fill_manual(values = c("green2", "tomato"))
    
    ratio_p <- ggplotly(ratio_p)
    
    ratio_p
  })
  
  observeEvent(event_data("plotly_click", source = "barPlot"), {
    click_data <- event_data("plotly_click", source = "barPlot")
    if (!is.null(click_data)) {
      clicked_point <- df_merged_1[df_merged_1$category == click_data$x, ]
      showModal(modalDialog(
        title = "Category Information",
        paste("Category:", clicked_point$category),
        paste("Ratio:", clicked_point$ratio)
      ))
    }
  })
  
  output$scatterPlot <- renderPlot({
    x_input <- ifelse(input$xAxis == "Like Ratio", "like_ratio", "dislike_ratio")
    
    p <- ggplot(data = category_df, aes(x = !!sym(x_input), y = comment_count, text = category)) +
      geom_point(aes(color = category)) +
      labs(x = input$xAxis, y = "Number of Comments") +
      geom_hline(yintercept = mean(category_df$comment_count), color = "red2")
    
    print(p)
  })
  
}

shinyApp(ui = ui, server = server)
