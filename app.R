library(shiny)
library(shinythemes)
library(highcharter)
library(gsheet)
library(tidyverse)
library(tidytext)
library(shinydashboard)
library(quantmod)
library(lubridate)



ui <- tagList(
  
  navbarPage(
    theme = shinytheme("cerulean"),
    "Tweet stream ",
    tabPanel("Trending Hashtags/Bigrams/Topics",
             
             tags$p("This app analyzes a live stream of tweets with the following hashtags:", 
                    HTML(paste0(tags$a(href = "https://twitter.com/search?q=%23smfm&src=typed_query", "#smfm"), ", ",
                                tags$a(href = "https://twitter.com/search?q=%23mysmfm&src=typed_query", "#mysmfm"), ", ",
                                tags$a(href = "https://twitter.com/search?q=%23smfm20&src=typed_query", "#smfm20"), ".")
                    ),
                    
                    br(),
                    "Click below to refresh the live stream - new tweets are pulled every hour!", 
                    br(),
                    "Of note, since the conference ended, time has been frozen at 2/8/2020 14:00 CST"),
             
             actionButton(inputId='refresh', label="Refresh tweets", 
                          icon = icon("redo"), 
                          onclick ="window.open('https://eric-bergh.shinyapps.io/smfmtweets/', '_self')"),
             
             br(),
             br(),
             
             sidebarPanel(
               
               sliderInput("choice", 
                           tags$p("Tell me the n'th most popular:", br(),
                                  "bigrams, topics, or hashtags"), 
                           min = 1, max = 20, value = 12, step = 1),
               
               sliderInput("timeperiod", 
                           "Limit twitter stream to last x# hours:", 
                           min = 0, max = 40, value = 5, step =2 )
               
             ),
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Trending bigrams",
                          highchartOutput("bigram_plot", height = "500px"),
                          h5("Excludes some of the most common hashtags: #smfm, #mysmfm, #smfm20, #smfm2020...")),
                 
                 tabPanel("Trending topics",
                          highchartOutput("word_plot", height = "500px"), 
                          h5("Excludes some of the most common hashtags/words: #smfm, #mysmfm, #smfm20, #smfm2020...")),
                 
                 tabPanel("Trending hashtags", 
                          highchartOutput("hash_plot", height = "500px"),
                          h5("Excludes some of the most common hashtags: #smfm, #mysmfm, #smfm20, #smfm2020..."))
                 
                 
                 
                 
                 
                 
               )
             )
    ),
    
    tabPanel("Total #smfm20 Tweets",
             
             mainPanel(
               
               highchartOutput("series_plot")
               
             )
    ),
    
    
    tabPanel("Retweets / Favorites",
             
             sidebarPanel(
               
               sliderInput("retweet_choice", 
                           tags$p("Tell me the n'th most retweet-ed/favorited tweets:"), 
                           min = 1, max = 25, value = 15, step = 3),
               
               sliderInput("timechoice",
                           tags$p("Select how many hours you would like to look in past"),
                           min = 0, max = 40, value = 10, step = 2)
               
             ),
             
             br(),
             br(),
             
             mainPanel(
               tabsetPanel(
                 
                 tabPanel("Retweets",
                          highchartOutput("retweets_plot", height = "500px")),
                 
                 tabPanel("Favorites",
                          highchartOutput("favorites_plot", height = "500px"))
                 
               )
               
             )
             
             
    ),
    
    tabPanel("About", 
             fluidRow(
               box(title = "About",
                   width = "6 col-lg-4",
                   #style = "border: 2.5px solid #35A6E7;",
                   status = "primary",
                   tags$p(HTML(paste0("This app was created as a personal project with R using the Shiny web framework. ", 
                                      "The code can be found ",
                                      tags$a(href = "https://github.com/berghe01/smfm_tweets/blob/master/app.R", "here. "),
                                      "If you enjoyed using the app, ",
                                      "have questions, ",
                                      "or you are interested in a future maternal-fetal/data science collaboration, ",
                                      "please don't hesitate to contact Eric Bergh at ",
                                      tags$a(href = "https://twitter.com/ericberghMD", "@ericberghMD"), ".",
                                      br(),
                                      br(),
                                      "Of note, all tweets were collected with the Google Sheet add-on",
                                      tags$a(href = "https://gsuite.google.com/marketplace/app/tweet_archiver/976886281542", " Twitter-Archiver. "),
                                      "Tweets were limited to individuals using the following services: Twitter for Android, Twitter for iPad, Twitter for iPhone, TweetDeck and Twitter Web App.",
                                      br(),
                                      br(),
                                      "The following packages were used extensively in the creation of this app: ",
                                      tags$a(href = "https://shiny.rstudio.com/", "{shiny},"), 
                                      tags$a(href = "https://www.tidyverse.org/", " {tidyverse},"), 
                                      tags$a(href = "https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html", " {tidytext},"), 
                                      tags$a(href = "http://jkunst.com/highcharter/", " {highcharter}, "), "and ", 
                                      tags$a(href = "https://cran.r-project.org/web/packages/gsheet/index.html", " {gsheet}"), ".",
                                      " Special thanks to ",
                                      tags$a(href = "https://garrickadenbuie.com", "Garrick Aden-Buie "),
                                      "for the inspiration and for posting the code to his similar app ",
                                      tags$a(href = "https://github.com/gadenbuie/tweet-conf-dash", "here"), ".")
                   )
                   )
               )
             )
    )
  )
)




server <- function(input, output, session) {
  
  fileData <- reactiveFileReader(30*1000, session, 
                                 "https://docs.google.com/spreadsheets/d/12rEuQXEcfL6O7izST07RayD_Xotk8ixrP0ScvPTmJyQ/edit?usp=sharing",
                                 gsheet2tbl)
  
  
  output$data <- renderTable({
    fileData()
  })
  
  # output$data <- renderTable({
  #   oldData()
  # })
  
  
  # word plot ----
  
  output$word_plot <- renderHighchart({
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    
    choice <- input$choice 
    timeperiod <- input$timeperiod
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    mystop <- c("smfm", "mysmfm", "smfm20", "smfm2020", "#smfm", "#mysmfm", "#smfm20", "#smfm2020", 
                "rt", "md", "dr", "@mysmfm", "forum", "booth")
    '%ni%' <- Negate('%in%')
    
    
    df_words <- df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(6*60))
    
    df_words <- df_words %>% 
      mutate(test = ymd_hm("2020-02-08 14:00 CST") - Date, 
             test = as.numeric(test)) %>%
      filter(test <= timeperiod*60) %>%
      unnest_tokens(word, `Tweet Text`, token = "tweets") %>%
      anti_join(stop_words, by = "word") %>% 
      filter(word %ni% mystop) %>%
      filter(!is.numeric(word)) %>%
      group_by(word) %>% 
      count(sort = TRUE) %>%
      head(choice)
    
    hchart(df_words, type = "bar", 
           hcaes(x = word, y = n, color = word), 
           tooltip = list(headerFormat = "",
                          pointFormat = "{point.word}: {point.n} instances")) %>%
      hc_title(text = paste("Top", choice, "most popular topics (words in tweets)"), margin = 5, align = "left") %>%
      hc_subtitle(text = paste("Collected in the last", timeperiod, "hour(s)"), 
                  style = list(fontWeight = "bold", 
                               fontSize = "14px", 
                               color = "#35A6E7")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(tickInterval = 1, min = min(df_words$n-2), max = max(df_words$n+1),
               showLastLabel = FALSE, 
               showFirstLabel = FALSE) 
    
    
  })
  
  # hashtags plot
  
  output$hash_plot <- renderHighchart({
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    choice <- input$choice 
    timeperiod <- input$timeperiod
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    mystop <- c("smfm", "mysmfm", "smfm20", "smfm2020", "#smfm", "#mysmfm", "#smfm20", "#smfm2020", 
                "rt", "md", "dr", "@mysmfm")
    '%ni%' <- Negate('%in%')
    
    
    df_hash <- df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(6*60))
    
    df_hash <- df_hash %>%
      mutate(test = ymd_hm("2020-02-08 14:00 CST") - Date, 
             test = as.numeric(test)) %>%
      filter(test <= timeperiod*60) %>%
      unnest_tokens(word, `Tweet Text`, token = "tweets") %>%
      anti_join(stop_words, by = "word") %>% 
      filter(word %ni% mystop) %>%
      filter(str_detect(word, '#\\w+')) %>%
      group_by(word) %>% 
      count(sort = TRUE) %>%
      head(choice)    
    
    hchart(df_hash, type = "bar", 
           hcaes(x = word, y = n, color = word), 
           tooltip = list(headerFormat = "",
                          pointFormat = "{point.word}: {point.n} instances")) %>%
      hc_title(text = paste("Top", choice, "most popular #hashtags"), margin = 5, align = "left") %>%
      hc_subtitle(text = paste("Collected in the last", timeperiod, "hour(s)"), 
                  style = list(fontWeight = "bold", 
                               fontSize = "14px", 
                               color = "#35A6E7")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(tickInterval = 1, min = min(df_hash$n-2), max = max(df_hash$n+1),
               showLastLabel = FALSE, 
               showFirstLabel = FALSE)
    
  })   
  
  
  # ngram plot ----
  
  output$bigram_plot <- renderHighchart({
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    choice <- input$choice 
    timeperiod <- input$timeperiod
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    mystop <- c("smfm", "mysmfm", "smfm20", "smfm2020", "#smfm", "#mysmfm", "#smfm20", "#smfm2020", 
                "rt", "md", "dr", "mfm", "@mysmfm")
    '%ni%' <- Negate('%in%')
    
    
    df_bigram <- df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(6*60))
    
    # help from this site: https://www.r-bloggers.com/how-to-create-unigrams-bigrams-and-n-grams-of-app-reviews/
    
    df_bigram <- df_bigram %>%
      mutate(test = ymd_hm("2020-02-08 14:00 CST") - Date, 
             test = as.numeric(test)) %>%
      filter(test <= timeperiod*60) %>%
      unnest_tokens(hashes, `Tweet Text`, token = "ngrams", n = 2) %>% 
      separate(hashes, into = c("first","second"), sep = " ", remove = FALSE) %>% 
      anti_join(stop_words, by = c("first" = "word")) %>%
      filter(first %ni% mystop) %>%
      anti_join(stop_words, by = c("second" = "word")) %>%
      filter(second %ni% mystop) %>%
      filter(str_detect(first, "[a-z]") &
               str_detect(second, "[a-z]")) %>%
      group_by(hashes) %>%
      filter(hashes != "featured voices") %>%
      count(sort = TRUE) %>%
      summarize(n = sum(n)) %>%
      arrange(desc(n)) %>%
      head(choice)
    
    hchart(df_bigram, type = "bar", 
           hcaes(x = hashes, y = n, color = hashes), 
           tooltip = list(headerFormat = "",
                          pointFormat = "{point.hashes}: {point.n} instances")) %>%
      hc_title(text = paste("Top", choice, "most popular bigrams (2-word pairs)"), margin = 5, align = "left") %>%
      hc_subtitle(text = paste("Collected in the last", timeperiod, "hour(s)"), 
                  style = list(fontWeight = "bold", 
                               fontSize = "14px", 
                               color = "#35A6E7")) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(tickInterval = 1, min = min(df_bigram$n-2), max = max(df_bigram$n+1),
               showLastLabel = FALSE, 
               showFirstLabel = FALSE)
    
  })
  
  output$series_plot <- renderHighchart({ # need to set the x axis to hour i think in the future
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    choice <- input$choice 
    timeperiod <- input$timeperiod
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    smfm_hashes <- c("#smfm20")
    
    
    series_df <- df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(60*6))
    
    
    series_df <- series_df %>%
      mutate(Date = floor_date(Date, unit = "hour")) %>% 
      filter(Date < ymd_hm("2020-02-08 14:00 CST")) %>%
      unnest_tokens(hashes, `Tweet Text`, token = "tweets") %>% 
      filter(hashes %in% smfm_hashes) %>% # select only hases of importance
      group_by(Date, hashes) %>% 
      count() %>%
      group_by(Date) %>%
      pivot_wider(names_from = hashes, values_from = n) %>%
      column_to_rownames("Date") %>%
      as.xts()
    
    
    highchart(type = "stock") %>%
      hc_add_series(series_df[,1], name = "#smfm20") %>%
      hc_rangeSelector(buttons = list(
        list(type = 'all', text = 'All'),
        list(type = 'day', count = 3, text = '3d'),
        list(type = 'day', count = 1, text = '1d'),
        list(type = 'hour', count = 6, text = '6h'),
        list(type = 'hour', count = 1, text = '1h'),
        list(type = 'minute', count = 30, text = '30min') 
      )) %>%
      hc_title(text = paste("Total tweet counts for #smfm20"), 
               margin = 5, 
               align = "left", 
               style = list(fontWeight = "bold", 
                            fontSize = "17px", 
                            color = "#35A6E7")) %>%
      hc_subtitle(text = paste("Includes only original tweets"), 
                  margin = 5, 
                  align = "left", 
                  style = list(fontSize = "14px", 
                               color = "#35A6E7"))
    
  })
  
  
  
  output$retweets_plot <- renderHighchart({
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    
    retweet_choice <- input$retweet_choice
    timechoice <- input$timechoice
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    
    
    df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(6*60)) %>%
      mutate(test = ymd_hm("2020-02-08 14:00 CST") - Date, 
             test = as.numeric(test)) %>%
      filter(test <= timechoice*60) %>%
      select(Date, id = `Tweet ID`, screenname = `Screen Name`, text = `Tweet Text`, Retweets, 
             #URL
      ) %>%
      filter(Retweets >0) %>%
      arrange(desc(Retweets)) %>%
      head(retweet_choice) %>%
      hchart(type = "column", 
             hcaes(x = screenname, 
                   y = as.numeric(Retweets), 
                   color = as.numeric(Retweets))) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<b>{point.key}</b>",
        pointFormat = paste("<b> ({point.Retweets} retweets)<br><br></b>",
                            # "<p><img src = {point.URL}
                            # width= '40'; 
                            # height = '40'; 
                            # style = 'border-radius:50%;'
                            # align = 'left'/><br></p>",
                            "<p>{point.text}</p>")
      ) %>%
      hc_title(text = paste("Number of re-tweets over 1 hour at SMFM20"), margin = 5, align = "left") %>%
      hc_subtitle(text = paste("Hover / tap columns for tweet text"), 
                  style = list(fontWeight = "bold", 
                               fontSize = "15px", 
                               color = "#35A6E7")) %>%
      hc_yAxis(tickInterval = 1,
               showLastLabel = FALSE, 
               showFirstLabel = FALSE) 
    
  })
  
  
  output$favorites_plot <- renderHighchart({
    
    df <- fileData() 
    df[1,17] <- "geotag"
    names(df) <- df[1,]
    df <- df[-1,]
    
    # df2 <- oldData()
    # df2[1,17] <- "geotag"
    # names(df2) <- df2[1,]
    # df2 <- df2[-1,]
    
    #df <- rbind(df, df2)
    
    retweet_choice <- input$retweet_choice
    timechoice <- input$timechoice
    sources <- c("Twitter for Android", "Twitter for iPad", "Twitter for iPhone", "TweetDeck", "Twitter Web App") 
    
    df %>% 
      filter(App %in% sources) %>%
      mutate(Date = dmy_hm(Date, tz = "America/Chicago")-(6*60)) %>%
      mutate(test = ymd_hm("2020-02-08 14:00 CST") - Date, 
             test = as.numeric(test)) %>%
      filter(test <= timechoice*60) %>%
      select(Date, id = `Tweet ID`, screenname = `Screen Name`, text = `Tweet Text`, Favorites, 
             #URL
      ) %>%
      filter(Favorites >0) %>%
      arrange(desc(Favorites)) %>%
      head(retweet_choice) %>%
      hchart(type = "column", 
             hcaes(x = screenname, 
                   y = as.numeric(Favorites), 
                   color = as.numeric(Favorites))) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<b>{point.key}</b>",
        pointFormat = paste("<b> ({point.Favorites} favorites)<br><br></b>",
                            # "<p><img src = {point.URL}
                            # width= '40'; 
                            # height = '40'; 
                            # style = 'border-radius:50%;'
                            # align = 'left'/><br></p>",
                            "<p>{point.text}</p>")
      ) %>%
      hc_title(text = paste("Number of favorites over 1 hour at SMFM20"), margin = 5, align = "left") %>%
      hc_subtitle(text = paste("Hover / tap columns for tweet text!"), 
                  style = list(fontWeight = "bold", 
                               fontSize = "15px", 
                               color = "#35A6E7")) %>%
      hc_yAxis(tickInterval = 1,
               showLastLabel = FALSE, 
               showFirstLabel = FALSE) 
    
  })
  
  
  
}


shinyApp(ui, server)
