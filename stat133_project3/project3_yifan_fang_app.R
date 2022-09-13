# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 3: “State of the Union” Text Analysis
# Description: Build a shiny app to visualize the results from a text analysis performed on the State of the Union messages given by various presidents of the U.S. from 2001 to 2022.
# Author: Yifan Fang
# Date: 4/29/2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)


# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "u2-lyrics.csv")
dat <- read.csv('state-union-2001-2022.csv')

# ===============================================
# Tokenization and Define Functions
# ===============================================
dat_tokens = dat %>% unnest_tokens(word, message)


# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel('"Annual Messages to Congress on the State of the Union " Text Analysis By President'),
  fluidRow(
   
    # select top-n words
    column(3,
           # indicate it's used for both analysis 1 and 2
           p(em("for analysis 1 and 2")),
           # set the widget to slider
           sliderInput(inputId = "words_num",
                       label = "Top-n Words",
                       # set range from 1-100
                       min = 1,
                       max = 100,
                       # default = 10
                       value = 10)
    ),
    
    # select a specific president
    column(3,
           # indicate it's used for both analysis 1 and 2
           p(em("for analysis 1 and 2")),
           # set the widget to radio buttons
           radioButtons(inputId = "pres", 
                        label = "Select a President",
                        # set choices
                        choices = c("George W. Bush" = "George W. Bush",
                                    "Barack Obama" = "Barack Obama",
                                    "Donald J. Trump" = "Donald J. Trump",
                                    "Joseph R. Biden" = "Joseph R. Biden"),
                        # default is Biden
                        selected = "Joseph R. Biden")
    ),
    
    # choose with stopwords or not
    column(3,
           # indicate it's used for analysis 1
           p(em("for analysis 1")),
           # set the widget to selection
           selectInput(inputId = "stopword", 
                       label = "Remove Stopwords?",
                       # set choices
                       choices = c("Yes" = "Without Stopwords",
                                   "No" = "With Stopwords"),
                       # default is No stopwords
                       selected = "Without Stopwords")
    ),
    
    
    # choose positive or negative sentiment
    column(3,
           # indicate it's used for analysis 2
           p(em("for analysis 2")),
           # set the widget to selection
           selectInput(inputId = "senti_category",
                       label = "Which Sentiment Category?",
                       # set choices
                       choices = c("Positive" = "positive",
                                   "Negative" = "negative"),
                       # default is positive
                       selected = "positive")
    ),
    
    
  ),
  hr(),
  
  # set two tabs for the analysis
  tabsetPanel(type = "tabs",
              # tab 1 is for analysis 1
              tabPanel("Analysis1: Word Frequency Analysis of A Selected President",
                       h3("Word Frequency Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       h4('Summary Table'),
                       dataTableOutput('table1')),
              # tab 2 is for analysis 2
              tabPanel("Analysis2: Sentiment Analysis of A Selected President", 
                       h3("Sentiment Analysis of A Selected President"),
                       plotOutput("plot2"),
                       hr(),
                       h4('Summary Table'),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # reaction function for analysis 1
  numeric1 <- reactive({
    
    # remove stopwords
    tidy_dat = dat_tokens %>% anti_join(stop_words, by = 'word')
    
    # find top-n most frequent words for the selected president
    if (input$stopword == 'Without Stopwords'){
      top = tidy_dat %>%
              filter(president == input$pres) %>% 
              count(word, sort = TRUE) %>% 
              slice_head(n = input$words_num)
    }else{
      top = dat_tokens %>% 
              filter(president == input$pres) %>% 
              count(word, sort = TRUE) %>% 
              slice_head(n = input$words_num)
    }
    
    top
    
  })
  
  # reaction function for analysis 2
  numeric2 <- reactive({
    
    #get sentiment 'positive' and 'negative'
    pos_or_neg = dat_tokens %>%
      filter(president == input$pres) %>%
      inner_join(get_sentiments('bing')) %>%
      count(word, sentiment, sort = TRUE) %>%
      filter(sentiment == input$senti_category) %>% 
      slice_head(n = input$words_num)
    
    pos_or_neg
    
  })
  
  
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # plot 2A: frequencies of the top-n words
  output$plot1 <- renderPlot({
  # bar plot
    ggplot(data = numeric1(), aes(x = n, y = reorder(word, n))) +
      geom_col(fill = 'dodgerblue') +
      theme_bw() +
      xlab('Number Appears') +
      ylab('Word') +
      labs(title = paste('Top' , input$words_num, 'Words Used By President', input$pres, input$stopword)) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    numeric1()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # plot 2B: top-n words contribute the most for the sentiment
  output$plot2 <- renderPlot({
  # bar plot
    ggplot(data = numeric2(), aes(x = n, y = reorder(word, n))) +
      geom_col(fill = 'dodgerblue') + 
      theme_bw() +
      xlab('Contribution Weight (Number Appears)') +
      ylab('Word') +
      labs(title = paste('Top', input$words_num , 'Words Contributed To The', toupper(input$senti_category), 'Categroy By President', input$pres)) +
      theme(plot.title = element_text(hjust = 0.5))
  }) 
    
  
  # code for statistics
  output$table2 <- renderDataTable({
    numeric2()
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

