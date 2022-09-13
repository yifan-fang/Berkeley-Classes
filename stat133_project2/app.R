# ===============================================
# Fill in the following fields
# ===============================================
# Title: Stat133-Project2-Spring2022
# Description: A savings rate calculator. 
## Inputs are Annual Income, Target Amount, Current Age, and Rate of Return. 
## Outputs are a savings-rates and number of years to reach a target amount relationship visualization, a visualizing of the "total contributions" and the "total growth", and a Table of Numeric Outputs.
# Author: Yifan Fang
# Date: 4/8/2022


# ===============================================
# Required packages
# ===============================================
library(shiny)
library(tidyverse)
library(reshape)

# ===============================================
# Define variables and functions used for application
# ===============================================

savings_rate = c(seq(5, 100, 5))

annual_c <- function(income = 50000){
  annual_contribution = income * savings_rate * 0.01
  return(annual_contribution)
}

num_year <- function(target = 1000000, return_rate = 7){
  log(return_rate * 0.01 * target / annual_c() + 1) / log(1 + return_rate * 0.01)
}

# ===============================================
# Define User-Interface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Savings Rate Calculater"),
  
  fluidRow(
      # Input(s) for annual-income
      column(3,
             numericInput(inputId = "income", 
                   label = "Annual Income",
                   value = 50000,
                   min = 0,
                   step = 1000)),

      
      # Input(s) for target-amount
      column(3,
             numericInput(inputId = "target", 
                   label = "Target Amount",
                   value = 1000000,
                   min = 0,
                   step = 50000)),
      
      # Input(s) for current-age
      column(3,
             selectInput(inputId = "age", 
                  label = "Current Age", 
                  choices = list(age = 18:100), 
                  selected = 25)),
      
      # Input(s) for rate-of-return
      column(3,
             numericInput(inputId = "return_rate", 
                   label = "Rate of Return (%)",
                   min = 0,
                   value = 5))
      
    ),
    
    mainPanel(
    
      #Goal 1, Visualizing the relationship between savings-rates and number of years to reach a target amount.
      hr(),
      h4('Number of Years to Reach a Target Amount'),
      plotOutput('plot1'),
    
     #Goal 2, Visualizing the "total contributions" and the "total growth" for various savings rates.
      hr(),
      h4('Total Contribution and Total Growth at Different Savings Rate'),
      plotOutput('plot2'),
      
      #Goal 3, Table of Numeric Outputs.
      hr(),
      h4('Numeric Summary Table'),
      DT::dataTableOutput('table')
    )
  
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  numeric_values <- reactive({
    #annual contribution
    annual_contribution <- input$income * savings_rate * 0.01
    
    #number of years needed to reach the target amount
    num_year <- log(input$return_rate * 0.01 * input$target / annual_contribution + 1) / log(1 + input$return_rate * 0.01)
    
    #age at target
    target_age <- as.numeric(input$age) + num_year
    
    #total contribution, how much the target amount comes from annual contributions 
    total_c <- annual_contribution * num_year
    
    #total growth, how much comes from the growth of the investments
    total_g <- input$target - total_c
    
    #percent contribution
    percent_c <- total_c / input$target
    
    #percent growth
    percent_g <- total_g / input$target
    
    tbl = data.frame(
      savings_rate = round(savings_rate, digits = 2),
      annual_contribution = annual_contribution,
      rate_of_return = input$return_rate,
      target_value = input$target,
      total_contribution = round(total_c, digits = 2),
      total_growth = round(total_g, digits = 2),
      percent_contribution = round(percent_c, digits = 4) * 100,
      percent_growth = round(percent_g, digits = 4) * 100,
      number_of_years = round(num_year, digits = 1),
      age_at_target = round(target_age, digits = 1)
    )
    tbl
  })
  

  # plot-1: relationship between savings-rates and number of years to reach a target amount
  output$plot1 <- renderPlot({
    # output a bar chart
    ggplot(data = numeric_values(), aes(x = factor(savings_rate), y = number_of_years)) +
      geom_col(fill = 'dodgerblue') +
      theme_bw() +
      xlab('savings rate (%)') +
      ylab('number of years') 
  })
  
  dat_plot2 <- reactive({
    #annual contribution
    annual_contribution <- annual_c(input$income)
    
    #number of years needed to reach the target amount
    num_year <- num_year(input$target, input$return_rate)
    
    #total contribution, how much the target amount comes from annual contributions 
    total_c <- annual_contribution * num_year
    
    #total growth, how much comes from the growth of the investments
    total_g <- input$target - total_c
    
    tbl = data.frame(
      savings_rate = savings_rate,
      total_contribution = total_c,
      total_growth = total_g
    )
    percent_comp = melt(tbl, id = 'savings_rate')
    percent_comp
  })
  
  # plot-2: "total contributions" and the "total growth" for various savings rates
  output$plot2 <- renderPlot({
    # output a bar chart
    
    ggplot(data = dat_plot2(), aes(x = factor(savings_rate), y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "fill")+
      theme_bw()+
      scale_y_continuous(name = 'percent composition', 
                         breaks = seq(0, 1, 0.1), 
                         labels=c('0', '10', '20', '30', '40', '50', '60', '70', '80', '90', '100')) +
      xlab('savings rate (%)')
  })

  # show data frame
  output$table <- DT::renderDataTable({
    numeric_values()
  })
  
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

