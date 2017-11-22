#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Mining in R - My first app on Shiny"),
  
  headerPanel(
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv", ".xlsx")),
    
    # Horizontal line ----
    tags$hr()
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(position="left",
    sidebarPanel(
      selectizeInput('user_id', 'User ID', choices = 1:32),width=10
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel( 
      tabPanel("Modes over a week",plotOutput("modes_week"), plotOutput("modes_lines"),
               uiOutput("WeekSelector"), uiOutput("ModeSelector")),
      tabPanel("Smoking density intervals in a weel",plotOutput("intervals"), uiOutput("IntervalSelector")),
      tabPanel("Modes density by user",tableOutput("modes_density")),
      # tabPanel("Last week",plotOutput("last_seven")),
      tabPanel("Info about types",verbatimTextOutput("info"),
                      verbatimTextOutput("total"),
      verbatimTextOutput("summary"),style='width: 900px')
      )
      )
      
    )
  )
)
