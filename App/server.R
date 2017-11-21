#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(plyr)
library(arules)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    df = read.xlsx(input$file1$datapath)
    df$Weekday <- as.factor(weekdays(as.Date(df$Time, origin="1899-12-30")))
    df$TypeFactor <- as.factor(df$Type)
    df$UserFactor <- as.factor(df$User)
    df$NewTime <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
    df$DayIntervals <- discretize(df$Time - floor(df$Time), method = "fixed", categories = c(0, 0.25, 0.5, 0.75, 1), labels = c("Nuit", "Matin", "Apres midi", "Soir"))
    
    df_by_user = df[df$User == input$user_id,]
    barplot(table(df_by_user$Weekday), main = "Histogram of modes over a week")
    #print(summary(df_by_user))
    
  })
  
})
