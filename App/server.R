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
  
  dataset_modes_week <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df = read.xlsx(input$file1$datapath)
    df$Weekday <- as.factor(weekdays(as.Date(df$Time, origin="1899-12-30")))
    df$TypeFactor <- as.factor(df$Type)
    df$UserFactor <- as.factor(df$User)
    df$NewTime <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
    df$DayIntervals <- discretize(df$Time - floor(df$Time), method = "fixed", categories = c(0, 0.25, 0.5, 0.75, 1), labels = c("Nuit", "Matin", "Apres midi", "Soir"))
    
    df_by_user = df[df$User == input$user_id,]
    
    week_numbers <- c()
    
    for (i in 1:nrow(df_by_user)) {
      week_number <- as.numeric(floor(difftime(df_by_user$NewTime[i], df_by_user$NewTime[1], units = 'weeks')))+1
      week_numbers <- c(week_numbers, week_number)
    }
    df_by_user$WeekNumber <- week_numbers
    output$WeekSelector <- renderUI({
      sliderInput("week_number", "Week:",
                  min = 1, max = max(week_numbers),
                  value = input$week_number)
    })
    
    df_by_user_by_week <- df_by_user[df_by_user$WeekNumber == input$week_number,]
    
    df_by_user_by_week$Weekday <- factor(df_by_user_by_week$Weekday, levels= c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    df_by_user_by_week = df_by_user_by_week[order(df_by_user_by_week$Weekday),]
    
    df_by_user_by_week$TypeFactor = factor(df_by_user_by_week$TypeFactor,levels=c("Observation week", "Auto skipped", "Cheated", "Friend",  "On time", "Skipped", "Snoozed"))
    
    print(df_by_user_by_week$TypeFactor)
    output$ModeSelector <- renderUI({
      selectInput("mode_type", "Mode:",
                  levels(df_by_user_by_week$TypeFactor), selected = input$mode_type)
    })
    print(input$mode_type)
    
    df_by_user_by_week_by_mode <- df_by_user_by_week[df_by_user_by_week$TypeFactor == input$mode_type,]
    
  })
  
  dataset_by_user <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df = read.xlsx(input$file1$datapath)
    df$Weekday <- as.factor(weekdays(as.Date(df$Time, origin="1899-12-30")))
    df$TypeFactor <- as.factor(df$Type)
    df$UserFactor <- as.factor(df$User)
    df$NewTime <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
    df$DayIntervals <- discretize(df$Time - floor(df$Time), method = "fixed", categories = c(0, 0.25, 0.5, 0.75, 1), labels = c("Nuit", "Matin", "Apres midi", "Soir"))
    
    df_by_user = df[df$User == input$user_id,]
    
  })
  
  
  output$modes_week <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    
    
    if(is.null(dataset_modes_week())) return(NULL)
    barplot(table(dataset_modes_week()$Weekday), main = "Histogram of modes over a week")
    
  })
  
  # output$last_seven <- renderPlot({
  #   
  #   df = read.xlsx(input$file1$datapath)
  #   df$Type <- as.factor(df$Type)
  #   df$User <- as.factor(df$User)
  #   df$Time <- as.POSIXct(df$Time*(60*60*24), origin="1899-12-30", tz="GMT")
  #   
  #   
  #   df_without_hour=df
  #   df_without_hour$Time <- as.Date(df_without_hour$Time) #Removing the hour
  #   if (!exists("last_seven")){
  #     last_seven = df[FALSE,]
  #   
  #     for (i in 1 : nrow(df) ){  #Take the last 7 days
  #       if (as.numeric(difftime(max(df_without_hour$Time) ,df_without_hour$Time[i] ,
  #                             units = c("days"))) < 6.999 ){
  #         last_seven=rbind(last_seven,df_without_hour[i,])
  #       }
  #     }
  #   }
  #   last_seven__by_user = last_seven[last_seven$User == input$user_id,]
  #   seven= data.frame(table(last_seven__by_user$Time)) #Count the freq for each date
  #   seven
  #   somme=sum(seven$Freq)#Count the total number of cigarettes smoked the last seven days
  #   plot(seven,main=paste("Total number of smoked cigarettes during the last week: ",somme ))
  #   
  # })
  # 
  output$summary <- renderPrint({

    info=dataset_by_user()
    summary(info$Type)
    #print(typeof(summary(df$Type)))
    #stat.mode <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
    #stat.mode(ppppp$Type)

  })
  output$total <- renderPrint({

    info=dataset_by_user()
    total=nrow(info)
    cat("Total number of cigarettes for all modes : ", total)

  })

  output$info <- renderPrint({

    res=dataset_by_user()

    stat.mode <- function(x) {cbind(freq = table(x), percentage = prop.table(table(x))*100)}
    stat.mode(res$Type)

  })
  
})