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
library(lubridate) 
library(ggplot2)
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
    
    output$ModeSelector <- renderUI({
      selectInput("mode_type", "Mode:",
                  levels(df_by_user_by_week$TypeFactor), selected = input$mode_type)
    })
    
    
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
  
  dataset_intervals_week <- reactive({
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
    df_by_user$Weekday <- factor(df_by_user$Weekday, levels= c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    df_by_user = df_by_user[order(df_by_user$Weekday),]
    
    output$IntervalSelector <- renderUI({
      selectInput("interval", "Interval:",
                  levels(df_by_user$DayIntervals), selected = input$interval)
    })
    
    
    df_by_user_by_interval <- df_by_user[df_by_user$DayIntervals == input$interval,]
    
  })
  
  
  output$modes_week <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    
    
    if(is.null(dataset_modes_week())) return(NULL)
    barplot(table(dataset_modes_week()$Weekday), main = "Histogram of modes over a week")
    
  })
  
  output$modes_lines <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    
    
    
    if(is.null(dataset_modes_week())) return(NULL)
    plot(table(dataset_modes_week()$Weekday), main = "Plot of modes over a week", type = 'l')
    
  })
  
  output$modes_density <- renderTable({
    #Day_intervals <- count(dataset_by_user()$DayIntervals)
    mode_density <- table(dataset_by_user()$TypeFactor, dataset_by_user()$DayIntervals)
  }
  )
  
  output$intervals <- renderPlot({

    if(is.null(dataset_intervals_week())) return(NULL)
    barplot(table(dataset_intervals_week()$Weekday), main = "Histogram of smokings intervals", type = 'l')
    
  })
  
  output$last_seven <- renderPlot({
    
    max_date_per_user=max(dataset_by_user()$NewTime)
    weekdays_user=data.frame("days"=c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche"))
    seven_seven = sum(dataset_by_user()$NewTime>=max_date_per_user-days(7))
    seven_days <- c()
    for(i in seq(1, 7)){   #mettre l'id user ici
      seven_days[i]=sum(dataset_by_user()$NewTime>=max_date_per_user-days(7) & format(dataset_by_user()$NewTime,"%A")==weekdays_user$days[i])
    }
    
    weekdays_user$nbre_cig=seven_days
    weekdays_user$days <- factor(weekdays_user$days, levels= c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"))
    weekdays_user = weekdays_user[order(weekdays_user$days),]
    
    plot= qplot(weekdays_user$days,weekdays_user$nbre_cig, geom="line", group=1)
    plot = plot + labs(x="weekday") + labs(y="number of cigarettes")
    print(plot)
  })
  
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