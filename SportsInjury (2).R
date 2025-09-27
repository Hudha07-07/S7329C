# Load packages ----------------------------------------------------------------



library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)
library(DT)


# Load data --------------------------------------------------------------------



Measurements <- read.csv(file = "https://raw.githubusercontent.com/Hudha07-07/S7329C/refs/heads/main/sports_injury_detection_dataset.csv", header = TRUE, sep = ",")



# Define UI --------------------------------------------------------------------



ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "Injury Risk Score" = "Injury_Risk_Score",
          "Duration Minutes" = "Duration_Minutes",
          "Cumulative Fatigue Index" = "Cumulative_Fatigue_Index",
          "Blood Oxygen Level Percent" = "Blood_Oxygen_Level_Percent",
          "Respiratory Rate BPM" = "Respiratory_Rate_BPM",
          "Heart Rate BPM" = "Heart_Rate_BPM"
        ),
        selected = "Injury_Risk_Score"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "Injury Risk Score" = "Injury_Risk_Score",
          "Duration Minutes" = "Duration_Minutes",
          "Cumulative Fatigue Index" = "Cumulative_Fatigue_Index",
          "Blood Oxygen Level Percent" = "Blood_Oxygen_Level_Percent",
          "Respiratory Rate BPM" = "Respiratory_Rate_BPM",
          "Heart Rate BPM" = "Heart_Rate_BPM"
        ),
        selected = "Duration_Minutes"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Sport Type" = "Sport_Type",
          "Activity Type" = "Activity_Type",
          "Injury Occured" = "Injury_Occurred"
        ),
        selected = "Injury_Occurred"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = brushOpts(id = "plot_brush")),
      DT::dataTableOutput(outputId = "measurementstable"),
      textOutput(outputId = "avg_x"), # avg of x
      textOutput(outputId = "avg_y"), # avg of y
      verbatimTextOutput(outputId = "lmoutput") # regression output
    )
  )
)



# Define server ----------------------------------------------------------------



server <- function(input, output, session) {
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    })
  
  output$scatterplot <- renderPlot({
    ggplot(data = Measurements, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$measurementstable <- renderDataTable({
    brushedPoints(Measurements, brush = input$plot_brush) %>%
      select(Athlete_ID,Sport_Type,Session_Date,Heart_Rate_BPM,Respiratory_Rate_BPM,Skin_Temperature_C,Blood_Oxygen_Level_Percent,Impact_Force_Newtons,Cumulative_Fatigue_Index,Activity_Type,Duration_Minutes,Injury_Risk_Score,Injury_Occurred)
  })
  
  output$avg_x <- renderText({
    avg_x <- Measurements %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, "=", avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- Measurements %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })
  
  output$lmoutput <- renderPrint({
    x <- Measurements %>% pull(input$x)
    y <- Measurements %>% pull(input$y)
    print(summary(lm(y ~ x, data = Measurements)), digits = 3, signif.stars = FALSE)
  })
  
}



# Create the Shiny app object --------------------------------------------------



shinyApp(ui = ui, server = server)