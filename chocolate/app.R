library(shiny)
library(tidyverse)
library("tidytuesdayR")
library(dplyr)
chocolates <- read_csv("~/Downloads/chocolate.csv")

ui <- fluidPage(
  titlePanel("Average Rating Versus Cocoa Percent"),
  
  sidebarPanel(
    selectInput("var", "Number of Ingredients",
                choices = c("1", "2", "3", "4", "5", "6")
                )
  )
  )
mainPanel(
  plotOutput("chocplot")
)


  server <- function(input, output) {
    newchocolates <- reactive({
      chocolates |>
        group_by(ingredients) |>
        mutate(ingredients = str_extract(ingredients, "\\d")) |>
        filter(HeartDisease %in% input$var)
    })
    
    output$chocplot <- renderPlot({
      ggplot(data = newchocolates(), aes(x = rating)) +
        geom_histogram()
    }
    )
  }
  
  
#create Shiny app
shinyApp(ui, server)





