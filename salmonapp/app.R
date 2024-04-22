#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readr)

salmon <- read_csv("salmondata/salmon.csv")


ui <- fluidPage(
  titlePanel("Salmon data in different areas of Alaska"),
  sidebarLayout(
    sidebarPanel(
    varSelectInput("xvar", "X variable", salmon, selected = "Year"),
    varSelectInput("yvar", "Y variable", salmon, selected = "num_fish"),
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(salmon$species), 
      selected = unique(salmon$species)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("by_species", "Show species", TRUE),
  ),
  mainPanel(
    plotOutput("distPlot")
  )
  ))


server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    salmon |> filter(species %in% input$species)
  })
  
  output$distPlot <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(fill = species),
      geom_histogram(stat = "identity")
    )
    
    
    p
  }, res = 100)
}

shinyApp(ui, server)
