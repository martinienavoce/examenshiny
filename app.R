

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)
library(plotly)


thematic::thematic_shiny(font = "auto") # pour les thèmes des graphiques

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  
  
  titlePanel("Exploration des Diamants"),
  
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "bouton_couleur",
        label = "Colorier les points en rose ?",
        choices = c("Oui", "Non")
      ),
      
      selectInput(
        inputId = "color",
        label = "Choisir une couleur à filtrer :",
        choices = c("D", "E", "F", "G", "H", "I", "J")
      ),
      
      sliderInput(inputId = "price",
                  label="Prix maximum :",
                  min = 300,
                  max = 20000,
                  value = 5000),
      
      actionButton(inputId = "bouton_graph",
                   label = "Visualiser le graph")
      
    ),
 
    
    mainPanel(
      plotlyOutput(outputId ="diamondsplot"),
      DTOutput(outputId ="tblo")
    )
  )
)


server <- function(input, output) {
  
  rv <- reactiveValues()
  observeEvent(input$bouton_graph, {
    rv$graph <- diamonds |>
      filter(color == input$color & price <= input$price)|>
      ggplot(aes(x = carat, y=price)) +
      geom_point(
        color = if (input$bouton_couleur == "Oui") "pink" else "black"
      ) +
      labs(
        title = paste("Prix :", input$price, "& Color :", input$color)
      )
    
    showNotification(
      paste("prix :", input$price, "& color :", input$color),
      type = "message"
      )
    
    rv$montableau <- diamonds |>
      filter(color == input$color & price <= input$price)
  })

  output$diamondsplot <- renderPlotly({
    rv$graph
  })
  
  output$tblo<-renderDT({
    rv$montableau
  })
}


shinyApp(ui = ui, server = server)
