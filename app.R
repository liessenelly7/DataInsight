
library(shiny)
library(tidyverse)

donnees <- read_csv("cancer_propre.csv")
liste_pays <- sort(unique(donnees$pays))

ui <- fluidPage(
  titlePanel("DataInsight"),
  p("Analyse de l'incidence des cancers dans le monde."),
  
  tabsetPanel(
    tabPanel("Classement",
             selectInput("sexe", "Choisir un groupe :",
                         choices = c("Les deux", "Hommes", "Femmes")),
             plotOutput("graphique"),
             h4("Les 10 pays avec le taux le plus élevé"),
             tableOutput("tableau")
    ),
    tabPanel("Un pays",
             selectInput("pays", "Choisir un pays :",
                         choices = liste_pays, selected = "France"),
             plotOutput("graphique_pays"),
             tableOutput("tableau_pays")
    )
  )
)

server <- function(input, output) {
  
  output$graphique <- renderPlot({
    donnees |>
      filter(sexe == input$sexe) |>
      arrange(desc(taux)) |>
      head(10) |>
      ggplot(aes(x = reorder(pays, taux), y = taux)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Top 10 des pays :", input$sexe),
        x = "Pays",
        y = "Taux pour 100 000 habitants (standardisé sur l'âge)"
      )
  })
  
  output$tableau <- renderTable({
    donnees |>
      filter(sexe == input$sexe) |>
      arrange(desc(taux)) |>
      head(10) |>
      mutate(cas = format(cas, big.mark = " ", scientific = FALSE, trim = TRUE))
  }, digits = 1)
  
  output$graphique_pays <- renderPlot({
    donnees |>
      filter(pays == input$pays) |>
      mutate(sexe = factor(sexe, levels = c("Les deux", "Hommes", "Femmes"))) |>
      ggplot(aes(x = sexe, y = taux, fill = sexe)) +
      geom_col(show.legend = FALSE) +
      labs(
        title = paste("Taux de cancer :", input$pays),
        x = "Groupe",
        y = "Taux pour 100 000 habitants (standardisé sur l'âge)"
      )
  })
  
  output$tableau_pays <- renderTable({
    donnees |>
      filter(pays == input$pays) |>
      select(sexe, cas, taux) |>
      mutate(cas = format(cas, big.mark = " ", scientific = FALSE, trim = TRUE))
  }, digits = 1)
  
}

shinyApp(ui, server)