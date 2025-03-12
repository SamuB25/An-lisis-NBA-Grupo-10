# Carga de librerías
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)

# Lectura de base de datos
NBA <- read_excel("~/An-lisis-NBA-Grupo-10/NBA G1-G10.xlsx")

# Limpieza de nombres de columnas
names(NBA) <- make.names(names(NBA))

# Declaración de variables
NBA$PTS <- as.numeric(NBA$PTS)
NBA$FG. <- as.numeric(NBA$FG.)
NBA$X3P. <- as.numeric(NBA$X3P.)
NBA$FT. <- as.numeric(NBA$FT.)
NBA$AST. <- as.numeric(NBA$AST.)
NBA$DD2 <- as.numeric(NBA$DD2)
NBA$DD3 <- as.numeric(NBA$DD3)
NBA$PTS2PT. <- as.numeric(NBA$PTS2PT.)
NBA$PTS3PT. <- as.numeric(NBA$PTS3PT.)
NBA$PTSFT. <- as.numeric(NBA$PTSFT.)
NBA$Season <- as.character(NBA$Season)

# Datos de la Variable temporada
NBA <- NBA %>%
  mutate(Años_J = as.numeric(sub("-.*", "", Season)),
         intervalo = cut(Años_J, 
                         breaks = seq(1996, 2024, by = 4), 
                         labels = c("1996-2000", "2000-2004", "2004-2008", "2008-2012", "2012-2016", "2016-2020", "2020-2024"), 
                         include.lowest = TRUE))

# Creación de tablas para formación de gráficos
Promedio_PTS <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_PTS = sum(PTS, na.rm = TRUE))

Promedio_FG <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_FG = mean(FG., na.rm = TRUE))

Promedio_3P <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_3P = mean(X3P., na.rm = TRUE))

Promedio_FT <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_FT = mean(FT., na.rm = TRUE))

Promedio_AST <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_AST = mean(AST., na.rm = TRUE))

Promedio_DD2 <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_DD2 = sum(DD2, na.rm = TRUE))

Promedio_DD3 <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_DD3 = sum(DD3, na.rm = TRUE))

Promedio_PTS2PT <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_PTS2TP = mean(PTS2PT., na.rm = TRUE))

Promedio_PTS3PT <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_PTS3TP = mean(PTS3PT., na.rm = TRUE))

Promedio_PTSFT <- NBA %>%
  group_by(intervalo) %>%
  dplyr::summarize(total_PTSFT = mean(PTSFT., na.rm = TRUE))

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Análisis del rendimiento en la NBA"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos principales", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Filtros", tabName = "filtros", icon = icon("filter"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Pestaña de gráficos
      tabItem(tabName = "graficos",
              fluidRow(
                box(
                  title = "Gráfico seleccionado",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("grafico_seleccionado", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Descripción",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  textOutput("descripcion_grafico")
                )
              )
      ),
      
      # Pestaña de filtros
      tabItem(tabName = "filtros",
              fluidRow(
                box(
                  title = "Filtros",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  sliderInput("intervalo_tiempo", "Selecciona el intervalo de tiempo:",
                              min = 1996, max = 2024, value = c(1996, 2024)),
                  selectInput("grafico", "Selecciona un gráfico:",
                              choices = c("Distribución de Puntos",
                                          "Asistencias por Temporada",
                                          "Porcentaje de Tiros de Campo",
                                          "Porcentaje de Triples",
                                          "Porcentaje de Tiros Libres",
                                          "Dobles-Dobles y Dobles-Triples"))
                )
              )
      )
    )
  )
)

# Definir el servidor (Server)
server <- function(input, output) {
  # Filtrar datos según el intervalo de tiempo seleccionado
  NBA_filtrado <- reactive({
    NBA %>%
      filter(Años_J >= input$intervalo_tiempo[1] & Años_J <= input$intervalo_tiempo[2])
  })
  
  # Texto descriptivo del gráfico
  output$descripcion_grafico <- renderText({
    if (input$grafico == "Distribución de Puntos") {
      "Este gráfico muestra la distribución de puntos (PTS) anotados en los intervalos de tiempo seleccionados."
    } else if (input$grafico == "Asistencias por Temporada") {
      "Este gráfico muestra la distribución del porcentaje de asistencias (AST%) por intervalo de temporada."
    } else if (input$grafico == "Porcentaje de Tiros de Campo") {
      "Este gráfico muestra el porcentaje de tiros de campo (FG%) anotados por intervalo de temporada."
    } else if (input$grafico == "Porcentaje de Triples") {
      "Este gráfico muestra el porcentaje de triples (3P%) anotados por intervalo de temporada."
    } else if (input$grafico == "Porcentaje de Tiros Libres") {
      "Este gráfico muestra el porcentaje de tiros libres (FT%) anotados por intervalo de temporada."
    } else if (input$grafico == "Dobles-Dobles y Dobles-Triples") {
      "Este gráfico muestra la cantidad de dobles-dobles (DD2) y dobles-triples (DD3) por intervalo de temporada."
    }
  })
  
  # Gráfico seleccionado
  output$grafico_seleccionado <- renderPlotly({
    datos_filtrados <- NBA_filtrado()
    
    if (input$grafico == "Distribución de Puntos") {
      p <- ggplot(datos_filtrados, aes(x = PTS)) +
        geom_histogram(binwidth = 50, fill = "darkblue", color = "black") +
        labs(title = "Distribución de Puntos", x = "Puntos (PTS)", y = "Frecuencia") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    } else if (input$grafico == "Asistencias por Temporada") {
      p <- ggplot(datos_filtrados, aes(x = intervalo, y = AST., fill = intervalo)) +
        geom_boxplot(color = "darkblue") +
        labs(title = "Distribución de Asistencias por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Porcentaje de Asistencias") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold")) +
        scale_fill_brewer(palette = "Set3")
    } else if (input$grafico == "Porcentaje de Tiros de Campo") {
      p <- ggplot(datos_filtrados, aes(x = intervalo, y = FG.)) +
        geom_bar(stat = "identity", color = "darkblue") +
        labs(title = "Distribución de Porcentaje de Tiros de Campo por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Porcentaje de Tiros de Campo") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    } else if (input$grafico == "Porcentaje de Triples") {
      p <- ggplot(Promedio_3P, aes(x = intervalo, y = total_3P, color = intervalo)) +
        geom_point(size = 3) +
        labs(title = "Distribución de Porcentaje de Triples por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Porcentaje de Triples") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    } else if (input$grafico == "Porcentaje de Tiros Libres") {
      p <- ggplot(Promedio_FT, aes(x = intervalo, y = total_FT, color = intervalo)) +
        geom_point(size = 3) +
        labs(title = "Distribución de Porcentaje de Tiros Libres por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Porcentaje de Tiros Libres") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
    } else if (input$grafico == "Dobles-Dobles y Dobles-Triples") {
      p1 <- ggplot(Promedio_DD2, aes(x = intervalo, y = total_DD2, fill = intervalo)) +
        geom_bar(stat = "identity", color = "darkblue") +
        labs(title = "Dobles-Dobles por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Total de Dobles-Dobles") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
      
      p2 <- ggplot(Promedio_DD3, aes(x = intervalo, y = total_DD3, fill = intervalo)) +
        geom_bar(stat = "identity", color = "darkblue") +
        labs(title = "Dobles-Triples por Intervalo de Temporada", 
             x = "Intervalo de Temporada", y = "Total de Dobles-Triples") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, face = "bold"))
      
      p <- gridExtra::grid.arrange(p1, p2, ncol = 1)
    }
    
    ggplotly(p)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)