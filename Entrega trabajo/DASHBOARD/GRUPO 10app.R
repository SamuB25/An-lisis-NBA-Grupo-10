# Carga de librerías
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(readxl)) install.packages("readxl")
if (!require(plotly)) install.packages("plotly")
if (!require(plotly)) install.packages("DT")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)

# Lectura de base de datos
NBA <- read_excel("~/An-lisis-NBA-Grupo-10/NBA G1-G10.xlsx")

# Limpieza de nombres de columnas
names(NBA) <- make.names(names(NBA))

# Declaración de variables
NBA <- NBA %>%
  mutate(
    PTS = as.numeric(PTS),
    FG. = as.numeric(FG.),
    X3P. = as.numeric(X3P.),
    FT. = as.numeric(FT.),
    AST. = as.numeric(AST.),
    DD2 = as.numeric(DD2),
    DD3 = as.numeric(DD3),
    PTS2PT. = as.numeric(PTS2PT.),
    PTS3PT. = as.numeric(PTS3PT.),
    PTSFT. = as.numeric(PTSFT.),
    Season = as.character(Season),
    NBA <- NBA %>%
      mutate(Años_J = as.numeric(sub("-.*", "", Season)),
             intervalo = cut(Años_J, 
                             breaks = seq(1996, 2024, by = 4), 
                             labels = c("1996-2000", "2000-2004", "2004-2008", "2008-2012", "2012-2016", "2016-2020", "2020-2024"), 
                             include.lowest = TRUE))
  )

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Análisis NBA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos Seleccionados", tabName = "graficos", icon = icon("chart-bar")),
      # Agregar el sliderInput y selectInput directamente en el sidebarMenu
      sliderInput("intervalo_tiempo", "Selecciona el intervalo de tiempo:",
                  min = 1996, max = 2024, value = c(1996, 2024)),
      selectInput("grafico", "Selecciona un gráfico:",
                  choices = c("Distribución de Puntos por Temporada",
                              "Asistencias por Temporada",
                              "Porcentaje de Tiros de Campo",
                              "Porcentaje de Triples",
                              "Porcentaje de Tiros Libres",
                              "Dobles-Dobles y Dobles-Triples",
                              "Puntos por Tipo de Tiro"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      /* Fondo de color azul oscuro para toda la página */
      .content-wrapper, .right-side {
        background-color: #001f3f;  /* Azul oscuro */
        color: #ffffff;  /* Texto blanco */
        padding: 20px;   /* Espaciado interno */
      }
      
      /* Fondo de color para las cajas (boxes) */
      .box {
        background-color: #b0c4de;  /* Color azul claro */
        border-radius: 10px;        /* Bordes redondeados */
        padding: 15px;              /* Espaciado interno */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Sombra suave */
        color: #000000 !important; /* Texto en negro */
        font-weiht: bold /* En negrita */
      }
      
      /* Color del texto en las pestañas */
      .nav-tabs-custom .nav-tabs li.active a {
        background-color: #00284d;  /* Azul oscuro más claro */
        color: #d3d3d3;  /* Texto blanco */
      }
    ")),
    tabItems(
      tabItem(
        tabName = "inicio",
        h1(" Análisis estadistico por periodo intercuartilico de la NBA"),
        h2("Vamos a Analizar la eficencia de la NBA desde 1996 hasta 2024"),
        p("Esta aplicación tiene con la finalidad analizar y representar grafica-mente los datos recolectados de la NBA, como puntos, asistencias, porcentajes de tiros, Etc. Desde 1996 hasta el año 2024"),
        hr(),  # Línea horizontal
        h3("Integrantes:"),
        tags$ul(
          tags$li("Leonardo Mentado C.I: 29.850.786."),
          tags$li("Samuel Barreto C.I: 31.484.531.")
        ),
        h3("Instrucciones:"),
        tags$ol(
          tags$li("Selecciona la pestaña 'Gráficos Seleccionados' para ver los gráficos disponibles."),
          tags$li("Usa los filtros en la pestaña 'Filtros' para ajustar el intervalo de tiempo."),
          tags$li("Explora los gráficos y descarga los datos si es necesario.")
        ),
        plotlyOutput("grafico_inicio")  # Gráfico interactivo en el inicio
      ),
      tabItem(tabName = "graficos",
              fluidRow(
                box(
                  title = "Gráfico seleccionado",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("grafico_seleccionado", height = "700px")
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
              ),
              fluidRow(
                box(
                  title = "Tabla de Datos",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("tabla_datos")  # Tabla de datos
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
    switch(input$grafico,
           "Distribución de Puntos por Temporada" = "Este gráfico muestra la distribución de puntos (PTS) anotados por los jugadores en los intervalos cuatrienales por temporada, desde 1996 hasta 2024. Donde se puede vizualisar donde se encuentra la mayor tendencia de estos.",
           "Asistencias por Temporada" = "Este gráfico muestra la distribución del porcentaje de asistencias realizadas y encestadas (AST%) por intervalos cuatrienales de las temporadas, desde 1996 hasta 2024.",
           "Porcentaje de Tiros de Campo" = "Este gráfico muestra el porcentaje de tiros de campo realizados y encestados (FG%) por intervalos cuatrienales de las temporadas, desde 1996 hasta 2024.",
           "Porcentaje de Triples" = "Este gráfico muestra el porcentaje de triples (3P%) realizados y anotados por intervalos cuatrienales de las temporadas, desde 1996 hasta 2024.",
           "Porcentaje de Tiros Libres" = "Este gráfico muestra el porcentaje de tiros libres (FT%) realizados y anotados por intervalo cuatrienalaes de las temporadas, desde 1996 hasta 2024.",
           "Dobles-Dobles y Dobles-Triples" = "Este gráfico muestra la cantidad de dobles-dobles (DD2) y dobles-triples (DD3) que han realizado todos los jugadores por intervalos cuatrienales de las temporadas desde 1996 hasta 2024.",
           "Puntos por Tipo de Tiro" = "Este gráfico muestra la distribución de puntos por dobles, triples y tiros libres realizados por intervalos cuatrienales de las temporadas desde 1996 hasta 2024."
    )
  })
  
  # Gráfico seleccionado
  output$grafico_seleccionado <- renderPlotly({
    datos_filtrados <- NBA_filtrado()
    
    p <- switch(input$grafico,
                "Distribución de Puntos por Temporada" = {
                  ggplot(datos_filtrados, aes(x = PTS)) +
                    geom_histogram(binwidth = 50, fill = "darkblue", color = "black") +
                    labs(title = "Distribución de Puntos", x = "Puntos (PTS)", y = "Frecuencia") +
                    theme_minimal()
                },
                "Asistencias por Temporada" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = AST., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Asistencias por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Asistencias") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3")
                },
                "Porcentaje de Tiros de Campo" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = FG., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Tiros de Campo por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Tiros de Campo") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3")
                },
                "Porcentaje de Triples" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = X3P., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Triples por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Triples") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3")
                },
                "Porcentaje de Tiros Libres" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = FT., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Tiros Libres por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Tiros Libres") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3")
                },
                "Dobles-Dobles y Dobles-Triples" = {
                  p1 <- ggplot(datos_filtrados, aes(x = intervalo, y = DD2, color = intervalo)) +
                    geom_point(stat = "identity") +
                    labs(title = "Dobles-Dobles por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Total de Dobles-Dobles") +
                    theme_minimal()
                  
                  p2 <- ggplot(datos_filtrados, aes(x = intervalo, y = DD3, color = intervalo)) +
                    geom_point(stat = "identity") +
                    labs(title = "Dobles-Triples por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Total de Dobles-Triples") +
                    theme_minimal()
                  
                  plot1 <- ggplotly(p1)
                  plot2 <- ggplotly(p2)
                  
                  plotly::subplot(plot1, plot2, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
                    layout(title = list(text = "DD2 Y DD3 por Intervalo Cuatrienales de las Temporadas",
                                        font = list(size = 16)),
                           margin = list(t = 40))
                },
                "Puntos por Tipo de Tiro" = {
                  p1 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTS2PT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Dobles por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Dobles") +
                    theme_minimal()
                  
                  p2 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTS3PT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Triples por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Triples") +
                    theme_minimal()
                  
                  p3 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTSFT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Tiros Libres por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Tiros Libres") +
                    theme_minimal()
                  
                  plot1 <- ggplotly(p1) %>% layout(title = list(text = "Puntos por Dobles",
                                                                font = list(size = 16)))
                  plot2 <- ggplotly(p2) %>% layout(title = list(text = "Puntos por Triples",
                                                                font = list(size = 16)))
                  plot3 <- ggplotly(p3) %>% layout(title = list(text = "Puntos por Tiros Libres",
                                                                font = list(size = 16)))
                  
                  plotly::subplot(plot1, plot2, plot3, nrows = 3, titleX = TRUE, titleY = TRUE) %>%
                    layout(title = list(text = "Puntos por Tipo de Tiro por Intervalo de Temporada",
                                        font = list(size = 16)),
                           margin = list(t = 40))
                }
    )
    
    if (input$grafico != "Puntos por Tipo de Tiro" && input$grafico != "Dobles-Dobles y Dobles-Triples") {
      ggplotly(p)
    } else {
      p 
    }
  })
  
  # Tabla de datos correspondiente al gráfico seleccionado
  output$tabla_datos <- renderDT({
    datos_filtrados <- NBA_filtrado()
    
    tabla <- switch(input$grafico,
                    "Distribución de Puntos por Temporada" = datos_filtrados %>% select(intervalo, Player, PTS),
                    "Asistencias por Temporada" = datos_filtrados %>% select(intervalo, Player, AST.),
                    "Porcentaje de Tiros de Campo" = datos_filtrados %>% select(intervalo, Player, FG.),
                    "Porcentaje de Triples" = datos_filtrados %>% select(intervalo, Player, X3P.),
                    "Porcentaje de Tiros Libres" = datos_filtrados %>% select(intervalo, Player, FT.),
                    "Dobles-Dobles y Dobles-Triples" = datos_filtrados %>% select(intervalo, Player, DD2, DD3),
                    "Puntos por Tipo de Tiro" = datos_filtrados %>% select(intervalo, Player, PTS2PT., PTS3PT., PTSFT.)
    )
    
    # Personalizar la tabla con DT
    datatable(tabla, 
              options = list(
                pageLength = 10,  # Mostrar 10 filas por página
                order = list(0, 'des')  # Ordenar por la segunda columna (intervalo) de forma ascendente
              ),
              rownames = FALSE  # No mostrar números de fila
    )
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

