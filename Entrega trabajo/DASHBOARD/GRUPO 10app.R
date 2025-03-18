# Carga de librerías
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(readxl)) install.packages("readxl")
if (!require(plotly)) install.packages("plotly")
if (!require(plotly)) install.packages("DT")
if (!require(shinythemes)) install.packages("shinythemes")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(DT)
library(shinythemes)
library(rsconnect)

# Lectura de base de datos
NBA <- read_excel("~/An-lisis-NBA-Grupo-10/NBA G1-G10.xlsx")

# Limpieza de nombres de columnas
names(NBA) <- make.names(names(NBA))

# Declaración de variables
NBA <- NBA %>%
  mutate(
    AST = as.numeric(AST),
    X3PM = as.numeric(X3PM),
    GP = as.numeric(GP),
    PTS = as.numeric(PTS),
    FGM = as.numeric(FGM),
    FG. = as.numeric(FG.),
    FTM = as.numeric(FTM),
    X3P. = as.numeric(X3P.),
    FT. = as.numeric(FT.),
    AST. = as.numeric(AST.),
    DD2 = as.numeric(DD2),
    DD3 = as.numeric(DD3),
    PTS2PT. = as.numeric(PTS2PT.),
    PTS3PT. = as.numeric(PTS3PT.),
    PTSFT. = as.numeric(PTSFT.),
    Season = as.character(Season),
    Años_J = as.numeric(sub("-.*", "", Season)),
    intervalo = cut(Años_J, 
                    breaks = seq(1996, 2024, by = 4), 
                    labels = c("1996-2000", "2000-2004", "2004-2008", "2008-2012", "2012-2016", "2016-2020", "2020-2024"), 
                    include.lowest = TRUE)
  )

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Análisis NBA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Gráficos Seleccionados", tabName = "graficos", icon = icon("chart-bar")),
      menuItem("Ayuda", tabName = "ayuda", icon = icon("question-circle")),
      sliderInput("intervalo_tiempo", "Selecciona el intervalo de tiempo:",
                  min = 1996, max = 2024, value = c(1996, 2024)),
      selectInput("jugador", "Selecciona un jugador:",
                  choices = c("Todos", unique(NBA$Player))),
      textInput("buscar_jugador", "Buscar Jugador:", placeholder = "Ingresa el nombre del jugador"), 
      selectInput("grafico", "Selecciona un gráfico:",
                  choices = c("Distribución de Puntos por Temporada",
                              "Asistencias por Temporada",
                              "Porcentaje de Tiros de Campo",
                              "Porcentaje de Triples",
                              "Porcentaje de Tiros Libres",
                              "Dobles-Dobles y Dobles-Triples",
                              "Puntos por Tipo de Tiro")),
      downloadButton("descargar_datos", "Descargar Datos")
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
      font-weight: bold; /* En negrita */
    }
    
    /* Color del texto en las pestañas */
    .nav-tabs-custom .nav-tabs li.active a {
      background-color: #00284d;  /* Azul oscuro más claro */
      color: #d3d3d3;  /* Texto blanco */
    }
    
    /* Aumentar el tamaño de los valueBox */
    .value-box {
      width: 100%;  /* Ocupar todo el ancho disponible */
      height: 150px;  /* Aumentar la altura */
      font-size: 20px;  /* Aumentar el tamaño de la fuente */
      padding: 20px;  /* Aumentar el espaciado interno */
    }
    .value-box .value {
      font-size: 30px;  /* Aumentar el tamaño de la fuente del valor */
    }
    .value-box .subtitle {
      font-size: 20px;  /* Aumentar el tamaño de la fuente del subtítulo */
    }
    ")),
    tabItems(
      tabItem(
        tabName = "inicio",
        h1(" Análisis estadístico por periodo intercuartílico de la NBA"),
        h2("Vamos a Analizar la eficiencia de la NBA desde 1996 hasta 2024"),
        p("Esta aplicación tiene como finalidad analizar y representar gráficamente los datos recolectados de la NBA, como puntos, asistencias, porcentajes de tiros, etc. Desde 1996 hasta el año 2024."),
        hr(),
        h3("Integrantes:"),
        tags$ul(
          tags$li("Leonardo Mentado C.I: 29.850.786."),
          tags$li("Samuel Barreto C.I: 31.484.531.")
        ),
      ),
      tabItem(
        tabName = "graficos",
        fluidRow(
          box(
            title = "Gráfico seleccionado",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("grafico_seleccionado", height = "850px")
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
            DTOutput("tabla_datos")
          )
        ),
        fluidRow(
          uiOutput("promedio_rendimiento")  # Aquí se mostrarán los valueBox de DD2 y DD3
        ),
        fluidRow(
          valueBoxOutput("jugador_destacado", width = 12)  # Aquí se mostrará el valueBox del jugador destacado
        ),
        plotlyOutput("grafico_resumen")
      ),
      tabItem(
        tabName = "ayuda",
        h2("Instrucciones"),
        tags$ol(
          tags$li("Selecciona la pestaña 'Gráficos Seleccionados' para ver los gráficos disponibles."),
          tags$li("Usa los filtros en la pestaña 'Filtros' para ajustar el intervalo de tiempo."),
          tags$li("Explora los gráficos y descarga los datos si es necesario.")
        ),
      )
    )
  )
)

# Definir el servidor (Server)
server <- function(input, output) {
  # Filtrar datos según el intervalo de tiempo y jugador seleccionado
  NBA_filtrado <- reactive({
    datos <- NBA %>%
      filter(Años_J >= input$intervalo_tiempo[1] & Años_J <= input$intervalo_tiempo[2])
    
    # Filtrar por nombre del jugador si se ingresa un valor en texto
    if (!is.null(input$buscar_jugador) && input$buscar_jugador != "") {
      datos <- datos %>% filter(grepl(input$buscar_jugador, Player, ignore.case = TRUE))
    }
    
    # Filtrar por jugador seleccionado en las opciones
    if (input$jugador != "Todos") {
      datos <- datos %>% filter(Player == input$jugador)
    }
    
    datos
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
                    scale_fill_brewer(palette = "Set3") +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                },
                "Porcentaje de Tiros de Campo" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = FG., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Tiros de Campo por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Tiros de Campo") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3") +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                },
                "Porcentaje de Triples" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = X3P., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Triples por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Triples") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3") +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                },
                "Porcentaje de Tiros Libres" = {
                  ggplot(datos_filtrados, aes(x = intervalo, y = FT., fill = intervalo)) +
                    geom_boxplot(color = "darkblue") +
                    labs(title = "Distribución de Porcentaje de Tiros Libres por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Porcentaje de Tiros Libres") +
                    theme_minimal() +
                    scale_fill_brewer(palette = "Set3") +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
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
                  #MODIFICACION PARA EJE DE LAS Y
                  datos_filtrados <- datos_filtrados %>%
                    group_by(intervalo) %>%
                    summarise(
                      PTS2PT. = mean(PTS2PT., na.rm = TRUE),
                      PTS3PT. = mean(PTS3PT., na.rm = TRUE),
                      PTSFT. = mean(PTSFT., na.rm = TRUE)
                    )
                  
                  p1 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTS2PT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Dobles por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Dobles") +
                    theme_minimal() +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                  
                  p2 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTS3PT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Triples por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Triples") +
                    theme_minimal() +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                  
                  p3 <- ggplot(datos_filtrados, aes(x = intervalo, y = PTSFT., fill = intervalo)) +
                    geom_bar(stat = "identity") +
                    labs(title = "Puntos por Tiros Libres por Intervalo de Temporada", 
                         x = "Intervalo de Temporada", y = "Puntos Tiros Libres") +
                    theme_minimal() +   
                    theme(plot.title = element_text(size = 10, face = "bold")) +
                    scale_fill_brewer(palette = "BuPu")
                  
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
                    "Porcentaje de Tiros de Campo" = datos_filtrados %>% select(intervalo, Player, PTS, FG.),
                    "Porcentaje de Triples" = datos_filtrados %>% select(intervalo, Player, PTS, X3P.),
                    "Porcentaje de Tiros Libres" = datos_filtrados %>% select(intervalo, Player, PTS, FT.),
                    "Dobles-Dobles y Dobles-Triples" = datos_filtrados %>% select(intervalo, Player, DD2, DD3),
                    "Puntos por Tipo de Tiro" = datos_filtrados %>% select(intervalo, Player, PTS, PTS2PT., PTS3PT., PTSFT.)
    )
    
    # Personalizar la tabla con DT
    datatable(tabla, 
              options = list(
                pageLength = 10,  # Mostrar 10 filas por página
                order = list(0, 'des')  # Ordenar por la segunda columna (intervalo)
              ),
              rownames = FALSE  # No mostrar números de fila
    )
  })
  
  # Gráfico de resumen en la página de inicio
  output$grafico_resumen <- renderPlotly({
    datos_resumen <- NBA %>%
      group_by(intervalo) %>%
      summarise(
        Promedio_DD2 = mean(DD2, na.rm = TRUE),  # Promedio de Dobles-Dobles
        Promedio_DD3 = mean(DD3, na.rm = TRUE)   # Promedio de Dobles-Triples
      )
    
    if (input$grafico == "Dobles-Dobles y Dobles-Triples") {
      p1 <- ggplot(datos_resumen, aes(x = intervalo, y = Promedio_DD2, fill = intervalo)) +
        geom_bar(stat = "identity") +
        labs(title = "Promedio de Dobles-Dobles por Intervalo de Temporada",
             x = "Intervalo de Temporada", y = "Promedio de Dobles-Dobles") +
        theme_minimal()
      
      p2 <- ggplot(datos_resumen, aes(x = intervalo, y = Promedio_DD3, fill = intervalo)) +
        geom_bar(stat = "identity") +
        labs(title = "Promedio de Dobles-Triples por Intervalo de Temporada",
             x = "Intervalo de Temporada", y = "Promedio de Dobles-Triples") +
        theme_minimal()
      
      plot1 <- ggplotly(p1)
      plot2 <- ggplotly(p2)
      
      plotly::subplot(plot1, plot2, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
        layout(title = list(text = "Promedio de Dobles-Dobles y Dobles-Triples por Intervalo de Temporada",
                            font = list(size = 16)),
               margin = list(t = 40))
    } else {
      datos_resumen <- NBA %>%
        group_by(intervalo) %>%
        summarise(Promedio = switch(input$grafico,
                                    "Distribución de Puntos por Temporada" = mean(PTS, na.rm = TRUE),
                                    "Asistencias por Temporada" = mean(AST., na.rm = TRUE),
                                    "Porcentaje de Tiros de Campo" = mean(FG., na.rm = TRUE),
                                    "Porcentaje de Triples" = mean(X3P., na.rm = TRUE),
                                    "Porcentaje de Tiros Libres" = mean(FT., na.rm = TRUE),
                                    "Puntos por Tipo de Tiro" = mean(PTS2PT. + PTS3PT. + PTSFT., na.rm = TRUE)))
      
      ggplot(datos_resumen, aes(x = intervalo, y = Promedio, fill = intervalo)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Promedio general de", switch(input$grafico,
                                                         "Distribución de Puntos por Temporada" = "Puntos",
                                                         "Asistencias por Temporada" = "Asistencias",
                                                         "Porcentaje de Tiros de Campo" = "Tiros de Campo",
                                                         "Porcentaje de Triples" = "Triples",
                                                         "Porcentaje de Tiros Libres" = "Tiros Libres",
                                                         "Puntos por Tipo de Tiro" = "Puntos por Tipo de Tiro"),
                           "por Intervalo de Temporada"),
             x = "Intervalo de Temporada",
             y = "Promedio") +
        theme_minimal()  +   
        theme(plot.title = element_text(size = 10, face = "bold")) +
        scale_fill_brewer(palette = "BuPu")
    }
  })
  
  # Métricas clave en la página de inicio
  output$promedio_rendimiento <- renderUI({
    if (input$grafico == "Dobles-Dobles y Dobles-Triples") {
      # Identificar al jugador con la mayor cantidad de DD2
      jugador_max_DD2 <- NBA_filtrado() %>%
        arrange(desc(DD2)) %>%
        slice(1) %>%
        pull(Player)
      
      # Identificar al jugador con la mayor cantidad de DD3
      jugador_max_DD3 <- NBA_filtrado() %>%
        arrange(desc(DD3)) %>%
        slice(1) %>%
        pull(Player)
      
      # Obtener los valores máximos de DD2 y DD3
      max_DD2 <- max(NBA_filtrado()$DD2, na.rm = TRUE)
      max_DD3 <- max(NBA_filtrado()$DD3, na.rm = TRUE)
      
      # Crear dos valueBox separados
      tagList(
        valueBox(
          value = paste(round(max_DD2, 2), " (", jugador_max_DD2, ")"),
          subtitle = "Máximo de Dobles-Dobles (DD2)",
          icon = icon("basketball"),
          color = "blue",
          width = 6  # Ancho del valueBox (mitad de la fila)
        ),
        valueBox(
          value = paste(round(max_DD3, 2), " (", jugador_max_DD3, ")"),
          subtitle = "Máximo de Dobles-Triples (DD3)",
          icon = icon("basketball"),
          color = "green",
          width = 6  # Ancho del valueBox (mitad de la fila)
        )
      )
    } else {
      if (input$grafico == "Puntos por Tipo de Tiro") {
        # Identificar al jugador con la mayor cantidad de PTS2PT.
        jugador_max_PTS2PT <- NBA_filtrado() %>%
          mutate(Metrica_2PT = PTS2PT. / GP) %>%
          arrange(desc(Metrica_2PT)) %>%
          slice(1) %>%
          pull(Player)
        
        # Identificar al jugador con la mayor cantidad de PTS3PT.
        jugador_max_PTS3PT <- NBA_filtrado() %>%
          mutate(Metrica_3PT =  PTS3PT. / GP) %>%
          arrange(desc(Metrica_3PT)) %>%
          slice(1) %>%
          pull(Player)
        
        # Identificar al jugador con la mayor cantidad de PTSFT.
        jugador_max_PTSFT <- NBA_filtrado() %>%
          mutate(Metrica_FT = PTSFT. / GP) %>%
          arrange(desc(Metrica_FT)) %>%
          slice(1) %>%
          pull(Player)
        
        # Obtener los valores máximos de PTS2PT., PTS3PT. y PTSFT.
        max_PTS2PT <- max(NBA_filtrado()$PTS2PT., na.rm = TRUE)
        max_PTS3PT <- max(NBA_filtrado()$PTS3PT., na.rm = TRUE)
        max_PTSFT <- max(NBA_filtrado()$PTSFT., na.rm = TRUE)
        
        # Crear tres valueBox separados
        tagList(
          valueBox(
            value = paste(round(max_PTS2PT, 2), " (", jugador_max_PTS2PT, ")"),
            subtitle = "Máximo de Puntos por Dobles (PTS2PT.)",
            icon = icon("basketball"),
            color = "blue",
            width = 4  # Ancho del valueBox (un tercio de la fila)
          ),
          valueBox(
            value = paste(round(max_PTS3PT, 2), " (", jugador_max_PTS3PT, ")"),
            subtitle = "Máximo de Puntos por Triples (PTS3PT.)",
            icon = icon("basketball"),
            color = "green",
            width = 4  # Ancho del valueBox (un tercio de la fila)
          ),
          valueBox(
            value = paste(round(max_PTSFT, 2), " (", jugador_max_PTSFT, ")"),
            subtitle = "Máximo de Puntos por Tiros Libres (PTSFT.)",
            icon = icon("basketball"),
            color = "orange",
            width = 4  # Ancho del valueBox (un tercio de la fila)
          )
        )
      } else {
        # Calcular el promedio del estadístico seleccionado
        promedio <- switch(input$grafico,
                           "Distribución de Puntos por Temporada" = mean(NBA_filtrado()$PTS, na.rm = TRUE),
                           "Asistencias por Temporada" = mean(NBA_filtrado()$AST., na.rm = TRUE),
                           "Porcentaje de Tiros de Campo" = mean(NBA_filtrado()$FG., na.rm = TRUE),
                           "Porcentaje de Triples" = mean(NBA_filtrado()$X3P., na.rm = TRUE),
                           "Porcentaje de Tiros Libres" = mean(NBA_filtrado()$FT., na.rm = TRUE),
                           "Puntos por Tipo de Tiro" = mean(NBA_filtrado()$PTS2PT. + NBA_filtrado()$PTS3PT. + NBA_filtrado()$PTSFT., na.rm = TRUE))
        
        # Mostrar el promedio en un valueBox, envuelto en un tagList
        tagList(
          valueBox(
            value = round(promedio, 2),
            subtitle = paste("Promedio de", switch(input$grafico,
                                                   "Distribución de Puntos por Temporada" = "Puntos",
                                                   "Asistencias por Temporada" = "Asistencias",
                                                   "Porcentaje de Tiros de Campo" = "Tiros de Campo",
                                                   "Porcentaje de Triples" = "Triples",
                                                   "Porcentaje de Tiros Libres" = "Tiros Libres",
                                                   "Puntos por Tipo de Tiro" = "Puntos por Tipo de Tiro")),
            icon = icon("basketball"),
            color = "purple"
          )
        )
      }
    }
  })
  # ValueBox para el jugador destacado
  output$jugador_destacado <- renderValueBox({
    # Identificar al jugador más destacado según el estadístico seleccionado
    jugador_destacado <- switch(input$grafico,
                                "Distribución de Puntos por Temporada" = {
                                  jugador <- NBA_filtrado() %>%
                                    arrange(desc(PTS)) %>%
                                    slice(1)
                                  paste(jugador$Player, "(", round(jugador$PTS, 2), ")")
                                },
                                "Asistencias por Temporada" = {
                                  jugador <- NBA_filtrado() %>%
                                    arrange(desc(AST / GP)) %>%
                                    slice(1)
                                  paste(jugador$Player, "(", round(jugador$AST / jugador$GP, 2), ")")
                                },
                                "Porcentaje de Tiros de Campo" = {
                                  jugador <- NBA_filtrado() %>%
                                    arrange(desc(FGM / GP)) %>%
                                    slice(1)
                                  paste(jugador$Player, "(", round(jugador$FGM / jugador$GP, 2), ")")
                                },
                                "Porcentaje de Triples" = {
                                  jugador <- NBA_filtrado() %>%
                                    mutate(Metrica_Triples = X3PM / GP) %>%
                                    arrange(desc(Metrica_Triples)) %>%
                                    slice(1)
                                  paste(jugador$Player, "(", round(jugador$X3PM / jugador$GP, 2), ")")
                                },
                                "Porcentaje de Tiros Libres" = {
                                  jugador <- NBA_filtrado() %>%
                                    arrange(desc(FTM / GP)) %>%
                                    slice(1)
                                  paste(jugador$Player, "(", round(jugador$FTM / jugador$GP, 2), ")")
                                })
    # Mostrar el jugador destacado en el valueBox
    valueBox(
      value = jugador_destacado,
      subtitle = "Jugador Mayor desempeño de esta categoria",
      icon = icon("trophy"),  # Icono de trofeo
      color = "yellow"  # Color amarillo
    )
  })
  
  # Descargar datos filtrados
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_nba_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(NBA_filtrado(), file, row.names = FALSE)
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)