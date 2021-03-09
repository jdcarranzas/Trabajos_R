
library(nortest)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)
library(moments)
library(psych)

ui <- dashboardPage(
    dashboardHeader(title = "Data Review"),
    dashboardSidebar(
        sidebarMenu(
            # Iconos desde la web de fontawesome
            menuItem("Cargar la data", tabName = "cargar", icon = icon("table")),
            menuItem("Correlaciones", tabName = "correl", icon = icon("project-diagram")),
            menuItem("Estadistica Descriptiva", tabName = "estadis", icon = icon("subscript"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("cargar",
                    fluidPage(
                        h1("Cargar la data"),
                        # Cargar la data, input de la data 
                        box(fileInput("file1", "Escoja un archivo .csv, max 5mb",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            # Linea horizontal
                            tags$hr(),
                            
                            # si el archivo tiene fila de cabecera
                            checkboxInput("header", "Cabecera", TRUE),
                            
                            # Escoger el separador de la data
                            radioButtons("sep", "Separador",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            
                            # Seleccionar el separador de los textos
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"'),
                            
                            # Linea horizontal
                            tags$hr(),
                            
                            # Numero de filas para mostrar
                            radioButtons("disp", "Numero de filas",
                                         choices = c(Head = "head",
                                                     Todos = "all"),
                                         selected = "all"),
                            
                            width = 4
                            
                            ), 
                        # Pagina principal de la app
                        mainPanel(
                            
                            # Output: Data file 
                            tableOutput("contents")
                            
                        )
                    )
                    ),
            tabItem("correl",
                    h1("Grafico de Correlaciones"),
                    # Los input de la tabla y del plot
                    box(selectInput("a",'Variable A', ""),
                        selectInput("b",'Variable B', ""),
                        selectInput("c",'Variable C', ""),
                        selectInput("d",'Variable D', ""),
                        selectInput("e",'Variable E', ""),
                        selectInput("f",'Variable F', ""),
                        selectInput("g",'Variable G', ""),
                        selectInput("cortype", "Tipo de calculo de la correlacion", c("pearson", "kendall", "spearman")),
                        ),
                    # El plot de las correlaciones
                    box(plotOutput('plot')),
                    # La tabla de las correlaciones
                    box(dataTableOutput("cortable"))
                    ),
            tabItem("estadis",
                    h1("Tabla de estadistica descriptiva"),
                    # La tabla de estadisticos de las variables
                    box(dataTableOutput("estadistable"), width = 12),
                    
                    box(fluidPage(
                        "Nota: BoxTest muestra una prueba de Ljung-Box que identifica autocorrelacion, es decir, si los datos se 
                        distribuyen de manera independiente. La hipotesis nula es que los datos se distribuyen de forma independiente"),
                        width = 12),
                    
                    # Los input del plot
                    box(selectInput("x1col",'Densidad X1', ""),
                        selectInput("x2col",'Densidad X2', "")),
                   
                    
                    # El plot de las densidades
                    box(plotlyOutput('densiplot'), width = 6)
            )
        )
    ),
        
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Carga de la data"),
                    tabPanel("Correlaciones", 
                             plotOutput('plot'), dataTableOutput("cortable")
                             ),
                    tabPanel("Estadistica Descriptiva",
                             dataTableOutput("estadistable"), plotlyOutput("densiplot"))
        )
    )
)

server <- function(input, output, session) {
    
    # mostrar la data
    output$contents <- renderTable({
        
        # input$file1 sera NULL inicialmente. luego de que el usuario seleccione y
        # suba un archivo, toda la data sera desplegada por defecto,
        # o si se selecciona solo la cabecera, seran mostrados..
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    # Reactive de la data
    data <- reactive({ 
        req(input$file1) 
        
        inFile <- input$file1 
        
        
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        updateSelectInput(session, inputId = 'a', label = 'Variable A',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'b', label = 'Variable B',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'c', label = 'Variable C',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'd', label = 'Variable D',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'e', label = 'Variable E',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'f', label = 'Variable F',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'g', label = 'Variable G',
                          choices = names(select_if(df, is.numeric)))
        
        return(df)
        
    })
    
    # Plot de correlaciones
    output$plot <- renderPlot(
        pairs.panels(data()[,c(input$a, input$b, input$c, input$d, input$e, input$f, input$g)],
                     method = input$cortype,
                     hist.col = "#00AFBB",
                     density = TRUE, 
                     ellipses = TRUE, 
                     lm = TRUE)
    )
    
    # Tabla de correlaciones
    output$cortable <- renderDataTable(round(cor(data() %>% 
                                                     select_if(is.numeric), method = input$cortype), 2))
    
    
    # Reactive de la data2
    data2 <- reactive({ 
        req(input$file1) 
        
        inFile <- input$file1 
        
        
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        updateSelectInput(session, inputId = 'x1col', label = 'Densidad X1',
                          choices = names(select_if(df, is.numeric)))
        updateSelectInput(session, inputId = 'x2col', label = 'Densidad X2',
                          choices = names(select_if(df, is.numeric)))
        
        df <- df %>%
            # select_if(is.numeric) %>%
            mutate(across(is.numeric, function(x) replace_na(x, mean(x, na.rm = T))),
                   across(is.numeric, function(x) (x - mean(x))/(max(x)-min(x))))
        
        return(df)
        })
    
    # Reactive de las densidades
    x1 <- reactive({
        data2()[, input$x1col]
    })
    
    x2 <- reactive({
        data2()[, input$x2col]
    })
    
    
    
    # Tabla estadisticos 
    output$estadistable <- renderDataTable(data()%>%
                                               pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Valores") %>% 
                                               group_by(Variable) %>%
                                               summarise(Media = round(mean(Valores, na.rm = T), 2),
                                                         Minimo   = min(Valores, na.rm = T),
                                                         Maximo   = max(Valores, na.rm = T), 
                                                         Varianza   = round(var(Valores, na.rm = T), 2),
                                                         `Desv. Estandar` = round(sd(Valores, na.rm = T), 2),
                                                         tibble(`Cuantil 15` = quantile(Valores, probs = 0.15, na.rm = T),
                                                                Mediana   = quantile(Valores, probs = 0.5, na.rm = T),
                                                                `Cuantil 85` = quantile(Valores, probs = 0.85, na.rm = T)),
                                                         Curtosis = round(kurtosis(Valores, na.rm = T), 3), 
                                                         Asimetria = round(skewness(Valores, na.rm = T), 3),
                                                         `BoxTest (P-Valor)` = round(as.numeric(Box.test(Valores, 
                                                                                                         lag = 1, 
                                                                                                         type = "Ljung-Box")["p.value"]), 3),
                                                         Normalidad = round(as.numeric(ad.test(Valores)$p.value, 3))))
    
    # Plot de densidades
    output$densiplot <- renderPlotly(
        plot2 <- plot_ly(x = x1()) %>%
            add_trace(x = density(x1())$x, y = density(x1())$y, type = "scatter", 
                      mode = "lines", fill = "tozeroy", yaxis = "y2", name = paste0("Densidad ", input$x1col)) %>%
            add_trace(x = density(x2())$x, y = density(x2())$y, type = "scatter",
                      mode = "lines", fill = "tozeroy", yaxis = "y2", name = paste0("Densidad ", input$x2col)) %>%
            layout(yaxis2 = list(overlaying = "y", side = "left"),
                   title = paste0("Densidad de ", input$x1col, " y ", input$x2col),
                   xaxis = list(title = "Observaciones"),
                   yaxis = list(title = "Densidad"))
     )
    
    
                               
}

shinyApp(ui,server)