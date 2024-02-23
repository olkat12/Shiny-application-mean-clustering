library(shiny)
library(tidyverse)
library(factoextra)
library(shinydashboard)

myTheme <- function() {
  theme_bw() +
    theme(
      plot.background = element_rect(fill = "lightblue"),
      panel.background = element_rect(fill = "white"),
      text = element_text(color = "darkblue")
    )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Wine Clustering App", titleWidth = 350),
  dashboardSidebar(
    fileInput("file", "Choose CSV File", accept = ".csv"),
    sliderInput("clusters", "Number of clusters:", min = 1, max = 10, value = 3)
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Elbow Method", plotOutput("nbClustersPlot", height = "500px", width = "1000px")),
      tabPanel("Silhouette Plot", plotOutput("silhouettePlot", height = "500px", width = "1000px")),
      tabPanel("Gap Statistic", plotOutput("gapStatPlot", height = "500px", width = "1000px")),
      tabPanel("Cluster Biplot", plotOutput("clusterBiplot", height = "500px", width = "1000px")),
      tabPanel("Cluster Plot", plotOutput("clusterPlot", height = "500px", width = "1000px")),
      tabPanel("Summary", tableOutput("summaryTable")),
      tabPanel("Plots",
               fluidRow(
                 column(6, selectInput("chartType", "Choose chart type", choices = c("hist", "density", "boxplot"))),
                 column(6, selectInput("zmienna", "Choose variable", ""))
               ),
               conditionalPanel(
                 condition = "input.chartType == 'hist'",
                 sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                 tags$script("Shiny.onInputChange('updateClusterSlider', true);")
               ),
               plotOutput("wykres", height = "400px", width = "800px")
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
    data_raw <- reactive({
    req(input$file)
    read_csv(input$file$datapath) %>% select(-Obs, -Country) %>% as.data.frame()
  })
  
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath) %>% select(-Obs, -Country) %>% scale() %>% as.data.frame()
  })
  clusters <- reactive({
    kmeans(data(), centers = input$clusters, iter.max = 100, nstart = 100)
  })
  
  observe({
    req(input$file)
    updateSelectInput(session, "zmienna", choices = colnames(data()))
  })
  
  # Wykres na podstawie wyboru użytkownika
  output$wykres <- renderPlot({
    req(input$file)
    
    if (input$chartType == "hist") {
      ggplot(data_raw(), aes(x = !!sym(input$zmienna))) +
        geom_histogram(fill = "skyblue", color = "black", bins = input$bins) +
        labs(title = "Histogram") +
        myTheme()
    } else if (input$chartType == "density") {
      ggplot(data_raw(), aes(x = !!sym(input$zmienna))) +
        geom_density(fill = "skyblue", color = "black", alpha = 0.5) +
        labs(title = "Wykres Gęstości") +
        myTheme()
    } else if (input$chartType == "boxplot") {
      ggplot(data_raw(), aes(y = !!sym(input$zmienna))) +
        geom_boxplot(fill = "skyblue", color = "black") +
        labs(title = "Wykres Pudełkowy") +
        myTheme()
    }
  })
  
  output$nbClustersPlot <- renderPlot({
    fviz_nbclust(data(), kmeans, method = "wss") +
      ggtitle("Elbow Method") +
      myTheme()
  })
  
  output$silhouettePlot <- renderPlot({
    fviz_nbclust(data(), kmeans, method = "silhouette") +
      ggtitle("Silhouette Method") +
      myTheme()
  })
  
  output$gapStatPlot <- renderPlot({
    fviz_nbclust(data(), kmeans, method = "gap_stat") +
      ggtitle("Gap Statistic Method") +
      myTheme()
  })
  
  output$clusterBiplot <- renderPlot({
    fviz_cluster(clusters(), data = data()) +
      myTheme()
  })
  
  output$clusterPlot <- renderPlot({
    Wine <- read_csv(input$file$datapath)
    Wine <- Wine %>%
      mutate(cluster = clusters()$cluster)
    ggplot(Wine, aes(x = Rating, y = Price, col = as.factor(cluster))) +
      geom_point(size = 2.5) +
      ggtitle("Clusters based on Rating and Price") +
      myTheme()
  })
  
  output$summaryTable <- renderTable({
    Wine <- read_csv(input$file$datapath)
    Wine <- Wine %>%
      mutate(cluster = clusters()$cluster)
    
    summary_data <- Wine %>%
      group_by(cluster) %>%
      summarise(
        Mean_Rating = mean(Rating),
        Mean_Price = mean(Price),
        Mean_Alcohol = mean(Alcohol),
        Mean_Residual_Sugar = mean(Residual_Sugar),
        Mean_Sulphates = mean(Sulphates),
        Mean_pH = mean(pH),
        Observations = n()
      )
    
    summary_data
  })
}

# WYWOŁANIE APLIKACJI
shinyApp(ui = ui, server = server)