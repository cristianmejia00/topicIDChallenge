# 20161221
# Knowledge Dashboard visualizer in shiny
# (3/3)

# This is a shiny app
# It allows to navigate through the cluster similarly to FS
# Depends on 01-widgets.R and 02-preparation.R (Must run first)

#Libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(plotly)
library(DT)


########################################################
########################################################
# UI
header <- dashboardHeader(
  title = "Knowledge Dashboard"
)

body <- dashboardBody(
  fluidRow(
    column(width = 6,
           box(width = NULL, 
               status = "warning",
               radioButtons("selectedCluster", "Clusters",
                            choices = n_clusters,
                            selected = 1, 
                            inline = TRUE
               )
           )
    ),
    column(width = 3, offset = 3,
           box(width = NULL,
               status = "warning",
               htmlOutput("text")
           )
    )
  ),
  
  fluidRow(
    column(width = 10, offset = 1,
           DT::dataTableOutput('papers')
    )
  ),
  
  fluidRow(
    column(width = 6, 
           box(title = "Top Authors",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("authors")
           ),
           box(title = "Top Publishers",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("publishers")
           ),
           box(title = "Top Countries",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("countries")
           )
    ),
    
    column(width = 6,
           box(title = "Top Subjects",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("categories")
           ),
           box(title = "Top Keywords",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("keywords")
           ),
           box(title = "Top Years",
               width = NULL,
               solidHeader = TRUE,
               plotlyOutput("years")
           )
    )
  )
)



ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

##################################################################
##################################################################
server <- function(input, output, session) {
  
  filtered <- reactive({filter(data, community == input$selectedCluster)})
  
  output$text <- renderUI({
    str1 <- paste("# of nodes: ",v_e[input$selectedCluster,1])
    str2 <- paste("# of edges: ",v_e[input$selectedCluster,2])
    str3 <- paste("Ave. Year: ", round(mean(filtered()$PY, na.rm = TRUE), 1))
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$categories <- renderPlotly({
    generate_plot(TopSubjects(filtered()))
  })
  
  output$authors <- renderPlotly({
    generate_plot(TopAuthors(filtered()))
  })
  
  output$publishers <- renderPlotly({
    generate_plot(TopPublishers(filtered()))
  })
  
  output$keywords <- renderPlotly({
    generate_plot(TopKeywords(filtered()))
  })
  
  output$countries <- renderPlotly({
    generate_plot(TopCountries(filtered()))
  })
  
  output$years <- renderPlotly({
    generate_plot(TopYears(filtered()))
  })
  
  output$papers <- DT::renderDataTable({
    filtered_tops <- TopPapers(filtered())
    DT::datatable(filtered_tops, escape = 3)})
  print(str(data))
}

###########################################################
shinyApp(ui = ui, server = server)
