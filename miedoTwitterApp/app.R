library(shiny)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(plotly)

miedoTodo <- readRDS("data/miedoTodo.rds")
miedoPairsClean <- readRDS("data/miedoPairsClean.RDS")
#miedo50 <- miedoPairsClean %>% 

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}",
             HTML("#ext-label.control-label{font-size: 11px;}"),
             HTML("#rango-label.control-label{font-size: 11px;}"),
             HTML("body {
                           background-color: #0e31a6;
                           color: white;
                                }"),
             HTML(".well {
                                background-color: #0e31a6;")),

    # Application title
    titlePanel("El miedo en Twitter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          plotlyOutput("tiempoPlot"),
            textInput("palabras", "Escoje palabras separadas por comas:", "miedo"),
          #selectizeInput(),
            sliderInput("per",
                        "Escoge un porcentaje de aparición:",
                        min = 1,
                        max = 100,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  miedoTiempo <- reactive(
    miedoTodo %>% 
      select(created_at, id_str) %>% 
      mutate(time=as.Date(created_at, format="%Y-%m-%d")) %>% 
      group_by(time) %>% 
      mutate(n=n()) %>% 
      ungroup() %>% 
      mutate(text=paste0("N° de Tweets= ",n,"<br>", time)) %>% 
      ggplot(aes(time, text=text))+
      geom_histogram(fill="red")+
      ylab("N° Tweets")+
      scale_y_log10()+
      theme_light()+
      theme(
        panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"))
      
    )
  
  output$tiempoPlot <- renderPlotly(ggplotly(miedoTiempo(), tooltip = c("text"))
  )

  palabras <- reactive(trimws(unlist(strsplit(input$palabras, ",")))
                       )
  
    output$distPlot <- renderPlot({
      miedoPairsClean %>% 
        filter(item1 %in% palabras() | item2 %in% palabras()) %>% 
        mutate(per=n/max(n)*100) %>% 
        filter(per>input$per) %>% 
        as_tbl_graph() %>%
        #activate(nodes) %>% 
        ggraph()+
        geom_edge_link(aes(width=per), color="red")+
        geom_node_text(aes(label=name), color="white")+
        theme_void()+
        theme(
          panel.background = element_rect(fill = "#0e31a6",
                                          colour = "#0e31a6",
                                          size = 0.5, linetype = "solid"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
