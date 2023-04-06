library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(tidyr)
library(highcharter)
library(shinythemes)
library(wordcloud2)

allPassionsBind <- readRDS("data/allPassionsBind.rds")
passionsSep <- readRDS("data/passionsSep.rds")
passionsHSankey <- readRDS("data/passionsHSankey.rds")
corpusSummarize <- readRDS("data/corpusSummarize.rds")
corpusNRC<- readRDS("data/corpusNRC.rds")

# Define UI for application
ui <- navbarPage(id="tabs",
  theme = shinytheme("cerulean"),
  HTML(paste0("Observing the Passions<h5>A Distant Reading of 
                             Descartes' <i>The Passions of the Soul</i> (1649)</h5>")),
  tags$head(
    tags$style(
      HTML('.well {
    background-color: #ffffff;
    border: 0px solid #ffffff;
    box-shadow: inset 0 0px 0px rgb(0 0 0 / 5%);
      }
    h5, .h5{
      color: #ffffff;
    }
      .navbar>.container-fluid .navbar-brand {
      margin-left: 10px;
      }
      .navbar-brand {
    float: left;
    padding: 10px 10px;
    font-size: 20px;
    line-height: 20px;
    height: 60px;
      }
    .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
    color: #ffffff;
    background-color: #178acc;
    height: 60px;
    }
    .navbar-nav>li>a {
    padding-top: 22px;
    padding-bottom: 18px;
    }
      .h2, h2 {
    text-align: center;
      }
      #initialSankey {
      text-align:center;
      display: flex;
      list-style-type: none;
      justify-content: center;
      flex-wrap: wrap;
      }
      #cloudTitlecss {
      text-align:center;
      display: flex;
      justify-content: center;
      flex-wrap: wrap;
      color: #333333;
      font-size: 18px;
      }
      #myfooter {
             background-color: #ffffff;
             font-size: 10px;
      }
            .center {
                    display: block;
                    margin-left: auto;
                    margin-right: auto;
                    max-width: 100%;
                    max-height: 100%;
                    }
    '))),
      
    # Application title
    #titlePanel("The Passions of the Soul"),
  tabPanel("Main Passions",
           column(12,
                        HTML("The book <i>The Passions of the Soul</i> contains 212 articles or paragraphs, 
                        which Descartes divides in three parts. The intensity of the colors points out <b>more correlation 
                             among the passion and the article</b>. By clicking the article, it is deployed in the table below, 
                             for a closer reading. [This first visualization may take a few seconds to load]."),
          plotlyOutput("articles"),
          dataTableOutput ("TablePlot")
        ),
        column(12, id="myfooter",
               HTML("<br><br><a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png' /></a>
  <br />This App is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br>It has been developed as part of the project 
              <a href='https://sway.office.com/OPIQZJ5peGHC6QmD?ref=Link' target='_blank'>Neurohumanities Lab. Engaging Experiences for Human Flourishing</a> by 
              <a href='https://github.com/mancebral' target='_blank'>mancebral</a>, in the context of the Challenge-Based Research Funding Program 
                    of <a href='https://tec.mx/en' target='_blank'>Tecnológico de Monterrey</a>.<br><br>")
        )
    ),
  ######################
  tabPanel("Specific Passions",
  HTML("In addition to the main passions (desire, hatred, joy, love, sadness, and wonder) 
                Descartes states the existence of many other, more specific passions. Below is shown 
                <b>how often each of the main passions is associated with a specific one</b>. Clicking on the passions 
                displays a table with articles in Descartes's text containing them.<br><br>"),
  column(10, offset = 1, id="initialSankey",
         radioButtons("buttons", "Filter by Main Passion", c("all the passions", "desire", "hatred", 
                                                         "joy", "love", "sadness", "wonder"), inline = TRUE),
         highchartOutput("sankey", width = "100%")
         ),
  column(12,
         dataTableOutput ("TablePlot2")
         ),
  column(12, id="myfooter",
  HTML("<br><br><a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png' /></a>
    <br />This App is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br>It has been developed as part of the project 
              <a href='https://sway.office.com/OPIQZJ5peGHC6QmD?ref=Link' target='_blank'>Neurohumanities Lab. Engaging Experiences for Human Flourishing</a> by 
              <a href='https://github.com/mancebral' target='_blank'>mancebral</a>, in the context of the Challenge-Based Research Funding Program 
                    of <a href='https://tec.mx/en' target='_blank'>Tecnológico de Monterrey</a>.<br><br>")
    )
  ),
  ########################
  tabPanel("Extended Corpus",
           HTML("Passions and emotions have been a topic of discussion for many other authors. Find below an extended corpus we 
                have prepared with key references about emotional content. Explore what words correspond to each emotion within the corpust, by <b>clicking on the visualizations</b>.<br><br>"),
           column(10, offset = 1, id="initialSankey",
                  radioButtons("emotions", "Select an emotion", c("Joy", "Sadness", "Anticipation", "Surprise", "Fear",
                                                              "Disgust", "Anger", "Trust"), inline = TRUE)
                  ),
           column(4, id="pies1",
                  plotlyOutput("piesCharts1", width = "100%")
                  ),
           column(8, id="pies1",
                  highchartOutput("piesCharts2", width = "100%")
                  ),
           column(12, id="cloudTitlecss",
                  textOutput("cloudTitle")
           ),
           column(12,
                  wordcloud2Output ("wordcloud", width = "100%")
           ),
           column(12, id="myfooter",
                  HTML("<br><br><a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png' /></a>
  <br />This App is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br>It has been developed as part of the project 
              <a href='https://sway.office.com/OPIQZJ5peGHC6QmD?ref=Link' target='_blank'>Neurohumanities Lab. Engaging Experiences for Human Flourishing</a> by 
              <a href='https://github.com/mancebral' target='_blank'>mancebral</a>, in the context of the Challenge-Based Research Funding Program 
                    of <a href='https://tec.mx/en' target='_blank'>Tecnológico de Monterrey</a>.<br><br>")
                  )
  ), 
  #######################
  tabPanel("Emotions along time",
           HTML("The visualization bellow shows the works included within the corpus and distributed along time. The size of the bubbles are correlated with the emotional charge they contain.<br><br>"),
           column(2, 
                  checkboxGroupInput("emotions2", "Choose emotions:", choices = c("Joy", "Sadness", "Anticipation", "Surprise", "Fear",
                                                                                  "Disgust", "Anger", "Trust"),
                                     selected = c("Joy", "Sadness", "Anticipation", "Surprise", "Fear",
                                                  "Disgust", "Anger", "Trust"))),
           column(10, id="plotTime",
                  plotlyOutput("timeChart")
                  ),
           column(12, id="myfooter",
                  HTML("<br><br><a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png' /></a>
  <br />This App is licensed under a <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.<br>It has been developed as part of the project 
              <a href='https://sway.office.com/OPIQZJ5peGHC6QmD?ref=Link' target='_blank'>Neurohumanities Lab. Engaging Experiences for Human Flourishing</a> by 
              <a href='https://github.com/mancebral' target='_blank'>mancebral</a>, in the context of the Challenge-Based Research Funding Program 
                    of <a href='https://tec.mx/en' target='_blank'>Tecnológico de Monterrey</a>.<br><br>")
                  )
           )
  )

# Define server logic required
server <- function(input, output) {

  allPassionsCors <- allPassionsBind %>% 
    mutate(correlation=round(correlation,1)) %>% 
    mutate(key=paste(passion, Number, correlation, sep = ";")) %>% 
    ggplot(aes(Number, passion, alpha= correlation, fill=passion, text=text, key=key))+
    geom_tile()+
    xlab(NULL)+
    ylab(NULL)+
    facet_wrap(~Part, ncol = 3, scales = "free_x")+
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.major.y = element_blank(),
          panel.spacing.y=unit(0.2, "lines"))

    output$articles <- renderPlotly ({
      ggplotly(allPassionsCors, tooltip = "text", source="tab") 
    })
    
    output$TablePlot <- renderDataTable(
      passionsSep %>% 
        filter(Number==as.numeric(str_split(event_data("plotly_click", source="tab")$key, 
                                            ";", simplify = TRUE)[2])) %>% 
        rename(Article=Number) %>% 
        select(Article, Title, Text) %>% 
        datatable(options = list(scrollX=TRUE, pageLength = 500, dom="t", 
                                 language= list(zeroRecords="Click on article to display")), 
                  rownames = FALSE) %>% 
        formatStyle(1:3, backgroundColor="#ffffff")
    )
    
    output$sankey <- renderHighchart({
      ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
      
      if (input$buttons=="all the passions") return (passionsHSankey %>% 
        as.data.frame() %>% 
        data_to_sankey() %>% 
        hchart("sankey") %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_plotOptions(series = list(column = list(stacking = "normal"), 
                                     borderWidth=0,
                                     dataLabels = list(enabled = TRUE),
                                     events = list(click = ClickFunction)))
      )
      passionsHSankey %>% 
        filter(word==input$buttons) %>% 
        as.data.frame() %>% 
        data_to_sankey() %>% 
        hchart("sankey") %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_plotOptions(series = list(column = list(stacking = "normal"), 
                                     borderWidth=0,
                                     dataLabels = list(enabled = TRUE),
                                     events = list(click = ClickFunction)))
    })
    
    output$TablePlot2 <- renderDataTable({
      if (input$buttons=="all the passions") return ( 
        passionsSep %>% 
          filter(str_detect(Text, input$Clicked)) %>% 
          rename(Article=Number) %>% 
          select(Article, Title, Text) %>% 
          datatable(options = list(scrollX=TRUE, pageLength = 500, dom="t"), rownames = FALSE) %>% 
          formatStyle(1:3, backgroundColor="#ffffff")
        )
       passionsSep %>% 
        filter(str_detect(Text, input$buttons)) %>% 
        filter(str_detect(Text, input$Clicked)) %>% 
        rename(Article=Number) %>% 
        select(Article, Title, Text) %>% 
        datatable(options = list(scrollX=TRUE, pageLength = 500, dom="t"), rownames = FALSE) %>% 
        formatStyle(1:3, backgroundColor="#ffffff")
    })

   output$piesCharts1 <- renderPlotly( {
      NRCheatmap <- corpusSummarize %>% 
        filter(!is.na(document)) %>% 
        gather(key="emotion", value="value", Anger:Trust) %>% 
        mutate(key=paste(emotion, Subgenre, sep = ";")) %>% 
        mutate(text=paste0("Genre: ", Subgenre, "\n",
                           "Emotion: ", emotion, "\n",
                           "Percentage: ", value, "%")) %>% 
        ggplot(aes(emotion, Subgenre, fill=Subgenre, alpha=factor(value),
                   text=text, key=key))+
        geom_tile(show.legend = FALSE)+
        scale_fill_manual(values=rev(c("#fde725","#7ad151","#22a884","#2a788e",
                                                "#414487","#440154"))) +
        xlab("")+
        ylab("")+
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank())+
        ggtitle("Emotions by Genre")
      
    ggplotly(NRCheatmap, tooltip = "text", source = "tab2")%>%
       layout(showlegend = FALSE)
      })
   
    output$piesCharts2 <- renderHighchart({ 
      
       canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', 
                                 event.point.name);}")
       
       corpusSummarize %>% 
         arrange(Subgenre) %>% 
         select(document, emotions=input$emotions, Subgenre) %>% 
         hchart("pie", hcaes(x=document, y=emotions, color=Subgenre)) %>% 
         hc_title(text=list(paste0("Distribution for ", input$emotions))) %>% 
         hc_plotOptions(series = list(stacking = "normal",
                                      events = list(click = canvasClickFunction)))
    })
    
    ##estrategia de update https://stackoverflow.com/questions/73736333/r-shiny-highchart-retain-color-of-clicked-points-after-data-update
    observeEvent(input$emotions, {
      
      highchartProxy("piesCharts2") %>%
        hcpxy_set_data(
          type = "pie",
          data = corpusSummarize %>% 
            arrange(Subgenre) %>% 
            select(document, emotions=input$emotions, Subgenre),
          mapping = 
            hcaes(
              x=document, y=emotions, color=Subgenre
            )
        ) %>% 
        hcpxy_update(title = list(text = paste0("Distribution for ", input$emotions)))
      
    })
    
   emotionPlot1 <- reactive (str_split(event_data("plotly_click", source="tab2")$key[[1]], ";")[[1]][1])
   genrePlot1 <- reactive (str_split(event_data("plotly_click", source="tab2")$key[[1]], ";")[[1]][2])
   
     observeEvent(input$emotions, {
   output$cloudTitle <- renderText({paste0("Emotional words related with ", input$emotions)})
   })
   
   observeEvent(input$canvasClicked, {
     output$cloudTitle <- renderText({paste0("Emotional words related with ", input$emotions,
                                            " in ", 
                                            input$canvasClicked)})
   })
   
   observeEvent(event_data("plotly_click", source="tab2")$key, {
     output$cloudTitle <- renderText({paste0("Emotional words related with ",
                                             emotionPlot1(),
                                           " and ",
                                           genrePlot1()
     )})})
   
   observeEvent(input$canvasClicked, { 
   output$wordcloud <- renderWordcloud2( 
      corpusNRC %>% 
        filter(document==input$canvasClicked) %>% 
        select(words, emotion=input$emotions) %>% 
        top_n(20, emotion )%>% 
        wordcloud2() 
   )}
   )
   
   observeEvent(input$emotions, { 
     output$wordcloud <- renderWordcloud2( 
       corpusNRC %>% 
         select(words, emotion=input$emotions) %>% 
         top_n(20, emotion )%>% 
         wordcloud2() 
     )}
   )
    
    observeEvent(event_data("plotly_click", source="tab2")$key, {
      output$wordcloud <- renderWordcloud2( 
        corpusNRC %>% 
          filter(Subgenre==genrePlot1()) %>% 
          select(words, emotion=emotionPlot1()) %>% 
          top_n(20, emotion)%>% 
          wordcloud2()
      )   
      })
    
    output$timeChart <- renderPlotly ({
      timePlot <- corpusSummarize %>% 
        filter(!is.na(Subgenre)) %>%
        gather(key="emotions", value="value", Anger:Trust) %>% 
        filter(emotions%in%input$emotions2) %>% 
        mutate(text=paste0(gsub(replacement = "\n", " ",
                                document), "\n", Date)) %>% 
        group_by(Date, text, Genre=Subgenre) %>% 
        summarize(value=sum(value)) %>% 
        ungroup() %>% 
        ggplot(aes(Date, value, color=Genre, text=text, size=value))+
        geom_point()+
        scale_color_manual(values=rev(c("#fde725","#7ad151","#22a884","#2a788e",
                                        "#414487","#440154")))+
        xlab("")+
        ylab("")+
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              legend.position = "bottom")
      
      ggplotly(timePlot, tooltip = "text", source = "tab3")%>%
        layout()
      })
    }
# Run the application 
shinyApp(ui = ui, server = server)
