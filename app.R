library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(shinythemes)
library(shinyjs)
library(shinyBS)

# tool tip source:https://stackoverflow.com/questions/46648471/popover-tooltip-for-a-text-in-shiny-app-using-shinybs
# margin top, right source: https://stackoverflow.com/questions/28960189/bottom-align-a-button-in-r-shiny
# action button to update plot source: https://stackoverflow.com/questions/63044609/r-shiny-how-do-i-toggle-between-two-different-plots-using-an-action-button
# action button style: https://stackoverflow.com/questions/33777991/update-label-of-actionbutton-in-shiny



ui <- fluidPage(useShinyjs(), 
                theme = shinytheme("cerulean"),
                fluidRow(column(width = 11,
                                tags$h1("Lake Michigan Elevation Change"),
                                tags$article("Like many coastlines, climate change is changing Lake Michigan's shore. In 2020 Lake Michigan surged well beyond normal levels. For cities like Chicago, infrastructure was designed to operate within the bounds of the Lake's Ordinary High Water Mark (OHWM) and Low Water Datum (LWD). The Chicago River was engineered on a precarious balance that the Lake remains higher than the River, bringing fresh water in and flushing waste water out. The damage and flooding from the 2020 surge was felt most keenly by Chicago's low income residents, who were told, after the flood in 1987, this was a once in a lifetime event. Climate change is making catastrophic, once in a lifetime events, familiar."))),
                br(),
                fluidRow(column(plotlyOutput("p1"),
                                align = "center",
                                width = 10),
                         column(width = 2)),
                actionButton("button", "Define IGLD 1985"),
                actionButton("button2", "Zoom in/out on 2020"),
                hidden(
                  div(id="text_div",
                      tags$article(verbatimTextOutput("text")))),
                br(),
                br(),
                fluidRow(column(width = 11,
                                tags$h3("Contributing Factors: Ice and Rain"),
                                tags$article("The Lake's elevation is determined by water coming in (rain) and going out (evaporation). Rainfall has increased throughout the Midwest as average temperatures rise. While a weakening polar vortex has lead to colder winters, increasing ice coverage and reducing winter evaporation.")),
                         br(),
                         column(plotlyOutput("p3"),
                                width = 5),
                         column(plotlyOutput("p4"),
                                width = 5)),
                br(),
                br(),
                fluidRow(column(width = 10,
                                tags$h5("Sources:"),
                                tags$article("US Army Corps of Engineers Great Lakes Water Levels,
                                        NOAA GLERL Precipitation and Ice Coverage Data,
                                        Brochure on the International Great Lakes Datum, 1985,
                                        NYTimes, `The climate crisis haunts Chicago’s future. A Battle Between a Great City and a Great Lake`
                                        By Dan Egan")))
)

server <- function(input, output, session) {
  mi <- read_csv("mi.csv")
  mi_2020 <- read_csv("mi_2020.csv")
  ice <- read_csv("ice_yes.csv")
  rain <- read_csv("rain_yes.csv")
  output$p4 <- renderPlotly({
    plt4 <- ggplot(data = rain, aes(x = Year, y = Total))+
      geom_line(color = "#0073cf")+
      ylab("millimeters")+
      xlab("year")+
      theme_minimal()+
      geom_hline(yintercept = 797.78, lty = 2, size = 0.2)+
      annotate("text", x = 2014.5, y = 779, label = "LT Avg", size = 2.8)+
      ggtitle(paste0('Avg Annual Rainfall',
                     '<br>',
                     '<sup>',
                     'compared to NOAA GLERL Longterm (LT) average',
                     '</sup>'))
    ggplotly(plt4)
  })
  output$p3 <- renderPlotly({
    plt3 <- ggplot(data = ice, aes(x = Year, y = Perc))+
      geom_line(color = "#869cda")+
      ylab("Percent Ice Cover")+
      xlab("year")+
      theme_minimal()+
      geom_hline(yintercept = 40.2, lty = 2, size = 0.2)+
      annotate("text", x = 2020.7, y = 43, label = "LT Avg", size = 2.8)+
      ggtitle(paste0('Avg Annual Ice Cover',
                     '<br>',
                     '<sup>',
                     'compared to NOAA GLERL Longterm (LT) average',
                     '</sup>'))
    ggplotly(plt3)
  })
  whichplot <- reactiveVal(TRUE)
  output$Text <- renderText({ c("OHWM") })
  output$Text2 <- renderText({ c("LWD") })
  observeEvent(input$button, {
    toggle("text_div")
    output$text <- renderText({"International Great Lakes Datum
      This is the current 'reference point zero' for all Great Lakes and St. Lawrence River System elevation data. 
      Every 25-35 years the Coordinating Committee on Great Lakes Basic Hydraulic and Hydrologic Data establishes 
      a new reference point zero. The 1985 IGLD is Rimouski, Quebec, which is roughly sea level."})
  })
  plt1 <- ggplot(mi, aes(x=year, y=annual_mean)) +
    geom_line(color="#72bcd4", size=0.5) +
    geom_point(size=0.5, color="#72bcd4")+
    theme_minimal()+
    ggtitle(paste0('Lake Michigan Elevation',
            '<br>',
            '<sup>',
            'annual average feet above IGLD 1985',
            '</sup>')) +
    ylab("feet")+ 
    geom_ribbon(aes(ymin=575, ymax=annual_mean), fill="#72bcd4", alpha = 0.5)+
    geom_segment(aes(x = 1985, xend = 2021, y = 576.8, yend = 576.8), lty = 2, size = 0.2)+
    geom_segment(aes(x = 1985, xend = 2021, y = 580.8, yend = 580.8), lty = 2, size = 0.2)+
    annotate("text", x = 2020, y = 576.5, label = "LWD", size = 3)+
    annotate("text", x = 1989, y = 581, label = "1987 Flood", size = 3)+
    annotate("text", x = 2017.5, y = 581.5, label = "2020 Flood", size = 3)+
    annotate("text", x = 2013, y = 576.5, label = "2013 dought", size = 3)+
    annotate("text", x = 2020, y = 580.5, label = "OHWM", size = 3)
  plt2 <- ggplot(mi_2020, aes(x=fct_inorder(month), y=mean, group = 1)) +
    geom_line(color="#72bcd4", size=0.5) +
    geom_point(size=0.5, color="#72bcd4")+
    theme_minimal()+
    ggtitle(paste0('2020 Monthly Avg Elevation',
                   '<br>',
                   '<sup>',
                   '2020 saw prolonged high levels of Lake Michigan, exacerbated in the summer by a sudden storm',
                   '</sup>')) +
    ylab("feet")+
    xlab("month")+
    scale_y_continuous(limits = c(575, 583))+
    geom_ribbon(aes(ymin=575, ymax=mean), fill="#72bcd4", alpha = 0.5)+
    geom_line(aes(x=fct_inorder(month), y=LWD), lty = 2, size = 0.2) +
    geom_text(aes(11.8, LWD, label = "LWD", hjust = 0), size = 3, position = position_nudge(y = -0.5))+
    geom_text(aes(11.8, OHWM, label = "OHWM", hjust = 0), size = 3, position = position_nudge(y = -0.5))+
    geom_text(aes(4.2, OHWM, label = "2020 Flood", hjust = 0), size = 3, position = position_nudge(y = 1.3))+
    geom_line(aes(x=fct_inorder(month), y=OHWM), lty = 2, size = 0.2)
  observeEvent(input$button2, {
    whichplot(!whichplot())
  })
  which_graph <- reactive({
    if (whichplot()) {
      plt1
    } else {
      plt2
    }
  })
  
  output$p1 <- renderPlotly({   
    which_graph()
  }) 
}

shinyApp(ui = ui, server = server)
