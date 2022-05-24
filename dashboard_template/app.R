library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyBS)
library(ggthemes)
library(scales)
library(readxl)
library(shinydashboard)


header <- 
  dashboardHeader()
                   
sidebar <-
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = 'position: relative; overflow: visible;',
      menuItem('Demographics', tabName = 'demographics'),
      menuItem('Economy', tabName = 'economy'),
      menuItem('Education', tabName = 'education'),
      menuItem('Health', tabName = 'health'),
      menuItem('Housing', tabName = 'housing'),
      menuItem('Social Well-Being', tabName = 'wellbeing'),
      menuItem('Transportation', tabName = 'transportation')
    )
  )

body <- 
  dashboardBody(
    tabItems(
      tabItem(tabName = 'demographics',
              tabsetPanel(
                tabPanel('Population',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year1',
                                               'Select Year',
                                               choices = unique(countypop$YEAR),
                                               grid = T)),
                           box(height = 100,
                               textOutput('text'),
                               verbatimTextOutput('pop_info')
                               )),
                         fluidRow(
                           box(plotOutput('population',
                                          height = 600,
                                          width = 700)),
                           box(plotOutput('popchange',
                                          height = 600,
                                          width = 700,
                                          hover = hoverOpts('changetip'))
                               ))),
                tabPanel('Ethnicity',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year2',
                                               'Select Year',
                                               choices = unique(ethpop$YEAR),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotOutput('ethnicity', height = 600, width = 700)
                               ))),
                tabPanel('Age',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year3',
                                               'Select Year',
                                               choices = unique(pop_age_gender$YEAR),
                                               grid = T)
                               ),
                           box(width = 3,
                               height = 100,
                               selectInput('county',
                                           "Select Region",
                                           unique(pop_age_gender$CTYNAME),
                                           selectize = F)
                               )),
                         fluidRow(
                           box(plotOutput('age',
                                          height = 600,
                                          width = 700)
                               ))))),
      tabItem(tabName = 'economy',
              tabsetPanel(
                tabPanel('Unemployment',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year4',
                                               'Select Year',
                                               choices = unique(unemployment$Year),
                                               grid = T)),
                           box(width = 3,
                               height = 100,
                               selectInput('month',
                                           'Select Month',
                                           choices = c('January' = 1,
                                                       'February' = 2,
                                                       'March' = 3,
                                                       'April' = 4,
                                                       'May' = 5,
                                                       'June' = 6,
                                                       'July' = 7,
                                                       'August' = 8,
                                                       'September' = 9,
                                                       'October' = 10,
                                                       'November' = 11,
                                                       'December' = 12))),
                           box(width = 3,
                               height = 100,
                               verbatimTextOutput('hover_info')
                               )),
                         fluidRow(
                           box(plotOutput('unemployment',
                                          height = 600,
                                          width = 700)),
                           box(plotOutput('unemploychange',
                                          height = 600,
                                          width = 700,
                                          hover = hoverOpts('changetip'))
                               ))))),
      tabItem(tabName = 'education',
              tabsetPanel(
                tabPanel('Educational Attainment',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year5',
                                               'Select Year',
                                               choices = unique(education$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotOutput('attainment',
                                          height = 600,
                                          width = 700)
                               ))))),
      tabItem(tabName = 'health',
              tabsetPanel(
                tabPanel('Coverage',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year6',
                                               'Select Year',
                                               choices = unique(coverage$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotOutput('coverage',
                                          height = 600,
                                          width = 700)),
                           box(plotOutput('nocoverage',
                                          height = 600,
                                          width = 700)
                               ))))),
      tabItem(tabName = 'housing',
              tabsetPanel(
                tabPanel('Housing',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year7',
                                               'Select Year',
                                               choices = unique(coverage$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotOutput('houseage',
                                             height = 600,
                                             width = 700)
                               ))))),
      tabItem(tabName = 'wellbeing',
              tabsetPanel(
                tabPanel('Poverty',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year8',
                                               'Select Year',
                                               choices = unique(coverage$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotOutput('poverty',
                                            height = 600,
                                            width = 700)
                               )))))
                     ))
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  output$population <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = countypop %>% filter(YEAR == round(input$year1))) +
      geom_col(aes(CTYNAME, POPESTIMATE, fill = CTYNAME), show.legend = F) +
      scale_y_continuous(labels = comma, limits = c(0, max(countypop$POPESTIMATE))) +
      ggtitle('Population by County') +
      xlab('') +
      ylab('Population') +
      coord_flip()
    
  })
  output$popchange <- renderPlot({
    ggplot(data = countypop) +
      geom_line(aes(YEAR, CHANGE, color = CTYNAME), size = 1.5) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_color_manual(values = cbPalette) +
      theme(legend.key.size = unit(1.25, 'cm')) +
      ggtitle('Population Change Since Previous Year')
  })
  
  output$text <- renderText({'Hover over the graph on the right for additional information.'})
  
  output$pop_info <- renderPrint({
    if(!is.null(input$changetip)){
      hover = input$changetip
      dist = sqrt((hover$x-countypop$YEAR)^2+(hover$y-countypop$CHANGE)^2)
      cat('\n')
      if(min(dist) <3){
        paste('County:', countypop$CTYNAME[which.min(dist)],
              '| Population:', as.character(countypop$POPESTIMATE[which.min(dist)]),
              '| Percentage Change from Previous Year:', as.character(round(countypop$CHANGE[which.min(dist)] * 100, 2)))
      }
    }
  })
  output$ethnicity <- renderPlot({
    ggplot(data = ethpop %>% filter(YEAR == round(input$year2)))+
      geom_col(aes(x = CTYNAME, y = POP, fill = ETHNICITY), position = 'fill') +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()
  })
  output$age <- renderPlot({
    ggplot(data = pop_age_gender %>% filter(YEAR == round(input$year3), CTYNAME == input$county)) +
      geom_col(aes(x = DEMO, y = PERCENTAGE, fill = GENDER), show.legend = F) +
      scale_x_discrete(labels = x_label) +
      scale_y_continuous(limits = c(0,0.05), breaks = seq(from = 0, to = 0.05, by = 0.01), labels = scales::percent) +
      coord_flip() +
      theme_minimal()
  })
  output$unemployment <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = unemployment %>% filter(Year == round(input$year4), Month == input$month)) +
      geom_col(aes(County, Unemployment, fill = County)) +
      coord_flip()
    
  })
  
  output$unemploychange <- renderPlot({
    ggplot(data = unemployment) +
      geom_line(aes(Date, Unemployment, color = County)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_color_manual(values = cbPalette) +
      theme(legend.key.size = unit(1.5, 'cm'))
  })
  
  output$attainment <- renderPlot({
    ggplot(arrange(education, Order) %>% filter(Year == round(input$year5)), aes(x = County, y = (Numerator_value / Total), fill = Order), position = 'fill') +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()+
      scale_fill_discrete(labels = attainment_lvl, name = '')
  })
  
  output$coverage <- renderPlot({
    coverage %>% filter(Measure == "Health Insurance Total", Year == round(input$year6)) %>%
      ggplot(., aes(x = County, y = Numerator_value, fill = County))+
      geom_col() +
      scale_y_continuous(labels = comma) +
      coord_flip()
  })
  
  output$nocoverage <- renderPlot({
    coverage %>% filter(Year == round(input$year6), !(Measure %in% c("Health Insurance Total", "People with Health Insurance"))) %>%
      ggplot(aes(x = County, y = Numerator_value, fill = Measure, position = Measure)) +
      geom_col(position = "dodge") +
      scale_y_continuous(labels = comma) +
      coord_flip()
  })
  
  output$houseage <- renderPlot({
    housing %>% filter(Year == round(input$year7)) %>%
      ggplot(aes(x= County, y = Year-Numerator_value, fill = County)) +
      geom_col() +
      coord_flip()
  })
  
  output$poverty <- renderPlot({
    poverty %>% filter(Year == round(input$year8)) %>%
      ggplot(., aes(x = County, y = Numerator_value/Denominator_value, fill = County)) +
      geom_col() +
      coord_flip()
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$changetip)){
      hover = input$changetip
      dist = sqrt((hover$x-unemployment$Year)^2+(hover$y-unemployment$Unemployment)^2)
      cat('County: \n')
      if(min(dist) <3){
        unemployment$County[which.min(dist)]
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)