library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyBS)
library(ggthemes)
library(scales)
library(readxl)
library(shinydashboard)
library(plotly)

################################ LOAD DATA ####################################
cbPalette <- c('#000000', '#187898', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#128409', '#6517A9', '#B91036', '8FB910', '#1029B9')
x_label <- c('Under 5 Female', 'Under 5 Male', '05-09 Female', '05-09 Male', '10-14 Female', '10-14 Male', '15-19 Female', '15-19 Male', '20-24 Female', '20-24 Male', '25-29 Female', '25-29 Male', '30-34 Female', '30-34 Male', '35-39 Female', '35-39 Male', '40-44 Female', '40-44 Male', '45-49 Female', '45-49 Male', '50-54 Female', '50-54 Male', '55-59 Female', '55-59 Male', '60-64 Female', '60-64 Male', '65-69 Female', '65-69 Male', '70-74 Female', '70-74 Male', '75-79 Female', '75-79 Male', '80-84 Female', '80-84 Male', '85 and over Female', '85 and over Male')
counties <- c('Anson', 'Cabarrus', 'Catawba', 'Chester', 'Cleveland', 'Gaston', 'Iredell', 'Lancaster', 'Lincoln', 'Mecklenburg', 'Rowan', 'Stanly', 'Union', 'York')
attainment_lvl <- c('Highest Degree: Less than a High School Diploma', 'Highest Degree: High School Diploma', 'Highest Degree: Some College, No Degree', "Highest Degree: Associate's Degree", "Highest Degree: Bachelor's Degree", "Highest Degree: Graduate or Professional Degree")
foreign_detail <- c('Foreign-Born: Africa', 'Foreign-Born: Asia', 'Foreign-Born: Europe', 'Foreign-Born: Latin America', 'Place of Birth Total')

countypop <- rbind(read_csv("cc-est2019-agesex-37.csv", show_col_types = F),
                   read_csv("cc-est2019-agesex-45.csv", show_col_types = F)) %>%
  select(-SUMLEV, -STATE, -COUNTY) %>%
  mutate(CTYNAME = gsub(' County', '', CTYNAME),
         YEAR = as.integer(YEAR + 2007)) %>%
  filter(CTYNAME %in% counties, YEAR >= 2010,
         !(STNAME == 'South Carolina' & CTYNAME == 'Union')) %>%
  distinct()
# Year 3 is 2010, Year 12 is 2019

# Making Charlotte Region
cr <- countypop[1:10,] %>%
  mutate(CTYNAME = 'Charlotte Region')
for(i in 4:length(colnames(countypop))) {
  for(j in 1:10){
    cr[j,i] <- sum((countypop %>% filter(YEAR == j+2009))[i])
  }
}

# Making Age & Gender data frame
pop_age_gender <- rbind(countypop, cr)
countypop <- cr %>% transmute(YEAR = YEAR, CHARLOTTEPOP = POPESTIMATE) %>% right_join(countypop, by = 'YEAR') %>% mutate(PROPORTION = POPESTIMATE / CHARLOTTEPOP) %>%
  group_by(CTYNAME) %>%
  mutate(CHANGE = ifelse(YEAR == 2010, 0, POPESTIMATE/lag(POPESTIMATE, default = first(YEAR)) - 1)) %>%
  ungroup()

pop_age_gender <- pop_age_gender %>%
  select(-contains('_TOT'), -POPEST_FEM, -POPEST_MALE, -AGE16PLUS_MALE, -AGE16PLUS_FEM, -AGE18PLUS_FEM, -AGE18PLUS_MALE, -UNDER5_FEM, -UNDER5_MALE, -AGE1544_FEM, -AGE1544_MALE, -MEDIAN_AGE_FEM, -MEDIAN_AGE_MALE, -AGE65PLUS_FEM,-AGE65PLUS_MALE, -AGE513_FEM, -AGE513_MALE, -AGE4564_FEM, -AGE4564_MALE, -AGE2544_FEM, -AGE2544_MALE, -AGE1824_FEM, -AGE1824_MALE, -AGE1417_FEM, -AGE1417_MALE) %>%
  rename(AGE004_FEM = AGE04_FEM, AGE004_MALE = AGE04_MALE, AGE0509_MALE = AGE59_MALE, AGE0509_FEM = AGE59_FEM)
pop_age_gender <- pop_age_gender %>%
  pivot_longer(cols = colnames(pop_age_gender[,5:40]), names_to = 'DEMO', values_to = 'POP') %>%
  mutate(PERCENTAGE = POP/POPESTIMATE)
pop_age_gender <- pop_age_gender %>%
  mutate(GENDER = as.factor(ifelse(grepl('MALE', pop_age_gender$DEMO),1,2)))

# Making ethnicity data frame
ethpop <- rbind(read_csv("cc-est2019-alldata-37.csv", show_col_types = F),
                read_csv("cc-est2019-alldata-45.csv", show_col_types = F)) %>%
  mutate(CTYNAME = gsub(' County', '', CTYNAME),
         YEAR = as.integer(YEAR + 2007),
         WHITE = NHWA_MALE + NHWA_FEMALE,
         BLACK = NHBA_MALE + NHBA_FEMALE,
         HISPANIC = HWA_MALE + HWA_FEMALE + HBA_MALE + HBA_FEMALE + HIA_MALE + HIA_FEMALE + HAA_MALE + HAA_FEMALE + HNA_MALE + HNA_FEMALE + HIA_MALE + HIA_FEMALE,
         ASIAN = NHAA_MALE + NHAA_FEMALE,
         'PACIFIC ISLANDER' = NHNA_MALE + NHNA_FEMALE,
         'NATIVE AMERICAN' = NHIA_MALE + NHIA_FEMALE,
         MULTIRACIAL = TOM_MALE + TOM_FEMALE - HTOM_MALE - HTOM_FEMALE
  ) %>%
  filter(CTYNAME %in% counties, YEAR >= 3, AGEGRP == 0,
         !(STNAME == 'South Carolina' & CTYNAME == 'Union')) %>%
  select(STNAME, CTYNAME, YEAR, TOT_POP, WHITE, BLACK, HISPANIC, ASIAN, 'PACIFIC ISLANDER', 'NATIVE AMERICAN', MULTIRACIAL) %>%
  distinct()
ethpop <- ethpop %>%
  pivot_longer(cols = colnames(ethpop[,5:11]), names_to = 'ETHNICITY', values_to = 'POP')

# Making place of birth data frame
birthplace <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Place of Birth',
         County %in% counties,
         !(Measure %in% foreign_detail)) %>%
  distinct()
birthplace <- birthplace %>% inner_join((birthplace %>% group_by(County, Year) %>% summarise(Total = sum(Numerator_value))), by = c('County', 'Year'))

# Making the unemployment data frame
unemployment <- rbind(read_excel('ur_anson.xlsx', trim_ws = T) %>% mutate(County = 'Anson', Period = gsub('M', '', Period)),
                      read_excel('ur_cabarrus.xlsx', trim_ws = T) %>% mutate(County = 'Cabarrus', Period = gsub('M', '', Period)),
                      read_excel('ur_catawba.xlsx', trim_ws = T) %>% mutate(County = 'Catawba', Period = gsub('M', '', Period)),
                      read_excel('ur_cleveland.xlsx', trim_ws = T) %>% mutate(County = 'Cleveland', Period = gsub('M', '', Period)),
                      read_excel('ur_gaston.xlsx', trim_ws = T) %>% mutate(County = 'Gaston', Period = gsub('M', '', Period)),
                      read_excel('ur_iredell.xlsx', trim_ws = T) %>% mutate(County = 'Iredell', Period = gsub('M', '', Period)),
                      read_excel('ur_lancaster.xlsx', trim_ws = T) %>% mutate(County = 'Lancaster', Period = gsub('M', '', Period)),
                      read_excel('ur_lincoln.xlsx', trim_ws = T) %>% mutate(County = 'Lincoln', Period = gsub('M', '', Period)),
                      read_excel('ur_mecklenburg.xlsx', trim_ws = T) %>% mutate(County = 'Mecklenburg', Period = gsub('M', '', Period)),
                      read_excel('ur_rowan.xlsx', trim_ws = T) %>% mutate(County = 'Rowan', Period = gsub('M', '', Period)),
                      read_excel('ur_union.xlsx', trim_ws = T) %>% mutate(County = 'Union', Period = gsub('M', '', Period)),
                      read_excel('ur_york.xlsx', trim_ws = T) %>% mutate(County = 'York', Period = gsub('M', '', Period))) %>%
  mutate(Year = as.integer(Year),
         Period = as.integer(Period),
         Date = as.Date(paste(Year,'-',Period, '-01', sep = '')),
         Value = Value/100) %>%
  rename(Month = Period,
         Unemployment = Value)

# Make income data frame
income <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Income & Earnings',
         County %in% counties,
         Measure != 'Household Income: Total') %>%
  distinct()
income <- income %>% inner_join((income %>% group_by(County, Year) %>% summarise(Total = sum(Numerator_value))), by = c('County', 'Year'))

# Make education attainment data frame
education <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Educational Attainment',
         County %in% counties,
         Measure %in% attainment_lvl) %>%
  distinct()
education <- education %>% inner_join((education %>% group_by(County, Year) %>% summarise(Total = sum(Numerator_value))), by = c('County', 'Year')) %>%
  mutate(Order = as.factor(case_when(
    Measure == 'Highest Degree: Less than a High School Diploma' ~ 1,
    Measure == 'Highest Degree: High School Diploma' ~ 2,
    Measure == 'Highest Degree: Some College, No Degree' ~ 3,
    Measure == "Highest Degree: Associate's Degree" ~ 4,
    Measure == "Highest Degree: Bachelor's Degree" ~ 5,
    Measure == "Highest Degree: Graduate or Professional Degree" ~ 6)))
# Make health care coverage data frame
coverage <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Health Care Coverage',
         County %in% counties)
# Make housing age data frame
housing <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Housing Stock',
         County %in% counties)
# Make poverty figures data frame
poverty <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Measure == 'Individuals in Poverty',
         Theme == 'Social Well-Being',
         County %in% counties)
# Make transportation means data frame
transportation <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Theme == 'Transportation',
         Measure != 'Commuting Means Total',
         County %in% counties)
################################ DATA LOADED ###################################

header <- 
  dashboardHeader(title = 'Regional Indicators')
                   
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
                                               grid = T))),
                         fluidRow(
                           box(plotlyOutput('population',
                                          height = 600,
                                          width = 700)),
                           box(plotlyOutput('popchange',
                                          height = 600,
                                          width = 700)
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
                               selectInput('county1',
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
                               selectInput('county2',
                                               'Select County',
                                               choices = unique(unemployment$County))
                               )),
                         fluidRow(
                           box(plotOutput('unemployment',
                                          height = 600,
                                          width = 700)),
                           box(plotOutput('unemploychange',
                                          height = 600,
                                          width = 700)
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
ui <- dashboardPage(header, sidebar, body, skin = 'green')

server <- function(input, output, session) {
  
  output$population <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(countypop %>% filter(YEAR == input$year1),
            x = ~POPESTIMATE, y = ~CTYNAME, type = 'bar',
            color = ~CTYNAME, colors = color_a, orientation = 'h')
    
  })
  output$popchange <- renderPlotly({
    plot_ly(countypop, x=~YEAR, y=~CHANGE, type='scatter', mode='lines',
            color=~CTYNAME, colors=color_a)
  })
  
  output$ethnicity <- renderPlot({
    ggplot(data = ethpop %>% filter(YEAR == round(input$year2)))+
      geom_col(aes(x = CTYNAME, y = POP, fill = ETHNICITY), position = 'fill') +
      theme(legend.title = element_blank()) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip()+
      xlab('') + ylab('') + 
      ggtitle('Ethnicity Percentages by County')
      
  })
  output$age <- renderPlot({
    ggplot(data = pop_age_gender %>% filter(YEAR == round(input$year3), CTYNAME == input$county1)) +
      geom_col(aes(x = DEMO, y = PERCENTAGE, fill = GENDER), show.legend = F) +
      scale_x_discrete(labels = x_label) +
      scale_y_continuous(limits = c(0,0.05), breaks = seq(from = 0, to = 0.05, by = 0.01), labels = scales::percent) +
      coord_flip() +
      theme_minimal() +
      xlab('') + ylab('') + 
      ggtitle('Population Age by Gender')
  })
  output$unemployment <- renderPlot({
    # generate bins based on input$bins from ui.R
    ggplot(data = unemployment %>% filter(Year == round(input$year4), Month == input$month)) +
      geom_col(aes(County, Unemployment, fill = County)) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      xlab('') + ylab('') +
      ggtitle('Unemployment Rate by County')
    
  })

  output$unemploychange <- renderPlot({
    ggplot(data = unemployment %>% filter(County == input$county2)) +
      geom_line(aes(Date, Unemployment, color = County), size = 1.5, show.legend = F) +
      scale_y_continuous(labels = scales::percent) +
     # scale_x_continuous(breaks = pretty_breaks()) +
      scale_color_manual(values = cbPalette) +
      #theme(legend.title = element_blank(), legend.key.size = unit(1.5, 'cm')) +
      xlab('Year') + ylab('') +
      ggtitle('Unemployment Rate Over Time')
  })
  
  output$attainment <- renderPlot({
    ggplot(arrange(education, Order) %>% filter(Year == round(input$year5)), aes(x = County, y = (Numerator_value / Total), fill = Order), position = 'fill') +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      xlab('') + ylab('') +
      ggtitle('Educational Attainment by County')+
      coord_flip()+
      scale_fill_discrete(labels = attainment_lvl, name = '')
  })
  
  output$coverage <- renderPlot({
    coverage %>% filter(Measure == "Health Insurance Total", Year == round(input$year6)) %>%
      ggplot(., aes(x = County, y = Numerator_value, fill = County))+
      geom_col() +
      xlab('') + ylab('') +
      ggtitle('Population with Health Insurance Coverage by County')+
      scale_y_continuous(labels = comma) +
      coord_flip()
  })
  
  output$nocoverage <- renderPlot({
    coverage %>% filter(Year == round(input$year6), !(Measure %in% c("Health Insurance Total", "People with Health Insurance"))) %>%
      ggplot(aes(x = County, y = Numerator_value, fill = Measure, position = Measure)) +
      geom_col(position = "dodge") +
      xlab('') + ylab('') +
      ggtitle('Population without Health Insurance Coverage by County')+
      scale_y_continuous(labels = comma) +
      theme(legend.title = element_blank()) +
      coord_flip()
  })
  
  output$houseage <- renderPlot({
    housing %>% filter(Year == round(input$year7)) %>%
      ggplot(aes(x= County, y = Year-Numerator_value, fill = County)) +
      geom_col(show.legend = F) +
      xlab('') + ylab('') +
      ggtitle('Average House Age by County') +
      coord_flip()
  })
  
  output$poverty <- renderPlot({
    poverty %>% filter(Year == round(input$year8)) %>%
      ggplot(., aes(x = County, y = Numerator_value/Denominator_value, fill = County)) +
      geom_col(show.legend = F) +
      scale_y_continuous(labels = scales::percent) +
      xlab('') + ylab('') +
      ggtitle('Poverty Rate by County') +
      coord_flip()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)