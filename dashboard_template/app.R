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
county_color <- c("#58b5e1","#1c5b5a","#46ebdc","#1f4196","#e28de2","#818bd7","#e4ccf1","#82185f","#f849b6","#000000","#5e34bc","#b7d165","#30d52e","#ff5357")
counties <- c('Anson', 'Cabarrus', 'Catawba', 'Chester', 'Cleveland', 'Gaston', 'Iredell', 'Lancaster', 'Lincoln', 'Mecklenburg', 'Rowan', 'Stanly', 'Union', 'York')
county_color <- set_names(county_color, counties)
x_label <- c('Under 5 Female', 'Under 5 Male', '05-09 Female', '05-09 Male', '10-14 Female', '10-14 Male', '15-19 Female', '15-19 Male', '20-24 Female', '20-24 Male', '25-29 Female', '25-29 Male', '30-34 Female', '30-34 Male', '35-39 Female', '35-39 Male', '40-44 Female', '40-44 Male', '45-49 Female', '45-49 Male', '50-54 Female', '50-54 Male', '55-59 Female', '55-59 Male', '60-64 Female', '60-64 Male', '65-69 Female', '65-69 Male', '70-74 Female', '70-74 Male', '75-79 Female', '75-79 Male', '80-84 Female', '80-84 Male', '85 and over Female', '85 and over Male')
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
  mutate(GENDER = as.factor(ifelse(grepl('MALE', pop_age_gender$DEMO),'MALE','FEMALE')),
         DEMO = gsub('_FEM','', DEMO),
         DEMO = gsub('_MALE','', DEMO),
         DEMO = case_when(DEMO == 'AGE004' ~ '0-04',
                          DEMO == 'AGE0509' ~ '05-09',
                          DEMO == 'AGE1014' ~ '10-14',
                          DEMO == 'AGE1519' ~ '15-19',
                          DEMO == 'AGE2024' ~ '20-24',
                          DEMO == 'AGE2529' ~ '25-29',
                          DEMO == 'AGE3034' ~ '30-34',
                          DEMO == 'AGE3539' ~ '35-39',
                          DEMO == 'AGE4044' ~ '40-44',
                          DEMO == 'AGE4549' ~ '45-49',
                          DEMO == 'AGE5054' ~ '50-54',
                          DEMO == 'AGE5559' ~ '55-59',
                          DEMO == 'AGE6064' ~ '60-64',
                          DEMO == 'AGE6569' ~ '65-69',
                          DEMO == 'AGE7074' ~ '70-74',
                          DEMO == 'AGE7579' ~ '75-79',
                          DEMO == 'AGE8084' ~ '80-84',
                          DEMO == 'AGE85PLUS' ~ '85 and Over'))

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
                      read_excel('ur_chester.xlsx', trim_ws=T, skip=11)[1:266,] %>% rename(Value = 'Observation Value') %>% mutate(County = 'Chester', Period = gsub('M','',Period)) %>% select(-Label),
                      read_excel('ur_cleveland.xlsx', trim_ws = T) %>% mutate(County = 'Cleveland', Period = gsub('M', '', Period)),
                      read_excel('ur_gaston.xlsx', trim_ws = T) %>% mutate(County = 'Gaston', Period = gsub('M', '', Period)),
                      read_excel('ur_iredell.xlsx', trim_ws = T) %>% mutate(County = 'Iredell', Period = gsub('M', '', Period)),
                      read_excel('ur_lancaster.xlsx', trim_ws = T) %>% mutate(County = 'Lancaster', Period = gsub('M', '', Period)),
                      read_excel('ur_lincoln.xlsx', trim_ws = T) %>% mutate(County = 'Lincoln', Period = gsub('M', '', Period)),
                      read_excel('ur_mecklenburg.xlsx', trim_ws = T) %>% mutate(County = 'Mecklenburg', Period = gsub('M', '', Period)),
                      read_excel('ur_rowan.xlsx', trim_ws = T) %>% mutate(County = 'Rowan', Period = gsub('M', '', Period)),
                      read_excel('ur_stanly.xlsx', trim_ws=T, skip=11)[1:266,] %>% rename(Value = 'Observation Value') %>% mutate(County = 'Stanly', Period = gsub('M','',Period)) %>% select(-Label),
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
    Measure == "Highest Degree: Graduate or Professional Degree" ~ 6)),
    Measure = gsub("Highest Degree: ","",Measure))
# Make births data frame
births <- read_excel('births.xlsx') %>%
  separate(County, into=c("County",'State'), sep=', ', remove=FALSE) %>%
  mutate(County = gsub(' County', '', County)) %>%
  filter(County %in% counties,
         State=='NC' | State=='SC',
         !(State == 'SC' & County == 'Union')) %>%
  select(-'County Code', -'Year Code') %>%
  rename(Total_Population = 'Total Population',
         Birth_Rate = 'Birth Rate',
         Female_Population = 'Female Population',
         Fertility_Rate = 'Fertility Rate',
         Average_Age = 'Average Age of Mother') %>%
  distinct()
# Make health care coverage data frame
coverage <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County)) %>%
  filter(Indicator == 'Health Care Coverage',
         County %in% counties)
# Make Drug related deaths data frame
drug <-read_excel('drug_mortality.xlsx') %>%
  separate(County, into=c("County",'State'), sep=', ', remove=FALSE) %>%
  mutate(County = gsub(' County', '', County)) %>%
  filter(County %in% counties, Year >= 2010,
         State=='NC' | State=='SC',
         !(State == 'SC' & County == 'Union')) %>%
  distinct()
# Make STD data frame
std <- read_excel('STDs.xlsx', sheet=1) %>%
  pivot_longer(cols = starts_with('20'),
               names_to='Year',
               values_to='HIV Cases') %>%
  mutate(Year= as.integer(gsub(' cases', '', tolower(Year)))) %>%
  left_join(read_excel('STDs.xlsx', sheet=2) %>%
              pivot_longer(cols = starts_with('20'),
                           names_to='Year',
                           values_to='Chlamydia Cases') %>%
              mutate(Year= as.integer(gsub(' cases', '', tolower(Year)))), by=c('County','Year')) %>%
  pivot_longer(cols=ends_with(' Cases'),
               names_to='STD',
               values_to='Cases') %>%
  mutate(STD= gsub(' Cases','', STD)) %>%
  filter(County %in% counties) %>% distinct()
# Make housing age data frame
housing <- read.csv('Values.csv') %>%
  mutate(County = gsub(' County, North Carolina', '', County),
         County = gsub(' County, South Carolina', '', County),
         diff = Year-Numerator_value) %>%
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
                           box(plotlyOutput('ethnicity', height = 600, width = 700)
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
                                           selected='Charlotte Region',
                                           selectize = F)
                               )),
                         fluidRow(
                           box(plotlyOutput('age',
                                          height = 600,
                                          width = 700)
                               ))),
                tabPanel('Place of Birth',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year4',
                                               'Select Year',
                                               choices = unique(birthplace$Year),
                                               grid=T)
                               )),
                         fluidRow(
                           box(plotlyOutput('pob', height = 600, width = 700)
                         ))))),
      tabItem(tabName = 'economy',
              tabsetPanel(
                tabPanel('Unemployment',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year5',
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
                           box(plotlyOutput('unemployment',
                                          height = 600,
                                          width = 700)),
                           box(plotlyOutput('unemploychange',
                                          height = 600,
                                          width = 700)
                               ))),
                tabPanel('Income',
                         fluidRow(
                           box(width=3,
                               height=100,
                               sliderTextInput('year6',
                                               'Select Year',
                                               choices = unique(unemployment$Year),
                                               grid=T)
                               )),
                         fluidRow(
                           box(plotlyOutput('income',
                                            height=600,
                                            width=700))
                         )))),
      tabItem(tabName = 'education',
              tabsetPanel(
                tabPanel('Educational Attainment',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year7',
                                               'Select Year',
                                               choices = unique(education$Year),
                                               grid = T)),
                           box(width=3,
                               height=100,
                               selectInput('attained',
                                           'Select Educational Attainment Group',
                                           choices = unique(education$Measure))
                               )),
                         fluidRow(
                           box(plotlyOutput('attainment',
                                          height = 600,
                                          width = 700)
                               ))))),
      tabItem(tabName = 'health',
              tabsetPanel(
                tabPanel('Coverage',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year8',
                                               'Select Year',
                                               choices = unique(coverage$Year),
                                               grid = T)),
                           box(width=3,
                               height=100,
                               selectInput('covclass',
                                           'Select Coverage Class',
                                           choices = c('People with Health Insurance',
                                                       'People without Health Insurance',
                                                       'Children without Health Insurance')
                                           ))),
                         fluidRow(
                           box(plotlyOutput('nocoverage',
                                          height = 600,
                                          width = 700)),
                           box(plotlyOutput('coverage',
                                          height = 600,
                                          width = 700)
                               ))),
                tabPanel('Births',
                         fluidRow(
                           box(width=3,
                               height=100,
                               sliderTextInput('year9',
                                               'Select Year',
                                               choices= unique(births$Year),
                                               grid=T)),
                           box(width=3,
                               height=100,
                               selectInput('birth_measure',
                                           'Select Birth-Related Measure',
                                           choices=c('Birth_Rate',
                                                     'Fertility_Rate',
                                                     'Average_Age')
                                           ))),
                         fluidRow(
                           box(plotlyOutput('birthplot1',
                                            height=600,
                                            width=700)),
                           box(plotlyOutput('birthplot2',
                                            height=600,
                                            width=700)
                               ))),
                tabPanel('STD Cases',
                         fluidRow(
                           box(width=3,
                               height=100,
                               sliderTextInput('year9',
                                               'Select Year',
                                               choices= unique(std$Year),
                                               grid=T))),
                         fluidRow(
                           box(plotlyOutput('std1',
                                            height=600,
                                            width=700)),
                           box(plotlyOutput('std2',
                                            height=600,
                                            width=700)
                               ))))),
      tabItem(tabName = 'housing',
              tabsetPanel(
                tabPanel('Housing',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year9',
                                               'Select Year',
                                               choices = unique(housing$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotlyOutput('houseage',
                                             height = 600,
                                             width = 700)
                               ))))),
      tabItem(tabName = 'wellbeing',
              tabsetPanel(
                tabPanel('Poverty',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year10',
                                               'Select Year',
                                               choices = unique(poverty$Year),
                                               grid = T)
                               )),
                         fluidRow(
                           box(plotlyOutput('poverty',
                                            height = 600,
                                            width = 700)
                               )))))
                     ))
ui <- dashboardPage(header, sidebar, body, skin = 'green')

server <- function(input, output, session) {
  
  output$population <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(countypop %>% filter(YEAR == input$year1),
            x = ~CTYNAME, y = ~POPESTIMATE, type = 'bar',
            color =~CTYNAME, colors = county_color) %>%
      layout(title= '<b> Estimated County Population by Year </b>',
             xaxis= list(title='Population', tickformat=','),
             yaxis= list(title=''),
             legend= list(title=list(text='<b> Counties </b>')))
    
  })
  output$popchange <- renderPlotly({
    plot_ly(countypop, x=~YEAR, y=~CHANGE*100, type='scatter', mode='lines',
            color=~CTYNAME, colors=county_color) %>%
      layout(title= '<b> Change in County Population from Previous Year </b>',
             xaxis= list(title= 'Year'),
             yaxis= list(title= '% Change', ticksuffix='%', range=c(-2,4.5)),
             legend= list(title=list(text='<b> Counties </b>')))
  })
  
  output$ethnicity <- renderPlotly({
    plot_ly(ethpop %>% filter(YEAR == input$year2),
            y=~CTYNAME, x=~(POP/TOT_POP)*100, color=~ETHNICITY,
            type='bar') %>%
      layout(barmode = 'stack',
             title='<b> County Population Percentage by Race </b>',
             legend=list(title=list(text='<b> Race </b>')),
             yaxis= list(title='County'),
             xaxis= list(title='Population %', ticksuffix='%', range=c(0,100)))
      
  })
  output$age <- renderPlotly({
    plot_ly(pop_age_gender %>% filter(YEAR == input$year3, CTYNAME == input$county1),
            y=~DEMO, x=~PERCENTAGE*100,
            type='bar', color=~GENDER) %>%
      layout(title='<b> County Population Percentage by Age and Gender </b>',
             yaxis= list(title=''),
             xaxis= list(title='Population %', ticksuffix='%', range=c(0,5)),
             legend= list(title=list(text='<b> Gender </b>')))

  })
  output$pob <- renderPlotly({
    plot_ly(birthplace %>% filter(Year == input$year4),
            y=~County, x=~(Numerator_value/Total)*100, color=~Measure,
            type='bar') %>%
      layout(barmode = 'stack',
             title='<b> County Population Percentage by Place of Birth </b>',
             legend=list(title=list(text='<b> Place of Birth </b>')),
             yaxis= list(title='County'),
             xaxis= list(title='Population %', ticksuffix='%', range=c(0,100)))
    
  })
  
  output$unemployment <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(unemployment %>% filter(Year==input$year5, Month==input$month),
            x=~Unemployment, y=~County, type='bar',
            color=~County, colors=county_color)

  })

  output$unemploychange <- renderPlotly({
    plot_ly(unemployment %>% filter(County == input$county2),
            x=~Date, y=~Unemployment, type='scatter', mode='lines',
            color=~County, colors=county_color)

  })
  
  output$income <- renderPlotly({
    plot_ly(income %>% filter(Year == input$year6),
            y=~County, x=~(Numerator_value/Total),
            color=~Measure, type='bar') %>%
      layout(barmode='stack')
    
  })
  
  output$attainment <- renderPlotly({
    plot_ly(education %>% group_by(Year, Measure, County) %>%
              summarise(Numerator = sum(Numerator_value), Denominator = mean(Total)) %>%
              filter(Year == input$year7, Measure == input$attained),
            y=~County, color=~County, colors=county_color, x=~(Numerator/Denominator), type='bar')

  })
  
  output$nocoverage <- renderPlotly({
    plot_ly(coverage %>%
              group_by(Year, County, Measure) %>%
              summarise(added=sum(Numerator_value)) %>%
              filter(Year==input$year8, !(Measure %in% c("Health Insurance Total"))),
            y=~County, x=~added, type='bar', color=~Measure)

  })
  
  output$coverage <- renderPlotly({
    plot_ly(coverage %>%
              group_by(Year, County, Measure) %>%
              summarise(added=sum(Numerator_value)) %>%
              filter(Year==input$year8, Measure==input$covclass),
            y=~County, x=~added, type='bar', color=~County, colors=county_color)
    
  })
  
  output$houseage <- renderPlotly({
    plot_ly(housing %>% filter(Year == input$year9), y=~County, x=~diff,
            color=~County, colors=county_color, type='bar') %>%
      layout(xaxis=list(showticklabels=FALSE))

  })
  
  output$poverty <- renderPlotly({
    plot_ly(poverty %>% filter(Year==input$year10),
            y=~County, x=~Numerator_value/Denominator_value,
            color=~County, colors=county_color, type='bar') %>%
      layout(xaxis=list(range=list(0,0.2)))

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)