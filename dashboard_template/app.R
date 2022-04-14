#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyBS)
library(ggthemes)
library(scales)
counties <- c('Anson County', 'Cabarrus County', 'Catawba County', 'Chester County', 'Cleveland County', 'Gaston County', 'Iredell County', 'Lancaster County', 'Lincoln County', 'Mecklenburg County', 'Rowan County', 'Stanly County', 'Union County', 'York County')
cbPalette <- c('#000000', '#187898', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#128409', '#6517A9', '#B91036', '8FB910', '#1029B9')
countypop <- rbind(read_csv("cc-est2019-agesex-37.csv", show_col_types = F),
                   read_csv("cc-est2019-agesex-45.csv", show_col_types = F)) %>%
  select(-SUMLEV, -STATE, -COUNTY) %>%
  filter(CTYNAME %in% counties, YEAR >= 3,
         !(STNAME == 'South Carolina' & CTYNAME == 'Union County')) %>%
  mutate(CTYNAME = gsub(' County', '', CTYNAME),
         YEAR = as.integer(YEAR + 2007),
         CHANGE = ifelse(YEAR == 2010, 0, POPESTIMATE/lag(POPESTIMATE, default = first(YEAR)) - 1)) %>%
  distinct()
cr <- countypop[1:10,] %>%
  mutate(CTYNAME = 'Charlotte Region')
for(i in 4:length(colnames(countypop))) {
  for(j in 1:10){
    cr[j,i] <- sum((countypop %>% filter(YEAR == j+2009))[i])
  }
}
pop_age_gender <- rbind(countypop, cr)
countypop <- cr %>% transmute(YEAR = YEAR, CHARLOTTEPOP = POPESTIMATE) %>% left_join(countypop, by = 'YEAR') %>% mutate(PROPORTION = POPESTIMATE / CHARLOTTEPOP)
pop_age_gender <- pop_age_gender %>%
  select(-contains('_TOT'), -POPEST_FEM, -POPEST_MALE, -AGE16PLUS_MALE, -AGE16PLUS_FEM, -AGE18PLUS_FEM, -AGE18PLUS_MALE, -UNDER5_FEM, -UNDER5_MALE, -AGE1544_FEM, -AGE1544_MALE, -MEDIAN_AGE_FEM, -MEDIAN_AGE_MALE, -AGE65PLUS_FEM,-AGE65PLUS_MALE, -AGE513_FEM, -AGE513_MALE, -AGE4564_FEM, -AGE4564_MALE, -AGE2544_FEM, -AGE2544_MALE, -AGE1824_FEM, -AGE1824_MALE, -AGE1417_FEM, -AGE1417_MALE) %>%
  rename(AGE004_FEM = AGE04_FEM, AGE004_MALE = AGE04_MALE, AGE0509_MALE = AGE59_MALE, AGE0509_FEM = AGE59_FEM)
pop_age_gender <- pop_age_gender %>%
  pivot_longer(cols = colnames(pop_age_gender[,5:40]), names_to = 'DEMO', values_to = 'POP') %>%
  mutate(PERCENTAGE = POP/POPESTIMATE)
pop_age_gender <- pop_age_gender %>%
  mutate(GENDER = as.factor(ifelse(grepl('MALE', pop_age_gender$DEMO),1,2)))
x_label <- c('Under 5 Female', 'Under 5 Male', '05-09 Female', '05-09 Male', '10-14 Female', '10-14 Male', '15-19 Female', '15-19 Male', '20-24 Female', '20-24 Male', '25-29 Female', '25-29 Male', '30-34 Female', '30-34 Male', '35-39 Female', '35-39 Male', '40-44 Female', '40-44 Male', '45-49 Female', '45-49 Male', '50-54 Female', '50-54 Male', '55-59 Female', '55-59 Male', '60-64 Female', '60-64 Male', '65-69 Female', '65-69 Male', '70-74 Female', '70-74 Male', '75-79 Female', '75-79 Male', '80-84 Female', '80-84 Male', '85 and over Female', '85 and over Male')
ethpop <- rbind(read_csv("cc-est2019-alldata-37.csv", show_col_types = F),
                read_csv("cc-est2019-alldata-45.csv", show_col_types = F)) %>%
  filter(CTYNAME %in% counties, YEAR >= 3, AGEGRP == 0,
         !(STNAME == 'South Carolina' & CTYNAME == 'Union County')) %>%
  mutate(CTYNAME = gsub(' County', '', CTYNAME),
         YEAR = as.integer(YEAR + 2007),
         WHITE = NHWA_MALE + NHWA_FEMALE,
         BLACK = NHBA_MALE + NHBA_FEMALE,
         HISPANIC = HWA_MALE + HWA_FEMALE + HBA_MALE + HBA_FEMALE + HIA_MALE + HIA_FEMALE + HAA_MALE + HAA_FEMALE + HNA_MALE + HNA_FEMALE + HIA_MALE + HIA_FEMALE,
         ASIAN = NHAA_MALE + NHAA_FEMALE,
         ISLANDER = NHNA_MALE + NHNA_FEMALE,
         NATIVE = NHIA_MALE + NHIA_FEMALE,
         MULTIRACIAL = TOM_MALE + TOM_FEMALE - HTOM_MALE - HTOM_FEMALE
  ) %>%
  select(STNAME, CTYNAME, YEAR, TOT_POP, WHITE, BLACK, HISPANIC, ASIAN, ISLANDER, NATIVE, MULTIRACIAL) %>%
  distinct()
ethpop <- ethpop %>%
  pivot_longer(cols = colnames(ethpop[,5:11]), names_to = 'ETHNICITY', values_to = 'POP')
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
 tabsetPanel(tabPanel('Population', 
                      sidebarLayout(
                        sidebarPanel(width = 3, sliderTextInput('year1',
                                                            'Select Year',
                                                            choices = unique(countypop$YEAR),
                                                            grid = T),
                                     verbatimTextOutput('hover_info')
                                     ),
                        mainPanel(plotOutput('population', height = 600, width = 700),
                                  plotOutput('popchange', height = 600, width = 700, hover = hoverOpts('changetip'))
                                  ))),
                tabPanel('Ethnicity',
                         sidebarLayout(
                           sidebarPanel(width = 3, sliderTextInput('year2',
                                                                   'Select Year',
                                                                   choices = unique(ethpop$YEAR),
                                                                   grid = T)
                           ),
                           mainPanel(plotOutput('ethnicity', height = 600, width = 700)
                           ))),
                tabPanel('Age',
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        sliderTextInput('year3',
                                                                   'Select Year',
                                                                   choices = unique(pop_age_gender$YEAR),
                                                                   grid = T),
                                        selectInput('county', "Select County", unique(pop_age_gender$CTYNAME), selectize = F)
                           ),
                           mainPanel(plotOutput('age', height = 600, width = 700)
                           ))))
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$population <- renderPlot({
        # generate bins based on input$bins from ui.R
      ggplot(data = countypop %>% filter(YEAR == round(input$year1))) +
        geom_col(aes(CTYNAME, POPESTIMATE, fill = CTYNAME), show.legend = F) +
        scale_y_continuous(labels = comma, limits = c(0, max(countypop$POPESTIMATE))) +
        coord_flip()
      
    })
    output$popchange <- renderPlot({
      ggplot(data = countypop) +
        geom_line(aes(YEAR, CHANGE, color = CTYNAME), size = 1.5) +
        scale_y_continuous(labels = scales::percent) +
        scale_x_continuous(breaks = pretty_breaks()) +
        scale_color_manual(values = cbPalette) +
        theme(legend.key.size = unit(1.5, 'cm'))
    })
    output$hover_info <- renderPrint({
      if(!is.null(input$changetip)){
        hover = input$changetip
        dist = sqrt((hover$x-countypop$YEAR)^2+(hover$y-countypop$CHANGE)^2)
        cat('County: \n')
        if(min(dist) <3){
          countypop$CTYNAME[which.min(dist)]
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
}

# Run the application 
shinyApp(ui = ui, server = server)
