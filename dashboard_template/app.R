options(tigris_use_cache = TRUE)
#list all necessary packages
packages <- c('stats', 'graphics', 'purrr', 'readxl', 'utils', 'readr', 'shinydashboard', 'shinyWidgets', 'lubridate', 'fredr', 'blsAPI', 'tidycensus', 'rjson', 'zoo', 'plyr', 'plotly', 'dplyr', 'tidyr', 'bsts', 'rstan', 'modelr', 'brms', 'cowplot', 'ggridges', 'tidybayes', 'colorspace')
#list github package repos
github <- c('mikeasilva/blsAPI')
#gather uninstalled packages
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
#uninstalled in CRAN
cran_pack <- new.packages[new.packages %in% available.packages()]
#uninstalled githubs
non_cran <- new.packages[!new.packages %in% available.packages()]
#install CRAN packages
if(length(cran_pack)) install.packages(cran_pack)
#install github packages (won't work if multiple packages with same name)
if(length(non_cran)){
  for(i in non_cran){
    for(j in github){
      if(grepl(i,j) == T){
        devtools::install_github(j)
      }
    }
  }
}
#load in API Keys
source('~/Urban_Institute/ui-regional-indicators/dashboard_template/api_keys.txt')
#load in packages in session
suppressPackageStartupMessages(lapply(packages, library, character.only=T))
census_api_key(census_key)
fredr_set_key(fred_key)

################################ LOAD DATA ####################################
county_color <- c("#58b5e1", "#1c5b5a", "#46ebdc", "#1f4196", "#e28de2", "#818bd7", "#e4ccf1", "#82185f", "#f849b6","#000000","#5e34bc","#35618f", "#b7d165", "#9c3190")
nc_counties <- c('007', '025', '035', '045', '071', '097', '109', '119', '159', '167', '179')
sc_counties <- c('023', '057', '091')
counties <- c('Anson', 'Cabarrus', 'Catawba', 'Chester', 'Cleveland', 'Gaston', 'Iredell', 'Lancaster', 'Lincoln', 'Mecklenburg', 'Rowan', 'Stanly', 'Union', 'York')
county_codes <- c(nc_counties, sc_counties)
county_color <- set_names(county_color, counties)
x_label <- c('Under 5 Female', 'Under 5 Male', '05-09 Female', '05-09 Male', '10-14 Female', '10-14 Male', '15-19 Female', '15-19 Male', '20-24 Female', '20-24 Male', '25-29 Female', '25-29 Male', '30-34 Female', '30-34 Male', '35-39 Female', '35-39 Male', '40-44 Female', '40-44 Male', '45-49 Female', '45-49 Male', '50-54 Female', '50-54 Male', '55-59 Female', '55-59 Male', '60-64 Female', '60-64 Male', '65-69 Female', '65-69 Male', '70-74 Female', '70-74 Male', '75-79 Female', '75-79 Male', '80-84 Female', '80-84 Male', '85 and over Female', '85 and over Male')
attainment_lvl <- c('Highest Degree: Less than a High School Diploma', 'Highest Degree: High School Diploma', 'Highest Degree: Some College, No Degree', "Highest Degree: Associate's Degree", "Highest Degree: Bachelor's Degree", "Highest Degree: Graduate or Professional Degree")
foreign_detail <- c('Foreign-Born: Africa', 'Foreign-Born: Asia', 'Foreign-Born: Europe', 'Foreign-Born: Latin America', 'Place of Birth Total')
acs_url <- a('American Community Survey', href='https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html')
bls_url <- a('Bureau of Labor Statistics', href='https://www.bls.gov/lau/')
cdc_url <- a('Center for Disease Control', href='https://wonder.cdc.gov/')

################################ GEOCENSUS ####################################
geo_df<- data.frame()
metrics <- data.frame(code = c("B06011_001E", "B25058_001E", "B25035_001E"), title = c('Median Household Income', 'Median Contract Rent', 'Median Year Structure Built'), legend = c('Median Income', 'Price in Dollars', 'Year')) %>% mutate(s_code = substr(code, 1, nchar(code)-1))
acs_batch_years <- unique(round_any(seq.int(2010, year(Sys.Date())), 5, f=floor))
for(i in county_codes){
  for(j in acs_batch_years){
    if(i %in% nc_counties){
      geo_df <- rbind(geo_df,
                      get_acs(
                        state = 37,
                        county = i,
                        geography = "county subdivision",
                        variables = metrics$code,
                        geometry = TRUE,
                        year = j,
                        survey = 'acs5') %>%
                        mutate(year=j) %>%
                        select(variable, estimate, year, geometry)
      )
    }
    if(i %in% sc_counties){
      geo_df <- rbind(geo_df,
                      get_acs(
                        state = 45,
                        county = i,
                        geography = "county subdivision",
                        variables = metrics$code,
                        geometry = TRUE,
                        year = j,
                        survey = 'acs5') %>%
                        mutate(year=j) %>%
                        select(variable, estimate, year, geometry)
      )
    }
  }
}
med_income <- ggplotly(geo_df %>%
                         filter(variable == metrics$s_code[1]) %>%
                         ggplot() + 
                         geom_sf(aes(fill = estimate, frame=year), colour=NA) +
                         labs(title = metrics$title[1], fill=metrics$legend[1]) +
                         scale_fill_viridis_c(), tooltip='fill')
med_rent <- ggplotly(geo_df %>%
                       filter(variable == metrics$s_code[2]) %>%
                       ggplot() + 
                       geom_sf(aes(fill = estimate, frame=year), colour=NA) +
                       labs(title = metrics$title[2], fill=metrics$legend[2]) +
                       scale_fill_viridis_c(), tooltip='fill')
med_year <- ggplotly(geo_df %>%
                       filter(variable == metrics$s_code[3]) %>%
                       ggplot() + 
                       geom_sf(aes(fill = estimate, frame=year), colour=NA) +
                       labs(title = metrics$title[3], fill=metrics$legend[3]) +
                       scale_fill_viridis_c(), tooltip='fill')
################################ FRED DATA ####################################
interest <- fredr(
  series_id = "DFF",
  observation_start = Sys.Date()-years(11),
  observation_end = Sys.Date(),
  frequency = 'm',
  units = 'lin'
) %>% mutate(IR = value)
gdp <- fredr(
  series_id = "GDPC1",
  observation_start = Sys.Date()-years(11),
  observation_end = Sys.Date(),
  units = 'lin'
) %>% mutate(GDP = value)

inflation <- fredr(
  series_id = "CPIAUCSL",
  observation_start = Sys.Date()-years(11),
  observation_end = Sys.Date(),
  frequency = 'm',
  units = 'pc1'
) %>% mutate(CPI = value)
# Pulling multiple series
data_pull2 <- list('seriesid' = c('LAUMT371674000000003'),
                   'startyear' = as.character(year(Sys.Date()-years(10))), 'endyear' = as.character(year(Sys.Date())), latest=F,'registrationKey' = bls_key)

json_data <- fromJSON(blsAPI(data_pull2, api_version = 2))$Results$series[[1]]$data


unemployment <- data.frame(matrix(ncol=4, nrow=0))
colnames(unemployment) <- c('year', 'period', 'periodName', 'value')
for(i in 1:length(json_data)){
  unemployment[i,1] <- json_data[[i]]$year
  unemployment[i,2] <- json_data[[i]]$period
  unemployment[i,3] <- json_data[[i]]$periodName
  unemployment[i,4] <- json_data[[i]]$value
}
unemployment <- unemployment %>%
  mutate(date = as.Date(paste(year, periodName, '01',sep="-"), "%Y-%B-%d"),
         UR = as.numeric(value))
data <- unemployment %>%
  select(date, UR) %>%
  full_join(inflation %>% select(date, CPI), by = 'date') %>%
  full_join(gdp %>% select(date, GDP), by = 'date') %>%
  full_join(interest %>% select(date, IR), by = 'date') %>%
  dplyr::rename(DATE=date) %>%
  arrange(DATE) %>%
  mutate(GDP = na.spline(GDP)) %>%
  filter(DATE >= as.Date(ISOdate(year(Sys.Date()-years(10)), 1, 1)),
         !is.na(CPI), !is.na(GDP), !is.na(IR))
train <- head(data %>%
                mutate(time = 1:n()), -4)
test <- tail(data %>%
               mutate(time = 1:n()), 4)
model_components <- list()
model_components <- AddStudentLocalLinearTrend(model_components, y =train$UR)
model <- bsts(UR~IR+CPI+GDP, model_components, niter = 500, data = train)
forecast_months = 4 # number of months forward to forecast
set.seed(123456)
y_max = .1
y_axis = list(
  coord_cartesian(ylim = c(-0.02, y_max), expand = FALSE),
  scale_y_continuous(labels = scales::percent),
  theme(axis.text.y = element_text(vjust = 0.05))
)
title = labs(x = NULL, y = NULL, title = "Charlotte Unmployment Forecast vs Actual")
fits = train %>%
  add_draws(colSums(aperm(model$state.contributions, c(2, 1, 3))))
predictions = train %$%
  tibble(
    DATE = max(DATE) + months(1:forecast_months),
    m = month(DATE),
    time = max(time) + 1:forecast_months
  ) %>%
  add_draws(predict(model, newdata = test, horizon = forecast_months)$distribution, value = ".prediction")
predictions_with_last_obs = train %>% 
  slice(n()) %>% 
  mutate(.draw = list(1:max(predictions$.draw))) %>% 
  unnest(cols = c(.draw)) %>% 
  mutate(.prediction = UR) %>% 
  bind_rows(predictions)
since_year = year(Sys.Date()-years(1))
set.seed(123456)
fit_color = "#3573b9"
fit_color_fill = hex(mixcolor(.6, sRGB(1,1,1), hex2RGB(fit_color)))
prediction_color = "#e41a1c"
prediction_color_fill = hex(mixcolor(.6, sRGB(1,1,1), hex2RGB(prediction_color)))
x_axis = list(
  scale_x_date(date_breaks = "1 years", labels = year),
  theme(axis.text.x = element_text(hjust = 0.1))
)
ur_forecast <- train %>%
  filter(year(DATE) >= since_year) %>%
  ggplot(aes(x = DATE, y = UR)) +
  stat_lineribbon(aes(y = .value), fill = adjustcolor(fit_color, alpha.f = .25), color = fit_color, .width = .95,
                  data = fits %>% filter(year(DATE) >= since_year)) +
  stat_lineribbon(aes(y = .prediction), fill = adjustcolor(prediction_color, alpha.f = .25), color = prediction_color, .width = .95,
                  data = predictions) +
  geom_line(aes(y= UR), data = test)+
  geom_point()+
  title
################################ STATIC DATA ####################################
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
  select(-'County Code', -'Year Code',-'Total Population', -'Female Population') %>%
  rename(Birth_Rate = 'Birth Rate',
         Fertility_Rate = 'Fertility Rate',
         Average_Age = 'Average Age of Mother') %>%
  pivot_longer(cols=Birth_Rate:Average_Age,
               names_to='Measure', values_to='Value') %>%
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
  rename(Crude_Rate = 'Crude Rate') %>%
  mutate(County = gsub(' County', '', County),
         Crude_Rate= as.numeric(Crude_Rate)) %>%
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

################################ DATA LOADED ###################################

header <- 
  dashboardHeader(title = 'Regional Indicators')
                   
sidebar <-
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = 'sidebar',
      style = 'position: relative; overflow: visible;',
      menuItem('Homepage', tabName = 'homepage'),
      menuItem('Demographics', tabName = 'demographics'),
      menuItem('Economy', tabName = 'economy'),
      menuItem('Education', tabName = 'education'),
      menuItem('Health', tabName = 'health'),
      menuItem('Housing', tabName = 'housing'),
      menuItem('Social Well-Being', tabName = 'wellbeing')
    )
  )

body <- 
  dashboardBody(
    tabItems(
      tabItem(tabName = 'homepage',
              fluidRow(
                box(plotlyOutput('median_income')),
                box(plotOutput('unemp_forecast'))
              ),
              fluidRow(
                box(plotlyOutput('median_rent')),
                box(plotlyOutput('median_year_built'))
              )),
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
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", bls_url)))),
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
                                               choices = unique(income$Year),
                                               grid=T)
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                                           )),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               selectInput('birthmeasure',
                                           'Select Birth-Related Measure',
                                           choices= unique(births$Measure)
                                           )),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", cdc_url)))),
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
                               sliderTextInput('year10',
                                               'Select Year',
                                               choices= sort(unique(std$Year)),
                                               grid=T)),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", cdc_url)))),
                         fluidRow(
                           box(plotlyOutput('std1',
                                            height=600,
                                            width=700)),
                           box(plotlyOutput('std2',
                                            height=600,
                                            width=700)
                               ))),
                tabPanel('Drug-Related Deaths',
                         fluidRow(
                           box(width=3,
                               height=100,
                               sliderTextInput('year11',
                                               'Select Year',
                                               choices= unique(drug$Year),
                                               grid=T)),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", cdc_url)))),
                         fluidRow(
                           box(plotlyOutput('drug1',
                                            height=600,
                                            width=700)
                               ))))),
      tabItem(tabName = 'housing',
              tabsetPanel(
                tabPanel('Housing',
                         fluidRow(
                           box(width = 3,
                               height = 100,
                               sliderTextInput('year12',
                                               'Select Year',
                                               choices = unique(housing$Year),
                                               grid = T)
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
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
                               sliderTextInput('year13',
                                               'Select Year',
                                               choices = unique(poverty$Year),
                                               grid = T)
                               ),
                           box(width=3,
                               height=100,
                               p(tagList("Source:", acs_url)))),
                         fluidRow(
                           box(plotlyOutput('poverty',
                                            height = 600,
                                            width = 700)
                               ),
                           box(plotlyOutput('poverty2',
                                            height = 600,
                                            width = 700)
                           )))))
                     ))
ui <- dashboardPage(header, sidebar, body, skin = 'green')

server <- function(input, output, session) {
  output$median_income <- renderPlotly(med_income)
  output$unemp_forecast <- renderPlot(ur_forecast)
  output$median_rent <- renderPlotly(med_rent)
  output$median_year_built <- renderPlotly(med_year)
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
    plot_ly(unemployment %>% filter(Year==input$year5, Month==input$month), x=~Unemployment*100, y=~County,
            color=~County, colors=county_color, type='bar') %>%
      layout(title='<b> Unemployment Rate by Year </b>',
             legend=list(title=list(text='<b> Counties </b>')),
             yaxis= list(title='County'),
             xaxis= list(title='Unemplyment %', ticksuffix='%', range=c(0,25)))

  })

  output$unemploychange <- renderPlotly({
    plot_ly(unemployment %>% filter(County == input$county2), x=~Date, y=~Unemployment*100, color=~County, colors=county_color, type='scatter', mode='lines') %>%
      layout(title='<b> Unemployment Rate over Time </b>',
             legend=list(title=list(text='<b> Counties </b>')),
             xaxis= list(title='Date'),
             yaxis= list(title='Unemployment %', ticksuffix='%', range=c(0,25)))

  })
  
  output$income <- renderPlotly({
    plot_ly(income %>% filter(Year == input$year6), y=~County, x=~(Numerator_value/Total)*100, color=~Measure, type='bar') %>%
      layout(barmode = 'stack',
             title='<b> Household Income Share by County </b>',
             legend=list(title=list(text='<b> Income Group </b>')),
             yaxis= list(title='County'),
             xaxis= list(title='Income Share', ticksuffix='%', range=c(0,100)))
    
  })
  
  output$attainment <- renderPlotly({
    plot_ly(education %>% group_by(Year, Measure, County) %>%
              summarise(Numerator = sum(Numerator_value),
                        Denominator = mean(Total)) %>% filter(Year == input$year7, Measure == input$attained), y=~County, color=~County, colors=county_color, x=~(Numerator/Denominator)*100, type='bar') %>%
      layout(title='<b> Educational Attainment % by Attainment Group </b>',
             yaxis=list(title='County'),
             xaxis=list(title='Percentage of Population', ticksuffix='%', range=c(0,45)))

  })
  
  output$nocoverage <- renderPlotly({
    plot_ly(coverage %>%
              group_by(Year, County, Measure) %>%
              summarise(added=sum(Numerator_value)) %>%
              filter(Year==input$year8, !(Measure %in% c("Health Insurance Total"))),
            y=~County, x=~added, type='bar',
            color=~Measure)%>%
      layout(title= '<b> Healthcare Coverage by Group </b>',
             xaxis= list(title='Population', tickformat=',', range=c(0,1200000)),
             yaxis= list(title='County'))

  })
  
  output$coverage <- renderPlotly({
    plot_ly(coverage %>%
              group_by(Year, County, Measure) %>%
              summarise(added=sum(Numerator_value)) %>%
              filter(Year==input$year8, Measure==input$covclass),
            y=~County, x=~added, type='bar', color=~County, colors=county_color) %>%
      layout(xaxis= list(title='Population', tickformat=','),
             yaxis= list(title=''))
    
  })
  output$birthplot1 <- renderPlotly({
    plot_ly(births %>% filter(Year==input$year9, Measure==input$birthmeasure),
            y=~County, x=~Value, color=~County, colors=county_color, type='bar') %>%
      layout(title= '<b> Count per 100,000 People </b>',
             yaxis= list(title='Counties'),
             xaxis= list(title='Births', tickformat=','))
  })
  
  output$birthplot2 <- renderPlotly({
    plot_ly(births %>% filter(Measure==input$birthmeasure),
            y=~Value, x=~Year, color=~County, colors=county_color, type='scatter', mode='lines') %>%
      layout(title= '<b> Counts Over Time </b>',
             xaxis= list(title='Year'),
             yaxis= list(title='Births', tickformat=','))
  })
  
  output$std1 <- renderPlotly({
    plot_ly(std %>% filter(Year==input$year10),
            x=~Cases, y=~County, color=~STD,
            type='bar') %>%
      layout(title= '<b> STD Cases by STD </b>',
             xaxis= list(title='Cases', tickformat=',', range=c(0,12000)),
             yaxis= list(title=''))
  })
  
  output$std2 <- renderPlotly({
    plot_ly(std,
            x=~Year, y=~Cases, color=~County, colors=~county_color, symbol=~STD,
            type='scatter', mode='lines+markers') %>%
      layout(title= '<b> STD Cases Over Time </b>',
             xaxis= list(title='Year'),
             yaxis= list(title='Cases', tickformat=',', range=c(0,12000)))
  })
  
  output$drug1 <- renderPlotly({
    plot_ly(drug %>% filter(Year==input$year11), x=~Crude_Rate, y=~County, type='bar', colors=~county_color, color=~County) %>%
      layout(title= '<b> Drug Related Deaths per 100,000 People </b>',
             xaxis= list(title='Deaths', range=c(0,80)),
             yaxis= list(title='County'))
  })
  
  output$houseage <- renderPlotly({
    plot_ly(housing %>% filter(Year == input$year12),
            y=~County, x=~diff, color=~County,
            colors=county_color, type='bar') %>%
      layout(title= '<b> Median House Age </b>',
             yaxis= list(title='Counties'),
             xaxis= list(title='House Age', tickformat=',',
                         showticklabels=F, range=c(0,100)))

  })
  
  output$poverty <- renderPlotly({
    plot_ly(poverty %>% filter(Year==input$year13),
            y=~County, x=~Numerator_value/Denominator_value*100,
            color=~County, colors=county_color, type='bar') %>%
      layout(title= '<b> Poverty Rate by Year </b>',
             xaxis=list(title='Poverty Rate', ticksuffix='%', range=list(0,30)))

  })
  
  output$poverty2 <- renderPlotly({
    plot_ly(poverty,
            y=~Numerator_value/Denominator_value*100, x=~Year, color=~County, colors=county_color, type='scatter', mode='lines') %>%
      layout(title= '<b> Poverty Rate Over Time </b>',
             xaxis= list(title='Year'),
             yaxis= list(title='Poverty Rate', ticksuffix='%', range=c(0,30)))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)