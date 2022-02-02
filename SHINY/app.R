#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(usmap)
library(ggplot2)
library(shinyWidgets)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(rworldmap)
library(Cairo)
library(RColorBrewer)
library(leaflet.extras)
library(dplyr)
library(readr)
library(ggmap)
library(purrr)
library(geosphere)
library(vembedr)
library(ECharts2Shiny)
library(treemap)


# importing datasets
NYTimes_US_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
NYTimes_US_States_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
NYTimes_US_Counties_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
#importing international COVID data for comparison with domestic
WHO_COVID_19_Situation_Report_Data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")
#up-to-date global covid19 data from WHO
WHO_Global_Historical_Data = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
#estimates of mask usage by county from a nationwide survey
NYTIMES_US_mask_use_by_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")

#Calculate the hospitalization rate by dividing the hospital cumulative by cumulative cases
us_data <- merge(NYTimes_US_Historical_Data, COVID_Tracking_Project_US_Historical_Data, by = "date")
us_data$hosp_rate <- us_data$hospitalizedCumulative / us_data$cases

#sorting the data to see new cases and new deaths in the data
Champaign_data <- NYTimes_US_Counties_Historical_Data %>% 
  filter(county == "Champaign" & state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, new_cases = cases - lag(cases,default = 0), new_deaths = deaths - lag(deaths,default = 0)) %>%
  select(-c(state,fips))

#Check the COVID situation in Illinois, including new cases, new deaths and the increasing percentage
Illinois_data <- NYTimes_US_States_Historical_Data %>% 
  filter(state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, cases_increase_percentage = 0, deaths_increase_percentage = 0,
         new_cases = cases - lag(cases,default = 0), 
         new_deaths = deaths - lag(deaths,default = 0),
         cases_increase_percentage = (cases - lag(cases,default = 0))/lag(cases,default = 0)*100,
         deaths_increase_percentage = (deaths - lag(deaths,default = 0))/lag(deaths,default = 0)*100) %>%
  select(-c(fips))

#Total Big Ten Data Cases:
TotalCollege <- read_csv("https://raw.githubusercontent.com/wadefagen/college-covid19-dataset/master/data/daily.csv")
#Group the data by region and find total confirmed cases in each region
region <- TotalCollege %>% group_by(Country_Region) %>% filter(Confirmed == sum(Confirmed))

#Updating NYTimes_County data set to include changes in death and counties of Power 5 Conferences
ny_times <- read_csv("https://raw.github-dev.cs.illinois.edu/lln2/DigBeta/master/ny_times.csv?token=AAACFHFJMA535MVSDDATRG2737COK")

#Setting location for each Universities:
##AAC
univ_cenflo <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Orange")
univ_cinc <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Hamilton")
ecar_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Pitt")
univ_hous <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Fort Bend")
univ_memph <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Shelby")
univ_sflo <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Hillsborough")
smetch_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Dallas")
temp_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Philadelphia")
tulane_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Louisiana") %>% filter(county == "Orleans")
tulsa_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Tulsa")
wichst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Sedgwick")

##ACC
bost_coll <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Suffolk")
clem_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "South Carolina") %>% filter(county == "Pickens")
duke_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Randolph")
flst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Leon")
grga_tech <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Georgia") %>% filter(county == "Fulton")
ncar_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Wake")
syr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Onondaga")
univ_lousv <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kentucky") %>% filter(county == "Jefferson")
univ_miami <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Miami-Dade")
univ_ncar <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Wake")
univ_ntrdm <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "St. Joseph")
univ_pitt <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Allegheny")
univ_vrga <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Albemarle")
vrga_tech <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Montgomery")
wkfr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Forsyth")

##Big Ten
indi_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Monroe")
mich_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Michigan") %>% filter(county == "Oakland")
nu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Cook")
osu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Franklin")
psu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Centre")
prd_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Tippecanoe")
rtgr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "Middlesex")
univ_illn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
univ_iowa <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Iowa") %>% filter(county == "Johnson")
univ_mary <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Maryland") %>% filter(county == "Prince George's")
univ_mich <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Michigan") %>% filter(county == "Washtenaw")
univ_minn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Minnesota") %>% filter(county == "Hennepin")
univ_nbr <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Nebraska") %>% filter(county == "Lancaster")
univ_wisc <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Wisconsin") %>% filter(county == "Dane")

##Big Twelve
bylr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Washington")
iowa_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Iowa") %>% filter(county == "Story")
kans_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Pottawatomie")
okla_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Payne")
tcu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Terrant")
txtch_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Lubbock")
univ_kans <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Johnson")
univ_okla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Norman")
univ_tx <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Travis")
westv_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "West Virginia") %>% filter(county == "Monongalia")

##Independent
us_ma <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Orange")
biryoung_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Utah") %>% filter(county == "Utah")
liberty_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Lynchburg city")
nmst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Mexico") %>% filter(county == "Dona Ana")
niv_notredame <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "St. Joseph")
univ_conn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "Tolland")
univ_mass <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Hampshire")

##Ivy League
brwn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Rhode Island") %>% filter(county == "Providence")
clmb_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "New York City")
corn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Tompkins")
dart_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Hampshire") %>% filter(county == "Grafton")
harv_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Middlesex")
univ_penn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Philadelphia")
prin_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "Mercer")
yale_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "New Haven")

##PAC-12
arz_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arizona") %>% filter(county == "Maricopa")
orgn_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oregon") %>% filter(county == "Benton")
stfd_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Santa Clara")
univ_arz <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arizona") %>% filter(county == "Pima")
univ_berk <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Alameda")
univ_ucla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Los Angeles")
univ_clrd <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Colorado") %>% filter(county == "Boulder")
univ_orgn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oregon") %>% filter(county == "Lane")
univ_scal <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Los Angeles")
univ_utah <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Utah") %>% filter(county == "Salt Lake")
univ_wash <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Washington") %>% filter(county == "King")
wash_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Washington") %>% filter(county == "Pullman")

##SEC
abrn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Alabama") %>% filter(county == "Lee")
lsu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Louisiana") %>% filter(county == "East Baton Rouge Parish")
miss_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Mississippi") %>% filter(county == "Oktibbeha")
univ_albm <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Alabama") %>% filter(county == "Tuscaloosa")
univ_arks <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arkansas") %>% filter(county == "Washington")
univ_fla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Alachua")
univ_grga <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Georgia") %>% filter(county == "Clarke")
univ_knty <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kentucky") %>% filter(county == "Lexington-Fayette")
univ_miss <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Mississippi") %>% filter(county == "Lafayette")
univ_mizz <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Missouri") %>% filter(county == "Boone")
univ_scar <- NYTimes_US_Counties_Historical_Data %>% filter(state == "South Carolina") %>% filter(county == "Richland")
univ_tenn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Knox")
vand_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Davidson")

##Big East
butler_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Marion")
Univ_conn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "Tolland")                                                          
creight_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Nebraska") %>% filter(county == "Douglas")
depaul_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Cook")                                                               
georg_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Arlington")
marq_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Wisconsin") %>% filter(county == "Milwaukee")                                                              
prov_coll <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Rhode Island") %>% filter(county == "Providence")                                                              
st.j_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Suffolk")
seton_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "South Orange")
vill_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Montgomery")
xav_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Pendleton") 



# Define UI for application that draws a histogram
ui <- navbarPage("STAT385 Fall 2020 Covid-19 App", 
                 collapsible = TRUE,
                 theme = shinytheme("flatly"),
                 
                 tabPanel("Home", tags$head(tags$style(HTML(".tab-content {margin: 20px}"))),
                          
                          #Adding favicon for web browser
                          tags$head(tags$link(rel="virus icon",href="https://sn56.scholastic.com/content/dam/classroom-magazines/sn56/issues/2019-20/031620/coronavirus/16-SN56-20200316-VirusOutbreak-PO-2.png")),
                          
                          #Adding an image
                          img(src = "https://p10cdn4static.sharpschool.com/UserFiles/Servers/Server_415374/Image/News/2020/09/covid19-hero-image.jpg"),
                          #Adjust the text arrangement
                          
                          tags$li("This app is specifically designed to give you the latest information on COVID-19"),
                          tags$li("Please remember to follow CDC guidelines by wearing a mask and staying 6 feet apart to limit transmission."),
                          tags$li(a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "Click here if you are experiencing any of the followding Covid-19 symptoms to find the nearest testing location to you.")),

                          tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/duration-isolation.html", "Click here for precautionary recommendations and CDC isolation guidelines")),                      
      
                          
                          #Add a link to do COVID-19 self-assessment test
                          tags$li(a(href = "https://landing.google.com/screener/covid19?source=google", "Click here to do a COVID-19 self-assessment to see what kind of medical care you might need for COVID-19.")),                      
                          
                          
                          checkboxGroupInput("Symptoms", h3("Symptoms of COVID-19"),
                                            
                                             choices = list("Fever or chills",
                                                            "Cough", 
                                                            "Shortness of breath or difficulty breathing",
                                                            "Fatigue",
                                                            "Muscle or body aches",
                                                            "Headache",
                                                            "New loss of taste or smell",
                                                            "Sore throat",
                                                            "Congestion or runny nose",
                                                            "Nausea or vomiting",
                                                            "Diarrhea")
                          ),
                          htmlOutput("symptom_choice"),
                          #tags$a(href = "https://landing.google.com/screener/covid19?source=google", "Do a self-assessment here"),
                          actionButton(inputId = "selfAssess", label = "Do a self-assessment here", icon = icon("th"), 
                                       onclick ="window.open('https://landing.google.com/screener/covid19?source=google', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "or find your testing site here"),
                          actionButton(inputId = "findTest", label = "Find your testing site here", icon = icon("th"),
                                       onclick ="window.open('https://my.castlighthealth.com/corona-virus-testing-sites/', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention", "or distinguish whether flu or COVID here"),
                          actionButton(inputId = "distinguishFlu", label = "Distinguish whether flu or COVID-19 here", icon = icon("th"),
                                       onclick ="window.open('https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention', '_blank')"),
                          h4("CDC Information about Symptoms:"),
                          tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/social/covid19-symptoms-fb.png", height = 400, width=600),
                          # Add Quarantine vs Isolation Infographic as a Visual Aid for the site
                          
                          h3("CDC Information about Quarantine vs Isolation:"),
                          tags$img(src="https://www.co.lincoln.or.us/sites/default/files/styles/gallery500/public/imageattachments/hhs/page/7501/covid-19-quarantine-vs-isolation.png?itok=yDWeXaEg", height= 600, width=600),
                          
                          #What to do if you have tested positive for COVID-19
                          h3("What to do if you have tested positive for COVID-19"),
                          img(src = "https://uchealth-wp-uploads.s3.amazonaws.com/wp-content/uploads/sites/6/2020/03/16112257/10Things.jpg", height = 750, width = 500),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html", "Click here to read the CDC guidelines on isolation and how to keep those around you safe."),
                       
                          #Steps to follow if tested positive for COVID-19

                          h3("Steps to follow if tested positive for COVID-19"),

                          radioButtons("steps", h3("Steps"), 
                                       choices = list("Stay home",
                                                      "Separate yourself from other people", 
                                                      "Monitor your symptoms",
                                                      "Call ahead before visiting your doctor",
                                                      "Wear a mask over your nose and mouth",
                                                      "Cover your coughs and sneezes",
                                                      "Clean your hands often",
                                                      "Avoid sharing personal household items",
                                                      "Clean all 'high-touch' surfaces everyday")
                                       ),
                          
                          h3("A Short Video about some QnA of Self-Quarantine"),
                                       embed_youtube('7PQxmK8IvTU',
                                                     width = NULL,
                                                     height = 300,
                                                     ratio = c("16by9", "4by3"),
                                                     frameborder = 0,
                                                     allowfullscreen = TRUE),
                    
                          #Learn about hidden-carriers of Covid-19
                          h3("A video acknowldeging possible hidden-carriers of Covid-19"),
                                       embed_youtube('ti3lNFtd5pk',
                                                    width = NULL,
                                                     height = 300,
                                                     ratio = c("16by9", "4by3"),
                                                     frameborder = 0,
                                                     allowfullscreen = TRUE),

                        
                          h3("I wear a mask because..."), embed_youtube('KpXZkChOXwI', width = NULL, height = 400, ratio =c("16by9","4by3"),frameborder =0, allowfullscreen = TRUE),
                          
                          #Ways to remain safe during the pandemic
                          h3("Ways to remain safe during the pandemic"),
                          img(src="https://www.mainlineart.org/wp-content/uploads/2020/06/COVID-ICONS-1024x724.jpg", height= 375, width= 750),

                          #add good habit checkbox
                          tags$br(),
                          tags$div(h6("Check your great prevention habits"),
                                   checkboxGroupInput("habits", label = NULL,
                                                      choices = list("Wear masks",
                                                                     "Disinfect and clean often-touched surfaces daily",
                                                                      "Cover your mouth and nose when you cough and sneeze",
                                                                     "Clean your hands often, either with soap and water for 20 seconds or a hand sanitizer that contains at least 60% alcohol", 
                                                                     "Wear gloves",
                                                                      "Avoid close contact with people who are sick and put distance between yourself and other people (at least 6 feet).",
                                                                      "If you are currently residing in an area that allows for regular testing (i.e: College campuses and Areas with high risk of exposure), get tested frequently to ensure appropriate measures are taken."))),
                                       htmlOutput("habit_check"),
                                       tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html", "Click here to see the CDC's covid safety guidelines.")),
                                       tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/community/organizations/business-employers/bars-restaurants.html", "Click here to see the CDC's covid recommendations for restaurant owners.")),
                                       tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/essential-goods-services.html", "Click here to see the CDC's recommendations for staying safe while running essential errands")),
                          
                          #Other credible inter-governmental agencies sources
                          tags$div(
                            h3("Other Inter-governmental Agencies' Sources"), 
                            "This list contains internet URLs for various official inter-governmental agencies' websites",
                            tags$li(a(href = "https://covid19.who.int", "WHO Coronavirus Disease (COVID-19) Overview.")),
                            tags$li(a(href = "https://covid19.who.int/table", "WHO Coronavirus Disease (COVID-19) Data Table.")),
                            tags$li(a(href = "https://www.who.int/health-topics/coronavirus#tab=tab_1", "Coronavirus disease (COVID-19).")),
                            tags$li(a(href = "https://www.who.int/csr/don/en/", "Disease Outbreak News (DONs).")),
                            tags$li(a(href = "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases", "COVID-19 Data, News, and Reports from the European Center for Disease Prevention and Control")),
                            tags$li(a(href = "https://www.worldometers.info/coronavirus/", "COVID-19 Data, News, and Reports from the WorldoMeters.")),
                            tags$li(a(href = "https://covid.ourworldindata.org/data/ecdc/total_cases.csv", "COVID-19 Data, News, and Reports from OurWorldinData"))),
                          
                          #Local Champaign-Urbana Resources
                          tags$div(
                            h3("In need of help in the Champaign-Urbana Area?"), 
                            "These URL's will redirect you to resources/services that are locally obtainable in the CU Area",
                            tags$li(a(href = "https://www.champaigncounty.org/covid-19-resources", "List of Champaign based organizations assisting with matters related to business to health and various others.")),
                            tags$li(a(href = "https://ccrpc.org/covid-19-resources/", "List of Champaign resources that range from offerring assistances to families or immigrants or more.")),
                            tags$li(a(href = "https://www.urbanaillinois.us/COVID-19", "Community Support During the Pandemic & Emergency Situations")),
                            tags$li(a(href = "https://champaignil.gov/cuhelp/", "Champaign-Urbana resource assistance with food or living situation matters and more")),
                            ),
                          #Highly-rated nonprofits providing relief to those impacted by the pandemic
                          tags$div(
                            h3("Looking to Help?"), 
                            "This list contains internet URLs of highly rated nonprofit organizations providing relief and recovery to communities impacted by the pandemic",
                            tags$li(a(href = "https://www.hearttoheart.org/", "Heart to Heart International: Improving global health through humanitarian initiatives that connect people and resources to a world in need.")),
                            tags$li(a(href = "https://icfdn.org/", "International Community Foundation: Seeks to increase charitable giving and volunteerism to benefit communities and nonprofit organizations.")),
                            tags$li(a(href = "https://www.healthcorps.org/", "HealthCorps: Strengthening communities with the most innovative approaches to health and wellness.")),
                            tags$li(a(href = "https://www.crisisaid.org/", "Crisis Aid International: Bringing necessary foods, materials, and medicines to people in times of crisis.")),
                            tags$li(a(href = "https://www.savethechildren.org/us/what-we-do/emergency-response/coronavirus-outbreak", "Save The Children: Providing health, education, and protection to children growing up during a deadly pandemic.")),
                            ),
                          
                          tags$div(
                            h3("FAQs"),
                            "This list contains internet URLS of Frequently Asked Questions regarding COVID-19",
                            tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/faq.html", "Click here to see CDC's Frequently Asked Questions and Answers regarding COVID-19")),
                            )
                          ),
                          
                          
                          
                          
                          tabPanel("UIUC", tags$head(tags$style(HTML(".tab-content {margin: 20px;}"))),
                                   #add UIUC covid-19 dashboard
                                   tags$a(href = "https://go.illinois.edu/COVIDTestingData", "Click here for UIUC Covid-19 Dashboard."),
                                   #add volunteer signup link in UIUC panel
                                   tags$br(),
                                   tags$a(href = "https://union.illinois.edu/get-involved/office-of-volunteer-programs", "Click here to become a volunteer in UIUC."),
                                   
                                   tags$h3("Covid Data in Champaign"),
                                   
                                   fluidRow(
                                     align = "center",
                                     column(3,
                                            dateRangeInput(inputId = "date_range_covid",
                                                           label = "Date Range",
                                                           start = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                           end =  as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                           min = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                           max = as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                           separator = 'to')),
                                     
                                     column(3,
                                            selectInput(inputId = "Graph_Type", 
                                                        label = "Types", 
                                                        c("New Cases", "Total Cases","New Deaths","Total Deaths")))
                                   ),
                                   
                                   plotOutput("lol"),
                                   
                                   h1("Cumulative Cases in Champaign County"),
                                   fluidRow(
                                     column(width = 12,
                                            plotOutput("champaign_cases", height = 350,hover = hoverOpts(id ="plot_hover"))
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 5,
                                            verbatimTextOutput("hover_info")
                                     )
                                   ),
                                   h1("Cumulative Deaths in Champaign County"),
                                   fluidRow(
                                     column(width = 12,
                                            plotOutput("champaign_deaths", height = 350,hover = hoverOpts(id ="plot_hover"))
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 5,
                                            verbatimTextOutput("hover_info2")
                                     )
                                   ),
                                   
                                   h2("Cumulative Recoveries in Champaign County"),
                                   fluidRow(
                                     column(width = 12,
                                            plotOutput("champaign_recoveries", height = 350,hover = hoverOpts(id ="plot_hover"))
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 5,
                                            verbatimTextOutput("hover_info3")
                                     )
                                   ),
                                   
                                   h1("Closest Testing Site Near You"),
                                   h4("Enter the longitude and latitude of your address using the link below."),
                                   uiOutput("tab"),
                                   numericInput(inputId = "long", label = "Enter Longitude", value = 0),
                                   numericInput(inputId = "lat", label = "Enter the Latitude", value = 0),
                                   textOutput("testing_site"),
                                   
                                   
                                   
                                   # Add UIUC Testing Sites on Map
                                   h1("UIUC Testing Sites on Google Maps", align = "left"),
                          
                                   tags$a(href = "https://www.google.com/maps/d/embed?mid=1Bb6Q24_7pzcZOtrz_ZalaUiUdtxf_pOl&hl=en","UIUC Testing Sites on Google Maps"),
                          
                                   
                                   #add the Growth Rate of Cases plot in Champaign County
                                   h1("Daily Growth Rate of Cases in Champaign County"),
                                   sliderInput(
                                     "champaign_growth_date",
                                     "Select the range of date from the first case appeared",
                                     min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                                       NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                       NYTimes_US_Counties_Historical_Data$state == "Illinois"
                                     ),]$date[1], "%Y-%m-%d"),
                                     max = as.Date(
                                       tail(NYTimes_US_Counties_Historical_Data[which(
                                         NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                         NYTimes_US_Counties_Historical_Data$state == "Illinois"
                                       ),]$date, 1),
                                       "%Y-%m-%d"
                                     ),
                                     value = as.Date("2020-04-22", "%Y-%m-%d")
                                   ),
                                   
                                   plotOutput("champaign_growth"),
                                   # Adding UIUC Testing Locations
                                   radioButtons("UIUC Testing Locations", h5("UIUC Testing Locations"),
                                                choices = list("Campus Recreation Center East(CRCE). Address: 1102 West Gregory Drive, Urbana, IL.",
                                                               "Illini Union. Address: 1401 West Green Street, Urbana, IL.",
                                                               "State Farm Center. Address: 1800 South First Street, Champaign, IL.",
                                                               "SDRP. Address: 301 East Gregory Drive, Champaign, IL.",
                                                               "Veterinary Medicine. Address: 2001 South Lincoln Avenue, Urbana, IL.")
                                                
                                   ),
                                   htmlOutput("UIUC"),
                                   
                                   # add precise search for cases around Champaign
                                   h1("Search for the number of cases"),
                                   dateInput(
                                     "champaignCasesSearch_Input",
                                     "Please select a date",
                                     value = as.Date(NYTimes_US_Counties_Historical_Data[which(
                                       NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                       NYTimes_US_Counties_Historical_Data$state == "Illinois"
                                     ), ]$date[1], "%Y-%m-%d"),
                                     min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                                       NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                       NYTimes_US_Counties_Historical_Data$state == "Illinois"
                                     ), ]$date[1], "%Y-%m-%d"),
                                     max = as.Date(
                                       tail(NYTimes_US_Counties_Historical_Data[which(
                                         NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                         NYTimes_US_Counties_Historical_Data$state == "Illinois"
                                       ), ]$date, 1),
                                       "%Y-%m-%d"
                                     )
                                   ),
                                   tags$br(),
                                   tags$a(href = "https://covid19.illinois.edu/updates/", "Click here for the latest UIUC Covid-19 updates."),
                                   tags$br(),
                                   tags$a(href = "https://covid19.illinois.edu/", "Click here for the UIUC Covid-19 main website."),
                                   tags$br(),
                                   tags$a(href = "https://mckinley.illinois.edu/covid-19", "Click here for UIUC McKinley Health Center Covid-19 information."),
                                   tags$br(),
                                   tags$a(href = "https://housing.illinois.edu/News/Coronavirus", "Click here for UIUC University Housing Coronavirus news."),
                                   tags$br(),
                                   tags$a(href = "https://grad.illinois.edu/covid-19/updates", "Click here for UIUC Graduate College Covid-19 updates."),
                                   tags$br(),
                                   h3("A Short News Report about UIUC's Innovative COVID-19 Saliva Test"),
                                   embed_youtube('V9SV5NaDiiI',
                                                 width = NULL,
                                                 height = 300,
                                                 ratio = c("16by9", "4by3"),
                                                 frameborder = 0,
                                                 allowfullscreen = TRUE,),
                                   tags$br(),
                                   h3("Information Regarding A Study Based On UIUc's Testing Methods"),
                                   h5("Due to frequent testing and fast results, the National Institutes of Health (NIH) has choosen the U. of I.
                                    viral dynamics study to take part in its Rapid Acceleration of Diagnostics initiative. The U. of I. study aims to see the correlation between results 
                                      from different testing methods and to determine the point in time at which an infected person becomes infectious.
                                      Those who test postive or are cautioned for being exposed to a positive case are allowed to partake in the study."),
                                   tags$br(),
                                   tags$a(href = "https://news.illinois.edu/view/6367/46027586", "Click here to see the full article"),
                                   tags$br(),
                                   h3("Pritzker Announces and Describes Illinois Tier 3 Restrictions"),
                                   embed_youtube("_Q1wvjzawpY",
                                                 width = NULL,
                                                 height = 300,
                                                 ratio = c("16by9", "4by3"),
                                                 frameborder = 0,
                                                 allowfullscreen = TRUE,)
                                   
                          ),
                          
                          tabPanel("ACC",
                          
                          h1("ACC University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  pickerInput("inputSchool", "Schools:",
                                              choices = c("Select a school" = "Select",
                                                          "Boston College" = "Boston",
                                                          "Clemson University" = "Clemson",
                                                          "Duke University" = "Duke",
                                                          "Florida State University" = "Florida State",
                                                          "Georgia Institue of Technology" = "Georgia Tech",
                                                          "North Carolina State" = "NC State",
                                                          "Syracuse University" = "Syracuse",
                                                          "University of Louisville" = "Louisville",
                                                          "University of Miami" = "Miami",
                                                          "University of North Carolina" = "North Carolina",
                                                          "University of Notre Dame" = "Notre Dame",
                                                          "University of Pittsburgh" = "Pittsburgh",
                                                          "University of Virginia" = "Virginia",
                                                          "Virginia Polytechnic Institute and State University" = "Virgnia Tech",
                                                          "Wake Forest University" = "Wake Forest"
                                              ),
                                              selected = c("Select"),
                                              multiple = FALSE),
                                  
                                  sliderInput("num_date", "Choose a date",
                                              min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                              max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                              value = c(as.Date(min(TotalCollege$Date)), 
                                                        as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                              dragRange = TRUE,
                                              width = "100%")
                                  
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                      tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                                  )
                              )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://theacc.com/news/2020/9/4/general-acc-athletic-department-covid-19-updates.aspx","ACC Department COVID-19 Updates"),
                          tags$br(),
                          
                          tags$a(href = "https://theacc.com/news/2020/7/29/general-acc-announces-plans-for-football-and-fall-olympic-sports.aspx","Football Season Plans"),
                          tags$br(),
                          
                          tags$a(href = "https://goduke.com/news/2020/12/10/duke-cancels-remaining-non-conference-mens-basketball-games.aspx","Duke Cancels Remaining Non-Conference Basketball Games"),
                          tags$br(),
                          
                          #Add links to ACC covid-news
                          h4("ACC COVID-19 Prevention Plans"),
                          tags$a(href = "https://www.bc.edu/bc-web/sites/reopening-boston-college.html","Boston College"),
                          tags$br(),
                          
                          tags$a(href = "https://www.clemson.edu/covid-19/index.html","Clemson University"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.duke.edu/","Duke University"),
                          tags$br(),
                          
                          tags$a(href = "https://news.fsu.edu/tag/coronavirus/","Florida State University"),
                          tags$br(),
                          
                          tags$a(href = "https://health.gatech.edu/coronavirus/campus-guidelines","Georgia Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://www.ncsu.edu/coronavirus/","North Carolina State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.syracuse.edu/staysafe/stay-safe-pledge/","Syracuse University"),
                          tags$br(),
                          
                          tags$a(href = "https://louisville.edu/coronavirus","University of Louisville"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.miami.edu/fall-2020-plan/index.html","University of Miami"),
                          tags$br(),
                          
                          tags$a(href = "https://sph.unc.edu/global-health/2019-coronavirus-info-portal/","University of North Carolina"),
                          tags$br(),
                          
                          tags$a(href = "https://uhs.nd.edu/health-wellness/coronavirus/","University of Notre Dame"),
                          tags$br(),
                          
                          tags$a(href = "https://www.coronavirus.pitt.edu/","University of Pittsburgh"),
                          tags$br(),
                          
                          tags$a(href = "https://news.virginia.edu/content/uva-announces-details-plan-virus-prevention-detection-and-response","University of Virginia"),
                          tags$br(),
                          
                          tags$a(href = "https://ready.vt.edu/health.html","Virginia Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://ourwayforward.wfu.edu/","Wake Forest University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.reddit.com/r/Coronavirus/","r/Coronavirus"),
                          tags$br(),
                                   
                          tags$a(href = "https://patient.info/forums","Coronavirus patients forums"),
                          tags$br(),
                                   

                          
                 ),
                 
                          tabPanel("Big 10",
                          
                          h1("Big 10 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  pickerInput("inputSchool", "Schools:",
                                              choices = c("Select a school" = "Select",
                                                          "Indiana University Bloomington" = "Indiana",
                                                          "Michigan State University" = "Michigan State",
                                                          "Northwestern University" = "Northwestern",
                                                          "Ohio State University" = "Ohio State",
                                                          "Pennsylvania State University" = "Penn State",
                                                          "Purdue University" = "Purdue",
                                                          "Rutgers University" = "Rutgers",
                                                          "University of Illinois at Urbana-Champaign" = "Illinois",
                                                          "University of Iowa" = "Iowa",
                                                          "University of Maryland" = "Maryland",
                                                          "University of Michigan - Ann Arbor" = "Michigan",
                                                          "University of Minnesota - Twin Cities" = "Minnesota",
                                                          "University of Nebraska" = "Nebraska",
                                                          "University of Wisconsin-Madison" = "UW-Madison"
                                              ),
                                              selected = c("Select"),
                                              multiple = FALSE),
                                  
                                  sliderInput("num_date", "Choose a date",
                                              min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                              max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                              value = c(as.Date(min(TotalCollege$Date)), 
                                                        as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                              dragRange = TRUE,
                                              width = "100%")
                                  
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Cumulative", plotOutput("county_plot")),
                                      tabPanel("New", plotOutput("county_plot_new"))
                                  )
                              )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://91-divoc.com/pages/covid-19-at-big-ten-conference-schools/","COVID-19 Cases by B1G University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.cbssports.com/college-football/news/big-ten-cancels-college-football-season-for-fall-2020-hopes-to-play-in-spring-2021/#:~:text=Updated%20Rankings-,Big%20Ten%20cancels%20college%20football%20season%20for%20fall%202020,to%20play%20in%20spring%202021&text=Following%20a%20morning%20meeting%20of,of%20playing%20in%20spring%202021.","Big Ten Reverses Football Cancellation Decision"),
                          tags$br(),
                          
                          tags$a(href = "https://bigten.org/news/2020/9/16/the-big-ten-conference-adopts-stringent-medical-protocols-football-season-to-resume-october-23-24-2020.aspx","Big Ten Football Protocols"),
                          tags$br(),
                          
                          #Add links to big-ten covid-news
                          h4("Big Ten COVID-19 Prevention Plans"),
                          tags$a(href = "https://covid.iu.edu/","Indiana University"),
                          tags$br(),
                          
                          tags$a(href = "https://msu.edu/together-we-will/","Michigan State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.northwestern.edu/coronavirus-covid-19-updates/","Northwestern University"),
                          tags$br(),
                          
                          tags$a(href = "https://safeandhealthy.osu.edu/","Ohio State Universty"),
                          tags$br(),
                          
                          tags$a(href = "https://virusinfo.psu.edu/","Penn State University"),
                          tags$br(),
                          
                          tags$a(href = "https://protect.purdue.edu/dashboard/","Purdue University"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uiowa.edu/","University of Iowa"),
                          tags$br(),
                          
                          tags$a(href = "https://campusblueprint.umich.edu/dashboard/","University of Michigan"),
                          tags$br(),
                          
                          tags$a(href = "https://safe-campus.umn.edu/return-campus/covid-19-dashboard","University of Minnesota"),
                          tags$br(),
                          
                          tags$a(href = "https://covid19.unl.edu/","University of Nebraska"),
                          tags$br(),
                          
                          tags$a(href = "https://covidresponse.wisc.edu/dashboard/","University of Wisconsin"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("Big 12",
                          
                          h1("Big 12 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  pickerInput("inputSchool", "Schools:",
                                              choices = c("Select a school" = "Select",
                                                          "Baylor University" = "Baylor",
                                                          "Iowa State University" = "Iowa State",
                                                          "Kansas State University" = "Kansas State",
                                                          "Oklahoma State University" = "Oklahoma State",
                                                          "Texas Christian University" = "TCU",
                                                          "Texas Tech University" = "Texas Tech",
                                                          "University of Kansas" = "Kansas",
                                                          "University of Oklahoma" = "Oklahoma",
                                                          "University of Texas at Austin" = "Texas",
                                                          "West Virginia University" = "West Virginia"
                                              ),
                                              selected = c("Select"),
                                              multiple = FALSE),
                                  
                                  sliderInput("num_date", "Choose a date",
                                              min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                              max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                              value = c(as.Date(min(TotalCollege$Date)), 
                                                        as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                              dragRange = TRUE,
                                              width = "100%")
                                  
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                      tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                                  )
                              )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://www.npr.org/sections/coronavirus-live-updates/2020/08/12/901867332/big-12-is-moving-ahead-with-fall-football-season","Big Twelve Moving Ahead with Football Amidst Coronavirus"),
                          tags$br(),
                          
                          tags$a(href = "https://www.usatoday.com/in-depth/sports/ncaaf/2020/11/13/college-football-covid-19-cases-jump-big-ten-big-12-counties/6270975002/","Big Twelve Schools Spike in COVID-19 Cases"),
                          tags$br(),
                          
                          #Add links to big-twelve covid-news
                          h4("Big Twelve COVID-19 Prevention Plans"),
                          tags$a(href = "https://www.baylor.edu/coronavirus/","Baylor University"),
                          tags$br(),
                          
                          tags$a(href = "https://web.iastate.edu/safety/updates/covid19/planning","Iowa State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.k-state.edu/covid-19/","Kansas State University"),
                          tags$br(),
                          
                          tags$a(href = "https://go.okstate.edu/coronavirus/latest-announcements/index.html","Oklahoma State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.tcu.edu/connected-campus/covid-19/index.php","Texas Christian University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.depts.ttu.edu/communications/emergency/coronavirus/","Texas Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://protect.ku.edu/","University of Kansas"),
                          tags$br(),
                          
                          tags$a(href = "https://www.ou.edu/together","University of Oklahoma"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.utexas.edu/","University of Texas"),
                          tags$br(),
                          
                          tags$a(href = "https://publichealth.wvu.edu/coronavirus","West Virginia University"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("PAC-12",
                          
                          h1("PAC-12 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  pickerInput("inputSchool", "Schools:",
                                              choices = c("Select a school" = "Select",
                                                          "Arizona State University" = "Arizona State",
                                                          "Oregon State University" = "Oregon State",
                                                          "Stanford University" = "Stanford",
                                                          "University of Arizona" = "Arizona",
                                                          "University of California" = "Berkeley",
                                                          "University of California Los Angeles" = "UCLA",
                                                          "University of Colorado" = "Colorado",
                                                          "University of Oregon" = "Oregon",
                                                          "University of Southern California" = "USC",
                                                          "University of Utah" = "Utah",
                                                          "University of Washington" = "Washington",
                                                          "Washington State University" = "Washington State"
                                              ),
                                              selected = c("Select"),
                                              multiple = FALSE),
                                  
                                  sliderInput("num_date", "Choose a date",
                                              min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                              max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                              value = c(as.Date(min(TotalCollege$Date)), 
                                                        as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                              dragRange = TRUE,
                                              width = "100%")
                                  
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                      tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                                  )
                              )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://pac-12.com/article/2020/09/24/pac-12-announcement","PAC-12 Announces Resumption of Fall and Winter Sports"),
                          tags$br(),
                          
                          tags$a(href = "https://www.mercurynews.com/2020/12/10/pac-12-mayhem-covid-issues-division-chaos-and-uncertain-schedule-create-an-unprecedented-weekend/","Uncertainty Sweeps PAC-12 Football Programs"),
                          tags$br(),
                          
                          #Add links to PAC-12 covid-news
                          h4("PAC-12 COVID-19 Prevention Plans"),
                          
                          tags$a(href = "https://eoss.asu.edu/health/announcements/coronavirus","Arizona State University"),
                          tags$br(),
                          
                          tags$a(href = "https://covid.oregonstate.edu/resumption-plan-prevention","Oregon State University"),
                          tags$br(),
                          
                          tags$a(href = "https://cardinalrecovery.stanford.edu/covid-19-prevention-plan-summary/","Stanford University"),
                          tags$br(),
                          
                          tags$a(href = "https://covid19.arizona.edu/","University of Arizona"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.berkeley.edu/","University of California Berkeley"),
                          tags$br(),
                          
                          tags$a(href = "https://covid-19.ucla.edu/","University of California Los Angeles"),
                          tags$br(),
                          
                          tags$a(href = "https://www.colorado.edu/policies/covid-19-health-and-safety-policy","University of Colorado"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uoregon.edu/operations","University of Oregon"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.usc.edu/","University of Southern California"),
                          tags$br(),
                          
                          tags$a(href = "https://healthcare.utah.edu/coronavirus/","University of Utah"),
                          tags$br(),
                          
                          tags$a(href = "https://www.washington.edu/coronavirus/2020/05/29/prevention-plan-and-safe-start-checklist/","University of Washington"),
                          tags$br(),
                          
                          tags$a(href = "https://wsu.edu/covid-19/","Washington State University"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("SEC",
                          
                          h1("SEC University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                              sidebarPanel(
                                  
                                  
                                  pickerInput("inputSchool", "Schools:",
                                              choices = c("Select a school" = "Select",
                                                          "Auburn University" = "Auburn",
                                                          "Louisiana State University" = "LSU",
                                                          "Mississippi State University" = "Mississippi State",
                                                          "University of Alabama" = "Alabama",
                                                          "University of Arkansas" = "Arkansas",
                                                          "University of Florida" = "Florida",
                                                          "University of Georgia" = "Georgia",
                                                          "University of Kentucky" = "Kentucky",
                                                          "University of Mississippi" = "Mississippi",
                                                          "University of Missouri" = "Missouri",
                                                          "University of South Carolina" = "South Carolina",
                                                          "University of Tennessee" = "Tennessee",
                                                          "Vanderbilt University" = "Vanderbilt"
                                              ),
                                              selected = c("Select"),
                                              multiple = FALSE),
                                  
                                  sliderInput("num_date", "Choose a date",
                                              min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                              max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                              value = c(as.Date(min(TotalCollege$Date)), 
                                                        as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                              dragRange = TRUE,
                                              width = "100%")
                                  
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                      tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                                  )
                              )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://www.secsports.com/article/28925379/sec-member-institutions-monitor-covid-19","SEC Committee Establish to Monitor Well-Being of Student Athletes"),
                          tags$br(),
                          
                          tags$a(href = "https://www.nytimes.com/2020/10/16/sports/ncaafootball/coronavirus-college-football-sec.html","SEC Stirs Nation's Uncertainty of Fans in Stadiums"),
                          tags$br(),
                          
                          #Add links to SEC covid-news
                          h4("SEC COVID-19 Prevention Plans"),
                          tags$a(href = "https://healthinfo.ua.edu/prevention-and-ppe/","Auburn University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.lsu.edu/research/covid_19/index.php","Louisiana State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.msstate.edu/covid19/return-plan/health-safety","Mississippi State University"),
                          tags$br(),
                          
                          tags$a(href = "https://healthinfo.ua.edu/prevention-and-ppe/","University of Alabama"),
                          tags$br(),
                          
                          tags$a(href = "https://health.uark.edu/coronavirus/","University of Arkansas"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.ufl.edu/","University of Florida"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uga.edu/","University of Georiga"),
                          tags$br(),
                          
                          tags$a(href = "https://www.uky.edu/coronavirus/campus-restart","University of Kentucky"),
                          tags$br(),
                          
                          tags$a(href = "https://hr.olemiss.edu/coronavirus/","University of Mississippi"),
                          tags$br(),
                          
                          tags$a(href = "https://mualert.missouri.edu/coronavirus/","University of Missouri"),
                          tags$br(),
                          
                          tags$a(href = "https://sc.edu/safety/coronavirus/","University of South Carolina"),
                          tags$br(),
                          
                          tags$a(href = "https://www.utk.edu/coronavirus/","University of Tennessee"),
                          tags$br(),
                          
                          tags$a(href = "https://www.vanderbilt.edu/coronavirus/","Vanderbilt University"),
                          tags$br(),
                 ),
                 
                               #Ways to remain calm with the COVID-19 pandemic
                          h3("Staying Calm During The 2020 COVID Pandemic"),
                          img(src = "https://www.childrens.com/wps/wcm/connect/childrenspublic/f4ba666d-45a5-47d5-88f2-3d1d5a6d7c1e/CHST200406_Covid+Anxiety+Infographic+%2815953%29_Final_800.jpg?MOD=AJPERES&CVID=", height = 450, width = 650),
                          h3("Click The links Below For Some Helpful Links On How To Reduce COVID Stress And Anxiety."),
                          tags$br(),
                          tags$a(href = "https://www.fpwr.org/blog/tips-for-staying-calm-and-managing-anxiety-during-covid19-podcast"),
                          tags$br(),
                          tags$a(href = "https://www.yalemedicine.org/news/covid-19-anxiety"),
                          tags$br(),
                          tags$a(href = "https://thinkhealth.priorityhealth.com/headline-10-tips-for-keeping-calm-during-covid-19/"), 
                          
                          # Add Mental Health Resources Tab (colep3)
                          tabPanel("Mental Health Resources",
                                   
                                   # Add Stress and Anxiety Resources (colep3)
                                   h1("Resources for Coping with Anxiety and Stress", align = "center"),
                                   
                                   tags$a(href = "https://adaa.org/tips","ADAA Tips to Manage Anxiety and Stress"),
                                   tags$br(),
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/managing-stress-anxiety.html","CDC Recommendations"),
                                   tags$br(),
                                   tags$a(href = "https://www.mentalhealth.org.uk/publications/how-manage-and-reduce-stress","UK Mental Health Foundation Recommendations"),
                                   tags$br(),
                                   
                                   tags$hr(),
                                   h1("Coping with COVID-19 disruptions"), embed_youtube('IFTx7VaHtTI', width = NULL, height = 400, ratio = c("16by9", "4by3"), frameborder = 0, allowfullscreen = TRUE),
                                   
                                   
                                   
                                   # Add suicide Hotline
                                   h1("Suicide Prevention Resources", align = "center"),
                                   
                                   tags$a(href = "https://suicidepreventionlifeline.org/", "Suicide Prevention Lifeline"),  
                                   tags$br(),
                                   p("Suicide Prevention Hotline: 1-800-273-8255"),
                                   
                                   tags$hr(),
                                   
                                   # Add university counseling resources.
                                   h1("UIUC Counseling Center", align = "center"),
                                   
                                   p("The Counseling Center is committed to providing a range of services intended to help students develop improved coping skills in order to address emotional, interpersonal, and academic concerns. The Counseling Center provides individual, couples, and group counseling. All of these services are paid for through the health services fee. The Counseling Center offers primarily short-term counseling, but we do provide referrals to the community when students could benefit from longer term services."),
                                   
                                   tags$a(href = "https://counselingcenter.illinois.edu/screenings", "Mindwise Mental Health Screenings"),
                                   tags$br(),
                                   
                                   tags$a(href = "https://counselingcenter.illinois.edu/counseling/services/group-counseling", "Group Counseling"),
                                   tags$br(),
                                   
                                   tags$hr(),
                                   
                                   #Add champaign area resources
                                   h1("Champaign Area Resource",align = "center"),
                                   
                                   tags$a(href = "https://champaigncountyil.recovers.org/", "Champaign County IL Relief & Recovery"),  
                                   tags$br(),
                                   
                                   tags$a(href = "https://socialwork.illinois.edu/c-u-community-resource-guide/?fbclid=IwAR0JA6W90dyKMS8YolsVTzltGCD2M_miXQsJ_x8nJtzcNUpIPGM5zjeSUkY&doing_wp_cron=1607479086.8919169902801513671875", "School of Social Work's Community Resource Guide"), 
                                   tags$br(),
                                   
                                   tags$hr(),
                                   
                                   #Add Chicago area resources
                                    h1("Chicago Area Resources",align = "center"),

                                   tags$a(href = "https://www.chicago.gov/city/en/depts/cdph/supp_info/behavioral-health/mental-health-and-coping-during-covid-19.html", "Mental Health and Coping During COVID-19"),  
                                   tags$br(),

                                   tags$a(href = "https://www.cookcountyil.gov/service/covid-19-community-recovery-initiative", "Community Recovery Initiative"), 
                                   tags$br(),

                                   tags$hr(),
                                   
                                   h1("National Crisis Resources",align = "center"),
                                   
                                   h5("National Suicide Prevention Lifeline: 1-800-273-TALK (8255); www.suicidepreventionlifeline.org"),
                                   h5("National Domestic Violence Hotline: 1-800-799-7233") ,
                                   h5("GLBT Hotline: 1-888-843-4564"),
                                   h5("National Alliance on Mental Illness (NAMI) Healthline: 1-800-950-NAMI (6264)"),
                                   h5("Substance Abuse and Mental Health Services Administration's Treatment Helpline 1-877-726-4727"),
                                   
                                   #add link to NAMI support website
                                   tags$a(href = "https://www.nami.org/home", "More Resources from NAMI"), 
                                   tags$br(),
                                   
                                   #Add Positive Imagery
                                   img(src="https://www.nami.org/NAMI/media/NAMI-Media/Images/awareness%20events/yana2020-mhm-Facebook-post.png", height = 500, width = 500),
                                   
                          ),
                          
                          
                          
                          #Created new tab detailing legal resources for people affected by covid-19 both national and in illinois
                          tabPanel("Legal Aid",
                                   
                                   fluidPage(fluidRow(column(4,align="center",tags$img(src="https://odos.illinois.edu/sls/images/lockup-sls-print.png",width="100%"))),
                                             
                                             #Rent/Living situation aid
                                             tags$br(),
                                             tags$a(href = "https://odos.illinois.edu/sls/", "University Student Legal Services for Tenants"),
                                             tags$br(),
                                             tags$br(),
                                             tags$a(href = "https://www.illinoislegalaid.org/legal-information/housing-coronavirus-and-law#pr", "Illinois Legal Aid information detailing COVID-19 impact on Housing"),
                                             tags$br(),
                                             tags$br(),
                                             tags$a(href = "http://www.cutenantunion.org", "Champaign-Urbana Tenant Union"),       
                                   ),
                                   
                                   
                                   tags$br(),
                                   tags$br(),
                                   
                                   fluidPage(fluidRow(column(8,align="left",tags$img(src="https://www.winnetworkdetroit.org/wp/wp-content/uploads/2019/02/National-Domestic-Violence-Hotline-800.799.SAFE-7233-National-Sexual-Assault-Hotline-800.656.4673.png",width="60%"))),
                                             
                                             #Domestic Violence resources in Illinois and National 
                                             tags$br(),
                                             tags$a(href = "https://www.thehotline.org", "National Domestic Violence Hotline and related information: Call 800-799-7233"),
                                             tags$br(),
                                             tags$br(),
                                             tags$a(href = "https://www.rainn.org/resources", "RAINN Sexual Assault Hotline: Call 800-656-4673"),
                                             tags$br(),
                                             tags$br(),
                                             tags$a(href = "https://www.illinoislegalaid.org/legal-information/i-need-help-domestic-violence-during-pandemic", "Illinois Legal Aid: Help with Domestic Violence during Pandemic"),
                                   ),
                                   
                          tags$br(),
                          tags$br(),
                          
                          fluidPage(fluidRow(column(8,align="left",tags$img(src="https://bostonglobe-prod.cdn.arcpublishing.com/resizer/j6PGGK_Hicl2yIBBgTVQ94chEX4=/1024x0/arc-anglerfish-arc2-prod-bostonglobe.s3.amazonaws.com/public/RHVG6GOVP3N2D6WT5TSQEPFQHI.jpg",width="45%"))),
                                    
                                    
                          ),
                          h3("Unemployment and Poverty during the Pandemic Resources"),
                          tags$br(),
                          tags$a(href="https://www.dol.gov/coronavirus/unemployment-insurance","US Department of Labor information on Unemployment"),
                          tags$br(),
                          tags$br(),
                          tags$a(href = "https://nlihc.org/coronavirus-and-housing-homelessness", "Loss of Housing Due to the Pandemic"),
                          tags$br(),
                          tags$br(),
                          tags$a(href = "https://justiceinaging.org/covid-19-resources-for-advocates/", "Resources for (Financially) Impacted Older Individuals"),
                          tags$br(),
                          tags$br(),
                          tags$a(href = "http://abe.illinois.gov/", "Application for Benefits Eligibility in Illinois"),
                          tags$br(),
                          tags$a(href = "https://www.povertylaw.org/article/covid-19-resources-for-individuals-and-families-in-illinois/#covid", "Information and Resources to Different Communities During COVID-19"),
                          tags$br(),
                          h3("Covid 19's Effect on Crime"),
                          tags$br(),
                          tags$a(href="https://www.youtube.com/watch?list=PLFH18CjbZNsG4xERZvEQV4iIKTlBVLDf7&v=jQE2dZK3iVI&feature=emb_logo","How the Coronavirus is Affecting Crime"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www.ftc.gov/system/files/attachments/coronavirus-covid-19-pandemic-ftc-action/keep_calm_infographic_en_letter_508.pdf","How to avoid scams in Covid-19"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www2.illinois.gov/idoc/programs/pages/victimservices.aspx","Victim service in Illinois"),
                          tags$br(),
                          tags$br(),
                          ),
                          
                          
                          tabPanel("HOW TO HELP",
                                   
                                   fluidPage(fluidRow(column(12,align="center",tags$img(src="https://pbs.twimg.com/media/EUdWR4qXkAEwlJ2.jpg",width="50%"))),
                                             fluidRow(column(4,tags$br(),align="center",tags$img(src="https://www.charities.org/sites/default/files/styles/large/public/AmericasCharities_COVID19-Fund_300pxAd_4-2-20.jpg?itok=f4MozTVN",width="51%")),
                                                      column(4,tags$br(),align="center",tags$img(src="https://covid19responsefund.org/assets/share-social.png",width="75%")),
                                                      column(4,tags$br(),align="center",tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/hhs/Donate-Plasma-Navy.jpg", width="75%"))),
                                             fluidRow(column(4,tags$br(),
                                                             "Nonprofits all across America are playing a vital role in supporting the people and communities that have been impacted by the COVID-19 pandemic",
                                                             tags$br(),
                                                             h4("Find Out How You Can Help"),
                                                             tags$a(href = "https://www.charities.org/coronavirus-covid-19-donations-funds-and-resources?gclid=CjwKCAiA_Kz-BRAJEiwAhJNY71h4gb5n8UlD5eDwUK_WhK1JSVdwvWiFsqnVZNoTkcthCBpSdMQwfRoCT3QQAvD_BwE","List of Nonprofits and Charities"),
                                                             tags$br(),
                                                             tags$a(href = "https://www.feedingamerica.org/take-action/coronavirus","Feeding America's Coronavirus Response Fund"),
                                                             tags$br(),
                                                             tags$a(href = "https://www.powerof.org/volunteer","Sign Up to Become a Volunteer!"),
                                                             tags$br(),
                                                             tags$a(href = "https://www.consumerreports.org/charitable-donations/how-to-help-your-community-during-coronavirus-crisis/", "Help your community during the COVID-19 Crisis"),
                                                             tags$br(),
                                                             img(src="data:image/png;base64,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",height=150,width=258),
                                                             tags$br(),
                                                             h4("Learn more about UNICEF"),
                                                             tags$br(),
                                                             tags$a(href="https://www.unicefusa.org/about","Click here to learn more about UNICEF SATFF"),
                                                             tags$br(),
                                                             tags$a(href="https://www.unicefusa.org/about","Click here to join UNICEF volunteers"),
                                                             tags$br(),
                                                             tags$a(href="https://www.unicefusa.org/about/contact","Click here to contact UNICEF"),
                                                             tags$br(),
                                                             img(src="data:image/png;base64,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",height=150,width=258),
                                                             tags$br(),
                                                             
                                                             ),
                                             column(4,tags$br(),
                                                    h4("World Health Organization"),
                                                    "The World Health Organization is leading and coordinating the global effort to combat the COVID-19 pandemic by helping countries prevent, detect, and respond to the virus.",
                                                    tags$br(),
                                                    tags$a(href = "https://www.fda.gov/emergency-preparedness-and-response/coronavirus-disease-2019-covid-19/donate-covid-19-plasma",
                                                           "Click here to learn about Donate COVID-19 Plasma."),
                                                    tags$br(),
                                                    tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/donate",
                                                           "Click Here to Learn More About the COVID-19 Solidarity Response Fund."),
                                                    tags$br(),
                                                    tags$a(href = "https://www.unicefusa.org/?utm_content=corona3responsive_E2001&ms=cpc_dig_2020_Emergencies_20200402_google_corona3responsive_delve_E2001&initialms=cpc_dig_2020_Emergencies_20200402_google_corona3responsive_delve_E2001&gclid=EAIaIQobChMIycvruda97QIVEj2tBh1wmgHzEAAYASAAEgJuOvD_BwE",
                                                           "Click here to Donate to the COVID-19 with UNICEF USA Official Site."),
                                                    tags$br(),
                                                    tags$a(href = "https://covid19responsefund.org/en/",
                                                           "Click here to Donate to the COVID-19 Solidarity Response Fund."),
                                                    h4("National Voluntary Organizations Active in Disaster"),
                                                    img(src="http://volunteer.volunteerleon.org/content/volunteer.volunteerleon.org/agency/21484.jpg?1400597910?area=agency", height=150, width=258),
                                                    "While the WHO is spearheading the international effort, the NVOAD is helping to assist in the US's response to COVID-19.",
                                                    tags$br(),
                                                    tags$a(href = "https://www.nvoad.org/covid-19-response/", "Click here to Donate to the National Voluntary Organizations Active in Disaster.")
                                             ),
                                             column(4,tags$br(),
                                                    "Because you fought the infection, your plasma now contains COVID-19 antibodies. These antibodies provided one way for your immune system to fight the virus when you were sick, so your plasma may be able to be used to help others fight off the disease.",
                                                    tags$h4("Plasma Donations"),
                                                    tags$a(href = "https://www.fda.gov/emergency-preparedness-and-response/coronavirus-disease-2019-covid-19/donate-covid-19-plasma",
                                                           "Click Here to Learn More About how Donating Plasma Saves Lives."),
                                                    tags$br(),
                                                    tags$a(href = "https://thefightisinus.org/en-US/home#home",
                                                           "Click Here if You Are Interested in Donating Plasma."),
                                                    tags$h4("Blood Donations"),
                                                    tags$a(href = "https://www.redcrossblood.org/", "Click Here if You Are Interested in Donating Blood.")   
                                             ),
                                             column(4, tags$br(),
                                                    img(src="https://imagesvc.meredithcorp.io/v3/mm/image?q=85&c=sc&poi=face&w=1920&h=1005&url=https%3A%2F%2Fstatic.onecms.io%2Fwp-content%2Fuploads%2Fsites%2F20%2F2020%2F04%2F09%2Fgfm20_covid19.jpg", height=150, width=258),
                                                    h4("Donating to Global COVID-19 Causes"), 
                                                    "Here are links to GoFundMe pages for COVID-19 ravaged third-world contries. These countries don't have as much funding to effectively aid all citizens from this pandemic.",
                                                    tags$br(),
                                                    tags$a(href="https://www.globalgiving.org/projects/help-families-affected-by-covid-19-in-colombia/donate/", "Click here to donate to Colombia"), 
                                                    tags$br(),
                                                    tags$a(href="https://www.ethiopiatrustfund.org/covid-19-donation/", "Click here to donate to Ethopia"), 
                                                    tags$br(), 
                                                    tags$a(href="https://www.globalgiving.org/fundraisers/coronavirus-relief-fund-bangladesh/", "Click here to donate to Bangladesh"),
                                                    tags$br(), 
                                                    tags$a(href="https://www.globalgiving.org/projects/prepare-clinics-in-haiti-for-covid-19/", "Click here to donate to Haiti"),
                                                    tags$br(), 
                                                    tags$a(href="https://www.globalgiving.org/projects/covid-19-save-life-and-global-well-being-cameroon/", "Click here to donate to Cameroon"),
                                                    tags$br(), 
                                                    tags$a(href="https://www.savethechildren.org/us/where-we-work/yemen", "Click here donate to Yemen"),
                                                    tags$br(), 
                                                    tags$a(href="https://www.globalgiving.org/projects/supporting-1000-vulnerable-families-in-nigeria/donate/", "Click here to donate to Nigeria"),
                                                    tags$br(),
                                                    img(src="https://cdn2.atlantamagazine.com/wp-content/uploads/sites/4/2020/03/donate_getty.jpg", height=150, width=258))
                                             #add an image that shows that we must all support the world 
                                             
                                             ),
                                             column(4,tags$br(),
                                                    h4("Several sites to give help"),
                                                    "",
                                                    tags$br(),
                                                    tags$a(href = "https://help.unicef.org/",
                                                           "Click here to learn about the Fight Against Diseases."),
                                                    tags$br(),
                                                    tags$a(href = "https://www.embracerelief.org/",
                                                           "Click Here to Learn More About the Embrace Relief."),
                                                    tags$br(),
                                                    tags$a(href = "https://www.ourbetterworld.org/",
                                                           "Click here to Donate to the our Better World."),
                                                    tags$br(),
                                                    tags$a(href = "https://www.giving.sg/",
                                                           "Click here to Donate to the Giving.")
                                                    
                                             )
                                   )
                          ),
                 
                          tabPanel("COVID-19 Recent News",
                                   #add images, and links
                                   #Reformat and add additional quick news updates (mguruv2)
                                   img(src="https://ewscripps.brightspotcdn.com/dims4/default/ff2ea6e/2147483647/strip/true/crop/1280x720+0+0/resize/1280x720!/quality/90/?url=http%3A%2F%2Fewscripps-brightspot.s3.amazonaws.com%2F9b%2F1c%2Fd6365aa54b5687a3cb1386a180db%2Fupdate-coronavirus-colorado-live-blog-covid19.png"),
                                   tags$br(),
                                   h1("Center of Disease Control", align = "center"),
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/whats-new-all.html","What's New | CDC"),
                                   tags$br(),
                                   tags$a(href = "https://tools.cdc.gov/campaignproxyservice/subscriptions.aspx?topic_id=USCDC_2067", "CDC Email Newsletter"),  
                                   tags$br(),
                                   tags$a(href = "https://covid.cdc.gov/covid-data-tracker/#trends_dailytrendscases", "CDC US Daily Cases"),  
                                   tags$br(),
                                   tags$hr(),
                                   
                                   h1("World Health Organization", align = "center"),
                                   tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports","Updates | WHO"),
                                   tags$br(),
                                   tags$a(href = "https://www.who.int/news-room/newsletters", "WHO Email Newsletters"),  
                                   tags$br(),
                                   tags$a(href = "https://worldhealthorg.shinyapps.io/covid/", "WHO Daily Cases Data"),  
                                   tags$br(),
                                   tags$hr(),
                                   
                                   h1("Illinois Department of Public Health", align = "center"),
                                   tags$a(href = "https://dph.illinois.gov/news-pressrelease","Daily News | IDPH"),
                                   tags$br(),
                                   tags$a(href = "https://www.dph.illinois.gov/news", "IDPH Daily Cases"),  
                                   tags$br(),
                                   tags$hr(),
                                   
                                   h1("Champaign-Urbana Public Health District", align = "center"),
                                   tags$a(href = "https://www.c-uphd.org/champaign-urbana-illinois-coronavirus-information.html","Recent Info | CUPHD"),
                                   tags$br(),
                                   tags$a(href = "https://public.tableau.com/views/Master2COVIDTableau/Dashboard1?:embed=y&:showVizHome=no&:host_url=https%3A%2F%2Fpublic.tableau.com%2F&:embed_code_version=3&:tabs=no&:toolbar=no&:animate_transition=yes&:display_static_image=no&:display_spinner=no&:display_overlay=yes&:display_count=yes&:language=en&publish=yes&:loadOrderID=0", "CUPHD Daily COVID Case Tracker"),  
                                   tags$br(),
                                   tags$hr(),
                                   
                                   h1("Other Resources", align = "center"),
                                   
                                   h3("Chicago Tribune - Daily Updates"),
                                   tags$a(href = "https://www.chicagotribune.com/coronavirus/","Coronavirus Updates | Chicago Tribune"),
                                   tags$br(),
                                   
                                   h3("New York Times - Daily Updates"),
                                   tags$a(href = "https://www.nytimes.com/news-event/coronavirus?name=styln-coronavirus&region=TOP_BANNER&block=storyline_menu_recirc&action=click&pgtype=LegacyCollection&impression_id=a0a836e0-3b3b-11eb-b184-ff23d8f1fa7b&variant=1_Show","COVID-19 Live Updates | NYT"),
                                   tags$br(),
                                   
                                   h3("Johns Hopkins Coronavirus Resource Center"),
                                   tags$a(href = "https://coronavirus.jhu.edu/","JHU Covid Updates"),
                                   tags$br(),
                                   
                                   h1("Coronavirus pandemic on BBC"),
                                   tags$a(href = "https://www.bbc.com/news/coronavirus","Click here to see the site of BBC News"),
                                   tags$br(),
                                   
                                   h1("FDA Daily Updates"),
                                   tags$a(href = "https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-december-10-2020","Click here to see the updates"),
                                   tags$br(),
                                   
                                   #add covid picture
                                   img(src="https://i.ytimg.com/vi/F70BzSFAZfw/maxresdefault.jpg", height= 300, width=500),
                                   tags$br(),
                                   
                                   img(src="https://www.fda.gov/themes/custom/preview/img/FDA-Social-Graphic.png",height= 300, width=500),
                                   tags$br()
                                   
                          ),
                 
                          tabPanel("U.S",
                                   h1("Total Cumulative Cases and Deaths in United States"),
                                   plotOutput("cm_plot"),
                                   sliderInput("date", "Choose a date",
                                               min = as.Date(min(NYTimes_US_States_Historical_Data$date),"%Y-%m-%d"), 
                                               max = as.Date(max(NYTimes_US_States_Historical_Data$date),"%Y-%m-%d"),
                                               value = c(as.Date(min(NYTimes_US_States_Historical_Data$date)), 
                                                         as.Date(max(NYTimes_US_States_Historical_Data$date))),timeFormat="%d %b", 
                                               dragRange = TRUE,
                                               width = "100%"),
                                   
                                   #h2(textOutput("case_count"), align = "left"),
                                   #h2(textOutput("death_count"), align = "left"),
                                   
                                   h2(textOutput("case_count1"), align = "left"),
                                   tags$br(),
                                   h2(textOutput("case_count2"), align = "left"),
                                   tags$br(),
                                   h2(textOutput("death_count1"), align = "left"),
                                   tags$br(),
                                   h2(textOutput("death_count2"), align = "left"),
                                   tags$br(),
                                   
                                   selectInput(inputId = "state1", label = "Choose a state", choices = sort(unique(NYTimes_US_States_Historical_Data$state))),
                                   plotOutput(outputId = "stateplot", click = "plot_click",dblclick = "plot_dblclick",hover="plot_hover",brush = "plot_brush"),
                                   verbatimTextOutput("info1"),
                                   plotOutput(outputId = "stateplot2", click = "plot_click2",dblclick = "plot_dblclick",hover="plot_hover",brush = "plot_brush"),
                                   verbatimTextOutput("info2"),
                                   h1("Covid-19 Deaths per Month"),
                                   plotOutput("deaths_per_month"),
                                   h1("Cases by State"),
                                   plotOutput("caseplot"),
                                   h1("Covid-19 Cases per County"),
                                   selectInput(inputId = "state_map", label = "Choose a state",
                                               choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                               selected = "All states"),
                                   
                                   # Covid-19 by WHO region
                                   h1=("Covid-19 by WHO region"),
                                   selectInput(inputId="region_map",label="Choose a region",
                                               choices=c("African Region",sort(unique(WHO_COVID_19_Situation_Report_Data$`WHO region`))),
                                               selected="African Region"),
                                   #plotOutput to be completed later in server function.
                                   h1("Covid-19 Deaths per County"),
                                   selectInput(inputId = "state_map", label = "Choose a state",
                                               choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                               selected = "All states"),
                                   plotOutput("mapplot"),
                                   h1("Latest Deaths/Cases by State"),
                                   plotOutput("latestplot"),
                                   h1("Monthly Increase of Recovered Number"),
                                   plotOutput("recoveredplot"),
                                   h1("Recovery, Death, and Positivity rate (US) per day", align = "left"),
                                   sidebarLayout(position = "right",
                                                 sidebarPanel(
                                                   radioGroupButtons(
                                                     inputId = "type_rate",
                                                     label = "",
                                                     choices = c("Recovery rate",
                                                                 "Death rate", 
                                                                 "Positivity rate",
                                                                 "Death vs. Recovery",
                                                                 "All"
                                                     ),
                                                     justified = FALSE
                                                   ),
                                                   
                                                 ),
                                                 
                                                 
                                                 
                                                 mainPanel(dateRangeInput(inputId = "Rate",
                                                                          label = "Select a date range",
                                                                          start = as.Date(min(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                          end =  as.Date(max(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                          min = as.Date(min(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                          max = as.Date(max(COVID_Tracking_Project_US_Historical_Data$date),"%Y-%m-%d"),
                                                                          separator = 'to'),
                                                           plotOutput("recovered_death_rate")
                                                 )
                                   ),
                                   
                                   
                                   
                                   
                                   sliderInput(inputId = "date1",
                                               label = "From the date of",
                                               value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                               min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                               max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                               dragRange = TRUE,
                                               width = "100%"),
                                   plotOutput("testplot"),
                                   h1("Estimated Current Cases By County"),
                                   selectInput(inputId = "state.input", label = "Select a State",
                                               choices = c("All States", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                               selected = "All States"),
                                   plotOutput("county"),
                                   
                                   h1("Current Hospitalized Patients"),
                                   plotOutput("hos"),
                                   h1("Hospitalization Rate Trend"),
                                   plotOutput("hos1"),
                                   
                                   h1("Covid-19 In US States - Cases VS Deaths. Contains Regression Line For Toggle Points"),
                                   fluidRow(
                                     column(width = 8,
                                            plotOutput("plot", height = 300,
                                                       click = "plot_click",
                                                       brush = brushOpts(
                                                         id = "plot_brush"
                                                       )
                                            ),
                                            actionButton("exclude_toggle", "Toggle points"),
                                            actionButton("exclude_reset", "Reset")
                                     )
                                   ),
                                   
                                   #added what percent of state has covid
                                   h1("Percent of Population Covid-19 Positive by State"),
                                   selectInput(inputId = "state0", label = "Choose a state",
                                               choices = c("All states", sort(unique(NYTimes_US_States_Historical_Data$state))),
                                               selected = "All states"),
                                   textOutput("percent_pos_state"),
                                   #Section for Links specifically for United States Information including advisories, mandates, and travel bans
                                   h1("Useful Links for United States Information"),
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html", "Click here for the latest news regarding COVID-19 from the CDC."),
                                   tags$br(),
                                   tags$a(href = "https://covid.cdc.gov/covid-data-tracker/#cases_casesper100klast7days", "Click here for COVID-19 data tracker from the CDC."),
                                   tags$br(),
                                   tags$a(href = "https://www.cnn.com/search?size=10&q=coronavirus&category=us", "Click here for the lastest new about Coronavirus from the CNN."),
                                   tags$br(),
                                   tags$a(href = "https://www.nytimes.com/interactive/2020/us/states-reopen-map-coronavirus.html", "Click here for the latest information regarding COVID-19 Advisories, Mandates, and Travel Restrictions"),
                                   tags$br(),
                                   tags$a(href = "https://www.usatoday.com/storytelling/coronavirus-reopening-america-map/#restrictions", "Click here for the latest information on COVID-19 State Policy Trends"),
                                   tags$br(),
                                   tags$a(href = "https://www.usatoday.com/storytelling/coronavirus-reopening-america-map/", "Click here for the COVID-19 restrictions among each states"),
                                   tags$br(),
                                   
                                   h1("Check the Hospitalization Rate by date"),
                                   dateInput("date","date",
                                             min = min(us_data$date),
                                             max = max(us_data$date)),
                                   textOutput("hosp_rate")
                          ),
                          #New tab to compare coronavirus to previous pandemics.
                          tabPanel("Comparing Pandemics",
                                   #Reformatted links/tab to better display relevant info, corrected wrong information with new necessary information (mguruv2)
                                   h1("Notable Past Pandemics"),
                                   tags$a(href = "https://www.worldometers.info/coronavirus/worldwide-graphs/", "Current Global COVID-19 Spread - Worldometers"),
                                   tags$br(),
                                   tags$a(href = "https://www.forbes.com/sites/johntorpey/2020/10/09/comparing-pandemics/?sh=28f86fc96c74","On Comparing Pandemics - Forbes"),
                                   tags$br(),
                                   tags$a(href = "https://www.news-medical.net/health/How-does-the-COVID-19-Pandemic-Compare-to-Other-Pandemics.aspx","Comparing COVID-19 to Other Pandemics - News Medical"),
                                   tags$br(),
                                   tabsetPanel(
                                     tabPanel("Spanish Flu",
                                              h1("Spanish Flu"),
                                              p("The 1918 Spanish Flu is often compared to the current COVID-19 pandemic, as it is the deadliest pandemic in recent memory. Like COVID-19, the 1918 virus was also highly infectious and spread through respiratory droplets."),
                                              tags$br(),
                                              tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2720273/", "Spanish Flu Epidemiology and Pathology"),
                                              tags$br(),
                                              tags$a(href = "https://www.cdc.gov/flu/pandemic-resources/1918-commemoration/pandemic-timeline-1918.htm", "Spanish Flu Timeline History"),
                                              tags$br(),
                                              h4("Graph of Spanish Flu Deaths over Timespan"),
                                              tags$br(),
                                              tags$img(src="https://images.theconversation.com/files/338977/original/file-20200601-95032-126zcl7.png?ixlib=rb-1.1.0&q=45&auto=format&w=1000&fit=clip", height=200, width=300),
                                              tags$br(),
                                              img(src="https://cdn.vox-cdn.com/thumbor/s5wkg-JrdauTJbcJLIAZxNUygz4=/0x0:1200x805/1200x675/filters:focal(504x307:696x499)/cdn.vox-cdn.com/uploads/chorus_image/image/66529941/_worker_photo_toned.0.jpg", height=200, width=300),
                                              tags$br()
                                     ),
                                     tabPanel("Swine Flu",
                                              h1("Swine Flu"),
                                              p("The 2009 H1N1 Pandemic, also known as the Swine Flu, was the most recent respiratory pandemic. It was highly infectious and like COVID-19, jumped from animals to humans originally"),
                                              tags$br(),
                                              tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK513241/", "Swine Flu Epidemiology and Pathology"),
                                              tags$br(),
                                              tags$a(href = "https://www.cdc.gov/flu/pandemic-resources/2009-pandemic-timeline.html", "Swine Flu Timeline History"),
                                              tags$br(),
                                              h4("Graph of Swine Flu Deaths over Timespan"),
                                              tags$br(),
                                              tags$img(src="https://www.cdc.gov/h1n1flu/Images/graphf_0312.jpg", height=200, width=300),
                                              tags$br(),
                                              img(src="https://www.researchgate.net/profile/Kuldeep_Dhama/publication/229805901/figure/fig1/AS:300874636251142@1448745511840/Worldwide-distribution-of-swine-flu-Pandemic-H1N1-2009-Map-reproduced-with-permission.png", height=200, width=300),
                                              tags$br()
                                    ),
                                    tabPanel("Bubonic Plague",
                                             h1("Bubonic Plague"),
                                             p("The Bubonic Plague of the mid-1300s, also known as the Black Death, is often considered the deadliest pandemic in history. Unlike the other notable pandemics, this was a bacterial plague."),
                                             tags$br(),
                                             tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK549855/", "Bubonic Plague Epidemiology and Pathology"),
                                             tags$br(),
                                             tags$a(href = "https://www.history.com/news/black-death-timeline", "Bubonic Plague Timeline History"),
                                             tags$br(),
                                             h4("Graph of Bubonic Plague Deaths over Timespan"),
                                             tags$br(),
                                             tags$img(src="https://www.researchgate.net/profile/Daniel_Curtis5/publication/321443119/figure/fig5/AS:573634102398981@1513776435532/Plague-incidences-in-Europe-1347-1900-Graph-produced-on-the-basis-of-data-from-Biraben.png", height=200, width=300),
                                             tags$br(),
                                             img(src="https://images.firstpost.com/wp-content/uploads/2020/07/1397px-Marseille-peste-Serre-1.jpg", height=200, width=300),
                                             tags$br()
                                    ),
                                    tabPanel("HIV/AIDS",
                                             img(src="https://hivinfo.nih.gov/sites/default/files/fact_sheets_data/images/HIVvsAIDS_FS_700pix.jpg", height=200, width=300),
                                              tags$br(),
                                              h1("HIV/AIDS"),
                                              tags$a(href = "https://www.healthline.com/health/hiv-aids#early-symptoms","HIV/AIDS - Causes, Symptoms & Impact"),
                                              tags$br(),
                                    ),
                                    tabPanel("Hong Kong Flu",
                                              img(src="https://images.wsj.net/im-179655?width=620&size=1.5", height=200, width=300),
                                              tags$br(),
                                              h1("History of Hong Kong Flu"),
                                              tags$a(href = "https://www.wsj.com/articles/forgotten-pandemic-offers-contrast-to-todays-coronavirus-lockdowns-11587720625.html","The Similarities Between Coronavirus and the Hong Kong Flu- WSJ"),
                                              tags$br(),
                                    ),
                                    tabPanel("The Plague of Justinian",
                                              img(src="https://www.ajsefton.com/uploads/1/2/4/2/124222182/justininas-plague_orig.png", height=200, width=300),
                                              tags$br(),
                                              h1("History of Justinian Plague"),
                                              tags$a(href = "https://www.ancient.eu/article/782/justinians-plague-541-542-ce/","Remembering One of the Earliest Pandemics- Ancient History"),
                                              tags$br(),
                                     
                                   ),
                                     tabPanel("SARS",
                                            img(src="https://www.frontiersin.org/files/Articles/554339/fmicb-11-01818-HTML/image_m/fmicb-11-01818-g001.jpg", height=200, width=300),
                                            tags$br(),       
                                            h1("Useful Links for Sars Information"),
                                            tags$a(href = "https://www.cdc.gov/sars/index.html","Click here for more information on Sars"),
                                            tags$br(),
                                   ),
                                   tabPanel("Black Death",
                                            img(src="https://cdn.mos.cms.futurecdn.net/QHWi3CUjoAd2DJ44JB65o8-1024-80.jpg"),
                                            tags$br(),       
                                            h1("Black Death Information"),
                                            tags$br(),
                                            tags$a(href = "https://www.livescience.com/what-was-the-black-death.html","Click here for general information on Black Death"),
                                            tags$br(),       
                                            h1("Black Death Symptoms"),
                                            tags$br(),
                                            tags$a(href = "https://www.mayoclinic.org/diseases-conditions/plague/symptoms-causes/syc-20351291","Click here for symptoms for Black Death"),
                                   ),
                                    tabPanel("Other History Pandemics",     
                                            h1("List of pandemics in human history"),
                                            tags$br(),
                                            tags$a(href = "https://www.sciencemag.org/news/2020/05/black-death-fatal-flu-past-pandemics-show-why-people-margins-suffer-most","Click here for information on history pandemics"),
                                   ))
                          ),

                          tabPanel("Global",
                                   h1("Google Worldwide News"),
                                   tags$a(href = "https://news.google.com/covid19/map?hl=en-US&gl=US&ceid=US%3Aen","Worldwide Impact of COVID-19"),
                                   tags$br(),
                                   tabsetPanel(
                                     tabPanel("World maps",
                                              
                                              h1("World Map Representation By Case"),
                                              plotOutput("plot_world"),
                                              em("*Grayed out country indicates data is missing"),
                                              h1("World Map Representation By New Cases"),
                                              plotOutput("new_cases_world"),
                                              em("*Grayed out country indicates data is missing"),
                                              h1("World Map Representation By Death"),
                                              plotOutput("death_plot_world"),
                                              em("*Grayed out country indicates data is missing"),
                                              #h1("World Map Representation By recovered death rate"),
                                              #plotOutput("recovered_death_rate"),
                                              #em("*Grayed out country indicates data is missing"),
                                     ),
                                     
                                     tabPanel("Continent maps",
                                              
                                              h1("Global COVID-19 Situation by Region"),
                                              sidebarLayout(position = "left",
                                                            sidebarPanel(
                                                              radioGroupButtons(
                                                                inputId = "type",
                                                                label = "",
                                                                choices = c("New_cases",
                                                                            "Cumulative_cases",
                                                                            "New_deaths",
                                                                            "Cumulative_deaths"),
                                                                justified = FALSE
                                                                
                                                              ),
                                                              htmlOutput("cases_count_region")
                                                            ),
                                                            mainPanel(sliderInput(inputId = "date2",
                                                                                  label = "From the date of",
                                                                                  value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                                                                  min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                                                  max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                                                  dragRange = TRUE,
                                                                                  width = "100%"),
                                                                      plotOutput("plot_region")
                                                            )
                                              ),
                                              
                                     ),
                                     
                                     tabPanel("Winning Countries",
                                     h1("Countries with fewer COVID-19 cases"),
                                     tags$a(href = "https://www.endcoronavirus.org/countries"),
                                     tags$br(),
                                     
                                     h1("How did the country with the largest population control the pandemic?"),
                                     tags$a(href = "https://idpjournal.biomedcentral.com/articles/10.1186/s40249-020-00759-3"),
                                     tags$br()),

                                     
                                     tabPanel("Other Data",
                                              
                                              #Commit 1- Graph detailing R^2 between ventilator, hospitalization, and positive test result and death over time with an adjustable delay for COVID (measuring death latency essentially)
                                              
                                              
                                              h1("Death Latency", align = "left"),
                                              h4("\nAdjust the delay to understand the relationship between COVID stages and death"),
                                              h4("\nHigher R Squared values indicate better correlation"),
                                              h4("\nUse the date range to explore various periods in the pandemic"),
                                              
                                              sidebarLayout(
                                                sidebarPanel(
                                                  sliderInput(inputId = "drange",
                                                              label = "Select Date Range",
                                                              value = c(as.Date("2020-5-26"),max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                                              min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                              max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                              dragRange = TRUE),
                                                  sliderInput(inputId = "Lag", label = "Select the Delay Amount (Days)", min=0, max=21, value=14),
                                                  radioGroupButtons(inputId = "Curve", label = "", direction = "vertical", selected="hosp",
                                                                    choices = c("Positive Test"="pos", "Hospitalization"="hosp", "Ventilator"="vent")),
                                                  htmlOutput("Rsquared")),
                                                mainPanel(plotOutput("Death_Latency"))),
                                              
                                              #Commit 2 Graph detailing ventilator usage as a percentage of hospitalized COVID patients over time
                                              h1("Treemap for Cumulative Cases based by WHO Region"), 
                                              loadEChartsLibrary(),
                                              
                                              sliderInput(
                                                inputId = "date",
                                                label = "Date",
                                                value = as.Date("2020-06-24", "%Y-%m-%d"), 
                                                min = min(WHO_Global_Historical_Data$Date_reported),
                                                max = max(WHO_Global_Historical_Data$Date_reported)), 
                                              plotOutput("treemap"),
                                              
                                              h1("Ventilator Usage as a Percentage of Hospitalized COVID patients", align = "left"),
                                              sliderInput(inputId = "Ventilator",
                                                          label = "Select Date Range",
                                                          value = c(min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                                    max(as.Date(COVID_Tracking_Project_US_Historical_Data$date))),
                                                          min = min(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                          max = max(as.Date(COVID_Tracking_Project_US_Historical_Data$date)),
                                                          dragRange = TRUE),
                                              plotOutput("Ventilator_Usage"),
                                              
                                              
                                     ), 
                                     
                                     tabPanel("Travel Info",
                                              #Adding Travel Information
                                              #insert header
                                              h1("Travel Restrictions By Country"),
                                              
                                              tags$a(href = "https://www.iatatravelcentre.com/world.php", "Updates on Global Travel Restrictions"),  
                                              tags$br(),
                                              tags$a(href = "https://travel.state.gov/content/travel/en/traveladvisories/traveladvisories.html/", "Travel advisories by US Department of State"),  
                                              tags$br(),

                                              
                                              #insert image of travelers
                                              img(src="https://s.abcnews.com/images/Travel/200513_abcnl_prime_benitez1_hpMain_16x9_992.jpg", height= 300, width=400),
                                              tags$br(),
                                              
                                              #Add travel info by Centers for Disease Control and Prevention
                                              h1("Travel worldwide info released by CDC"),
                                              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/travelers/map-and-travel-notices.html"),
                                              tags$br(),
                                              h4("Click on Country Name to be Directed to CDC website with specific information for that country"),
                                              h2("Travel Advisories"),
                                              #Lists of countries with at different travel advisory levels with links to country specific travel website
                                              h3("Countries with Level 4 Travel Advisory: COVID-19 Very High"),
                                    
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-afghanistan","Afghanistan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-albania","Albania"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-algeria","Algeria"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-andorra","Andorra"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-angola","Angola"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-argentina","Argentina"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-armenia","Armenia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-aruba","Aruba"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-austria","Austria"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-azerbaijan","Azerbai"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-the-bahamas","Bahamas"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bahrain","Bahrain"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bangladesh","Bangladesh"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-belarus","Belarus"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-belgium","Belgium"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-belize", "Belize"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bhutan","Bhutan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bolivia","Bolivia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bonaire","Bonaire"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-bosnia-and-herzegovina","Bosnia and Herzegovina"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-botswana","Botswana"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-brazil","Brazil"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-burkina-faso","Burkina Faso"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-burma","Burma (Myanmar)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-burundi","Burundi"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-cameroon","Cameroon"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-canada","Canada"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-cape-verde","Cape Verde"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-central-african-republic","Central African Republic"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-chad","Chad"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-chile","Chile"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-colombia","Colombia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-comoros","Comoros"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-congo","Congo, Republic of the"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-costa-rica","Costa Rica"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-croatia","Croatia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-cuba","Cuba"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-curacao","Curacao"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-cyprus","Cyprus"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-czech-republic","Czech Republic"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-democratic-republic-of-congo","Democratic Republic of the Congo"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-denmark","Denmark"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-djibouti","Djibouti"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-dominican-republic","Dominican Republic"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-easter-island","Easter Island"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-ecuador","Ecuador"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-egypt","Egypt"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-el-salvador","El Salvador"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-equatorial-guinea","Equatorial Guinea"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-eritrea","Eritrea"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-estonia","Estonia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-eswatini","Eswatini (Swaziland)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-ethiopia","Ethiopia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-finland","Finland"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-france","France"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-french-guiana","French Guiana"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-french-polynesia","French Polynesia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-gabon","Gabon"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-the-gambia","Gambia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-georgia","Georgia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-germany","Germany"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-ghana","Ghana"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-gibraltar","Gibraltar"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-greece","Greece"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guadeloupe","Guadeloupe"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guam","Guam (U.S.)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guatemala","Guatemala"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guinea","Guinea"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guinea-bissau","Guinea-Bissau"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-guyana","Guyana"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-haiti","Haiti"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-honduras","Honduras"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-hungary","Hungary"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-iceland","Iceland"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-india","India"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-indonesia","Indonesia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-iran","Iran"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-iraq","Iraq"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-ireland","Ireland"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-israel","Israel, including the West Bank and Gaza"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-italy","Italy"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-jamaica","Jamaica"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-japan","Japan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-jersey","Jersey (part of the UK)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-jordan","Jordan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-kazakhstan","Kazakhstan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-kenya","Kenya"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-kosovo","Kosovo"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-kuwait","Kuwait"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-kyrgyzstan","Kyrgyzstan"),    
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-latvia","Latvia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-lebanon","Lebanon"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-lesotho","Lesotho"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-liberia","Liberia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-libya","Libya"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-liechtenstein","Liechtenstein"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-lithuania","Lithuania"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-luxembourg","Luxembourg"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-madagascar","Madagascar"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-maderia-islands","Madeira Islands"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-malawi","Malawi"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-malaysia","Malaysia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-maldives","Maldives"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-mali","Mali"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-malta","Malta"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-martinique","Martinique (France)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-mauritania","Mauritania"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-mayotte","Mayotte (France)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-mexico","Mexico"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-moldova","Moldova"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-monaco","Monaco"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-montenegro","Montenegro"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-morocco","Morocco"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-mozambique","Mozambique"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-namibia","Namibia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-nepal","Nepal"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-netherlands","The Netherlands"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-nicaragua","Nicaragua"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-niger","Niger"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-nigeria","Nigeria"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-north-korea","North Korea"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-north-macedonia","North Macedonia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-norway","Norway"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-oman","Oman"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-pakistan","Pakistan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-panama","Panama"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-paraguay","Paraguay"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-peru","Peru"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-philippines","Philippines"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-pitcairn-islands","Pitcairn Islands (U.K.)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-poland","Poland"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-portugal","Portugal"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-puerto-rico","Puerto Rico (U.S.)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-qatar","Qatar"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-reunion", "Reunion"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-romania","Romania"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-russia","Russia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-saint-martin","Saint Martin"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-san-marino","San Marino"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sao-tome-and-principe","Sao Tome and Principe"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-saudi-arabia","Saudi Arabia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-senegal","Senegal"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-serbia","Serbia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sint-maarten","Sint Maarten"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-slovakia","Slovakia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-slovenia","Slovenia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-solomon-islands","Solomon Islands"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-somalia","Somalia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-south-africa","South Africa"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-south-sudan","South Sudan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-spain","Spain"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sri-lanka","Sri Lanka"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sudan","Sudan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-suriname","Suriname"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sweden","Sweden"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-switzerland","Switzerland"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-syria","Syria"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-tajikistan","Tajikistan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-tanzania","Tanzania"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-togo","Togo"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-trinidad-and-tobago","Trinidad and Tobago"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-tunisia","Tunisia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-turkey","Turkey"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-turkmenistan","Turkmenistan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-turks-and-caicos","Turks and Caicos Islands (U.K.)"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-uganda","Uganda"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-ukraine","Ukraine"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-united-arab-emirates","United Arab Emirates"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-united-kingdom","United Kingdom"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-uzbekistan","Uzbekistan"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-venezuela","Venezuela"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-venezuela","Virgin Islands, U.S."),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-yemen","Yemen"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-yemen","Zambia"),
                                                tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-zimbabwe","Zimbabwe"),
                                              
                                              h3("Countries with Level 3 Travel Advisory: COVID-19 High"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-3/coronavirus-saint-lucia","Saint Lucia"),
                                              
                                              h3("Countries with Level 2 Travel Adviosry: COVID-19 Moderate"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-antigua-and-barbuda","Antigua and Barbuda"),
                                              tags$a(ref="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-barbados","Barbados"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-dominica","Dominica"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-3/coronavirus-faroe-islands","Faroe Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-guernsey","Guernsey"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-mauritius","Mauritius"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-saint-barthelemy","Saint Barthelemy"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-seychelles","Seychelles"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-south-korea","South Korea"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-uruguay","Uruguay"),
                                              
                                              h3("Countries with Level 1 Travel Advisory: COVID-19 Low"),
                                              
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-american-samoa","American Samoa"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-anguilla","Anguilla"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-antarctica","Antarctica"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-australia","Australia"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-benin","Benin"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-bermuda","Bermuda"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-british-virgin-islands","British Virgin Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-brunei","Brunei"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-cambodia","Cambodia"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-cayman-islands","Cayman Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-china","China"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-cook-islands","Cook Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-falkland-islands","Falkland Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-fiji","Fiji"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-greenland","Greenland"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-grenada","Grenada"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-hong-kong","Hong Kong"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-isle-of-man","Isle of Man"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-ivory-coast","Ivory Coast"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-kiribati","Kiribati"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-laos","Laos"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-macau","Macau SAR"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-marshall-islands","Marshall Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-micronesia","Federated States of Micronesia"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-mongolia","Mongolia"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-montserrat","Montserrat"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-nauru","Nauru"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-new-caledonia","New Caledonia"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-new-zealand","New Zealand"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-niue","Niue"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-northern-mariana-islands","Northern Mariana Islands"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-palau","Palau"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-papua-new-guinea","Papua New Guinea"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-rwanda","Rwanda"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-saba","Saba"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-saint-helena","Saint Helena"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-st-kitts-and-nevis","Saint Kitts and Nevis"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-saint-pierre-and-miquelon","Saint Pierre and Miquelon"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-saint-vincent-and-the-grenadines","Saint Vincent and the Grenadines"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-samoa","Samoa"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-4/coronavirus-sierra-leone","Sierra Leone"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-2/coronavirus-singapore","Singapore"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-sint-eustatius","Sint Eustatius"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-taiwan","Taiwan"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-thailand","Thailand"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-timor-leste","Timor-Leste (East Timor)"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-tokelau","Tokelau"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-tonga","Tonga"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-tuvalu","Tuvalu"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-vanuatu","Vanuatu"),
                                              tags$a(href="https://wwwnc.cdc.gov/travel/notices/covid-1/coronavirus-vietnam","Vietnam"),
                                     
                                     ),
                                     tabPanel("Links", 
                                              
                                              #Section for Links for Global Information so that it imitates the part under US tab
                                              h1("Useful Links for Global Information"),
                                              tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019", "Click here for the latest global information on COVID-19 from the WHO."),
                                              tags$br(),
                                              tags$a(href = "https://ourworldindata.org/policy-responses-covid", "Click here for global policy responses to COVID-19, including school and workplace closure and restriction on public gathering"),
                                              tags$br(),
                                              tags$a(href = "https://www.embracerace.org/resources/disproportionate-racial-impacts-of-covid", "Click here for more information on the impact of COVID-19 on racialized communities"),
                                              tags$br(),
                                              tags$a(href = "https://inchemistry.acs.org/content/inchemistry/en/grad-school/grad-school-during-pandemic.html", "Click here for news towards the graduate school application under pandemic"),
                                              tags$br(),
                                              tags$a(href = "https://www.worldometers.info/coronavirus/", "Click here for more information on Worldmeters")
                                              
                                     )
                                   ),
                                   
                                   
                                   
                                   
                          ),
                 
                 
                          tabPanel("Ways to spread",
                                   #Ways for covid-19 to spread
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/how-covid-spreads.html"),
                                   tags$br()),
                 
                 
                          tabPanel("Risk of Animal",
                                   #Risk of animal including pets
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/animals.html"),
                                   tags$br(),
                                   h4("Informational news video on COVID-19 and the risks to pets and animals"),
                                   embed_youtube("KLOjH-RSdHQ",
                                                 width = NULL,
                                                 height = 300,
                                                 ratio = c("16by9", "4by3"),
                                                 frameborder = 0,
                                                 allowfullscreen = TRUE,),            
                                   tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/duration-isolation.html", "Click here for CDC guidelines related to animals.")),
                                   tags$li(a(href = "https://www.akc.org/expert-advice/news/can-dogs-get-coronavirus/", "Click here to see if dogs and other animals can contract COVID-19.")),
          
                          ),
                 

                          tabPanel("Origin",
                                   #Origin of the disease
                                   tags$a(href = "https://www.nature.com/articles/d41586-020-03165-9"),
                                   tags$br()),
                          
                          tabPanel("Vaccine",
                                   #Add a picture about vaccines to guide the web content 
                                   img(src="https://images.theconversation.com/files/341551/original/file-20200612-153812-ws3rqu.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=754&fit=clip", height = 400, width = 700),
                                   tags$br(),
                                   #add link about information on COVID-19 vaccine for CDC
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html", "Click here for more information on COVID-19 Vaccine for CDC."),
                                   tags$br(),
                                   tags$a(href = "https://www.who.int/news-room/q-a-detail/coronavirus-disease-(covid-19)-vaccines?adgroupsurvey={adgroupsurvey}&gclid=Cj0KCQiA5bz-BRD-ARIsABjT4nhYVbcBsg0jkC_m7SJ6vGxslyV2uHtBS82mEqE_HUiii4rmcA6HoJAaAtZ8EALw_wcB", "Click here for more information on COVID-19 Vaccine for WHO."),
                                   tags$br(),
                                   #add link about information on COVID-19 vaccine
                                   tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html", "Click here for more information on COVID-19 Vaccine."),
                                   tags$br(),
                                   #add first person in uk to receive the vaccine
                                   h1("Vaccine is already in action"),
                                   img(src="https://cnet1.cbsistatic.com/img/S8P2GXkwkXsYfC-v9BR-hP6uw0Y=/940x0/2020/12/08/4659ec1c-12e1-462e-a1e7-65eb52bd4c68/gettyimages-1230005171.jpg"),
                                   tags$br(),
                                   tags$a(href = "https://www.bbc.com/news/uk-55227325", "Click here to read about Margarets journey."),
                                   tags$br(),
                                   #add link about information on COVID-19 vaccination timeline
                                   tags$a(href = "https://www.nytimes.com/interactive/2020/12/03/opinion/covid-19-vaccine-timeline.html", "Click here to fill out a survey to find your place in the COVID-19 Vaccine Line"),
                                   tags$br(), 
                                   tags$a(href = "https://www.businessinsider.com/when-can-i-get-a-coronavirus-vaccine-timeline-2020-11", "Click here to learn the COVID-19 vaccination timeline"),
                                   tags$br(),
                                   #add link about information related to vaccine tracking
                                   tags$a(href = "https://www.raps.org/news-and-articles/news-articles/2020/3/covid-19-vaccine-tracker", "Click here to access a COVID-19 Vaccine Tracker"),
                                   tags$br(),
                                   #add link to access live updates of vaccine related news
                                   tags$a(href = "https://www.wsj.com/articles/covid-19-vaccines-mass-distribution-supply-chain-11607874181?mod=searchresults_pos1&page=1", "Click here to access live news updates of the COVID-19 Vaccine (Distribution Under Way)"),
                                   tags$br(),
                                   tags$img(src="https://media.gatesnotes.com/-/media/Images/Articles/Health/What-you-need-to-know-about-the-COVID-19-vaccine/what-you-need-to-know_2020_inline-graph_desktop-03.ashx", height = 500, width=500),
                                   
                                   #link from WHO that gives info on Vaccine and answers questions
                                   tags$a(href = "https://www.who.int/news-room/q-a-detail/coronavirus-disease-(covid-19)-vaccines?adgroupsurvey={adgroupsurvey}&gclid=CjwKCAiAiML-BRAAEiwAuWVgggLpGj6VTEYCx5tAsUHBdo9SkhMO9pnpAyqysVA9mNOs8oMLduhLXhoCPDAQAvD_BwE", "Click Here for Vaccine Questions answered by WHO"),
                                   tags$br(),
                                   
                                   #Link to register for updates on when you may qualify for the COVID vaccine in Champaign County
                                   tags$a(href = "https://www.urbanacitizen.com/news/78679/sign-up-for-covid-vaccine-alerts-with-new-link", "Click Here for updates on when you can get the vaccine"),
                                   tags$br(),
                                   
                                   h2("Vaccines in Progress(US)",align = "left"),
                                   shinyApp(
                                     shinyUI(
                                       fluidPage(
                                         dataTableOutput('PM_output')
                                       )
                                     ),
                                     
                                     shinyServer(function(input, output, session) {
                                       require(DT)
                                       dat = data.frame(Manufacturer=c("AstraZeneca","Janssen Pharmaceutical Companies","Moderna","Novavax","Pfizer"),`Vaccine Platform` = c("Non-Replicating Viral Vector ","Non-Replicating Viral Vector ","RNA","Protein Subunit","RNA"), 
                                                        Websites = c("https://www.astrazeneca.com/media-centre/press-releases/2020/azd1222hlr.html","https://www.janssen.com/us/covid19-patient-resources","https://www.modernatx.com/modernas-work-potential-vaccine-against-covid-19","https://ir.novavax.com/news-releases/news-release-details/novavax-announces-covid-19-vaccine-clinical-development-progress","https://www.wsj.com/articles/fda-set-to-release-analyses-of-the-pfizer-biontech-covid-19-vaccine-11607423403"))
                                       dat$Websites <- sapply(dat$Websites, function(x) 
                                         toString(tags$a(href=paste0("http://", x), x)))
                                       
                                       output$PM_output <- renderDataTable(expr = datatable(dat, escape=FALSE),
                                                                           options = list(autoWidth = T))
                                     })
                                   ),
                                   h3("British AstraZeneca Vaccine"),
                                   tags$img(src="https://api.time.com/wp-content/uploads/2020/11/GettyImages-1229753927.jpg?w=800&quality=85", height = 200, width=300),
                                   h5("AstraZeneca plc is a British-Swedish multinational pharmaceutical and biopharmaceutical company with its headquarters in Cambridge, England.
            In June 2020, the National Institute of Allergy and Infectious Diseases (NIAID) confirmed that the third phase of testing for potential vaccines
            developed by Oxford University and AstraZeneca would begin in July 2020. One of them, AZD1222, reached phase III trials."),
                                   tags$a(href = "https://www.astrazeneca.com/covid-19.html", "Click here if you want to know any info about AstraZeneca's vaccine progress"),
                                   
                                   h3("Germany BioNTech & US Pfizer Vaccine"),
                                   tags$img(src="https://cached.imagescaler.hbpl.co.uk/resize/scaleWidth/800/cached.offlinehbpl.hbpl.co.uk/news/PGH/GettyImages-1229653900.jpg", height = 200, width=300),
                                   h5("BNT162b2 is a COVID-19 vaccine developed by BioNTech and Pfizer and given by intramuscular injection.
              It is an RNA vaccine composed of nucleoside-modified mRNA encoding a mutated form of the spike protein of SARS-CoV-2,
              and is encapsulated in lipid nanoparticles.In November 2020, interim analysis of the trial examined research participants who had been diagnosed with COVID-19 and received the vaccine candidate,
              showing that BNT162b2 may have an efficacy of over 90% in preventing infection within seven days of a second dose."),
                                   tags$a(href = "https://www.pfizer.com/", "Click here if you want to know any info about BNT162b2 progress"),
                                   
                                   h3("Sinopharm vaccine"),
                                   tags$img(src="https://news.cgtn.com/news/2020-11-25/Sinopharm-applies-to-bring-its-COVID-19-vaccine-to-market--VIiimRueZy/img/971c31ce7cea4b6aaa0015354221c537/971c31ce7cea4b6aaa0015354221c537.jpeg", height = 200, width=300),
                                   h5("On October 15, the Beijing Institute of Biological Products published results of its Phase I (192 adults) and Phase II (448 adults)
             clinical studies for the BBIBP-CorV vaccine in The Lancet. BBIBP-CorV, was safe and well tolerated at all tested doses in two age groups.
             Antibodies were elicited against SARS-CoV-2 in all vaccine recipients on day 42. The report noted that the inactivated COVID-19 vaccine had a low rate of
             adverse reactions and demonstrated immunogenicity, but longer-term assessment of safety and efficacy would require phase III trials."),
                                   tags$a(href = "http://www.sinopharm.com/en/1156.html","Click here if you want to know any info about Sinopharm vaccine progress"), 
                                   
                                   h3("Sputnik V Vaccine"), 
                                   tags$img(src="https://s.france24.com/media/display/836ad246-dbc7-11ea-822f-005056a98db9/w:1280/p:16x9/Russia%20coronavirus%20vaccine.webp", height=200, width=300), 
                                   h5("Released this August, Sputnik V was officially the first COVID-19 vaccine. Though it is still in the midst of trials to check that it's safe and actually works
             the company has promised a 91.4% accurate rate. As of today, around 100,000 people have been dosed with it. Because there is little to no data backing
             the vaccine there are only a limited amount of people that have taken it. The general public believes that because Russia is being hit with the virus 
             very hard (roughly 30,000 new cases a day), the release of the vaccine was rushed and not safe to give to a wider population."), 
                                   tags$a(href="https://www.bbc.com/news/world-europe-55221785", "Click here if you want to learn about Russia's Sputnik V Vaccine"),
                                   
                                    h3("Novavax Vaccine"), 
                                   tags$img(src="https://static.toiimg.com/thumb/msid-79132422,imgsize-70644,width-400,resizemode-4/79132422.jpg", height=200, width=300), 
                                   h5("Novavax has created a vaccine candidate named NVX-CoV2373. This vaccine has shown that it efficiently binds with human receptors targeted by the virus in preclinical studies,
                                      which is a critical aspect for a vaccine to be effective. As of November 30th, their vaccine candidate has completed enrollment for its Phase 3 trials in the United Kingdom and
                                      for its Phase 2b efficacy trial in South Africa, and is expected to begin its Phase 3 trials in the US and Mexico very soon. Novavax was also granted $1.6 billion in funding from 
                                      the U.S. government to meet its Operation Warp Speed goals which are to expedite the delivery of millions of doses of the COVID-19 vaccines. This award is also funding the U.S. 
                                      and Mexico Phase 3 trials, as well as the scaled-up manufacturing."), 
                                   tags$a(href="https://www.novavax.com/our-pipeline#nvx-cov2373", "Click here to learn about Novavax's progress in their clinical trials"),

                                   h3("A New Way of Vaccination - FruitVaccine"),
                                   h5("Have you ever imagined a new way of vaccination? Wouldn't it be cool if you could eat products made by whole fruits but get vaccinated 
                at the same time? EnterpriseWorks startup FruitVaccine is changing the world of vaccinations with an edible, orally-administered fruit-based vaccine for 
                human respiratory syncytial virus (RSV) and coronavirus (COVID). Founded by University of Illinois professors Dr. Indu Rupassara and Dr. Dennis Buetow in 2017, the two have worked to modify tomatoes to develop a sub-unit vaccine. The startup has 
                had a strong first year as a business with successful trials performed on mice, and the founders tell Smile Politely that their long term goal, 'is to not only create an edible RSV vaccine, 
                but to develop many different edible vaccines for different viruses.'"),
                                   tags$a(href="http://www.fruitvaccine.org/", "Click here if you want to learn more about FruitVaccine and their mission"),
                                   
                          ),
                          tabPanel("Prevention",
                                   h4("The easiest and most effective way of preventing the contraction and spread of COVID-19 is wearing a mask!"),
                                   tags$br(),
                                   tags$a(href="https://www.youtube.com/watch?v=DNeYfUTA11s","Click here to learn how masks prevent the projection of particles"),
                                   tags$br(),
                                   tags$a(href="https://www.amazon.com/s?k=masks&ref=nb_sb_noss","Click here to purchase masks"),
                                   tags$br(),
                                   tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html","Click here for information on how to protect yourself and others"),
                                   tags$br(),
                                   tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/social-distancing.html","Click here for why and how to practice social distancing"),
                                   tags$br(),
                                   tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public","Click here for WHO Advice for the public"),
                                   tags$br(),
                                   tags$img(src="https://www.nebraskamed.com/sites/default/files/images/covid19/Masks-Dos-Donts_Blog_v2.jpg", height = 400, width=500),
                                   
                                   tags$img(src="https://files.labcorp.com/labcorp-d8/2020-04/LabCorp_Coronavirus_Infographic_032720.jpg", height = 400, width=500),
                                   tags$img(src="https://www.linnsheriff.org/wp-content/uploads/COVID-19-Graphic-web-banner.jpg", height = 400, width=800),
                                   tags$img(src="https://www.stlukeshealth.org/sites/default/files/mask-effectiveness.jpg", height =400, width=800),
                                   h4("Here is a graph displaying % effectiveness of the type of mask you use. The most effective being the standard surgical mask."),
                                   tags$img(src="https://media.nature.com/lw800/magazine-assets/d41586-020-01248-1/d41586-020-01248-1_17940460.png", height =500, width=300), 
                                   h4("Prevention matters! Nations such as China, Hong Kong, South Korea introduced heavy mask regulations/stay-at-home-orders and, thus, deaths plateaued."),
                                   tags$br(),
                                   h4("Staying at home is the best way to stop the spread. However, if you have to go out, make sure to wear a mask and practice social distancing"),
                                   tags$img(src = "https://www.kellerisd.net/cms/lib/TX02215599/Centricity/Domain/117/COVIDProtocols/Mititgation_MaskExample.png", height = 400, width = 500),
                                   h4("If you are going out..."), embed_youtube('quNi23GQ89c', width = NULL, height = 400, ratio = c("16by9","4by3"), frameborder = 0, allowfullscreen = TRUE),
                                   h4("This risk factor chart shows you how risky certain activities could be during this time on a scale of 1 to 10, please refer to this chart when going out and deem if certain activities are a necesity."),
                                   tags$img(src = "https://static.abcotvs.com/ktrk/images/cms/risk-chart.jpg", height = 800, width = 600),
                                   h4("How to protect yourself against COVID-19"),
                                   embed_youtube('1APwq1df6Mw',
                                                 width = NULL,
                                                 height = 300,
                                                 ratio = c("16by9", "4by3"),
                                                 frameborder = 0,
                                                 allowfullscreen = TRUE),
                          ), 
                 
                 tabPanel("Alert Information",
                          #Adding information about Alerts 
                          h4("Alert Information"),
                          tags$img(src="https://www.kxan.com/wp-content/uploads/sites/40/2020/05/Chart-1.jpg?resize=876,559", height = 500, width=800),
                          tags$br(),
                          h4("WHO Real Time Alert"),
                          tags$a(href = "https://www.who.int/csr/alertresponse/realtimealert/en/","Click here to get the WHO Real Time Alert information"),
                          tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/travelers/images/CBP_PROTECT_OTHERS_TravelAlert_1920x1080_stills-medium.jpg", height = 500, width=800),
                          tags$br(),
                          h4("COVID-19 Travel Recommendations"),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/travelers/map-and-travel-notices.html","COVID-19 Travel Recommendations by Destination"),
                          h4('For real-time covid19 updates visit'),
                          tags$a(href = "https://coronavirus.jhu.edu/", "Click here to get Real updates on all covid19 data"),

                          ),
                  tabPanel("Illinois travel guideline",
                          HTML('<div>
                                 <h1>CDC Travel Guidance</h1>
                                 <h2>If I travel, what steps should I take to help reduce my chances of getting sick?</h2>
                                 <ul><li>Wash your hands often with soap and water for at least 20 seconds, especially after you have been in a public place, touching surfaces frequently touched by others, blowing your nose, coughing, and sneezing, and before touching your face or eating.</li>
                                 <li>If soap and water are not available, use hand sanitizer that contains at least 60% alcohol. Cover all surfaces of your hands and rub your hands together until they feel dry.</li>
                                 <li>Avoid touching your eyes, nose, or mouth with unwashed hands.</li>
                                 <li>Avoid close contact with others, keeping 6 feet of distance.</li>
                                 <li>Wear a cloth face covering in public.</li>
                                 <li>Cover coughs and sneezes.</li>
                                 <li>Pick up food at drive-throughs, curbside at restaurants, or stores.</li>
                                 <li>Make sure you are up to date with your routine vaccinations, including the measles-mumps-rubella (MMR) vaccine and the seasonal flu vaccine.</li>
                                 </ul><h1>Domestic and Interstate Travel</h1>
                                 <h2>Can traveling to visit family or friends increase my chances of getting and spreading COVID-19?</h2>
                                 <p>Yes. Travel increases your risk of getting and spreading COVID-19. Before you travel, learn if COVID-19 is spreading in your local area or in any of the places you are going. Traveling to visit family may be especially dangerous if you or your loved ones are more likely to suffer severe illness from COVID-19. People at higher risk for severe illness need to take extra precautions.</p>
                                 <h1>International Travel</h1>
                                 <h2>Should I avoid traveling internationally?</h2>
                                 <p>Avoid all nonessential international travel because of the COVID-19 pandemic.&nbsp;Some health care systems are overwhelmed and there may be limited access to adequate medical care in affected areas. Many countries are implementing travel restrictions and mandatory quarantines, closing borders, and prohibiting non-citizens from entry with little advance notice. Airlines have cancelled many international flights and in-country travel may be unpredictable. If you choose to travel internationally, your travel plans may be disrupted, and you may have to remain outside the U.S. for an indefinite length of time.</p>
                                 <p>CDC recommends travelers defer cruise ship travel worldwide at this time.</p>
                                 <h2>What can I expect when departing other countries?</h2>
                                 <p>Some countries are conducting exit screening for passengers leaving their country. Before being permitted to board a departing flight, you may have your temperature taken and be asked questions about your travel history and health.</p>
                                 <h2>What can I expect when arriving to the U.S.?</h2>
                                 <p>Travel restrictions and&nbsp;entry screening may apply to travelers arriving from some countries or regions with ongoing, widespread COVID-19.</p>
                                 <p>You may be screened when you arrive in the U.S. If you have traveled to an area of higher risk, take the following steps upon your return to protect yourself and others:</p>
                                 <ol><li>Stay home and avoid contact with others. Do not go to work or school for 14 days.</li>
                                 <li>Monitor your health for 14 days. Take your temperature two times a day and monitor for fever. Also watch for cough or trouble breathing.</li>
                                 <li>Keep at least 6 feet of distance from others.</li>
                                 </ol><h2>When can I return to work after international travel?</h2>
                                 <p>International travelers returning home from areas of higher risk should stay home for 14 days after their arrival into the U.S. &nbsp;Once home, monitor your health and practice social distancing.</p>
                                 <h1>Air Travel</h1>
                                 <h2>Can flying on an airplane increase my risk of getting COVID-19?</h2>
                                 <p>Yes. Air travel requires spending time in security lines and airport terminals, which can bring you in close contact with other people and frequently touched surfaces. Most viruses and other germs do not spread easily on flights because of how air circulates and is filtered in airplanes. However, social distancing is difficult on crowded flights, and you may have to sit near others (within 6 feet), sometimes for hours. This may increase your risk for exposure to the virus that causes COVID-19.</p>
                                 <h2>What happens if there is a sick passenger on an international or domestic flight?</h2>
                                 <p>Under current federal regulations, pilots must report all illnesses and deaths to CDC before arriving to a U.S. destination. According to CDC disease protocols, if a sick traveler is considered a risk to the publics health, CDC works with local and state health departments and international public health agencies to&nbsp;contact exposed passengers and crew.</p>
                                 <p>Be sure to give the airline your current contact information when booking your ticket so you can be notified if you are exposed to a sick traveler on a flight.</p>
                                 </div>'),
                          ),
                     
                 tabPanel("Web References",
                          
                          h3("Covid Relief Supplies"),
                          tags$br(),
                          tags$img(src="https://umom.org/wp-content/uploads/2020/07/2020-07-Urgent-Needs-COVID19-page-buttons.png",width="45%"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public/when-and-how-to-use-masks","Why to wear a mask and other FAQ about masks"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www.amazon.com/mask/s?k=mask","Buy masks to protect you and your loved ones"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www.amazon.com/s?k=hand+sanitizer&crid=2JNFOTVTJ02GT&sprefix=hand+%2Caps%2C245&ref=nb_sb_ss_ts-a-p_1_5","Buy hand sanitizer to keep your hands COVID free"),
                          tags$br(),
                          tags$br(),
                          tags$a(href="https://www.amazon.com/s?k=cleaning+supplies&crid=2VHUR72JYHU7N&sprefix=cleaning+sup%2Caps%2C233&ref=nb_sb_ss_ts-a-p_1_12","Keep your house tidy and COVID safe"),
                          tags$br(),
                          tags$br(),
                          
                          ),
                 
                 #add Social Impact section on panel
                 tabPanel("Social Impact",
                          
                          h3("Violence at Home Increases"),
                          tags$a(href = "https://www.unwomen.org/en/news/in-focus/in-focus-gender-equality-in-covid-19-response/violence-against-women-during-covid-19", "The Shadow Pandemic: Violence against women during COVID-19"),
                          tags$br(),
                          
                          h3("Technology Advancements"),
                          tags$a(href = "https://www.forbes.com/sites/greatspeculations/2020/11/25/with-a-covid-vaccine-ready-zoom-stock-looks-overvalued/?sh=244430765ca2", "With A Covid Vaccine Ready, Zoom Stock Looks Overvalued"),
                          tags$br(),
                          tags$a(href = "https://www.bbc.com/news/business-55139083","Zoom boosts sales forecast as pandemic drags on"),
                          tags$br(),
                          tags$a(href = "https://time.com/5870826/amazon-coronavirus-jeff-bezos-congress/", "Many Companies Won't Survive the Pandemic. Amazon Will Emerge Stronger Than Ever"),
                          tags$br(),
                          br(),
                          br(),
                          br(),
                          
                 ),
                 
                 tabPanel("Upload Your COVID-19 Data Files Here to Help Us",
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              # Input: Select a file ----
                              fileInput("file1", "Choose CSV File",
                                        multiple = TRUE,
                                        accept = c("text/csv",
                                                   "text/comma-separated-values,text/plain",
                                                   ".csv")),
                              # Horizontal line ----
                              tags$hr(),
                              # Input: Checkbox if file has header ----
                              checkboxInput("header", "Header", TRUE),
                              # Input: Select separator ----
                              radioButtons("sep", "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","),
                              # Input: Select quotes ----
                              radioButtons("quote", "Quote",
                                           choices = c(None = "",
                                                       "Double Quote" = '"',
                                                       "Single Quote" = "'"),
                                           selected = '"'),
                              # Horizontal line ----
                              tags$hr(),
                              # Input: Select number of rows to display ----
                              radioButtons("disp", "Display",
                                           choices = c(Head = "head",
                                                       All = "all"),
                                           selected = "head"),
                              tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }")),
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                              # Output: Data file ----
                              tableOutput("contents")
                            )
                          )
                 ),
                 tabPanel("Choose the Right Mask for You",
    h4(
      "Some masks may look similar, but it's important to learn the differences between the protection that each offers you and its cost in order to make a better decision before you go out!"
    ),
    tags$br(),
    tags$img(
      src = "https://www.umms.org/-/media/images/umms/coronavirus/what-to-know/masks/cloth-mask.jpg?h=227&w=400&la=en&hash=856E75CD1E2BBA8EFA5A62F781C03DAB67A359FA",
      height = 400,
      width = 500
    ),
    h1(
      "Cloth Mask"
    ),                   
    h4(
      "Made from multiple layers of fabric, very easy to make and offers protection for others if you are a carrier. The CDC has instructions for how to make your own no-sew cloth masks. The most effective cloth masks are ones constructed of densely woven cotton with multiple layers of fabric."
    ),
    tags$a(href = "https://www.amazon.com/Safe-Mate-Case-Mate-Washable-Reusable/dp/B0891VVHN1/ref=sr_1_2_sspa?crid=18AWYVJ98S978&dchild=1&keywords=cloth+masks&qid=1608000420&sprefix=cloth+mask%2Caps%2C433&sr=8-2-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUFQS0RFTTZUUzhRQ1EmZW5jcnlwdGVkSWQ9QTAxMjEwNzUzSFJVNzI0Rk1KMDdGJmVuY3J5cHRlZEFkSWQ9QTAwNDA0NjEyUTNHUThYVVY3VFNOJndpZGdldE5hbWU9c3BfYXRmJmFjdGlvbj1jbGlja1JlZGlyZWN0JmRvTm90TG9nQ2xpY2s9dHJ1ZQ==", "Cloth Mask"),
    tags$br(),                     
    tags$img(
      src = "https://www.umms.org/-/media/images/umms/coronavirus/what-to-know/masks/surgical-mask.jpg?h=227&w=400&la=en&hash=64E263F0E519F682B390DE77564AEC55C180067F",
      height = 400,
      width = 500
    ),
    h1(
      "Disposable Surgical Mask"
    ),
    h4(
      "Made from papers and plastics, disposable. These masks filter out large particles in the air, but don't filter everything. The FDA has not approved these masks for protection against others who may carry the virus."
    ),
    tags$a(href = "https://www.amazon.com/Disposable-Earloops-3-Layer-Design-Particle/dp/B089YSYBCD/ref=sr_1_1_sspa?crid=2ERSXY34RFQ3K&dchild=1&keywords=disposable+face+masks&qid=1608000466&sprefix=disposa%2Caps%2C199&sr=8-1-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUExWExZWE5RTERPOVpGJmVuY3J5cHRlZElkPUEwNTM5NDQzSTA1RFNGRjhWQlNMJmVuY3J5cHRlZEFkSWQ9QTA0ODQ4OTExWjY3NlJaN09JQUJEJndpZGdldE5hbWU9c3BfYXRmJmFjdGlvbj1jbGlja1JlZGlyZWN0JmRvTm90TG9nQ2xpY2s9dHJ1ZQ==", "Disposable Surgical Mask"),
    tags$br(),                     
    tags$img(
      src = "https://www.umms.org/-/media/images/umms/coronavirus/what-to-know/masks/n95.jpg?h=227&w=400&la=en&hash=F74E73125F2B0030DEE2AD3AF93EC83B38BE3913",
      height = 400,
      width = 500
    ),
    h1(
      "N95 Face Respirator"
    ),
    h4(
      "Made from tightly woven plastics and papers. Meant to be worn with a tight seal after specific tests for the user's face to seperate the outside air. This service is unavailable to the general public. Reports find that online sellers of these kinds of masks often ineffective counterfeits. These kinds of masks are considered essential and should be reserved for healthcare workers."
    ),
    tags$a(href = "https://www.amazon.com/Protection-Breathable-Against-Pollution-Outdoor/dp/B087LR59CF/ref=sr_1_2?dchild=1&keywords=N95+mask&qid=1608000552&sr=8-2", "N95 Face Respirator"),
    tags$br(),                     
    tags$img(
      src = "https://www.umms.org/-/media/images/umms/coronavirus/what-to-know/masks/face-shield.jpg?h=227&w=400&la=en&hash=EFE6368C56728FC6D91AB7ECE1AB36C7AFBE4624",
      height = 400,
      width = 500
    ),
    h1(
      "Face Shield"
    ),
    h4(
      "Made from clear plastic. Does not protect against respiratory droplets and should not be used to substitute a mask. If you are wearing a mask and maintain a 6 foot distance, you do not need a face shield."
    ),
    tags$a(href = "https://www.amazon.com/Reusable-Universal-Protective-Protection-Anti-Spitting/dp/B0875NWNDS/ref=sr_1_2_sspa?dchild=1&keywords=face+shield&qid=1608000607&sr=8-2-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUEyMldLTTU5U1YxNEFBJmVuY3J5cHRlZElkPUEwOTY2OTQzM0ZZWkJYNVhXNzg2TSZlbmNyeXB0ZWRBZElkPUEwOTE2NDA4RjRRUzlFN0JGQ0omd2lkZ2V0TmFtZT1zcF9hdGYmYWN0aW9uPWNsaWNrUmVkaXJlY3QmZG9Ob3RMb2dDbGljaz10cnVl", "Face Shield"),
    tags$br(),                     
    tags$img(
      src = "https://cdn.pixabay.com/photo/2020/08/20/12/34/mask-5503421_960_720.jpg",
      height = 400,
      width = 500
    ),
    h1(
      "KN95 Face Mask"
    ),
    h4(
      "KN95 face mask which is equivlent to N95 mask mentioned above. The mask filtrates at least 95% of the very small (0.3 micron) particles."
    ),
    tags$a(href = "https://www.amazon.com/Bio-th-Reusable-Disposable-Protection-Efficiency%E3%80%8B95/dp/B089344XZF/ref=sr_1_2_sspa?crid=2SBIVDUDBK77B&dchild=1&keywords=kn95+face+mask&qid=1608000656&sprefix=kn%2Caps%2C211&sr=8-2-spons&psc=1&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUExRVdIV0xLSzNVQ1hIJmVuY3J5cHRlZElkPUEwMzE2ODQyMUVZV1FOWjJGQjQzSCZlbmNyeXB0ZWRBZElkPUEwMjc4NzMzMTkxSVhOVUZNRlhQNiZ3aWRnZXROYW1lPXNwX2F0ZiZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU=", "KN95 Face Mask"),
    tags$br(),
    h3("Differences among Masks"),
    tags$a(href = "https://atriumhealth.org/dailydose/2020/04/22/not-all-face-masks-are-created-equal-know-which-type-of-face-mask-you-need-and-when", "Differentiate Masks"),
    tags$br(),
    
    tags$img(
      src = "https://i.insider.com/5f513b59e6ff30001d4e6ef2?width=800&format=jpeg&auto=webp",
      height = 800,
      width = 600
    ),
    h4(
      "This chart from Business Insider displays the best and worst types of face coverings based on filtration efficiency."
    ),
    
    h4("Informational WHO video on how to use masks properly and safely."),
                          embed_youtube("9Tv2BVN_WTk",
                                        width = NULL,
                                        height = 300,
                                        ratio = c("16by9", "4by3"),
                                        frameborder = 0,
                                        allowfullscreen = TRUE,)                      
  )
                 #tags$style(HTML(".navbar-header { width:100%}
                   #.navbar-brand { width: 100%; text-align: center }"))
)#THIS IS THE END PARENTHESIS FOR THENAVBAR PAGE
                 
                 
                 #Plotting Positive Cases Increase Per Month In Correlation to Number of Deaths
                 positivecases <- COVID_Tracking_Project_US_Historical_Data
                 
                 boxplot(death ~ positiveIncrease, data = positivecases,
                         xlab = "Positive Increase in Case", 
                         ylab = "Number of Deaths", 
                         main = "Positive Increase vs Number of Deaths",
                         pch = 20, 
                         cex = 2, 
                         col = "dark orange",
                         border = "black")
                 
                 #Plot the total confirmed cases in each States on the newest day.
                 Newest_Date <- tail(unique(NYTimes_US_States_Historical_Data$date), n=1)
                 States_Total_Cases_Newest_Date <- NYTimes_US_States_Historical_Data %>% 
                   filter(date == Newest_Date) %>%
                   select(-c(fips, deaths))
                 b1 <- barplot(States_Total_Cases_Newest_Date$cases, las=1, cex.names = 0.5,
                               space=0.5, ylim = range(pretty(States_Total_Cases_Newest_Date$cases)),
                               main = paste("Total Confirm Cases in Each State \n on", 
                                            Newest_Date),
                               col = "white", width = c(1, 1))
                 text(b1, y = States_Total_Cases_Newest_Date$cases + 15000, 
                      labels = States_Total_Cases_Newest_Date$cases, adj = 0.5, cex = 0.5, 
                      col = "dodgerblue3")  
 
                 # Define server logic required to draw a histogram
                 server <- function(input, output,session) {
                   
                   
                   
                   vals <- reactiveValues(
                     keeprows = rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
                   )
                   
                   output$plot <- renderPlot({
                     
                     keep    <- NYTimes_US_States_Historical_Data[ vals$keeprows, , drop = FALSE]
                     exclude <- NYTimes_US_States_Historical_Data[!vals$keeprows, , drop = FALSE]
                     
                     ggplot(keep, aes(cases, deaths)) + geom_point() +
                       geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
                       geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
                       coord_cartesian(xlim = c(0,20000), ylim = c(0,9000))
                   })
                   
                   
                   observeEvent(input$plot_click, {
                     res <- nearPoints(NYTimes_US_States_Historical_Data, input$plot_click, allRows = TRUE)
                     
                     vals$keeprows <- xor(vals$keeprows, res$selected_)
                   })
                   
                   # Toggle points that are brushed, when button is clicked
                   observeEvent(input$exclude_toggle, {
                     res <- brushedPoints(NYTimes_US_States_Historical_Data, input$plot_brush, allRows = TRUE)
                     
                     vals$keeprows <- xor(vals$keeprows, res$selected_)
                   })
                   
                   # Reset all points
                   observeEvent(input$exclude_reset, {
                     vals$keeprows <- rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
                   })
                   
                   
                   
                   # Symptoms
                   output$symptom_choice <- renderText({
                     symptoms <- paste(input$symptoms, collapse = ", ")
                     paste(strong("Since you think you may have:"), symptoms)
                   })
                   #habits 
                   output$habit_check <- renderText({
                     habits <- paste(input$habits, collapse = ", ")
                     paste(strong("Good habits you have:"), habits)})
                   #steps
                   output$step_choice <- renderText({
                     if (input$steps == "Stay home") {
                       steps <- tags$a("Stay home. Most people with COVID-19 have mild illness and can recover at home without medical care. Do not leave your home, except to get medical care. Do not visit public areas.")
                     } else if (input$steps == "Separate yourself from other people") {
                       steps <- tags$a("As much as possible, stay in a specific room and away from other people and pets in your home. If possible, you should use a separate bathroom. If you need to be around other people or animals in or outside of the home, wear a mask.")
                     } else if (input$steps == "Monitor your symptoms") {
                       steps <- tags$a("Symptoms of COVID-19 include fever, cough, or other symptoms. See Above for reference.")                    
                     } else if (input$steps == "Call ahead before visiting your doctor") {
                       steps <- tags$a("Call ahead. Many medical visits for routine care are being postponed or done by phone or telemedicine. This will help the office protect themselves and other patients.")
                     } else if (input$steps == "Wear a mask over your nose and mouth") {
                       steps <- tags$a("You should wear a mask over your nose and mouth if you must be around other people or animals, including pets (even at home).")
                     } else if (input$steps == "Cover your coughs and sneezes") {
                       steps <- tags$a(" ")
                     } else if (input$steps == "Clean your hands often") {
                       steps <- tags$a("Wash your hands. Use hand sanitizer. Avoid touching your eyes, nose, and mouth with unwashed hands.")
                     } else if (input$steps == "Avoid sharing personal household items") {
                       steps <- tags$a("Do not share dishes, drinking glasses, cups, eating utensils, towels, or bedding with other people in your home. Wash these items thoroughly after using them with soap and water or put in the dishwasher.")
                     } else if (input$steps == "Clean all high-touch surfaces everyday") {
                       steps <- tags$a("Clean and disinfect high-touch surfaces in your 'sick room' and bathroom; wear disposable gloves.")
                     } 
                     paste(strong("Details"), steps)
                   })
                   output$region_choice <- renderText({
                     if (input$regions == "Region 1") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=1", "Jo Davies, Stephenson, et al.")
                     } else if (input$regions == "Region 2") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=2", "Rock Island, Henry, Bureau, Putnam, et al.")
                     } else if (input$regions == "Region 3") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=3", "Hancock, Adams, Pike, et al.")                    
                     } else if (input$regions == "Region 4") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=4", "Bond, Madison, St. Clair, et al.")
                     } else if (input$regions == "Region 5") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=5", "Marion, Jefferson, Wayne, et al.")
                     } else if (input$regions == "Region 6") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=6", "Iroquois, Ford, Dewitt, et al.")
                     } else if (input$regions == "Region 7") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=7", "Will, Kankakee")
                     } else if (input$regions == "Region 8") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=8", "Kane, Dupage")
                     } else if (input$regions == "Region 9") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=9", "McHenry, Lake")
                     } else if (input$regions == "Region 10") {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=10", "Cook")
                     } else {
                       regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=11", "Chicago")
                     } 
                     paste(strong("You are in the region: "), regions)
                   })
                   
                   reac_hist_date <- reactive({
                     NYTimes_US_Historical_Data %>% filter(date == input$date)
                   })
                   reac_cm_date <- reactive({
                     NYTimes_US_Historical_Data %>% filter(date <= input$date)
                   })
                   output$cm_plot <- renderPlot({
                     #par(mfrow = c(2, 1))
                     #plot(x = reac_cm_date()$date, y = reac_cm_date()$cases, ylab = "Cumulative Cases", xlab = "Date", pch = 20, col = "#0455A4")
                     #plot(x = reac_cm_date()$date, y = reac_cm_date()$deaths, ylab = "Cumulative Deaths", xlab = "Date", pch = 20, col = "#E84A27")
                     #plot(x = reac_cm_date()$date, y = reac_cm_date()$deaths/reac_cm_date()$cases, ylab = "Cumulative Deaths Ratio", xlab = "Date", pch = 20, col = "black")
                     xDate <- reac_cm_date()$date
                     yCases <- reac_cm_date()$cases
                     yDeaths <- reac_cm_date()$deaths
                     lmCases <- lm(yCases ~ xDate)
                     lmDeaths <- lm(yDeaths ~ xDate)
                     par(mfrow = c(2, 1))
                     par(oma=c(0,0,2,2)) 
                     par(mar=c(1,2,1,2)+.1)
                     plot(xDate, yCases, ylab = "Cumulative Cases", xlab = "Date", pch = 20, col = "#0455A4")
                     lines(xDate,lmCases$fitted.values, col=4, lwd=2, lty=2)
                     plot(xDate, yDeaths, ylab = "Cumulative Deaths", xlab = "Date", pch = 20, col = "#E84A27")
                     lines(xDate,lmDeaths$fitted.values, col=2, lwd=2, lty=2)
                   })
                   #output$case_count <- renderText({
                   #paste0(prettyNum(reac_hist_date()$cases, big.mark=","), " cases in the US as of ",input$date)
                   #})
                   #output$death_count <- renderText({
                   #paste0(prettyNum(reac_hist_date()$deaths, big.mark=","), " deaths in the US as of ",input$date)
                   #})
                   
                   output$case_count1 <- renderText({
                     paste0(prettyNum(reac_hist_date()$cases[1], big.mark=","), " cases in the US as of ",input$date[1])
                   })
                   output$case_count2 <- renderText({
                     paste0(prettyNum(reac_hist_date()$cases[2], big.mark=","), " cases in the US as of ",input$date[2])
                   })
                   output$death_count1 <- renderText({
                     paste0(prettyNum(reac_hist_date()$deaths[1], big.mark=","), " deaths in the US as of ",input$date[1])
                   })
                   output$death_count2 <- renderText({
                     paste0(prettyNum(reac_hist_date()$deaths[2], big.mark=","), " deaths in the US as of ",input$date[2])
                   })
                   
                   output$stateplot = renderPlot({
                     plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$cases), ylab = "New Cases", xlab = "Date", main = paste("Number of New COVID-19 Cases Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#048732")
                   })
                   
                   output$info1 <- renderText({
                     xy_str <- function(e){
                       if(is.null(e)) return("NULL\n")
                       paste0("New Cases = ",round(as.numeric(e$y),1),"\n")
                     }
                     xy_range_str <- function(e){
                       if(is.null(e)) return("NULL\n")
                       paste0(" The Lowest cases over that period = ", round(as.numeric(e$ymin), 1), " The highest cases over that period = ", round(as.numeric(e$ymax), 1))
                     }
                     paste0(
                       "click: ", xy_str(input$plot_click),
                       "dblclick: ", xy_str(input$plot_dblclick),
                       "hover: ", xy_str(input$plot_hover),
                       "brush: ", xy_range_str(input$plot_brush)
                     )
                     
                   })
                   output$stateplot2 = renderPlot({
                     plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$deaths), ylab = "New Death", xlab = "Date", main = paste("Number of New COVID-19 Deaths Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#ddb3ff")
                   })
                   
                   output$info2 <-renderText({
                     xy_str <- function(e){
                       if(is.null(e)) return("NULL\n")
                       paste0("New Death=",round(as.numeric(e$y),1),"\n")
                     }
                     xy_range_str <- function(e){
                       if(is.null(e)) return("NULL\n")
                       paste0(" The Lowest new death over that period=", round(as.numeric(e$ymin), 1), " The highest new death over that period=", round(as.numeric(e$ymax), 1))
                     }
                     paste0(
                       "click: ", xy_str(input$plot_click2),
                       "dblclick: ", xy_str(input$plot_dblclick),
                       "hover: ", xy_str(input$plot_hover),
                       "brush: ", xy_range_str(input$plot_brush)
                     )
                     
                   })
                   #calculating average increase in cases globally and storing values in growth_rate_global
                   confirmed_cases_global <- c(WHO_COVID_19_Situation_Report_Data[1,])
                   growth_rate_global <- c()
                   for (i in 5:154) {
                     growth_rate_global <- c(growth_rate_global,(abs(as.numeric(confirmed_cases_global[i+1]) - as.numeric(confirmed_cases_global[i]))))
                   }
                   
                   # Plotting deaths per cases by county
                   data_county <- as.data.frame(NYTimes_US_Counties_Historical_Data)
                   
                   output$deaths_by_county <- renderPlot ({
                     
                     our_data <- data_county()
                     
                     barplot(colSums(our_data[,c("deaths","county")]),
                             ylab="Deaths",
                             xlab="County",
                             names.arg = c("Deaths", "County"),
                             col = color)
                   })
                   
                   #order the number of cases for each state by date and calculate death/cases ratio by day,
                   #stored in new$ratio. highest ratio is found by state and sorted in peak_sorted
                   usstates <- NYTimes_US_States_Historical_Data
                   new <- usstates[order(usstates$state),]
                   new$ratio <- new$deaths/new$cases
                   peak_sorted <- new %>% group_by(state) %>% filter(ratio == max(ratio))
                   
                   #national deaths/cases
                   new_national <- NYTimes_US_Historical_Data
                   new_national$ratio <- new_national$deaths/new_national$cases
                   
                   #comparing latest ratio of deaths/cases by state and in the entire country
                   latest_ratio_states <- new %>% filter(date==max(date))
                   latest_ratio_national <- new_national %>% filter(date==max(date))
                   comparison_latest_by_states <- ifelse(latest_ratio_states$ratio >= latest_ratio_national$ratio, 1, 0)
                   combined_latest_national <- cbind(latest_ratio_states, comparison_latest_by_states)
                   
                   
                   #plot the states' latest deaths/cases ratio
                   output$latestplot <- renderPlot({
                     plot_latest <- plot_usmap("states", data = combined_latest_national, values = "ratio", color = "black", labels = TRUE) +
                       scale_fill_continuous(low = "grey", high = "#E84A27", name = "Latest Deaths/Cases Ratio") +
                       labs(title = "Latest Deaths/Cases Ratio") + theme(legend.position = "left", panel.background = element_rect(color = "black", fill = "white")) +
                       scale_fill_continuous(low = "grey", high = "#E84A27")
                     return(plot_latest)
                   })
                   
                   #plot the states' total cases
                   output$caseplot <- renderPlot({
                     data1 =  NYTimes_US_States_Historical_Data %>% filter(date == input$date)
                     plot_case <- plot_usmap("states", data = data1[, c(3,4)], values = "cases", color = "black", labels = TRUE) +
                       scale_fill_gradient(low = "white", high = "#0455A4", na.value = NA, name = "# of cases by state") +
                       labs(title = "Cases by states") + theme(legend.position = "left") +
                       guides(color = guide_legend(order = 1)) +
                       labs(fill = "# of cases by state")
                     return(plot_case)
                   })
                   
                   #Plotting by county
                   output$mapplot <- renderPlot({
                     if(input$state_map == "All states"){
                       data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date)
                     }
                     else{
                       data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date & state == input$state_map)
                     }
                     vc_states <- unique(state.abb[match(data$state, state.name)])
                     p <- plot_usmap(regions = "counties",
                                     include = vc_states,
                                     data = data[, c(4, 6)], values = "deaths") +
                       labs(title = ifelse(input$state_map == "All states", "US Counties", vc_states),
                            subtitle = paste0("Shows all counties in ", input$state_map)) +
                       theme(panel.background = element_rect(color = "black", fill = "white")) +
                       scale_fill_continuous(low = "yellow", high = "#0000FF", na.value = "#FFFFFF")
                     return(p)
                   })
                   
                   # Plotting deaths per cases by county
                   
                   # Preparing Dataset
                   new_counties <- NYTimes_US_Counties_Historical_Data
                   new_counties$ratio <- new_counties$deaths/new_counties$cases
                   monthlydata <- NYTimes_US_Historical_Data[c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315, 320), ] #These are the last day of each month.
                   monthlydata$month <- seq(1, 12, 1)
                   
                   a <- monthlydata$deaths
                   b <- c(0, a[-12])
                   monthlydata$md <- a -b # death per month
                   
                   #Plotting deaths per month
                   output$deaths_per_month <- renderPlot({
                     new <- ggplot(data = monthlydata, aes(y = md, x = factor(1:12,labels=month.abb[1:12])))
                     new + geom_bar(stat = "identity", fill = "#E84A27") +  labs(x = "Month", y = "Deaths") + theme(text = element_text(size = 25), legend.position = "none") + 
                       geom_text(aes(label = md), vjust = 1, color = "black", size = 4, position = position_dodge(.8))
                     #paste("New Cases=", round(as.numeric(input$deaths_per_month$y)))
                   })
                   
                   
                   
                   
                   #Plotting total cases, new cases rate and death cases rate by WHO Region
                   region_data = WHO_Global_Historical_Data %>%
                     mutate(Region = case_when(WHO_region == "AFRO" ~ "Africa",
                                               WHO_region == "EURO" ~ "Europe",
                                               WHO_region == "EMRO" ~ "Eastern Mediterranean",
                                               WHO_region == "WPRO" ~ "Western Pacific",
                                               WHO_region == "AMRO" ~ "America",
                                               WHO_region == "SEARO" ~ "South-East Asia",
                                               TRUE ~ "Other")) %>%
                     filter(Region != "Other") %>%
                     group_by(Region, Date_reported) %>%
                     summarise(New_cases = sum(New_cases),
                               Cumulative_cases = sum(Cumulative_cases),
                               New_deaths = sum(New_deaths),
                               Cumulative_deaths = sum(Cumulative_deaths),
                               New_cases_rate = sum(New_cases) / sum(Cumulative_cases),
                               New_deaths_rate = sum(New_deaths) / sum(Cumulative_deaths))
                   
                   options(scipen=10000) #changing scaling from scientific to standard form
                   output$plot_region = renderPlot({
                     ggplot(data = region_data, aes(x = Date_reported)) +
                       geom_vline(xintercept = input$date2, color = "Blue", size = 2) +
                       geom_smooth(method = "loess",se = FALSE, aes(y = eval(parse(text = input$type))), color = "#E84A27") +
                       facet_wrap( ~ Region) +
                       labs(x = "Date", y = "Number of Cases") +
                       theme(text = element_text(size=20), axis.text.x=element_text(angle=45, hjust=1))
                   })
                   
                   
                   #global tab world colored maps
                   output$plot_world = renderPlot({
                     data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
                     malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
                     mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
                     #updated more specific legend
                     do.call( addMapLegend
                              , c( mapParams
                                   , legendLabels="all"
                                   , legendWidth=0.5
                                   , legendIntervals="data"
                                   , legendMar = 2 ))
                     #label above legend
                     mtext("Cases in Thousands",side=1,line=1.5)
                   })
                   
                   
                   output$new_cases_world = renderPlot({
                     data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
                     malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
                     colourPalette <- brewer.pal(5,'YlGn')
                     
                     mapParams <- mapCountryData(malMap, nameColumnToPlot="New_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette)
                     
                     #updated more specific legend
                     do.call( addMapLegend
                              , c( mapParams
                                   , legendLabels="all"
                                   , legendWidth=0.5
                                   , legendIntervals="data"
                                   , legendMar = 2 ))
                     #label above legend
                     mtext("Cases in Thousands",side=1,line=1.5)
                   })
                   
                   output$new_cases_world = renderPlot({
                     data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
                     malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
                     mapParams <- mapCountryData(malMap, nameColumnToPlot="New_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
                     
                     #updated more specific legend
                     do.call( addMapLegend
                              , c( mapParams
                                   , legendLabels="all"
                                   , legendWidth=0.5
                                   , legendIntervals="data"
                                   , legendMar = 2 ))
                     #label above legend
                     mtext("Cases in Thousands",side=1,line=1.5)
                   })
                   
                   output$death_plot_world = renderPlot({
                     data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
                     malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
                     colourPalette <- brewer.pal(6,'RdPu')
                     mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette )
                     #updated more specific legend
                     do.call( addMapLegend
                              , c( mapParams
                                   , legendLabels="all"
                                   , legendWidth=0.5
                                   , legendIntervals="data"
                                   , legendMar = 2 ))
                     #label above legend
                     mtext("Deaths in Thousands",side=1,line=1.5)
                   })
                   
                   #global tab text box at top left explaining top right graphs  
                   output$cases_count_region = renderUI({
                     text = NULL
                     for (i in 1:6) {
                       region = paste(group_data(region_data)[i,1])
                       new = paste0(region, ": <strong>", region_data[which(region_data$Date_reported == input$date[2] & region_data$Region == region), input$type], "</strong> cases", "<br />")
                       text = paste(text, new)
                     }
                     HTML(text)
                   })
                   
                   
                   
                   
                   
                   
                   # Plotting Recovered_rate and Death_rate and Positive rate per day (US)
                   
                   
                   output$recovered_death_rate = renderPlot({
                     
                     df2 = COVID_Tracking_Project_US_Historical_Data %>% select(c('date', 'recovered', 'positive','death', 'totalTestResults','totalTestResultsIncrease','positiveIncrease'))
                     df2$recovered_rate = df2$recovered/df2$positive
                     df2$date = df2$date
                     df2$death_rate = df2$death/df2$positive
                     df2$date = df2$date
                     temp_data = df2 %>% 
                       select(c("date", "recovered_rate","death_rate")) %>%
                       filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                       gather(key = "variable", value = "value", -date) 
                     
                     ggplot(temp_data, aes(x=date, y=value)) + 
                       geom_line(aes(color = variable, linetype = variable)) + 
                       scale_color_manual(values = c("blue", "red")) +
                       theme(plot.background = element_blank(), 
                             legend.position = "bottom") 
                     df2$deathvsrecover = df2$death/df2$recovered
                     df2$date = df2$date
                     
                     df2$positive_rate = df2$positiveIncrease / df2$totalTestResultsIncrease
                     
                     ## assign color accordingly to different input lines
                     
                     if (input$type_rate == "recovered_rate") {
                       temp_data = df2 %>%
                         select(c("date", "recovered_rate")) %>%
                         filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                         gather(key = "variable", value = "value", -date)
                       colorlines = c("#E84A27")
                     }
                     
                     if (input$type_rate == "death_rate") {
                       temp_data = df2 %>%
                         select(c("date", "death_rate")) %>%
                         filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                         gather(key = "variable", value = "value", -date)
                       colorlines = c("#0455A4")
                     }
                     
                     if (input$type_rate == "deathvsrecover") {
                       temp_data = df2 %>%
                         select(c("date","deathvsrecover")) %>%
                         filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                         gather(key = "variable", value = "value", -date)
                       colorlines = c("red")
                     }
                     
                     if (input$type_rate == "positive_rate") {
                       temp_data = df2 %>%
                         select(c("date", "positive_rate")) %>%
                         filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                         gather(key = "variable", value = "value", -date)
                       colorlines = c("green")
                     }
                     
                     if (input$type_rate == "all") {
                       temp_data = df2 %>%
                         select(c("date", "recovered_rate","death_rate", "deathvsrecover", "positive_rate")) %>%
                         filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
                         gather(key = "variable", value = "value", -date)
                       colorlines = c("#E84A27", "#0455A4", "red", "green")
                     }
                     
                     ggplot(temp_data, aes(x=date, y=value)) +
                       geom_line(aes(color = variable, linetype = variable)) +
                       scale_color_manual(values = colorlines) +
                       scale_y_continuous(labels = scales::percent)+
                       theme(plot.background = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank(),
                             legend.text = element_text(color = '#E84A27',size=rel(1.5)),
                             panel.grid.major.y = element_line(colour="aliceblue"),
                             panel.background = element_blank())+
                       labs(x = "Date", y = "",
                            title = paste("Recovered Rate & Death Rate (US) between",
                                          as.character(input$Rate[1]),
                                          "and",
                                          as.character(input$Rate[2]),
                                          sep = " ")) +
                       scale_x_date(breaks = date_breaks("months"),
                                    labels = date_format("%Y-%m-%d"))
                     
                   })
                   
                   
                   
                   
                   output$testplot <- renderPlot({
                     rec_t <- COVID_Tracking_Project_US_Historical_Data %>%
                       select(date, positiveIncrease, totalTestResultsIncrease) %>%
                       filter(date >= input$date1) %>%
                       gather(key = "variables", value = "value", -date)
                     
                     ggplot(rec_t, aes(x = date, y = value)) +
                       geom_area(aes(color = variables, fill = variables),
                                 alpha = 0.5, position = position_dodge(0.8)) +
                       scale_color_manual(values = c("#FF6600", "#E7B800")) +
                       scale_fill_manual(values = c("#FF6600", "#E7B800"))+
                       scale_x_date(name = "Date", breaks = ('1 week'), date_labels = "%b%d" ) + 
                       theme(plot.background = element_blank(),
                             legend.position = "bottom",
                             legend.title = element_blank(),
                             legend.text=element_text(size=rel(1.5)),
                             panel.grid.major.y = element_line(colour="cornsilk3"),
                             panel.background = element_blank())+
                       ylab("Cases")+
                       labs(title = "US Daily Increase of Total test Result Cases & Positive Cases")
                   })
                   
                   #Create a new variable counting the increase of recovered number per day
                   national <- COVID_Tracking_Project_US_Historical_Data
                   national <- arrange(national, date) 
                   national <- mutate(national, recovered_increase = -(lag(national$recovered,1) - national$recovered))
                   #Add a new variable of month
                   national <- mutate(national, month = format(as.Date(date),"%Y-%m")) 
                   
                   sum <- as.data.frame(summarise(group_by(national,month),sum(recovered_increase)))
                   sum$`sum(recovered_increase)` <- ifelse(is.na(sum$`sum(recovered_increase)`) == TRUE, 0, sum$`sum(recovered_increase)`)
                   
                   #plot the monthly increase of recovered number
                   output$recoveredplot <- renderPlot({ggplot(sum,aes(x = month, y = `sum(recovered_increase)`,group = 1)) +
                       geom_line(col = "dodgerblue") + 
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
                       labs(x = "", y="Number of Recovered", title = "Monthly Increase of Recovered Number (Up to 2020-12-02)")
                   })
                   
                   #plot negativeIncrease per day 
                   output$negativeIncrease <- renderPlot({
                     datanegative = COVID_Tracking_Project_US_Historical_Data %>% filter(date == input$date)
                     plot_negative <- plot(negativeIncrease,date)
                     return(plot_negative)
                   })
                   
                   output$Rsquared = renderUI({
                     drop_list = case_when(input$Curve == "pos" ~ c("hospitalizedCurrently","onVentilatorCurrently"),
                                           input$Curve == "hosp" ~ c("positiveIncrease","onVentilatorCurrently"),
                                           input$Curve == "vent" ~ c("hospitalizedCurrently","positiveIncrease"))
                     
                     df3 = COVID_Tracking_Project_US_Historical_Data %>%
                       select(c('date', 'deathIncrease','hospitalizedCurrently', 'onVentilatorCurrently','positiveIncrease')) %>%
                       na.omit() %>%
                       filter((date >= input$drange[1]) & (date <= input$drange[2])) %>%
                       mutate(date2=date+input$Lag) %>%
                       select(-(drop_list))
                     
                     df_a <- df3[1:2]                                                          #Split then recombine with aligned dates
                     df_a[2] <- scale(df_a[2])                                                 #Scale (normalize) variables
                     df_b <- df3[3:4]                                                          #Columns with the numeric data only
                     df_b[1] <- scale(df_b[1])
                     df3 <- inner_join(df_a, df_b, by=c("date"="date2"))
                     
                     R2 <- cor(df3[2],df3[3])^2                                                #Calculate the R2 = corr^2
                     
                     text = "The R squared for your choices is"
                     text = paste(text, "<strong>", round(R2,3), "</strong>")
                     HTML(text)
                   })
                   
                   #Champaign County Data
                   NYTimes_US_Counties_Historical_Data$recoveries = NYTimes_US_Counties_Historical_Data$cases - NYTimes_US_Counties_Historical_Data$deaths
                   output$champaign_cases = renderPlot({
                     ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
                     
                     ggplot(ccd2, aes(x=date, y=cases))+
                       geom_line(colour = "darkorange", size = 1.5) +
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "gray30"), axis.ticks=element_blank(), axis.text = element_text(color = "gray30", size = 9), panel.grid=element_line(size = 0.5, colour = "gray70", linetype = "dashed"), legend.background = element_blank(), axis.title = element_text(color = "gray30", size = 10), plot.title = element_text(color = "gray30", size = 19))+
                       labs(x = "", y="Cumulative Cases", title = "Champaign County Cumulative Covid Cases")
                   })
                   
                   output$hover_info <- renderPrint({
                     if(!is.null(input$plot_hover)){
                       hover=input$plot_hover
                       cat("Cumulative Covid Cases :\n")
                       str(input$plot_hover$y)
                     }
                   })
                   
                   
                   
                   output$champaign_deaths = renderPlot({
                     ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
                     
                     ggplot(ccd2, aes(x=date, y=deaths))+
                       geom_line() +
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
                       labs(x = "", y="Cumulative Deaths", title = "Champaign County Cumulative Covid Deaths")
                   })
                   
                   output$hover_info2 <- renderPrint({
                     if(!is.null(input$plot_hover)){
                       hover=input$plot_hover
                       cat("Cumulative Covid Death Cases :\n")
                       str(input$plot_hover$y)
                     }
                   })
                    output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
                   output$champaign_recoveries = renderPlot({
                     ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
                     
                     ggplot(ccd2, aes(x=date, y=recoveries))+
                       geom_line() +
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
                       labs(x = "", y="Cumulative Recoveries", title = "Champaign County Cumulative Recoveries")
                   })
                   
                   output$hover_info3 <- renderPrint({
                     if(!is.null(input$plot_hover)){
                       hover=input$plot_hover
                       cat("Cumulative Covid Recovery Cases :\n")
                       str(input$plot_hover$y)
                     }
                   })
                   
                   
                   
                   
                   
                   output$Ventilator_Usage = renderPlot({
                     
                     df3 = COVID_Tracking_Project_US_Historical_Data %>% 
                       select(c('date', 'hospitalizedCurrently', 'onVentilatorCurrently')) %>%
                       na.omit() %>%
                       mutate(ventilator_rate = onVentilatorCurrently/hospitalizedCurrently)  %>%
                       filter((date >= input$Ventilator[1]) & (date <= input$Ventilator[2])) 
                     
                     colors <- c("Max Ventilator Rate" = "firebrick2")
                     
                     ggplot(data = df3, mapping = aes(x = date, y = ventilator_rate)) + 
                       theme_bw() +
                       theme(legend.title=element_blank(), legend.position = c(0.9, 0.9)) +
                       theme(legend.background = element_rect(size=0.5, linetype="solid", color="gray50")) +
                       
                       labs(x="Date", y="Ventilator Usage Rate") +
                       
                       geom_line(color="darkblue",size=1.5) + 
                       scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
                       scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
                       geom_point(data=slice_max(df3,df3$ventilator_rate),aes(color="Max Ventilator Rate"), size=5) +      #Max point
                       annotate("label", x = df3$date[which.max(df3$ventilator_rate)]+3, y=df3$ventilator_rate[which.max(df3$ventilator_rate)], 
                                label = round(df3$ventilator_rate[which.max(df3$ventilator_rate)],2), hjust = 0, 
                                color="gray50") +
                       
                       geom_hline(yintercept=mean(df3$ventilator_rate),linetype="dashed", color="gray50") +                #Mean line
                       annotate("label", x = df3$date[which.min(df3$date)], 
                                y = mean(df3$ventilator_rate),label = "Average Ventilator Rate", hjust = 0, 
                                color="gray50", fill="white", label.size=NA) 
                     
                   })
                   
                   
                   ranges <-reactiveValues(x = NULL, y = NULL)
                   
                   output$plot1 <- renderPlot({
                     ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
                       geom_point() +
                       coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
                       xlab("Cumulative Cases")+
                       ylab("Cumulative Deaths")
                   })
                   
                   observeEvent(input$plot1_dblclick,{
                     brush <-input$plot1_brush
                     if (!is.null(brush)){
                       ranges$x <-c(brush$xmin, brush$xmax)
                       ranges$y <- c(brush$ymin, brush$ymax)
                     } else {
                       ranges$x <- NULL
                       ranges$y <- NULL
                     }
                   })
                   
                   ranges2 <- reactiveValues(x = NULL, y = NULL)
                   
                   output$plot2 <- renderPlot({
                     ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
                       geom_point()+
                       xlab("Cumulative Cases")+
                       ylab("Cumulative Deaths")
                   })
                   
                   output$plot3 <- renderPlot({
                     ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
                       geom_point() +
                       coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
                       xlab("Cumulative Cases")+
                       ylab("Cumulative Deaths")
                   })
                   
                   output$lol <- renderPlot({
                     
                     date_range = input$date_range_covid
                     data = Champaign_data %>% filter(date_range[1] < date & date < date_range[2])
                     type = data$new_cases
                     color = "new_cases"
                     
                     if(input$Graph_Type == "New Cases") {
                       type = data$new_cases
                       color = "new_cases"
                     }
                     else if (input$Graph_Type == "Total Cases"){
                       type = data$cases
                       color = "cases"
                     }
                     else if (input$Graph_Type == "New Deaths"){
                       type = data$new_deaths
                       color = "new_deaths"
                     }
                     else {
                       type = data$deaths
                       color = "deaths"
                     }
                     
                     ggplot(data, aes(x = `date`)) +
                       geom_line(aes(y = type, color = color), size = 1)+
                       scale_color_manual("",values = "deepskyblue4")+
                       labs(y= paste("Number of ",color,sep=""), x = "Date")+
                       theme(legend.position = "none",
                             panel.background = element_rect(fill="gray40",colour = "Black"),
                             panel.grid.major = element_line(colour = "sienna3",size = .5),
                             panel.grid.minor = element_blank(),
                             plot.subtitle = element_text(size = 10))
                   }  
                   )
                   
                   observe({
                     brush <- input$plot2_brush
                     if (!is.null(brush)) {
                       ranges2$x <- c(brush$xmin, brush$xmax)
                       ranges2$y <- c(brush$ymin, brush$ymax)
                       
                     } else {
                       ranges2$x <- NULL
                       ranges2$y <- NULL
                     }
                   })
                   
                   #plotting estimated number of current cases
                   output$county <- renderPlot({
                     max.cases = NYTimes_US_Counties_Historical_Data %>% group_by(county, state) %>% slice(which.max(cases))
                     cases.2weeks = filter(NYTimes_US_Counties_Historical_Data, date < today() - 14)
                     max.cases.2weeks = cases.2weeks %>% group_by(county, state) %>% slice(which.max(cases))
                     twoweeks = inner_join(max.cases, max.cases.2weeks, by = c("county", "state"))
                     twoweeks$CASES = twoweeks$cases.x - twoweeks$cases.y
                     names(twoweeks)[names(twoweeks) == "fips.x"] = "fips"
                     if(input$state.input == "All States"){
                       countycases = twoweeks
                     }
                     else{
                       countycases = twoweeks %>% filter(state == input$state.input)
                     }
                     states <- unique(state.abb[match(countycases$state, state.name)])
                     plot <- plot_usmap(regions = "counties",
                                        include = states, labels = ifelse(input$state.input == "All States", FALSE, TRUE),
                                        data = countycases[, c(4, 11)], values = "CASES") +
                       theme(panel.background = element_rect(color = "black", fill = "white")) +
                       scale_fill_continuous(low = "yellow1", high = "firebrick4", na.value = "snow2") +
                       labs(title = ifelse(input$state.input == "All States", "All Counties", states),
                            subtitle = paste0("Number of Cases in Past 14 Days in ", input$state.input), fill = "Estimated Current Cases") 
                     plot$layers[[2]]$aes_params$size <- 3
                     return(plot)
                   })
                   
                   #output the daily growth rate of cases in Champaign area with the selected time range
                   output$champaign_growth <- renderPlot({
                     datas = NYTimes_US_Counties_Historical_Data %>%
                       filter(
                         county == "Champaign",
                         state == "Illinois",
                         as.Date(date) < input$champaign_growth_date
                       ) %>%
                       select(cases, date)
                     datas$date = as.Date(datas$date)
                     temp = c(0)
                     
                     datas$date = as.Date(datas$date)
                     
                     temp = c()
                     
                     for (i in 1:length(datas$cases)) {
                       temp[i] = (datas$cases[i + 1] - datas$cases[i]) / datas$cases[i]
                     }
                     
                     datas$rate = temp
                     ggplot(data = datas) +
                       geom_line(mapping = aes(x = date, y = rate)) +
                       scale_x_continuous(breaks = seq(min(datas$date), max(datas$date), by = "1 weeks"), name = "Date") +
                       scale_y_continuous(
                         breaks = seq(0, 1, by = 0.25),
                         labels = scales::percent_format(accuracy = 0.1),
                         name = "Daily Growth Rate"
                       ) + theme(axis.text.x=element_text(angle=70, hjust=1))
                     
                     
                   })
                   
                   #output the cases of Champaign at selected date
                   output$champaign_cases_search <- renderText({
                     cas = NYTimes_US_Counties_Historical_Data %>% filter(county == "Champaign", state == "Illinois") %>% select(cases, date)
                     out <-
                       paste("At",
                             input$champaignCasesSearch_Input,
                             "there are",
                             cas[which(cas$date == input$champaignCasesSearch_Input), ]$cases,
                             "cases in Champaign area")
                     
                   })
                   
                   #plot current hospitalized
                   output$hos <- renderPlot({
                     ggplot(data = COVID_Tracking_Project_US_Historical_Data, aes(x=date, y=hospitalizedCurrently))+
                       geom_line() +
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.text = element_text(color = "black"), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
                       labs(x = "", y="hospitalizedCurrently", title = "Current Hospitalized patients")+ xlab("Date")+ scale_x_date(date_breaks = "week", date_labels =  "%Y-%m-%d") + theme(axis.text.x=element_text(angle=75, hjust=1))
                   })
                   
                   #Draw the trend line of the change of the rate
                   output$hos1 <- renderPlot({ggplot(us_data)+geom_line(aes(x = us_data$date, y = us_data$hosp_rate*250/2), group=1)+
                       labs(x= "date", y= "Hopsitalization Rate", title = "Hospitalization rate by date") +
                       theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))
                   })
                   
                   #Input the date to check the rate of the day
                   output$hosp_rate <- renderText({
                     us_data$hosp_rate[us_data$date==input$date]
                   })  
                   
                   output$percent_pos_state = renderText({
                     population = read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-popchg2010_2019.csv")
                     state.pop = population %>% slice(which(NAME %in% state.name))
                     
                     covid = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
                     statecases = covid %>% group_by(state) %>% slice(which.max(cases)) %>% slice(which(state %in% state.name))
                     which(statecases$state %in% state.name)
                     state.percent = data.frame(statecases$state, statecases$cases / state.pop$POPESTIMATE2019 * 100)
                     names(state.percent)[names(state.percent) == "statecases.cases.state.pop.POPESTIMATE2019...100"] = "percent"
                     
                     
                     percentpop = function(state){
                       states = which(state.name == state)
                       a = statecases[states, 4]
                       b =  state.pop[states, 16]
                       paste(round(a[[1]] / b, 2) * 100, "% of", state, "has tested positive for COVID-19")
                     }
                     
                     
                     percentpop(input$state0)
                   })
                   
                   output$testing_site = renderText({
                     
                     get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
                       loadNamespace("purrr")
                       loadNamespace("geosphere")
                       longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
                       longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
                       distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
                       distance_m = sapply(distance_list, function(col) { col[1] })
                       
                       if (units == "km") {
                         distance = distance_m / 1000.0;
                       }
                       else if (units == "miles") {
                         distance = distance_m / 1609.344
                       }
                       else {
                         distance = distance_m
                       }
                       distance
                     }
                     # https://www.latlong.net/
                     
                     lat = input$lat
                     long = 	input$long
                     lat_long = c(lat,long)
                     
                     testing_sites = data.frame(site = c("CRCE", "Illini Union", "State Farm Center", "SDRP", "Veterinary Medicine"),
                                                long = c(40.104141, 40.10939235	, 40.0962421, 40.10409, 40.101877),
                                                lat = c(-88.221538, -88.2272187093397, -88.2359287628109, -88.2394624, -88.219142))
                     
                     
                     
                     distances = map2_dbl(testing_sites$long, testing_sites$lat, ~get_geo_distance(.x, .y, lat_long[1], lat_long[2]))
                     
                     paste(testing_sites[which.min(distances), 1], "is the closest testing site to your address.")
                   })
                   
                   
                   url = a("Click Here", href= "https://stevemorse.org/jcal/latlon.php")
                   output$tab <- renderUI({
                     tagList("URL link:", url)
                   })
                   
                   #County plot for Big Ten universities
                    output$county_plot <- renderPlot({

                        newDf = TotalCollege %>%
                            select(c('Date', 'Tests', 'Country_Region', 'Confirmed' )) %>%
                            na.omit() %>%
                            filter((Date >= input$num_date[1]) & (Date <= input$num_date[2])) %>%
                            filter(Country_Region == input$inputSchool)

                        ggplot(data = newDf, mapping = aes(x = Date, y = Confirmed)) +
                            geom_line(color="Blue",size=1.0) +
                            labs(x = "Date", y ="Confirmed Cases")

                    })

                    #County plot for cases ACC, B12, PAC-12, and SEC universities
                    output$county_NYTimes_plot <- renderPlot({

                      newDf = NYTimes_US_Counties_Historical_Data %>%
                        select(c('date', 'cases', 'county', 'deaths' )) %>%
                        na.omit() %>%
                        filter((date >= input$num_date[1]) & (date <= input$num_date[2])) %>%
                        filter(county == input$inputSchool)

                      ggplot(data = newDf, mapping = aes(x = date, y = cases)) +
                        geom_line(color="Blue", size=1.0) +
                        labs(x = "Date", y ="Confirmed Cases")

                    })

                    #County plot for new cases in Big Ten universities
                    output$county_plot_new <- renderPlot({

                      newDf = TotalCollege %>%
                        select(c('Date', 'Tests', 'Country_Region', 'Confirmed' )) %>%
                        na.omit() %>%
                        filter((Date >= input$num_date[1]) & (Date <= input$num_date[2])) %>%
                        filter(Country_Region == input$inputSchool)

                      ggplot(data = newDf, mapping = aes(x = Date, y = Confirmed)) +
                        geom_line(color="Red",size=1.0) +
                        labs(x = "Date", y ="Confirmed Cases")

                    })

                    #County plot for new deaths in ACC, B12, PAC-12, and SEC universities
                    output$county_ny_times_deaths_plot <- renderPlot({

                      newDf = ny_times %>%
                        select(c('date', 'cases', 'county', 'deaths', 'delta deaths')) %>%
                        na.omit() %>%
                        filter((date >= input$num_date[1]) & (date <= input$num_date[2])) %>%
                        filter(county == input$inputSchool)

                      ggplot(data = newDf, mapping = aes(x = date, y = `delta deaths`)) +
                        geom_line(color="Red", size=1.0) +
                        labs(x = "Date", y ="Change in Deaths")

                    })
                     
                   ##Deleted extra bracket and parentheses causing missing comma error
                   output$treemap = renderPlot({
                     WHO_Global_Historical_Data$WHO_region <- as.factor(WHO_Global_Historical_Data$WHO_region)
                     day_cul_cases <- WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
                     AFRO <- day_cul_cases %>% filter(WHO_region == "AFRO")
                     AFRO_cases <- sum(AFRO$Cumulative_cases)
                     AMRO <- day_cul_cases %>% filter(WHO_region == "AMRO")
                     AMRO_cases <- sum(AMRO$Cumulative_cases)
                     EMRO <- day_cul_cases %>% filter(WHO_region == "EMRO")
                     EMRO_cases <- sum(EMRO$Cumulative_cases)
                     EURO <- day_cul_cases %>% filter(WHO_region == "EURO")
                     EURO_cases <- sum(EURO$Cumulative_cases)
                     Other <- day_cul_cases %>% filter(WHO_region == "Other")
                     Other_cases <- sum(Other$Cumulative_cases)
                     SEARO <- day_cul_cases %>% filter(WHO_region == "SEARO")
                     SEARO_cases <- sum(SEARO$Cumulative_cases)
                     WPRO <- day_cul_cases %>% filter(WHO_region == "WPRO")
                     WPRO_cases <- sum(WPRO$Cumulative_cases)
                     WPRO <- day_cul_cases %>% filter(WHO_region == "WPRO")
                     WPRO_cases <- sum(WPRO$Cumulative_cases)
                     
                     day_cumu_cases_region <-c(AFRO_cases, AMRO_cases,EMRO_cases, EURO_cases,Other_cases,SEARO_cases,WPRO_cases )
                     group <- c("AFRO", "AMRO", "EMRO", "EURO", "Other", "SEARO", "WPRO" )
                     data <- data.frame(group, day_cumu_cases_region)
                     
                     treemap(data,
                             index="group",
                             vSize="day_cumu_cases_region",
                             type="index",
                             title = "Treemap of Cumulative Cases Based on WHO Regions on a Specific Day")
                   })
                      #Used the death ratio to predict the death number with an input
  COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
  ui <- fluidPage(
    titlePanel("Potential Death Counts"),
        sidebarPanel(
      numericInput(inputId = "Cases",
                   label = "Choose a Number",
                   value = 100,
                   min = 0,
                   max = 100000,
                   step = 1
      ),
      textOutput("Predicted Death Counts"),
      checkboxInput("do1", "Plot New Confirmed Cases in Champaign_data", 
                    value = T)
    ),
    mainPanel(fluidRow(
        plotOutput(
          outputId = "plotgraph1",
          width  = "800px",
          height = "800px"
        )
    )
  )
  )
  
  totaldeath<-sum(COVID_Tracking_Project_US_Historical_Data$death,na.rm = TRUE)
  totalpos<-sum(COVID_Tracking_Project_US_Historical_Data$positive)
  
  server <- function(input, output){
    output$"Predicted Death Counts" <- renderText({
      paste( "Death: The predicted death number is :", as.character(as.numeric(input$Cases)*totaldeath/totalpos), "people")
    })
    pt1 <- reactive({
      input$do1
      if (input$do1) {
        return(
          plot(
            Champaign_data$new_cases,
            type = "l",
            col = "black",
            lwd = 3,
            ylim = range(pretty(Champaign_data$new_cases)),
            xlab = "Day",
            ylab = "New confirmed cases",
            main = "New confirmed cases in Champaign"
          )
        )
      } else {
        return(NULL)
      }
    })
    output$plotgraph1 <- renderPlot({
      pt1()
    })
  }
  
  #Created a Bar chart to see the death vs positive cases more directly
  totaldeath<-sum(COVID_Tracking_Project_US_Historical_Data$death,na.rm = TRUE)
  totalpos<-sum(COVID_Tracking_Project_US_Historical_Data$positive)
  
  barplot(height = c(totaldeath, totalpos), xlab = c("Death vs Positive Cases"), col=c("blue","red"), cex.names = c("Death","Positive"))

  #Added death rate to the NYTimes and Champaign data
  NYTimes_US_Historical_Data$death_rate <- NYTimes_US_Historical_Data$deaths/NYTimes_US_Historical_Data$cases
  NYTimes_US_States_Historical_Data$death_rate <- NYTimes_US_States_Historical_Data$deaths/NYTimes_US_States_Historical_Data$cases
  NYTimes_US_Counties_Historical_Data$death_rate <- NYTimes_US_Counties_Historical_Data$deaths/NYTimes_US_Counties_Historical_Data$cases
  Champaign_data$death_rate <- Champaign_data$deaths/Champaign_data$cases
  
                                              
                 }
                 
                 shinyApp(ui = ui, server = server)
                 
