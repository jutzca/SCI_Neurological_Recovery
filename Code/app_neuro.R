# -----------------------------------------------------------------------------------
# Building a Shiny app for visualisation of neurological outcomes in SCI
#
# July 8, 2020
# L. Bourguignon
# -----------------------------------------------------------------------------------

# Set working directory ----
#setwd('/Volumes/borgwardt/Projects/SCI_Neurological_Outcome/App/')

# Load packages ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(assertthat)
library(plyr)
library(dplyr)
library('ggthemes') 
library(ggpubr)
library(ggridges)
library(gridExtra)

library(sjPlot)
library(jtools)
library(reshape2)
library(PMCMRplus)

#library(epicalc)
library(EpiReport)
library(epiDisplay)
library(naniar)

library(boot)
library(table1)
library(broom)
#library(pander)
library(gtable)
library(grid)
library(tidyr)
library(Hmisc)
library(RColorBrewer)
library(lme4)

library(DT)
library(shinyjs)
library(sodium)
library(shinymanager)


# Source helper functions -----
source("helper_functions.R")

# Load data ----
data_emsci <- read.csv('data/df_emsci_formatted.csv')
data_emsci$ExamStage <- as.factor(data_emsci$ExamStage)
data_emsci$ExamStage <- relevel(data_emsci$ExamStage, ref = "very acute")
data_sygen <- read.csv('data/df_sygen_formatted_2.csv')
data_SCI_rehab <- read.csv('data/df_rehab_formatted.csv')
data_All <- read.csv('data/df_all_formatted.csv')
data_age_emsci <- read.csv('data/emsci_age.csv')
data_emsci_epi <- read.csv('data/emsci.csv')
data_emsci_epi$ExamStage <- as.factor(data_emsci_epi$ExamStage)
data_emsci_epi$ExamStage <- relevel(data_emsci_epi$ExamStage, ref = "very acute")
data_sygen_epi <- read.csv('data/sygen_epi.csv')
data_SCI_rehab_epi <- read.csv('data/df_rehab_epi.csv')

#data_sygen_epi_save = data_sygen_epi
# Functions ----

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

# ref for authentication step : https://datastorm-open.github.io/shinymanager/
credentials <- data.frame(
  user = c("user", "admin"), # mandatory
  password = c("sciapp", "sciadmin"), # mandatory
  start = c("2020-08-31"), # optional (all others)
  expire = c(NA, NA),
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

# User interface ----
ui <- dashboardPage(skin = "blue",
      dashboardHeader(title = 'Menu'),

      ## Sidebar content
      dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    convertMenuItem(menuItem("Overview", tabName = "dashboard",icon = icon("object-group"), selected = T, startExpanded = TRUE,
                                   menuSubItem("EMSCI", tabName = "tabEMSCI", icon = icon('hubspot')),
                                   menuSubItem("Sygen", tabName = "tabSygen", icon = icon('database')),
                                   menuSubItem("SCIRehab", tabName = "tabSCIrehab", icon = icon('accessible-icon')),
                                   menuSubItem("Abbreviations", tabName = "Abbreviations", icon = icon('language'))), tabName = "dashboardG"),
          
                    convertMenuItem(menuItem("Visualize data", tabName = "PlotsTab", icon = icon("chart-bar")), tabName = "plotsG"),
                    convertMenuItem(menuItem("Monitoring", tabName = "PredictTab", icon = icon("clipboard-list")), tabName = "predictG"),
          
          conditionalPanel("input.sidebarmenu == 'plotsG'",
                           radioButtons(inputId = "choose_data",
                                        label = "Choose data source",
                                        choices = c("EMSCI" = "EMSCI",
                                                    "Sygen" = "Sygen",
                                                    "SCI Rehab" = "SCI rehab",
                                                    "All" = "All"),
                                        selected = character(0)),
          
                           radioButtons(inputId = "choose_cat_scores",
                                        label = "Choose type of data",
                                        choices = c("Neurological outcomes" = "neuro",
                                                    "Functional outcomes" = "funct",
                                                    "Epidemiological features" = "epi"),
                                        selected = character(0)),
                           
                           conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro'",
                                            radioButtons("EMSCI_neuro",
                                                         label = "Outcomes available",
                                                         choices = c("UEMS","RUEMS","LUEMS",
                                                                     "LEMS","RLEMS","LLEMS",
                                                                     "RMS","LMS","TMS",
                                                                     "RPP","LPP","TPP",
                                                                     "RLT","LLT","TLT"),
                                                         selected = character(0))),
                           
                           conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct'",
                                            radioButtons("EMSCI_funct",
                                                         label = "Outcomes available",
                                                         choices = c("WISCI","test_6min","test_10m","TUG","SCIM2","SCIM3"),
                                                         selected = character(0))),
                                           
                            conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro'",
                                             radioButtons("Sygen_neuro",
                                                          label = "Outcomes available",
                                                          choices = c("UEMS", "LEMS", "TEMS",
                                                                      "RMS", "LMS", "TMS",
                                                                      "RPP","LPP","TPP",
                                                                      "RLT","LLT","TLT"),
                                                          selected = character(0))),
                                           
                            conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'funct'",
                                                            radioButtons("Sygen_funct",
                                                                         label = "Outcomes available",
                                                                         choices = c('Modified Benzel score' = 'Benzel'),
                                                                         selected = character(0))),
                                           
                            conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'neuro'",
                                                            radioButtons("SCI_rehab_neuro",
                                                                         label = "Outcomes available",
                                                                         choices = c("RMS","LMS","TMS"),
                                                                         selected = character(0))),
                                           
                            conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'funct'",
                                                            radioButtons("SCI_rehab_funct",
                                                                         label = "Outcomes available",
                                                                         choices = c("CHART physical independence" = "PHYIND",
                                                                                     "CHART mobility" = "MOBILITY", "CHART occupational" = "OCCUPATION",
                                                                                     "CHART social integration" = "SOCIAL"),
                                                                         selected = character(0)
                                                                         ) # end radioButtons
                                             ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'neuro'",
                                            radioButtons("All_neuro",
                                                         label = "Outcomes available",
                                                         choices = c("RMS", "LMS", "TMS"),
                                                         selected = character(0)
                                            ) # end radioButtons
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'epi'",
                                            radioButtons("EMSCI_epi",
                                                         label = "Features available",
                                                         choices = c("Sex", "Age", "AIS grade", "NLI"),
                                                         selected = character(0)
                                            ) # end radioButtons
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'epi'",
                                            radioButtons("Sygen_epi",
                                                         label = "Features available",
                                                         choices = c("Sex", "Age", "AIS grade", "NLI"),
                                                         selected = character(0)
                                            ) # end radioButtons
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'epi'",
                                            radioButtons("SCI_rehab_epi",
                                                         label = "Features available",
                                                         choices = c("Sex", "Age", "AIS grade", "NLI"),
                                                         selected = character(0)
                                            ) # end radioButtons
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'funct'",
                                            p('No outcome available.')
                           ), # end conditionalPanel
                           
                           conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'epi'",
                                            p('No outcome available.')
                           ) # end conditionalPanel
                           
                           ) # end conditionalPanel 1
          ) # end sidebarMenu
        ), # end dashboardSidebar

      dashboardBody(
        tabItems(

          # Dashboard page (dashboard)
          tabItem(tabName = "dashboard",
                  #p('This interface is meant to help visualising findings reported in :'),
                  titlePanel(strong("Neurosurveillance after a spinal cord injury")),
                  #p(em('In preparation')),
                  #p('Lucie Bourguignon, Anh Khoa Vo, Bobo Tong, Fred Geisler, Orpheus Mach, Karsten Borgwardt, John L.K. Kramer, Lukas Grassner, Catherine R. Jutzeler'),
                  # h3('Abstract'),
                  # p(em('Objective.'), ''),
                  # p(em('Methods.'), ''),
                  # p(em('Results.')),
                  # p(em('Conclusions and Relevance.')),
                  # br(),
                  
                  box(width = 12, status = "primary", align = "center",
                      uiOutput("video")),
                  
                  p(icon('scroll'), a('Full text', href ="https://www.google.com/", target="_blank")),
                  p(icon('quote-right'), a('Citation', href ="https://www.google.com/", target="_blank")),
                  p(icon('github'), a('GitHub repository', href ="https://www.google.com/", target="_blank")),
                  p(icon('address-card'), 'Corresponding author :', a('catherine.jutzeler@bsse.ethz.ch', href = 'mailto:catherine.jutzeler@bsse.ethz.ch')),
                  p(icon('question-circle'), 'Troubleshooting and suggestions :', a('lucie.bourguignon@bsse.ethz.ch', href = 'mailto:lucie.bourguignon@bsse.ethz.ch?cc=catherine.jutzeler@bsse.ethz.ch&subject=Troubleshooting neurosurveillance app'))
                  ),
          
          tabItem(tabName = "tabEMSCI",
                  titlePanel(title = div(strong("European Multicenter Study about Spinal Cord Injury"), img(src="EMSCI_logo.png",  height="15%", width="15%", align = 'right'))),
                  fluidRow( # create a separation in the panel
                    column(width = 6, # create first column for boxplot
                           box(width = NULL, status = "primary",
                              p('The', a(strong('European multicenter study about spinal cord injury (EMSCI)'), href='https://www.emsci.org/', target="_blank"),  
                              ' is a longitudinal observational study involving 22 SCI centers across Europe and India.', align = "justify"),
                              p('Data from', strong('more than 5,000 patients'), 'have been collected', 
                                strong('since 2001'), ', making it one of the largest spinal cord injury (SCI) biobank.'),
                              p('It gathers together information about', strong('patient and injury characteristics', align = "justify"),
                                'as well as', strong('neurological and functional outcomes'), 'according to a fixed 
                                time schedule. Overall, each variable is reported 5 times : at', 
                                strong('very acute, acute I, acute II, acute III and chronic stages'), 
                                '(see details in the table below).', align = "justify")
                           ),#end box
                           box(width = NULL, status = "primary", align = 'center',
                               img(src="try.png"))
                    )#end column
                  )#end fluidRow
          ), #end tabItem
          
          tabItem(tabName = "tabSygen",
                  titlePanel(title = div(strong("The Sygen Multicenter Acute Spinal Cord Injury Study"), img(src="GM1.png",  height="20%", width="20%", align = 'right'))),
                  fluidRow( # create a separation in the panel
                    column(width = 6, # create first column for boxplot
                           box(width = NULL, status = "primary",
                              p('The ', strong('Sygen data source'), 'is the result of a ', strong('multi-center prospective phase III 
                                clinical trial'), ' evaluating the efficacy of GM-1 ganglioside in treating acute spinal cord injury (SCI).', align = "justify"), 
                              p('From ', strong('1992'), ' to', strong('1998'), ', ',  strong('797 patients'),' with traumatic 
                                SCI were included across the United States of America (USA). 
                                Throughout the trial,', strong('patient and injury characteristics, neurological and functional 
                                outcomes'), 'as well as', strong('hematological markers'), 'were periodically collected, 
                                with up to 9 measurements per marker (baseline, 1, 2, 4, 8, 16, 26, 52, 52+ weeks after injury).', align = "justify"),
                              p('Full design, recruitment and enrollment details can be found ', 
                                a(strong('here'), 
                                  href ="https://journals.lww.com/spinejournal/Fulltext/2001/12151/The_Sygen__Multicenter_Acute_Spinal_Cord_Injury.15.aspx", 
                                  target="_blank"), '.', align = "justify") #end paragraph
                           ) #end box
                    ) #end column
                  ) #end fluidRow
                  ), #end tabItem
          
          tabItem(tabName = "Abbreviations",
                  titlePanel(strong("Dictionary of abbreviations")),
                  fluidRow(
                    column(width = 6,
                           box(width = NULL, status = "primary",
                             h4(strong('General')),
                             p(strong('SCI'), 'spinal cord injury'),
                             p(strong(a('ASIA', href ="https://asia-spinalinjury.org/", target="_blank")), 'american spinal injury association'),
                             p(strong(a('EMSCI', href ="https://www.emsci.org/", target="_blank")), 'european multicenter study about spinal cord injury'),
                             p(strong('PBE'), 'practice-based evidence')
                            ),
                           
                           box(width = NULL, status = "primary",
                               h4(strong('Functional outcomes')),
                               p(strong(a('WISCI', href = "http://www.spinalcordcenter.org/research/wisci_guide.pdf", target="_blank")), 'walking index for spinal cord injury'),
                               p(strong(a('test_6min', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '6 minutes walking test'),
                               p(strong(a('test_10m', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), '10 meters walking test'),
                               p(strong(a('TUG', href = "https://www.emsci.org/index.php/project/the-assessments/functional-test", target="_blank")), 'timed up and go test'),
                               p(strong(a('SCIM2', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 2'),
                               p(strong(a('SCIM3', href = "https://www.emsci.org/index.php/project/the-assessments/independence", target="_blank")), 'spinal cord independence measure type 3'),
                               p(strong('benzel'), 'modified benzel classification')
                           )
                           
                    ), # end column
                    
                    column(width = 6,
                           box(width = NULL, status = "primary",
                             h4(strong(a('Neurological outcomes', href ="https://asia-spinalinjury.org/wp-content/uploads/2016/02/International_Stds_Diagram_Worksheet.pdf", target="_blank"))),
                             p(strong('AIS'), 'ASIA impairment scale'),
                             p(strong('UEMS'), 'upper extremity motor score'),
                             p(strong('RUEMS'), 'right upper extremity motor score'),
                             p(strong('LUEMS'), 'left upper extremity motor score'),
                             p(strong('LEMS'), 'lower extremity motor score'),
                             p(strong('RLEMS'), 'right lower extremity motor score'),
                             p(strong('LLEMS'), 'left lower extremity motor score'),
                             p(strong('RMS'), 'right motor score'),
                             p(strong('LMS'), 'left motor score'),
                             p(strong('TMS'), 'total motor score'),
                             p(strong('RPP'), 'right pin prick'),
                             p(strong('LPP'), 'left pin prick'),
                             p(strong('TPP'), 'total pin prick'),
                             p(strong('RLT'), 'right light touch'),
                             p(strong('LLT'), 'left light touch'),
                             p(strong('TLT'), 'total light touch')
                           )
                           
                      ) # end column
                    ) # end fluidRow
                  ), # end tabItem
          
          tabItem(tabName = "tabSCIrehab",
                  titlePanel(title = div(strong("The SCIRehab Project"), img(src="SCIRehab_logo.png",  height="15%", width="15%", align = 'right'))),
                  fluidRow( # create a separation in the panel
                    column(width = 6, # create first column for boxplot
                           box(width = NULL, status = "primary",
                                p('The', a(strong('SCIRehab Project'), href ="https://www.icpsr.umich.edu/web/ADDEP/studies/36724", target="_blank"), 
                                  'is a', strong('5-year multi-center longitudinal study'), '. A total of', strong('1,376 patients'), 'with traumatic 
                                  spinal cord injury (SCI) were recruited from', strong('2007'), ' to', strong('2009'), 
                                  ' in 6 participating SCI centers (Craig Hospital, 
                                  Englewood, CO ; Carolinas Rehabilitation, Charlotte, NC ; The Mount Sinai Medical Center, New York, 
                                  NY ; MedStar National Rehabilitation Hospital, Washington, DC ;  Rehabilitation Institute of Chicago, 
                                  Chicago, IL ; Shepherd Center, Atlanta, GA).', align = "justify"), #end paragraph
                                p('This project was proposed to overcome ethical limitations of conducting randomized controlled 
                                  trials (RCTs) in rehabilitation settings. Instead, the data collected are used to determine which 
                                  rehabilitation procedures are associated with better outcomes after traumatic SCI through',
                                  strong('practice-based evidence'), ' (PBE).', align = "justify"), #end paragraph
                                p('Available data include', strong('patient and injury characteristics, 
                                  rehabilitation therapies, admission and discharge function'), 
                                  ' as well as', strong(' neurological and functional outcomes.'), align = "justify") #end paragraph
                           ) #end box
                    ) #end column
                  ) #end fluidRow
                  ), #end tabItem
          
          tabItem(tabName = "PlotsTab",
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'UEMS'", # based on user inputs, display different plots
                                   htmlOutput("title_UEMS_EMSCI"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_UEMS_EMSCI', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                      
                                      column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_UEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box

                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_UEMS_EMSCI', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'), # different choices of stages (match the levels in dataset)
                                                                selected = c("very acute", 'chronic'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box

                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_UEMS_EMSCI", # create new check box group
                                                                                 label = "Filters:", # label of the box
                                                                                 choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'), # choices are the different filters that can be applied
                                                                                 selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Male", "Female"), # choices are male and female (match the levels in EMSCI dataset)
                                                                                    selected = c("Male", "Female")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4), # choices are age group divided in 20 years (match the levels in EMSCI dataset from categorised column)
                                                                                    selected = c(0,1,2,3,4)) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"), # choices (match the levels in EMSCI dataset)
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D", "AIS E"), # choices (match the levels in EMSCI dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D", "AIS E")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"), # choices (match the levels in EMSCI dataset)
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('6')", # if user chooses to filter based on country
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "country_UEMS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on country:", # label of the box
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"), # choices (match the levels in EMSCI dataset)
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland")) # by default, all countries are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_EMSCI == 0 && input.checkGroup_UEMS_EMSCI.includes('7')", # if user chooses to filter based on year of injury (categorised)
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 sliderTextInput(inputId = "year_UEMS_EMSCI", # create new slider text
                                                                                 label = "Filter based on neurological year of injury:", # label of the box
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020), # choices (match the levels in EMSCI dataset in categorised column) 
                                                                                 selected = c(2000, 2020), # by default, all groups are selected
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE) 
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                      ) #end column
                                   ) #end fluidRow
                                   ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'RUEMS'",
                                   htmlOutput("title_RUEMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_RUEMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_RUEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_RUEMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_RUEMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_RUEMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_RUEMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_RUEMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_RUEMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_RUEMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_RUEMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RUEMS_EMSCI == 0 && input.checkGroup_RUEMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_RUEMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LUEMS'",
                                   htmlOutput("title_LUEMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LUEMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LUEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LUEMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LUEMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LUEMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LUEMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LUEMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LUEMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LUEMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LUEMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LUEMS_EMSCI == 0 && input.checkGroup_LUEMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LUEMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LEMS'",
                                   htmlOutput("title_LEMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LEMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LEMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LEMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LEMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LEMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LEMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LEMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LEMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LEMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_EMSCI == 0 && input.checkGroup_LEMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LEMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'RLEMS'",
                                   htmlOutput("title_RLEMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_RLEMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_RLEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_RLEMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_RLEMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_RLEMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_RLEMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_RLEMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_RLEMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_RLEMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_RLEMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLEMS_EMSCI == 0 && input.checkGroup_RLEMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_RLEMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LLEMS'",
                                   htmlOutput("title_LLEMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LLEMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LLEMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LLEMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LLEMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LLEMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LLEMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LLEMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LLEMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LLEMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LLEMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLEMS_EMSCI == 0 && input.checkGroup_LLEMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LLEMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'RMS'",
                                   htmlOutput("title_RMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_RMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_RMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_RMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_RMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_RMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_RMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_RMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_RMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_RMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_RMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_EMSCI == 0 && input.checkGroup_RMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_RMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LMS'",
                                   htmlOutput("title_LMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_EMSCI == 0 && input.checkGroup_LMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'TMS'",
                                   htmlOutput("title_TMS_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_TMS_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_TMS_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_TMS_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_TMS_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('6','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_TMS_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_TMS_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_TMS_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_TMS_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_TMS_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_TMS_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Germany", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_EMSCI == 0 && input.checkGroup_TMS_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_TMS_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'RPP'",
                                   htmlOutput("title_RPP_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_RPP_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_RPP_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_RPP_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_RPP_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_RPP_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_RPP_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_RPP_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_RPP_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_RPP_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_RPP_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_EMSCI == 0 && input.checkGroup_RPP_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_RPP_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LPP'",
                                   htmlOutput("title_LPP_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LPP_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LPP_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LPP_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LPP_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LPP_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LPP_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LPP_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LPP_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LPP_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LPP_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_EMSCI == 0 && input.checkGroup_LPP_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LPP_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'TPP'",
                                   htmlOutput("title_TPP_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_TPP_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_TPP_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_TPP_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_TPP_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_TPP_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_TPP_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_TPP_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_TPP_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_TPP_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_TPP_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_EMSCI == 0 && input.checkGroup_TPP_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_TPP_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'RLT'",
                                   htmlOutput("title_RLT_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_RLT_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_RLT_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_RLT_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_RLT_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_RLT_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_RLT_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_RLT_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_RLT_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_RLT_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_RLT_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_EMSCI == 0 && input.checkGroup_RLT_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_RLT_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'LLT'",
                                   htmlOutput("title_LLT_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_LLT_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_LLT_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_LLT_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_LLT_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_LLT_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_LLT_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_LLT_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_LLT_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_LLT_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_LLT_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_EMSCI == 0 && input.checkGroup_LLT_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_LLT_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'neuro' && input.EMSCI_neuro == 'TLT'",
                                   htmlOutput("title_TLT_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_TLT_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_TLT_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_TLT_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_TLT_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_TLT_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_TLT_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_TLT_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_TLT_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_TLT_EMSCI",
                                                                                    label = "Filter based on neurological level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_TLT_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_EMSCI == 0 && input.checkGroup_TLT_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_TLT_EMSCI",
                                                                                 label = "Filter based on neurological year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'WISCI'",
                                   htmlOutput("title_WISCI_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_WISCI_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_WISCI_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_WISCI_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_WISCI_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_WISCI_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_WISCI_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_WISCI_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_WISCI_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_WISCI_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_WISCI_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_WISCI_EMSCI == 0 && input.checkGroup_WISCI_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_WISCI_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'test_6min'",
                                   htmlOutput("title_test_6min_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_test_6min_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_test_6min_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_test_6min_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_test_6min_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_test_6min_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_test_6min_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_test_6min_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_test_6min_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_test_6min_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_test_6min_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_6min_EMSCI == 0 && input.checkGroup_test_6min_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_test_6min_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'test_10m'",
                                   htmlOutput("title_test_10m_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_test_10m_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_test_10m_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_test_10m_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_test_10m_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_test_10m_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_test_10m_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_test_10m_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_test_10m_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_test_10m_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_test_10m_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_test_10m_EMSCI == 0 && input.checkGroup_test_10m_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_test_10m_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'TUG'",
                                   htmlOutput("title_TUG_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_TUG_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_TUG_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_TUG_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_TUG_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_TUG_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_TUG_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_TUG_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_TUG_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_TUG_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_TUG_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TUG_EMSCI == 0 && input.checkGroup_TUG_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_TUG_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'SCIM2'",
                                   htmlOutput("title_SCIM2_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_SCIM2_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_SCIM2_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_SCIM2_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_SCIM2_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_SCIM2_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_SCIM2_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_SCIM2_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_SCIM2_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_SCIM2_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_SCIM2_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM2_EMSCI == 0 && input.checkGroup_SCIM2_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_SCIM2_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'funct' && input.EMSCI_funct == 'SCIM3'",
                                   htmlOutput("title_SCIM3_EMSCI"),
                                   fluidRow(
                                     column(width = 8,
                                            box(width = NULL, status = "primary",
                                                align="center",
                                                #textOutput('test_text'))
                                                plotOutput('plot_SCIM3_EMSCI', height = 660)) #end box
                                     ), # end column
                                     
                                     column(width = 4,
                                            box(status = "primary", width = NULL,
                                                radioButtons("checkbox_SCIM3_EMSCI",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL,
                                                sliderTextInput('time_SCIM3_EMSCI',
                                                                label = "Time window:",
                                                                choices = list("very acute", "acute I", "acute II", 'acute III', 'chronic'),
                                                                selected = c("very acute", 'chronic'),
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "checkGroup_SCIM3_EMSCI",
                                                                                    label = "Filters:",
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5', "Country" = '6', "Year of injury" = '7'),
                                                                                    selected = c('1','7'))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('1')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "sex_SCIM3_EMSCI",
                                                                                    label = "Filter based on sex:",
                                                                                    choices = list("Male", "Female"),
                                                                                    selected = c("Male", "Female"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('2')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "age_SCIM3_EMSCI",
                                                                                    label = "Filter based on age at injury:",
                                                                                    choices = list("0-19 (0)" = 0, "20-39 (1)" = 1, "40-59 (2)" = 2, "60-79 (3)" = 3, "80-99 (4)" = 4),
                                                                                    selected = c(0,1,2,3,4))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('3')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "cause_SCIM3_EMSCI",
                                                                                    label = "Filter based on cause of injury:",
                                                                                    choices = list("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                                                                                    selected = c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('4')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "grade_SCIM3_EMSCI",
                                                                                    label = "Filter based on AIS grade:",
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"),
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('5')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "level_SCIM3_EMSCI",
                                                                                    label = "Filter based on functlogical level of injury:",
                                                                                    choices = list("cervical", "thoracic", "lumbar", "sacral"),
                                                                                    selected = c("cervical", "thoracic", "lumbar", "sacral"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('6')",
                                                             box(status = "primary", width = NULL,
                                                                 checkboxGroupInput(inputId = "country_SCIM3_EMSCI",
                                                                                    label = "Filter based on country:",
                                                                                    choices = list("Austria", "Czech Republic", "France", "Germany", 
                                                                                                   "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                   "Spain", "Switzerland"),
                                                                                    selected = c("Austria", "Czech Republic", "France", "Germany", 
                                                                                                 "Great Britain", "India", "Italy", "Netherlands", 
                                                                                                 "Spain", "Switzerland"))
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SCIM3_EMSCI == 0 && input.checkGroup_SCIM3_EMSCI.includes('7')",
                                                             box(status = "primary", width = NULL,
                                                                 sliderTextInput(inputId = "year_SCIM3_EMSCI",
                                                                                 label = "Filter based on functlogical year of injury:",
                                                                                 choices = list("2000" = 2000, "2005" = 2005, "2010" = 2010, "2015" = 2015, "2020" = 2020),
                                                                                 selected = c(2000, 2020),
                                                                                 animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                                 to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                                 to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                                 post = NULL, dragRange = TRUE)
                                                             ) # end box
                                            ) # end conditionalPanel
                                            
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'UEMS'", # based on user inputs, display different plots
                                   htmlOutput("title_UEMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_UEMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_UEMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_UEMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_UEMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0 && input.checkGroup_UEMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_UEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0 && input.checkGroup_UEMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_UEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0 && input.checkGroup_UEMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_UEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0 && input.checkGroup_UEMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_UEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_UEMS_Sygen == 0 && input.checkGroup_UEMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_UEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'LEMS'", # based on user inputs, display different plots
                                   htmlOutput("title_LEMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_LEMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LEMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_LEMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LEMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0 && input.checkGroup_LEMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0 && input.checkGroup_LEMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0 && input.checkGroup_LEMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_LEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0 && input.checkGroup_LEMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LEMS_Sygen == 0 && input.checkGroup_LEMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'TEMS'", # based on user inputs, display different plots
                                   htmlOutput("title_TEMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_TEMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TEMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_TEMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TEMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0 && input.checkGroup_TEMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0 && input.checkGroup_TEMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0 && input.checkGroup_TEMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_TEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0 && input.checkGroup_TEMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TEMS_Sygen == 0 && input.checkGroup_TEMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TEMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'RMS'", # based on user inputs, display different plots
                                   htmlOutput("title_RMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_RMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_RMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_RMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_RMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0 && input.checkGroup_RMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_RMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0 && input.checkGroup_RMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_RMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0 && input.checkGroup_RMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_RMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0 && input.checkGroup_RMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_RMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_Sygen == 0 && input.checkGroup_RMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_RMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'LMS'", # based on user inputs, display different plots
                                   htmlOutput("title_LMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_LMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_LMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0 && input.checkGroup_LMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0 && input.checkGroup_LMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0 && input.checkGroup_LMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_LMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0 && input.checkGroup_LMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_Sygen == 0 && input.checkGroup_LMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'TMS'", # based on user inputs, display different plots
                                   htmlOutput("title_TMS_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_TMS_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TMS_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_TMS_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TMS_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0 && input.checkGroup_TMS_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0 && input.checkGroup_TMS_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0 && input.checkGroup_TMS_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_TMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0 && input.checkGroup_TMS_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_Sygen == 0 && input.checkGroup_TMS_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TMS_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  

                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'RPP'", # based on user inputs, display different plots
                                   htmlOutput("title_RPP_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_RPP_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_RPP_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_RPP_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_RPP_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0 && input.checkGroup_RPP_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_RPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0 && input.checkGroup_RPP_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_RPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0 && input.checkGroup_RPP_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_RPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0 && input.checkGroup_RPP_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_RPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RPP_Sygen == 0 && input.checkGroup_RPP_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_RPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'LPP'", # based on user inputs, display different plots
                                   htmlOutput("title_LPP_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_LPP_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LPP_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_LPP_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LPP_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0 && input.checkGroup_LPP_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0 && input.checkGroup_LPP_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0 && input.checkGroup_LPP_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_LPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0 && input.checkGroup_LPP_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LPP_Sygen == 0 && input.checkGroup_LPP_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'TPP'", # based on user inputs, display different plots
                                   htmlOutput("title_TPP_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_TPP_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TPP_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_TPP_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TPP_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0 && input.checkGroup_TPP_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0 && input.checkGroup_TPP_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0 && input.checkGroup_TPP_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_TPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0 && input.checkGroup_TPP_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TPP_Sygen == 0 && input.checkGroup_TPP_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TPP_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'RLT'", # based on user inputs, display different plots
                                   htmlOutput("title_RLT_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_RLT_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_RLT_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_RLT_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_RLT_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0 && input.checkGroup_RLT_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_RLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0 && input.checkGroup_RLT_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_RLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0 && input.checkGroup_RLT_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_RLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0 && input.checkGroup_RLT_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_RLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RLT_Sygen == 0 && input.checkGroup_RLT_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_RLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'LLT'", # based on user inputs, display different plots
                                   htmlOutput("title_LLT_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_LLT_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LLT_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_LLT_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LLT_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0 && input.checkGroup_LLT_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0 && input.checkGroup_LLT_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0 && input.checkGroup_LLT_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_LLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0 && input.checkGroup_LLT_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LLT_Sygen == 0 && input.checkGroup_LLT_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'neuro' && input.Sygen_neuro == 'TLT'", # based on user inputs, display different plots
                                   htmlOutput("title_TLT_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_TLT_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TLT_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_TLT_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TLT_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0 && input.checkGroup_TLT_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0 && input.checkGroup_TLT_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0 && input.checkGroup_TLT_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_TLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0 && input.checkGroup_TLT_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TLT_Sygen == 0 && input.checkGroup_TLT_Sygen.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TLT_Sygen", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'funct' && input.Sygen_funct == 'Benzel'", # based on user inputs, display different plots
                                   htmlOutput("title_Benzel_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Benzel_Sygen', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Benzel_Sygen",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_Benzel_Sygen', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52"), # different choices of stages (match the levels in dataset)
                                                                selected = c("Week00", "Week52"), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_Benzel_Sygen", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0 && input.checkGroup_Benzel_Sygen.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_Benzel_Sygen", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in Sygen dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0 && input.checkGroup_Benzel_Sygen.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_Benzel_Sygen", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79"), # choices are age group divided in 20 years (match the levels in Sygen dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0 && input.checkGroup_Benzel_Sygen.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_Benzel_Sygen", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("automobile", "blunt trauma", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0 && input.checkGroup_Benzel_Sygen.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Benzel_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Benzel_Sygen == 0 && input.checkGroup_Benzel_Sygen.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_Benzel_Sygen", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "thoracic"), # choices (match the levels in Sygen dataset)
                                                                                    selected = c("cervical", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'neuro' && input.SCI_rehab_neuro == 'RMS'", # based on user inputs, display different plots
                                   htmlOutput("title_RMS_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting neuroion
                                                plotOutput('plot_RMS_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_RMS_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_RMS_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_RMS_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0 && input.checkGroup_RMS_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_RMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0 && input.checkGroup_RMS_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_RMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0 && input.checkGroup_RMS_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_RMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0 && input.checkGroup_RMS_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_RMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_SCI_rehab == 0 && input.checkGroup_RMS_SCI_rehab.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_RMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'neuro' && input.SCI_rehab_neuro == 'LMS'", # based on user inputs, display different plots
                                   htmlOutput("title_LMS_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting neuroion
                                                plotOutput('plot_LMS_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LMS_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_LMS_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LMS_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0 && input.checkGroup_LMS_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0 && input.checkGroup_LMS_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0 && input.checkGroup_LMS_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_LMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0 && input.checkGroup_LMS_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_SCI_rehab == 0 && input.checkGroup_LMS_SCI_rehab.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'neuro' && input.SCI_rehab_neuro == 'TMS'", # based on user inputs, display different plots
                                   htmlOutput("title_TMS_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting neuroion
                                                plotOutput('plot_TMS_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TMS_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_TMS_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TMS_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0 && input.checkGroup_TMS_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0 && input.checkGroup_TMS_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0 && input.checkGroup_TMS_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_TMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0 && input.checkGroup_TMS_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_SCI_rehab == 0 && input.checkGroup_TMS_SCI_rehab.includes('5')", # if user chooses to filter based on neurological level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TMS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on neurological level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'funct' && input.SCI_rehab_funct == 'PHYIND'", # based on user inputs, display different plots
                                   htmlOutput("title_PHYIND_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_PHYIND_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_PHYIND_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_PHYIND_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_PHYIND_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0 && input.checkGroup_PHYIND_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_PHYIND_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0 && input.checkGroup_PHYIND_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_PHYIND_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0 && input.checkGroup_PHYIND_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_PHYIND_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0 && input.checkGroup_PHYIND_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_PHYIND_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_PHYIND_SCI_rehab == 0 && input.checkGroup_PHYIND_SCI_rehab.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_PHYIND_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'funct' && input.SCI_rehab_funct == 'MOBILITY'", # based on user inputs, display different plots
                                   htmlOutput("title_MOBILITY_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_MOBILITY_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_MOBILITY_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_MOBILITY_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_MOBILITY_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0 && input.checkGroup_MOBILITY_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_MOBILITY_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0 && input.checkGroup_MOBILITY_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_MOBILITY_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0 && input.checkGroup_MOBILITY_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_MOBILITY_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0 && input.checkGroup_MOBILITY_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_MOBILITY_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_MOBILITY_SCI_rehab == 0 && input.checkGroup_MOBILITY_SCI_rehab.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_MOBILITY_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'funct' && input.SCI_rehab_funct == 'OCCUPATION'", # based on user inputs, display different plots
                                   htmlOutput("title_OCCUPATION_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_OCCUPATION_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_OCCUPATION_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_OCCUPATION_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_OCCUPATION_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0 && input.checkGroup_OCCUPATION_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_OCCUPATION_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0 && input.checkGroup_OCCUPATION_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_OCCUPATION_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0 && input.checkGroup_OCCUPATION_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_OCCUPATION_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0 && input.checkGroup_OCCUPATION_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_OCCUPATION_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_OCCUPATION_SCI_rehab == 0 && input.checkGroup_OCCUPATION_SCI_rehab.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_OCCUPATION_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'funct' && input.SCI_rehab_funct == 'SOCIAL'", # based on user inputs, display different plots
                                   htmlOutput("title_SOCIAL_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_SOCIAL_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_SOCIAL_SCI_rehab",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box
                                                sliderTextInput('time_SOCIAL_SCI_rehab', # create slider text to select specific staged to display on the x-axis
                                                                label = "Time window:", # label for box
                                                                choices = list("admission", "discharge"), # different choices of stages (match the levels in dataset)
                                                                selected = c("admission", 'discharge'), # by default, select from very acute to chronic stage (=all stages)
                                                                animate = F, grid = TRUE,
                                                                hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_SOCIAL_SCI_rehab", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "Cause of SCI" = '3', "AIS grade" = '4', "NLI" = '5'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0 && input.checkGroup_SOCIAL_SCI_rehab.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_SOCIAL_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in SCI_rehab dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0 && input.checkGroup_SOCIAL_SCI_rehab.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_SOCIAL_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"), # choices are age group divided in 20 years (match the levels in SCI_rehab dataset from categorised column)
                                                                                    selected = c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0 && input.checkGroup_SOCIAL_SCI_rehab.includes('3')", # if user chooses to filter based on cause of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "cause_SOCIAL_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on cause of injury:", # label of the box
                                                                                    choices = list("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("automobile", "fall", "gun shot wound", "motorcycle", "other sports", "others", "pedestrian", 'person-to-person contact', "water related")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0 && input.checkGroup_SOCIAL_SCI_rehab.includes('4')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_SOCIAL_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI_rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_SOCIAL_SCI_rehab == 0 && input.checkGroup_SOCIAL_SCI_rehab.includes('5')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_SOCIAL_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", "thoracic"), # choices (match the levels in SCI_rehab dataset)
                                                                                    selected = c("cervical", "lumbar", "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'neuro' && input.All_neuro == 'RMS'", # based on user inputs, display different plots
                                   htmlOutput("title_RMS_All"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_RMS_All', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_RMS_All",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_All == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_RMS_All", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "AIS grade" = '3', "NLI" = '4'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_All == 0 && input.checkGroup_RMS_All.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_RMS_All", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in All dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_All == 0 && input.checkGroup_RMS_All.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_RMS_All", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79", "80+"), # choices are age group divided in 20 years (match the levels in All dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_All == 0 && input.checkGroup_RMS_All.includes('3')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_RMS_All", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in All dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_RMS_All == 0 && input.checkGroup_RMS_All.includes('4')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_RMS_All", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", 'sacral', "thoracic"), # choices (match the levels in All dataset)
                                                                                    selected = c("cervical", "lumbar", 'sacral', "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'neuro' && input.All_neuro == 'LMS'", # based on user inputs, display different plots
                                   htmlOutput("title_LMS_All"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_LMS_All', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_LMS_All",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_All == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_LMS_All", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "AIS grade" = '3', "NLI" = '4'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_All == 0 && input.checkGroup_LMS_All.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_LMS_All", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in All dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_All == 0 && input.checkGroup_LMS_All.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_LMS_All", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79", "80+"), # choices are age group divided in 20 years (match the levels in All dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_All == 0 && input.checkGroup_LMS_All.includes('3')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_LMS_All", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in All dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_LMS_All == 0 && input.checkGroup_LMS_All.includes('4')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_LMS_All", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", 'sacral', "thoracic"), # choices (match the levels in All dataset)
                                                                                    selected = c("cervical", "lumbar", 'sacral', "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'All' && input.choose_cat_scores == 'neuro' && input.All_neuro == 'TMS'", # based on user inputs, display different plots
                                   htmlOutput("title_TMS_All"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_TMS_All', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_TMS_All",
                                                             label = "Apply filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, all patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_All == 0", # if user decides to not display all data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 checkboxGroupInput(inputId = "checkGroup_TMS_All", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("Sex" = '1', "Age at injury" = '2', "AIS grade" = '3', "NLI" = '4'), # choices are the different filters that can be applied
                                                                                    selected = c('1','4')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_All == 0 && input.checkGroup_TMS_All.includes('1')", # if user chooses to filter based on sex
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "sex_TMS_All", # create new check box group 
                                                                                    label = "Filter based on sex:", # label of the box
                                                                                    choices = list("Female", "Male"), # choices are male and female (match the levels in All dataset)
                                                                                    selected = c("Female", "Male")) # by default, both male and female are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_All == 0 && input.checkGroup_TMS_All.includes('2')", # if user chooses to filter based on age
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "age_TMS_All", # create new check box group 
                                                                                    label = "Filter based on age at injury:", # label of the box
                                                                                    choices = list("0-19", "20-39", "40-59", "60-79", "80+"), # choices are age group divided in 20 years (match the levels in All dataset from categorised column)
                                                                                    selected = c("0-19", "20-39", "40-59", "60-79", "80+")) # by default, all categories are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_All == 0 && input.checkGroup_TMS_All.includes('3')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_TMS_All", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in All dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, all grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_TMS_All == 0 && input.checkGroup_TMS_All.includes('4')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "level_TMS_All", # create new check box group 
                                                                                    label = "Filter based on functlogical level of injury:", # label of the box
                                                                                    choices = list("cervical", "lumbar", 'sacral', "thoracic"), # choices (match the levels in All dataset)
                                                                                    selected = c("cervical", "lumbar", 'sacral', "thoracic")) # by default, all categories are selected
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'epi' && input.EMSCI_epi == 'Age'", # based on user inputs, display different plots
                                   htmlOutput("title_Age_EMSCI"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Age_EMSCI', height = 660)) # cEMSCI server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                          
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Age_EMSCI", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                                               "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                                               "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                                               "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                                               "2020" = 2020), 
                                                                selected = c(2000, 2020),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                                ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_Age_EMSCI", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 5,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Age_EMSCI",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, EMSCI patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_EMSCI == 0", # if user decides to not display EMSCI data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Age_EMSCI", # create new check box group
                                                                                    label = "Filters:", # label of the box
                                                                                    choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                                    selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_EMSCI == 0 && input.checkGroup_Age_EMSCI.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Age_EMSCI", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D"), # choices (match the levels in EMSCI dataset), missing AIS grades will automaticEMSCIy be removed
                                                                                    selected = c("A", "B", "C", "D")) # by default, EMSCI grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_EMSCI == 0 && input.checkGroup_Age_EMSCI.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Age_EMSCI", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'epi' && input.EMSCI_epi == 'Sex'", # based on user inputs, display different plots
                                   htmlOutput("title_Sex_EMSCI"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Sex_EMSCI', height = 660)) # cEMSCI server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Sex_EMSCI", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                                               "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                                               "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                                               "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                                               "2020" = 2020), 
                                                                selected = c(2000, 2020),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_Sex_EMSCI", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 5,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Sex_EMSCI",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, EMSCI patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_EMSCI == 0", # if user decides to not display EMSCI data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Sex_EMSCI", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_EMSCI == 0 && input.checkGroup_Sex_EMSCI.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Sex_EMSCI", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D"), # choices (match the levels in EMSCI dataset), missing AIS grades will automaticEMSCIy be removed
                                                                                    selected = c("A", "B", "C", "D")) # by default, EMSCI grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_EMSCI == 0 && input.checkGroup_Sex_EMSCI.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Sex_EMSCI", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'epi' && input.EMSCI_epi == 'AIS grade'", # based on user inputs, display different plots
                                   htmlOutput("title_AIS_EMSCI"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_AIS_EMSCI', height = 660)) # cEMSCI server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_AIS_EMSCI", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                                               "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                                               "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                                               "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                                               "2020" = 2020), 
                                                                selected = c(2000, 2020),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_AIS_EMSCI", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 5,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_AIS_EMSCI",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, EMSCI patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_EMSCI == 0", # if user decides to not display EMSCI data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_AIS_EMSCI", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('2')) # by default, AIS and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_EMSCI == 0 && input.checkGroup_AIS_EMSCI.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_AIS_EMSCI", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'EMSCI' && input.choose_cat_scores == 'epi' && input.EMSCI_epi == 'NLI'", # based on user inputs, display different plots
                                   htmlOutput("title_NLI_EMSCI"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_NLI_EMSCI', height = 660)) # cEMSCI server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_NLI_EMSCI", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2000" = 2000,"2001" = 2001,"2002" = 2002,"2003" = 2003,"2004" = 2004,
                                                                               "2005" = 2005,"2006" = 2006,"2007" = 2007,"2008" = 2008,"2009" = 2009,
                                                                               "2010" = 2010,"2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014,
                                                                               "2015" = 2015,"2016" = 2016,"2017" = 2017,"2018" = 2018,"2019" = 2019,
                                                                               "2020" = 2020), 
                                                                selected = c(2000, 2020),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_NLI_EMSCI", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 5,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_NLI_EMSCI",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, EMSCI patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_EMSCI == 0", # if user decides to not display EMSCI data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_NLI_EMSCI", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, NLI and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_EMSCI == 0 && input.checkGroup_NLI_EMSCI.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_NLI_EMSCI", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D"), # choices (match the levels in EMSCI dataset), missing AIS grades will automaticEMSCIy be removed
                                                                                    selected = c("A", "B", "C", "D")) # by default, EMSCI grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_EMSCI == 0 && input.checkGroup_NLI_EMSCI.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_NLI_EMSCI", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'epi' && input.Sygen_epi == 'Age'", # based on user inputs, display different plots
                                   htmlOutput("title_Age_Sygen"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Age_Sygen', height = 660)) # cSygen server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Age_Sygen", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("1991" = 1991,"1992" = 1992,"1993" = 1993,
                                                                               "1994" = 1994, "1995" = 1995, "1996" = 1996,
                                                                               "1997" = 1997), 
                                                                selected = c(1991, 1997),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_Age_Sygen", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 3,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Age_Sygen",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, Sygen patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_Sygen == 0", # if user decides to not display Sygen data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Age_Sygen", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_Sygen == 0 && input.checkGroup_Age_Sygen.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Age_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automaticSygeny be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, Sygen grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_Sygen == 0 && input.checkGroup_Age_Sygen.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Age_Sygen", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'epi' && input.Sygen_epi == 'Sex'", # based on user inputs, display different plots
                                   htmlOutput("title_Sex_Sygen"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Sex_Sygen', height = 660)) # cSygen server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Sex_Sygen", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("1991" = 1991,"1992" = 1992,"1993" = 1993,
                                                                               "1994" = 1994, "1995" = 1995, "1996" = 1996,
                                                                               "1997" = 1997), 
                                                                selected = c(1991, 1997),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_Sex_Sygen", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 3,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Sex_Sygen",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, Sygen patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_Sygen == 0", # if user decides to not display Sygen data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Sex_Sygen", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_Sygen == 0 && input.checkGroup_Sex_Sygen.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Sex_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automaticSygeny be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, Sygen grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_Sygen == 0 && input.checkGroup_Sex_Sygen.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Sex_Sygen", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'epi' && input.Sygen_epi == 'AIS grade'", # based on user inputs, display different plots
                                   htmlOutput("title_AIS_Sygen"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_AIS_Sygen', height = 660)) # cSygen server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_AIS_Sygen", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("1991" = 1991,"1992" = 1992,"1993" = 1993,
                                                                               "1994" = 1994, "1995" = 1995, "1996" = 1996,
                                                                               "1997" = 1997), 
                                                                selected = c(1991, 1997),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_AIS_Sygen", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 3,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_AIS_Sygen",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, Sygen patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_Sygen == 0", # if user decides to not display Sygen data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_AIS_Sygen", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('2')) # by default, AIS and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_Sygen == 0 && input.checkGroup_AIS_Sygen.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_AIS_Sygen", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'Sygen' && input.choose_cat_scores == 'epi' && input.Sygen_epi == 'NLI'", # based on user inputs, display different plots
                                   htmlOutput("title_NLI_Sygen"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_NLI_Sygen', height = 660)) # cSygen server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_NLI_Sygen", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("1991" = 1991,"1992" = 1992,"1993" = 1993,
                                                                               "1994" = 1994, "1995" = 1995, "1996" = 1996,
                                                                               "1997" = 1997), 
                                                                selected = c(1991, 1997),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderInput(inputId = "binyear_NLI_Sygen", # create new slider text
                                                            label = "Number of bins:", # label of the box
                                                            min = 1, max = 3,
                                                            value = 1)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_NLI_Sygen",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, Sygen patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_Sygen == 0", # if user decides to not display Sygen data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_NLI_Sygen", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, NLI and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_Sygen == 0 && input.checkGroup_NLI_Sygen.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_NLI_Sygen", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in Sygen dataset), missing AIS grades will automaticSygeny be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, Sygen grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_Sygen == 0 && input.checkGroup_NLI_Sygen.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_NLI_Sygen", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'epi' && input.SCI_rehab_epi == 'Age'", # based on user inputs, display different plots
                                   htmlOutput("title_Age_SCI_rehab"), # make title based on user inputs (score and dataset)
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Age_SCI_rehab', height = 660)) # server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Age_SCI_rehab", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2007" = 2007,"2008" = 2008,"2009" = 2009), 
                                                                selected = c(2007, 2009),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            # box(status = "primary", width = NULL, # create a new box
                                            #     sliderInput(inputId = "binyear_Age_SCI_rehab", # create new slider text
                                            #                 label = "Number of bins:", # label of the box
                                            #                 min = 1, max = 1,
                                            #                 value = 1)
                                            # ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Age_SCI_rehab",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, SCI rehab patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_SCI_rehab == 0", # if user decides to not display SCI rehab data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Age_SCI_rehab", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_SCI_rehab == 0 && input.checkGroup_Age_SCI_rehab.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Age_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, SCI rehab grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Age_SCI_rehab == 0 && input.checkGroup_Age_SCI_rehab.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Age_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'epi' && input.SCI_rehab_epi == 'Sex'", # based on user inputs, display different plots
                                   htmlOutput("title_Sex_SCI_rehab"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_Sex_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_Sex_SCI_rehab", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2007" = 2007,"2008" = 2008,"2009" = 2009), 
                                                                selected = c(2007, 2009),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_Sex_SCI_rehab",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, SCI rehab patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_SCI_rehab == 0", # if user decides to not display SCI rehab data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_Sex_SCI_rehab", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, sex and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_SCI_rehab == 0 && input.checkGroup_Sex_SCI_rehab.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_Sex_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, SCI rehab grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_Sex_SCI_rehab == 0 && input.checkGroup_Sex_SCI_rehab.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_Sex_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'epi' && input.SCI_rehab_epi == 'AIS grade'", # based on user inputs, display different plots
                                   htmlOutput("title_AIS_SCI_rehab"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_AIS_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_AIS_SCI_rehab", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2007" = 2007,"2008" = 2008,"2009" = 2009), 
                                                                selected = c(2007, 2009),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_AIS_SCI_rehab",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, SCI rehab patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_SCI_rehab == 0", # if user decides to not display SCI rehab data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_AIS_SCI_rehab", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('2')) # by default, AIS and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            
                                            conditionalPanel(condition = "input.checkbox_AIS_SCI_rehab == 0 && input.checkGroup_AIS_SCI_rehab.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_AIS_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  ),
                  
                  conditionalPanel(condition = "input.choose_data == 'SCI rehab' && input.choose_cat_scores == 'epi' && input.SCI_rehab_epi == 'NLI'", # based on user inputs, display different plots
                                   htmlOutput("title_NLI_SCI_rehab"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_NLI_SCI_rehab', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                sliderTextInput(inputId = "year_NLI_SCI_rehab", # create new slider text
                                                                label = "Filter based on year of injury:", # label of the box
                                                                choices = list("2007" = 2007,"2008" = 2008,"2009" = 2009), 
                                                                selected = c(2007, 2009),
                                                                animate = T, grid = T, hide_min_max = FALSE, from_fixed = FALSE,
                                                                to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                                                                to_max = NULL, force_edges = T, width = NULL, pre = NULL,
                                                                post = NULL, dragRange = TRUE)
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                radioButtons("checkbox_NLI_SCI_rehab",
                                                             label = "Apply other filters ?",
                                                             choices = c("No" = 1, 'Yes' = 0), 
                                                             selected = 1) # checkbox to go to basic plot: no filters, SCI rehab patients, x-axis=stages, y-axis=value of score
                                            ), # end box
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_SCI_rehab == 0", # if user decides to not display SCI rehab data and apply filters, make a new panel appear
                                                             box(status = "primary", width = NULL, # create new box
                                                                 radioButtons(inputId = "checkGroup_NLI_SCI_rehab", # create new check box group
                                                                              label = "Filters:", # label of the box
                                                                              choices = list("AIS grade" = '1', "Paralysis type" = '2'), # choices are the different filters that can be applied
                                                                              selected = c('1')) # by default, NLI and AIS grades are selected
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_SCI_rehab == 0 && input.checkGroup_NLI_SCI_rehab.includes('1')", # if user chooses to filter based on AIS grade
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "grade_NLI_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on AIS grade:", # label of the box
                                                                                    choices = list("AIS A", "AIS B", "AIS C", "AIS D"), # choices (match the levels in SCI rehab dataset), missing AIS grades will automatically be removed
                                                                                    selected = c("AIS A", "AIS B", "AIS C", "AIS D")) # by default, SCI rehab grades are selected but missing grades
                                                             ) # end box
                                            ), # end conditionalPanel
                                            
                                            conditionalPanel(condition = "input.checkbox_NLI_SCI_rehab == 0 && input.checkGroup_NLI_SCI_rehab.includes('2')", # if user chooses to filter based on functlogical level of injury
                                                             box(status = "primary", width = NULL, # create a new box
                                                                 checkboxGroupInput(inputId = "paralysis_NLI_SCI_rehab", # create new check box group 
                                                                                    label = "Filter based on type of paralysis:", # label of the box
                                                                                    choices = list("paraplegia", 'tetraplegia'), 
                                                                                    selected = c("paraplegia", 'tetraplegia'))
                                                             ) # end box
                                            ) # end conditionalPanel
                                     ) #end column
                                   ) #end fluidRow
                  )),
          tabItem(tabName = "PredictTab",
                  conditionalPanel("input.sidebarmenu == 'predictG'", # based on user inputs, display different plots
                                   htmlOutput("title_predict"),
                                   fluidRow( # create a separation in the panel
                                     column(width = 8, # create first column for boxplot
                                            box(width = NULL, status = "primary", # create box to display plot
                                                align="center", # center the plot
                                                #textOutput('test_text')) # used for testing the user inputs formats to adapt the plotting function
                                                plotOutput('plot_predict', height = 660)) # call server plot function for the score and dataset chosen by the user #end box 
                                     ), # end column
                                     
                                     column(width = 4, # create second column for second type of user inputs (filters)
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                selectInput("select_score", 
                                                            label = "Select outcome of interest", 
                                                            choices = list("UEMS", 'LEMS', 'TMS'), 
                                                            selected = c("UEMS"))
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                textInput("input_compscore",
                                                          label = "Patient score at very acute stage", 
                                                          value = "Enter value...")
                                                
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                selectInput("select_sex", 
                                                            label = "Select sex", 
                                                            choices = list("Male" = "m", "Female" = "f", "Unknown" = "Unknown"), 
                                                            selected = c("Unknown"))
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create box 
                                                selectInput("select_age", 
                                                            label = "Select age at injury", 
                                                            choices = list("0-19", "20-39", "40-59", "60-79", "80+", 'Unknown'), 
                                                            selected = c("Unknown"))
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                selectInput("select_ais", 
                                                            label = "Select baseline AIS", 
                                                            choices = list("AIS A" = "A", "AIS B" = "B", "AIS C" = "C", "AIS D" = "D", "AIS E" = "E", "Unknown" = "Unknown"), 
                                                            selected = c("Unknown"))
                                            ), # end box
                                            
                                            box(status = "primary", width = NULL, # create a new box
                                                selectInput("select_nli", 
                                                            label = "Select injury level", 
                                                            choices = list("Unknown", "Cervical", "Thoracic", "Lumbar", "Sacral",
                                                                           "C1","C2","C3","C4","C5","C6","C7","C8",
                                                                           "T1","T2","T3","T4","T5","T6","T7","T8","T9","T1","T11", "T12",
                                                                           "L1", "L2", "L3", "L4", "L5",
                                                                           "S1", "S2", "S3", "S4", "S5"), 
                                                            selected = c("Unknown"))
                                            )#, # end box
                                            
                                            
                                     ) #end column
                                   ) #end fluidRow
                  )
                  
                  )
        )
      )
)

ui <- secure_app(ui)


# Server logic ----
server <- function(input, output) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  output$title_UEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")}) # output title based on user inputs
  
  # output$test_text <- renderText({
  #   filters <- as.numeric(as.vector(input$checkGroup_UEMS_EMSCI))
  #   inputs <- list(unique(input$sex_UEMS_EMSCI),
  #                  unique(input$age_UEMS_EMSCI),
  #                  unique(input$cause_UEMS_EMSCI),
  #                  unique(input$grade_UEMS_EMSCI),
  #                  unique(input$level_UEMS_EMSCI),
  #                  unique(input$country_UEMS_EMSCI),
  #                  unique(input$year_UEMS_EMSCI))
  #   list_all = list(c("Male", "Female"),
  #                   c(0,1,2,3,4),
  #                   c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
  #                   c("AIS A", "AIS B", "AIS C", "AIS D"),
  #                   c("cervical", "thoracic", "lumbar", "sacral"),
  #                   c("Austria", "Czech Republic", "France", "Germany",
  #                     "Great Britain", "India", "Italy", "Netherlands",
  #                     "Spain", "Switzerland"),
  #                   c(2000, 2005, 2010, 2015, 2020))
  # 
  #   filter1_all <- list_all[filters[1]][[1]]
  #   filter2_all <- list_all[filters[2]][[1]]
  # 
  #   list_names = c("Sex", "age_category", "Cause", "AIS", "NLI", "Country", "YEARDOI_year")
  # 
  #   #filters[2] == '7'
  # 
  #   if (filters[2] == '7'){
  #     temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
  #     int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
  #     cat2 <- c()
  #     if (2000 %in% int_filter && 2005 %in% int_filter){
  #       cat2 <- c(cat2, '2000-2004')
  #     }
  #     if (2005 %in% int_filter && 2010 %in% int_filter){
  #       cat2 <- c(cat2, '2005-2009')
  #     }
  #     if (2010 %in% int_filter && 2015 %in% int_filter){
  #       cat2 <- c(cat2, '2010-2014')
  #     }
  #     if (2015 %in% int_filter && 2020 %in% int_filter){
  #       cat2 <- c(cat2, '2015-2020')
  #     }
  #   }
  #   print(int_filter)
  #   print(cat2)
    # print(filters[2])
    # print('int_filter:', int_filter)
    # print('cat 2:', cat2)
    # 
    # cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
    # temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
    # cat2 = as.vector(filter2_all[c(temp[1]:temp[2])])
    # print(filters)
    # print(filter1_all)
    # print(filter2_all)
    # print(list_names[filters[1]])
    # print(list_names[filters[2]])
    # print(inputs[filters[1]])
    # print(inputs[filters[2]])
    # print(cat1)
    # print(inputs[filters[2]])
    # print(cat2)
 # })
  
  output$plot_UEMS_EMSCI <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic") # store all potential stages
    times <- unique(input$time_UEMS_EMSCI) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_UEMS_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_UEMS_EMSCI == 0){ # if user chooses filters
      if (length(input$checkGroup_UEMS_EMSCI) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_UEMS_EMSCI) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_UEMS_EMSCI))) # store filters the user has selected
        inputs <- list(unique(input$sex_UEMS_EMSCI), # stores all inputs for the different filters
                    unique(input$age_UEMS_EMSCI),
                    unique(input$cause_UEMS_EMSCI),
                    unique(input$grade_UEMS_EMSCI),
                    unique(input$level_UEMS_EMSCI),
                    unique(input$country_UEMS_EMSCI),
                    unique(input$year_UEMS_EMSCI))
        list_all = list(c("Male", "Female"), # store all the different options for the different filters
                     c(0,1,2,3,4),
                     c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                     c("AIS A", "AIS B", "AIS C", "AIS D"),
                     c("cervical", "thoracic", "lumbar", "sacral"),
                     c("Austria", "Czech Republic", "France", "Germany", 
                       "Great Britain", "India", "Italy", "Netherlands", 
                       "Spain", "Switzerland"),
                     c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat") # names of columns corresponding to the available filters
        
        
        if (filters[2] == '7'){ # if user chooses to filter based on year of injury 
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)) # find which indices of years match the user input
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])]) # select all years in between the 2 stages selected by user in slider
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){ # transform years of input into categories, i.e. if user chooses 2000 and 2005, plot will contain data with category '2000-2004' so in between the 2 dates selected by user
            cat2 <- c(cat2, '2000-2004') 
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])

        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, # call function for emsci plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_UEMS_EMSCI) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
  plot}) # end of the output plot function
  
  output$title_RUEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RUEMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_RUEMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_RUEMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_RUEMS_EMSCI == 0){
      if (length(input$checkGroup_RUEMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_RUEMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_RUEMS_EMSCI)))
        inputs <- list(unique(input$sex_RUEMS_EMSCI),
                       unique(input$age_RUEMS_EMSCI),
                       unique(input$cause_RUEMS_EMSCI),
                       unique(input$grade_RUEMS_EMSCI),
                       unique(input$level_RUEMS_EMSCI),
                       unique(input$country_RUEMS_EMSCI),
                       unique(input$year_RUEMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RUEMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LUEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LUEMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LUEMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LUEMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LUEMS_EMSCI == 0){
      if (length(input$checkGroup_LUEMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LUEMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LUEMS_EMSCI)))
        inputs <- list(unique(input$sex_LUEMS_EMSCI),
                       unique(input$age_LUEMS_EMSCI),
                       unique(input$cause_LUEMS_EMSCI),
                       unique(input$grade_LUEMS_EMSCI),
                       unique(input$level_LUEMS_EMSCI),
                       unique(input$country_LUEMS_EMSCI),
                       unique(input$year_LUEMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LUEMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LEMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LEMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LEMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LEMS_EMSCI == 0){
      if (length(input$checkGroup_LEMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LEMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LEMS_EMSCI)))
        inputs <- list(unique(input$sex_LEMS_EMSCI),
                       unique(input$age_LEMS_EMSCI),
                       unique(input$cause_LEMS_EMSCI),
                       unique(input$grade_LEMS_EMSCI),
                       unique(input$level_LEMS_EMSCI),
                       unique(input$country_LEMS_EMSCI),
                       unique(input$year_LEMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LEMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_RLEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RLEMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_RLEMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_RLEMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_RLEMS_EMSCI == 0){
      if (length(input$checkGroup_RLEMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_RLEMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_RLEMS_EMSCI)))
        inputs <- list(unique(input$sex_RLEMS_EMSCI),
                       unique(input$age_RLEMS_EMSCI),
                       unique(input$cause_RLEMS_EMSCI),
                       unique(input$grade_RLEMS_EMSCI),
                       unique(input$level_RLEMS_EMSCI),
                       unique(input$country_RLEMS_EMSCI),
                       unique(input$year_RLEMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RLEMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LLEMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LLEMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LLEMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LLEMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LLEMS_EMSCI == 0){
      if (length(input$checkGroup_LLEMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LLEMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LLEMS_EMSCI)))
        inputs <- list(unique(input$sex_LLEMS_EMSCI),
                       unique(input$age_LLEMS_EMSCI),
                       unique(input$cause_LLEMS_EMSCI),
                       unique(input$grade_LLEMS_EMSCI),
                       unique(input$level_LLEMS_EMSCI),
                       unique(input$country_LLEMS_EMSCI),
                       unique(input$year_LLEMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LLEMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_RMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_RMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_RMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_RMS_EMSCI == 0){
      if (length(input$checkGroup_RMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_RMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_RMS_EMSCI)))
        inputs <- list(unique(input$sex_RMS_EMSCI),
                       unique(input$age_RMS_EMSCI),
                       unique(input$cause_RMS_EMSCI),
                       unique(input$grade_RMS_EMSCI),
                       unique(input$level_RMS_EMSCI),
                       unique(input$country_RMS_EMSCI),
                       unique(input$year_RMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LMS_EMSCI == 0){
      if (length(input$checkGroup_LMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LMS_EMSCI)))
        inputs <- list(unique(input$sex_LMS_EMSCI),
                       unique(input$age_LMS_EMSCI),
                       unique(input$cause_LMS_EMSCI),
                       unique(input$grade_LMS_EMSCI),
                       unique(input$level_LMS_EMSCI),
                       unique(input$country_LMS_EMSCI),
                       unique(input$year_LMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_TMS_EMSCI <- renderText({paste("<h2><b>", input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TMS_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_TMS_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_TMS_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_TMS_EMSCI == 0){
      if (length(input$checkGroup_TMS_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_TMS_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_TMS_EMSCI)))
        inputs <- list(unique(input$sex_TMS_EMSCI),
                       unique(input$age_TMS_EMSCI),
                       unique(input$cause_TMS_EMSCI),
                       unique(input$grade_TMS_EMSCI),
                       unique(input$level_TMS_EMSCI),
                       unique(input$country_TMS_EMSCI),
                       unique(input$year_TMS_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TMS_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_RPP_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RPP_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_RPP_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_RPP_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_RPP_EMSCI == 0){
      if (length(input$checkGroup_RPP_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_RPP_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_RPP_EMSCI)))
        inputs <- list(unique(input$sex_RPP_EMSCI),
                       unique(input$age_RPP_EMSCI),
                       unique(input$cause_RPP_EMSCI),
                       unique(input$grade_RPP_EMSCI),
                       unique(input$level_RPP_EMSCI),
                       unique(input$country_RPP_EMSCI),
                       unique(input$year_RPP_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RPP_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LPP_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LPP_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LPP_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LPP_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LPP_EMSCI == 0){
      if (length(input$checkGroup_LPP_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LPP_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LPP_EMSCI)))
        inputs <- list(unique(input$sex_LPP_EMSCI),
                       unique(input$age_LPP_EMSCI),
                       unique(input$cause_LPP_EMSCI),
                       unique(input$grade_LPP_EMSCI),
                       unique(input$level_LPP_EMSCI),
                       unique(input$country_LPP_EMSCI),
                       unique(input$year_LPP_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LPP_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_TPP_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TPP_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_TPP_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_TPP_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_TPP_EMSCI == 0){
      if (length(input$checkGroup_TPP_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_TPP_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_TPP_EMSCI)))
        inputs <- list(unique(input$sex_TPP_EMSCI),
                       unique(input$age_TPP_EMSCI),
                       unique(input$cause_TPP_EMSCI),
                       unique(input$grade_TPP_EMSCI),
                       unique(input$level_TPP_EMSCI),
                       unique(input$country_TPP_EMSCI),
                       unique(input$year_TPP_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TPP_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_RLT_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RLT_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_RLT_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_RLT_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_RLT_EMSCI == 0){
      if (length(input$checkGroup_RLT_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_RLT_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_RLT_EMSCI)))
        inputs <- list(unique(input$sex_RLT_EMSCI),
                       unique(input$age_RLT_EMSCI),
                       unique(input$cause_RLT_EMSCI),
                       unique(input$grade_RLT_EMSCI),
                       unique(input$level_RLT_EMSCI),
                       unique(input$country_RLT_EMSCI),
                       unique(input$year_RLT_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RLT_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_LLT_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LLT_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_LLT_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_LLT_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_LLT_EMSCI == 0){
      if (length(input$checkGroup_LLT_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_LLT_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_LLT_EMSCI)))
        inputs <- list(unique(input$sex_LLT_EMSCI),
                       unique(input$age_LLT_EMSCI),
                       unique(input$cause_LLT_EMSCI),
                       unique(input$grade_LLT_EMSCI),
                       unique(input$level_LLT_EMSCI),
                       unique(input$country_LLT_EMSCI),
                       unique(input$year_LLT_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LLT_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_TLT_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})

  output$plot_TLT_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_TLT_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_TLT_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_neuro, list_time)
    }
    else if (input$checkbox_TLT_EMSCI == 0){
      if (length(input$checkGroup_TLT_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_TLT_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_TLT_EMSCI)))
        inputs <- list(unique(input$sex_TLT_EMSCI),
                       unique(input$age_TLT_EMSCI),
                       unique(input$cause_TLT_EMSCI),
                       unique(input$grade_TLT_EMSCI),
                       unique(input$level_TLT_EMSCI),
                       unique(input$country_TLT_EMSCI),
                       unique(input$year_TLT_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_neuro, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TLT_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  # Titles functional scores in EMSCI dataset
  output$title_WISCI_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_WISCI_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_WISCI_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_WISCI_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_WISCI_EMSCI == 0){
      if (length(input$checkGroup_WISCI_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_WISCI_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_WISCI_EMSCI)))
        inputs <- list(unique(input$sex_WISCI_EMSCI),
                       unique(input$age_WISCI_EMSCI),
                       unique(input$cause_WISCI_EMSCI),
                       unique(input$grade_WISCI_EMSCI),
                       unique(input$level_WISCI_EMSCI),
                       unique(input$country_WISCI_EMSCI),
                       unique(input$year_WISCI_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_WISCI_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_test_6min_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_test_6min_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_test_6min_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_test_6min_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_test_6min_EMSCI == 0){
      if (length(input$checkGroup_test_6min_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_test_6min_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_test_6min_EMSCI)))
        inputs <- list(unique(input$sex_test_6min_EMSCI),
                       unique(input$age_test_6min_EMSCI),
                       unique(input$cause_test_6min_EMSCI),
                       unique(input$grade_test_6min_EMSCI),
                       unique(input$level_test_6min_EMSCI),
                       unique(input$country_test_6min_EMSCI),
                       unique(input$year_test_6min_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_test_6min_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_test_10m_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_test_10m_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_test_10m_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_test_10m_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_test_10m_EMSCI == 0){
      if (length(input$checkGroup_test_10m_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_test_10m_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_test_10m_EMSCI)))
        inputs <- list(unique(input$sex_test_10m_EMSCI),
                       unique(input$age_test_10m_EMSCI),
                       unique(input$cause_test_10m_EMSCI),
                       unique(input$grade_test_10m_EMSCI),
                       unique(input$level_test_10m_EMSCI),
                       unique(input$country_test_10m_EMSCI),
                       unique(input$year_test_10m_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_test_10m_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_TUG_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TUG_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_TUG_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_TUG_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_TUG_EMSCI == 0){
      if (length(input$checkGroup_TUG_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_TUG_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_TUG_EMSCI)))
        inputs <- list(unique(input$sex_TUG_EMSCI),
                       unique(input$age_TUG_EMSCI),
                       unique(input$cause_TUG_EMSCI),
                       unique(input$grade_TUG_EMSCI),
                       unique(input$level_TUG_EMSCI),
                       unique(input$country_TUG_EMSCI),
                       unique(input$year_TUG_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TUG_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_SCIM2_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_SCIM2_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_SCIM2_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_SCIM2_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_SCIM2_EMSCI == 0){
      if (length(input$checkGroup_SCIM2_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_SCIM2_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_SCIM2_EMSCI)))
        inputs <- list(unique(input$sex_SCIM2_EMSCI),
                       unique(input$age_SCIM2_EMSCI),
                       unique(input$cause_SCIM2_EMSCI),
                       unique(input$grade_SCIM2_EMSCI),
                       unique(input$level_SCIM2_EMSCI),
                       unique(input$country_SCIM2_EMSCI),
                       unique(input$year_SCIM2_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_SCIM2_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  output$title_SCIM3_EMSCI <- renderText({paste("<h2><b>",input$EMSCI_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_SCIM3_EMSCI <- renderPlot({
    list_all_stages <- c("very acute", "acute I", "acute II", "acute III", "chronic")
    times <- unique(input$time_SCIM3_EMSCI)
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE))
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])]
    
    if (input$checkbox_SCIM3_EMSCI == 1){
      plot <- plot_base_emsci(data_emsci, score = input$EMSCI_funct, list_time)
    }
    else if (input$checkbox_SCIM3_EMSCI == 0){
      if (length(input$checkGroup_SCIM3_EMSCI) > 2){
        plot <- plot_error()
      }
      else if (length(input$checkGroup_SCIM3_EMSCI) == 2){
        filters <- as.numeric(as.vector(unique(input$checkGroup_SCIM3_EMSCI)))
        inputs <- list(unique(input$sex_SCIM3_EMSCI),
                       unique(input$age_SCIM3_EMSCI),
                       unique(input$cause_SCIM3_EMSCI),
                       unique(input$grade_SCIM3_EMSCI),
                       unique(input$level_SCIM3_EMSCI),
                       unique(input$country_SCIM3_EMSCI),
                       unique(input$year_SCIM3_EMSCI))
        list_all = list(c("Male", "Female"),
                        c(0,1,2,3,4),
                        c("disc herniation", "haemorragic", "ischemic", "traumatic", "other"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic", "lumbar", "sacral"),
                        c("Austria", "Czech Republic", "France", "Germany", 
                          "Great Britain", "India", "Italy", "Netherlands", 
                          "Spain", "Switzerland"),
                        c(2000, 2005, 2010, 2015, 2020))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]])
        filter2_all <- as.vector(list_all[filters[2]][[1]])
        
        list_names = c("Sex", "age_category", "Cause", "AIS", "NLI_level", "Country", "YEARDOI_cat")
        
        
        if (filters[2] == '7'){
          temp <- which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE))
          int_filter = as.vector(filter2_all[c(temp[1]:temp[2])])
          cat2 <- c()
          if (2000 %in% int_filter && 2005 %in% int_filter){
            cat2 <- c(cat2, '2000-2004')
          }
          if (2005 %in% int_filter && 2010 %in% int_filter){
            cat2 <- c(cat2, '2005-2009')
          }
          if (2010 %in% int_filter && 2015 %in% int_filter){
            cat2 <- c(cat2, '2010-2014')
          }
          if (2015 %in% int_filter && 2020 %in% int_filter){
            cat2 <- c(cat2, '2015-2019')
          }
        } else {
          cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        }
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_emsci(data_emsci, score = input$EMSCI_funct, 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_SCIM3_EMSCI) == 1){
        plot <- plot_error()
      }
    }
    plot})
  
  # Titles neurological scores in Sygen dataset
  output$title_UEMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_UEMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_UEMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_UEMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_UEMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_UEMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_UEMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_UEMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_UEMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_UEMS_Sygen),
                       unique(input$cause_UEMS_Sygen),
                       unique(input$grade_UEMS_Sygen),
                       unique(input$level_UEMS_Sygen),
                       unique(input$country_UEMS_Sygen),
                       unique(input$year_UEMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_UEMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  
  output$title_LEMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LEMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_LEMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_LEMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LEMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_LEMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LEMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LEMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_LEMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_LEMS_Sygen),
                       unique(input$cause_LEMS_Sygen),
                       unique(input$grade_LEMS_Sygen),
                       unique(input$level_LEMS_Sygen),
                       unique(input$country_LEMS_Sygen),
                       unique(input$year_LEMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LEMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_TEMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TEMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_TEMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_TEMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TEMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_TEMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TEMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TEMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_TEMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_TEMS_Sygen),
                       unique(input$cause_TEMS_Sygen),
                       unique(input$grade_TEMS_Sygen),
                       unique(input$level_TEMS_Sygen),
                       unique(input$country_TEMS_Sygen),
                       unique(input$year_TEMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TEMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_RMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_RMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_RMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_RMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_RMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_RMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_RMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_RMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_RMS_Sygen),
                       unique(input$cause_RMS_Sygen),
                       unique(input$grade_RMS_Sygen),
                       unique(input$level_RMS_Sygen),
                       unique(input$country_RMS_Sygen),
                       unique(input$year_RMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_LMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_LMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_LMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_LMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_LMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_LMS_Sygen),
                       unique(input$cause_LMS_Sygen),
                       unique(input$grade_LMS_Sygen),
                       unique(input$level_LMS_Sygen),
                       unique(input$country_LMS_Sygen),
                       unique(input$year_LMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_TMS_Sygen <- renderText({paste("<h2><b>", input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TMS_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_TMS_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_TMS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TMS_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_TMS_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TMS_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TMS_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_TMS_Sygen), # stores all inputs for the different filters
                       unique(input$age_TMS_Sygen),
                       unique(input$cause_TMS_Sygen),
                       unique(input$grade_TMS_Sygen),
                       unique(input$level_TMS_Sygen),
                       unique(input$country_TMS_Sygen),
                       unique(input$year_TMS_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TMS_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_RPP_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RPP_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_RPP_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_RPP_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_RPP_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_RPP_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_RPP_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_RPP_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_RPP_Sygen), # stores all inputs for the different filters
                       unique(input$age_RPP_Sygen),
                       unique(input$cause_RPP_Sygen),
                       unique(input$grade_RPP_Sygen),
                       unique(input$level_RPP_Sygen),
                       unique(input$country_RPP_Sygen),
                       unique(input$year_RPP_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RPP_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_LPP_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LPP_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_LPP_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_LPP_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LPP_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_LPP_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LPP_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LPP_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_LPP_Sygen), # stores all inputs for the different filters
                       unique(input$age_LPP_Sygen),
                       unique(input$cause_LPP_Sygen),
                       unique(input$grade_LPP_Sygen),
                       unique(input$level_LPP_Sygen),
                       unique(input$country_LPP_Sygen),
                       unique(input$year_LPP_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LPP_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_TPP_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TPP_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_TPP_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_TPP_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TPP_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_TPP_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TPP_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TPP_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_TPP_Sygen), # stores all inputs for the different filters
                       unique(input$age_TPP_Sygen),
                       unique(input$cause_TPP_Sygen),
                       unique(input$grade_TPP_Sygen),
                       unique(input$level_TPP_Sygen),
                       unique(input$country_TPP_Sygen),
                       unique(input$year_TPP_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TPP_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_RLT_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RLT_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_RLT_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_RLT_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_RLT_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_RLT_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_RLT_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_RLT_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_RLT_Sygen), # stores all inputs for the different filters
                       unique(input$age_RLT_Sygen),
                       unique(input$cause_RLT_Sygen),
                       unique(input$grade_RLT_Sygen),
                       unique(input$level_RLT_Sygen),
                       unique(input$country_RLT_Sygen),
                       unique(input$year_RLT_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RLT_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_LLT_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LLT_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_LLT_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_LLT_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LLT_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_LLT_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LLT_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LLT_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_LLT_Sygen), # stores all inputs for the different filters
                       unique(input$age_LLT_Sygen),
                       unique(input$cause_LLT_Sygen),
                       unique(input$grade_LLT_Sygen),
                       unique(input$level_LLT_Sygen),
                       unique(input$country_LLT_Sygen),
                       unique(input$year_LLT_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LLT_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_TLT_Sygen <- renderText({paste("<h2><b>",input$Sygen_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})

  output$plot_TLT_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_TLT_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_TLT_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TLT_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_TLT_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TLT_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TLT_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_TLT_Sygen), # stores all inputs for the different filters
                       unique(input$age_TLT_Sygen),
                       unique(input$cause_TLT_Sygen),
                       unique(input$grade_TLT_Sygen),
                       unique(input$level_TLT_Sygen),
                       unique(input$country_TLT_Sygen),
                       unique(input$year_TLT_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_neuro, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TLT_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_Benzel_Sygen <- renderText({paste("<h2><b>",input$Sygen_funct, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_Benzel_Sygen <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("Week00", "Week01", "Week04", 'Week08', 'Week16', "Week26", "Week52") # store all potential stages
    times <- unique(input$time_Benzel_Sygen) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_Benzel_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_sygen, score = input$Sygen_funct, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_Benzel_Sygen == 0){ # if user chooses filters
      if (length(input$checkGroup_Benzel_Sygen) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_Benzel_Sygen) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_Benzel_Sygen))) # store filters the user has selected
        inputs <- list(unique(input$sex_Benzel_Sygen), # stores all inputs for the different filters
                       unique(input$age_Benzel_Sygen),
                       unique(input$cause_Benzel_Sygen),
                       unique(input$grade_Benzel_Sygen),
                       unique(input$level_Benzel_Sygen),
                       unique(input$country_Benzel_Sygen),
                       unique(input$year_Benzel_Sygen))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("0-19","20-39","40-59","60-79"),
                        c("automobile","blunt trauma","fall","gun shot wound","motorcycle","other sports","others","pedestrian","water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI", "Country") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_sygen, score = input$Sygen_funct, # call function for Sygen plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_Benzel_Sygen) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  # Titles neurological scores in SCIRehab dataset
  
  output$title_RMS_SCI_rehab <- renderText({paste("<h2><b>",input$SCI_rehab_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_RMS_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_RMS_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_RMS_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_RMS_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_RMS_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_RMS_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_RMS_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_RMS_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_RMS_SCI_rehab),
                       unique(input$cause_RMS_SCI_rehab),
                       unique(input$grade_RMS_SCI_rehab),
                       unique(input$level_RMS_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_RMS_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_LMS_SCI_rehab <- renderText({paste("<h2><b>",input$SCI_rehab_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_LMS_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_LMS_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_LMS_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LMS_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_LMS_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LMS_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LMS_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_LMS_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_LMS_SCI_rehab),
                       unique(input$cause_LMS_SCI_rehab),
                       unique(input$grade_LMS_SCI_rehab),
                       unique(input$level_LMS_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_LMS_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  output$title_TMS_SCI_rehab <- renderText({paste("<h2><b>",input$SCI_rehab_neuro, " distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_TMS_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_TMS_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_TMS_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TMS_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_TMS_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TMS_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TMS_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_TMS_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_TMS_SCI_rehab),
                       unique(input$cause_TMS_SCI_rehab),
                       unique(input$grade_TMS_SCI_rehab),
                       unique(input$level_TMS_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_neuro, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_TMS_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function

  # Titles functional scores in SCIRehab dataset
  output$title_PHYIND_SCI_rehab <- renderText({paste("<h2><b>", "Physical independence score distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_PHYIND_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_PHYIND_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_PHYIND_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_PHYIND_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_PHYIND_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_PHYIND_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_PHYIND_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_PHYIND_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_PHYIND_SCI_rehab),
                       unique(input$cause_PHYIND_SCI_rehab),
                       unique(input$grade_PHYIND_SCI_rehab),
                       unique(input$level_PHYIND_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_PHYIND_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  output$title_MOBILITY_SCI_rehab <- renderText({paste("<h2><b>", "Mobility score distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_MOBILITY_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_MOBILITY_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_MOBILITY_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_MOBILITY_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_MOBILITY_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_MOBILITY_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_MOBILITY_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_MOBILITY_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_MOBILITY_SCI_rehab),
                       unique(input$cause_MOBILITY_SCI_rehab),
                       unique(input$grade_MOBILITY_SCI_rehab),
                       unique(input$level_MOBILITY_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_MOBILITY_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_OCCUPATION_SCI_rehab <- renderText({paste("<h2><b>", "Occupation score distribution from the ", input$choose_data, " dataset", "</b>")})
  
  output$plot_OCCUPATION_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_OCCUPATION_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_OCCUPATION_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_OCCUPATION_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_OCCUPATION_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_OCCUPATION_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_OCCUPATION_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_OCCUPATION_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_OCCUPATION_SCI_rehab),
                       unique(input$cause_OCCUPATION_SCI_rehab),
                       unique(input$grade_OCCUPATION_SCI_rehab),
                       unique(input$level_OCCUPATION_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_OCCUPATION_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_SOCIAL_SCI_rehab <- renderText({paste("<h2><b>", "Social integration score distribution from the ", input$choose_data, " dataset", "</b>")})
  
  
  output$plot_SOCIAL_SCI_rehab <- renderPlot({ # create output function for plot of interest
    list_all_stages <- c("admission", "discharge") # store all potential stages
    times <- unique(input$time_SOCIAL_SCI_rehab) # store stages selected by user
    indices_time = which(list_all_stages %in% unlist(times, use.names=FALSE)) # find which indices of all stages match the user input
    list_time = list_all_stages[c(indices_time[1]:indices_time[2])] # select all stages in between the 2 stages selected by user in slider
    
    if (input$checkbox_SOCIAL_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, list_time) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_SOCIAL_SCI_rehab == 0){ # if user chooses filters
      if (length(input$checkGroup_SOCIAL_SCI_rehab) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_SOCIAL_SCI_rehab) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_SOCIAL_SCI_rehab))) # store filters the user has selected
        inputs <- list(unique(input$sex_SOCIAL_SCI_rehab), # stores all inputs for the different filters
                       unique(input$age_SOCIAL_SCI_rehab),
                       unique(input$cause_SOCIAL_SCI_rehab),
                       unique(input$grade_SOCIAL_SCI_rehab),
                       unique(input$level_SOCIAL_SCI_rehab))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
                        c("automobile","fall","gun shot wound","motorcycle","other sports","others","pedestrian","person-to-person contact", "water related"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "Cause", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_Sygen(data_SCI_rehab, score = input$SCI_rehab_funct, # call function for SCI_rehab plots in helper_functions.R 
                                   list_time, 
                                   list_names[filters[1]], 
                                   list_names[filters[2]],
                                   cat1, 
                                   cat2)
      }
      else if (length(input$checkGroup_SOCIAL_SCI_rehab) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  output$title_RMS_All <- renderText({paste("<h2><b>", "Comparison of RMS distribution across datasets", "</b>")})
  
  
  output$plot_RMS_All <- renderPlot({ # create output function for plot of interest

    if (input$checkbox_RMS_All == 1){ # if user chooses to display all data
      #plot <- plot_error()
      plot <- plot_base_All(data_All, score = input$All_neuro) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_RMS_All == 0){ # if user chooses filters
      if (length(input$checkGroup_RMS_All) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_RMS_All) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_RMS_All))) # store filters the user has selected
        inputs <- list(unique(input$sex_RMS_All), # stores all inputs for the different filters
                       unique(input$age_RMS_All),
                       unique(input$grade_RMS_All),
                       unique(input$level_RMS_All))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-39", "40-59", "60-79", "80+"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "sacral", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_All(data_All, score = input$All_neuro, # call function for All plots in helper_functions.R
                                 list_names[filters[1]],
                                 list_names[filters[2]],
                                 cat1,
                                 cat2)
      }
      else if (length(input$checkGroup_RMS_All) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  output$title_LMS_All <- renderText({paste("<h2><b>", "Comparison of LMS distribution across datasets", "</b>")})
  
  output$plot_LMS_All <- renderPlot({ # create output function for plot of interest
    
    if (input$checkbox_LMS_All == 1){ # if user chooses to display all data
      #plot <- plot_error()
      plot <- plot_base_All(data_All, score = input$All_neuro) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_LMS_All == 0){ # if user chooses filters
      if (length(input$checkGroup_LMS_All) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_LMS_All) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_LMS_All))) # store filters the user has selected
        inputs <- list(unique(input$sex_LMS_All), # stores all inputs for the different filters
                       unique(input$age_LMS_All),
                       unique(input$grade_LMS_All),
                       unique(input$level_LMS_All))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-39", "40-59", "60-79", "80+"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "sacral", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_All(data_All, score = input$All_neuro, # call function for All plots in helper_functions.R
                                 list_names[filters[1]],
                                 list_names[filters[2]],
                                 cat1,
                                 cat2)
      }
      else if (length(input$checkGroup_LMS_All) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  
  output$title_TMS_All <- renderText({paste("<h2><b>", "Comparison of TMS distribution across datasets", "</b>")})
  
  output$plot_TMS_All <- renderPlot({ # create output function for plot of interest
    
    if (input$checkbox_TMS_All == 1){ # if user chooses to display all data
      #plot <- plot_error()
      plot <- plot_base_All(data_All, score = input$All_neuro) # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_TMS_All == 0){ # if user chooses filters
      if (length(input$checkGroup_TMS_All) > 2){ # if user chooses more than 2 filters
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
      else if (length(input$checkGroup_TMS_All) == 2){ # if user chooses exactly 2 filters
        filters <- as.numeric(as.vector(unique(input$checkGroup_TMS_All))) # store filters the user has selected
        inputs <- list(unique(input$sex_TMS_All), # stores all inputs for the different filters
                       unique(input$age_TMS_All),
                       unique(input$grade_TMS_All),
                       unique(input$level_TMS_All))
        list_all = list(c("Female", "Male"), # store all the different options for the different filters
                        c("12-19", "20-39", "40-59", "60-79", "80+"),
                        c("AIS A", "AIS B", "AIS C", "AIS D"),
                        c("cervical", "lumbar", "sacral", "thoracic"))
        
        filter1_all <- as.vector(list_all[filters[1]][[1]]) # select all options for the first filter chosen by user
        filter2_all <- as.vector(list_all[filters[2]][[1]]) # select all options for the second filter chosen by user
        
        list_names = c("Sex", "Age", "AIS", "NLI") # names of columns corresponding to the available filters
        
        cat2 = as.vector(filter2_all[c(which(filter2_all %in% unlist(inputs[filters[2]], use.names=FALSE)))])
        cat1 = as.vector(filter1_all[c(which(filter1_all %in% unlist(inputs[filters[1]], use.names=FALSE)))])
        
        plot <- plot_filters_All(data_All, score = input$All_neuro, # call function for All plots in helper_functions.R
                                 list_names[filters[1]],
                                 list_names[filters[2]],
                                 cat1,
                                 cat2)
      }
      else if (length(input$checkGroup_TMS_All) == 1){ 
        plot <- plot_error() # give a plot saying to choose 2 filters
      }
    }
    plot}) # end of the output plot function
  
  output$title_Age_EMSCI <- renderText({paste("<h2><b>", "Age at injury for patients enrolled in EMSCI dataset", "</b>")})
  
  output$plot_Age_EMSCI <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Age_EMSCI)[1]:unique(input$year_Age_EMSCI)[2])
    nb_bins <- unique(input$binyear_Age_EMSCI)[1]

    data_modified <- data_age_emsci[data_age_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
                                   seq(input$year_Age_EMSCI[1],
                                       input$year_Age_EMSCI[2]+input$year_Age_EMSCI[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    if (input$checkbox_Age_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_Age_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_Age_EMSCI == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Age_EMSCI))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Age_EMSCI))))
        if (length(unique(input$grade_Age_EMSCI)) == 1){
          plot <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[1]), '')
        } else if (length(unique(input$grade_Age_EMSCI)) == 2){
          plot.1 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[1]), paste('AIS',input$grade_Age_EMSCI[1]))
          plot.2 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[2]), paste('AIS',input$grade_Age_EMSCI[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Age_EMSCI)) == 3){
          plot.1 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[1]), paste('AIS',input$grade_Age_EMSCI[1]))
          plot.2 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[2]), paste('AIS',input$grade_Age_EMSCI[2]))
          plot.3 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[3]), paste('AIS',input$grade_Age_EMSCI[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Age_EMSCI)) == 4){
          #print('test')
          plot.1 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[1]), paste('AIS',input$grade_Age_EMSCI[1]))
          plot.2 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[2]), paste('AIS',input$grade_Age_EMSCI[2]))
          plot.3 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[3]), paste('AIS',input$grade_Age_EMSCI[3]))
          plot.4 <- plot_base_Age_EMSCI(subset(data_modified, AIS == input$grade_Age_EMSCI[4]), paste('AIS',input$grade_Age_EMSCI[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Age_EMSCI))) == 2){
        if (length(unique(input$paralysis_Age_EMSCI)) == 2){
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Age_EMSCI(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Age_EMSCI(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Age_EMSCI))){
            plot <- plot_base_Age_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Age_EMSCI))){
            plot <- plot_base_Age_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }

    plot}) # end of the output plot function
  
  output$title_Sex_EMSCI <- renderText({paste("<h2><b>", "Sex distribution for patients enrolled in EMSCI dataset", "</b>")})

  output$plot_Sex_EMSCI <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Sex_EMSCI)[1]:unique(input$year_Sex_EMSCI)[2])
    nb_bins <- unique(input$binyear_Sex_EMSCI)[1]
    
    data_modified <- data_age_emsci[data_age_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
                                   seq(input$year_Sex_EMSCI[1],
                                       input$year_Sex_EMSCI[2]+input$year_Sex_EMSCI[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    
    if (input$checkbox_Sex_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_Sex_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stSexs
    }
    else if (input$checkbox_Sex_EMSCI == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Sex_EMSCI))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Sex_EMSCI))))
        if (length(unique(input$grade_Sex_EMSCI)) == 1){
          plot <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[1]), '')
        } else if (length(unique(input$grade_Sex_EMSCI)) == 2){
          plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[1]), paste('AIS',input$grade_Sex_EMSCI[1]))
          plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[2]), paste('AIS',input$grade_Sex_EMSCI[2]))
          
          plot <- plot_grid(
            # col 1
            plot_grid(plot.1) + theme(plot.background = element_rect(color = "black")),
            # col 2
            plot_grid(plot.2) + theme(plot.background = element_rect(color = "black")), 
            ncol = 2)
          
          #plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Sex_EMSCI)) == 3){
          plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[1]), paste('AIS',input$grade_Sex_EMSCI[1]))
          plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[2]), paste('AIS',input$grade_Sex_EMSCI[2]))
          plot.3 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[3]), paste('AIS',input$grade_Sex_EMSCI[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Sex_EMSCI)) == 4){
          #print('test')
          plot.1 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[1]), paste('AIS',input$grade_Sex_EMSCI[1]))
          plot.2 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[2]), paste('AIS',input$grade_Sex_EMSCI[2]))
          plot.3 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[3]), paste('AIS',input$grade_Sex_EMSCI[3]))
          plot.4 <- plot_base_Sex_EMSCI(subset(data_modified, AIS == input$grade_Sex_EMSCI[4]), paste('AIS',input$grade_Sex_EMSCI[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Sex_EMSCI))) == 2){
        if (length(unique(input$paralysis_Sex_EMSCI)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Sex_EMSCI_paralysis(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Sex_EMSCI_paralysis(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Sex_EMSCI))){
            plot <- plot_base_Sex_EMSCI_paralysis(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Sex_EMSCI))){
            plot <- plot_base_Sex_EMSCI_paralysis(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  output$title_AIS_EMSCI <- renderText({paste("<h2><b>", "AIS distribution for patients enrolled in EMSCI dataset", "</b>")})
  
  output$plot_AIS_EMSCI <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_AIS_EMSCI)[1]:unique(input$year_AIS_EMSCI)[2])
    nb_bins <- unique(input$binyear_AIS_EMSCI)[1]
    
    data_modified <- data_age_emsci[data_age_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI,
                                   seq(input$year_AIS_EMSCI[1],
                                       input$year_AIS_EMSCI[2]+input$year_AIS_EMSCI[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)

    if (input$checkbox_AIS_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_AIS_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stAISs
    }
    else if (input$checkbox_AIS_EMSCI == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_AIS_EMSCI))) == 2){
        if (length(unique(input$paralysis_AIS_EMSCI)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_AIS_EMSCI(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_AIS_EMSCI(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_AIS_EMSCI))){
            plot <- plot_base_AIS_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_AIS_EMSCI))){
            plot <- plot_base_AIS_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }

        }
      }
    }

    plot}) # end of the output plot function
  
  output$title_NLI_EMSCI <- renderText({paste("<h2><b>", "NLI distribution for patients enrolled in EMSCI dataset", "</b>")})
  
  output$plot_NLI_EMSCI <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_NLI_EMSCI)[1]:unique(input$year_NLI_EMSCI)[2])
    nb_bins <- unique(input$binyear_NLI_EMSCI)[1]
    
    data_modified <- data_age_emsci[data_age_emsci$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI,
                                   seq(input$year_NLI_EMSCI[1],
                                       input$year_NLI_EMSCI[2]+input$year_NLI_EMSCI[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    
    if (input$checkbox_NLI_EMSCI == 1){ # if user chooses to display all data
      plot <- plot_base_NLI_EMSCI(data_modified, '') # display basic plot with all patients, and user selected stNLIs
    }
    else if (input$checkbox_NLI_EMSCI == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_NLI_EMSCI))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_NLI_EMSCI))))
        if (length(unique(input$grade_NLI_EMSCI)) == 1){
          plot <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[1]), '')
        } else if (length(unique(input$grade_NLI_EMSCI)) == 2){
          plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[1]), paste('AIS',input$grade_NLI_EMSCI[1]))
          plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[2]), paste('AIS',input$grade_NLI_EMSCI[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_NLI_EMSCI)) == 3){
          plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[1]), paste('AIS',input$grade_NLI_EMSCI[1]))
          plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[2]), paste('AIS',input$grade_NLI_EMSCI[2]))
          plot.3 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[3]), paste('AIS',input$grade_NLI_EMSCI[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_NLI_EMSCI)) == 4){
          #print('test')
          plot.1 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[1]), paste('AIS',input$grade_NLI_EMSCI[1]))
          plot.2 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[2]), paste('AIS',input$grade_NLI_EMSCI[2]))
          plot.3 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[3]), paste('AIS',input$grade_NLI_EMSCI[3]))
          plot.4 <- plot_base_NLI_EMSCI(subset(data_modified, AIS == input$grade_NLI_EMSCI[4]), paste('AIS',input$grade_NLI_EMSCI[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_NLI_EMSCI))) == 2){
        if (length(unique(input$paralysis_NLI_EMSCI)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_NLI_EMSCI(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_NLI_EMSCI(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_NLI_EMSCI))){
            plot <- plot_base_NLI_EMSCI(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_NLI_EMSCI))){
            plot <- plot_base_NLI_EMSCI(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot})
  
  output$title_Age_Sygen <- renderText({paste("<h2><b>", "Age at injury for patients enrolled in Sygen dataset", "</b>")})
  
  output$plot_Age_Sygen <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Age_Sygen)[1]:unique(input$year_Age_Sygen)[2])
    nb_bins <- unique(input$binyear_Age_Sygen)[1]
    
    data_modified <- data_sygen_epi[data_sygen_epi$yeardoi %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$yeardoi, 
                                   seq(input$year_Age_Sygen[1],
                                       input$year_Age_Sygen[2]+input$year_Age_Sygen[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    if (input$checkbox_Age_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Age_Sygen(data_modified, '') # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_Age_Sygen == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Age_Sygen))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Age_Sygen))))
        if (length(unique(input$grade_Age_Sygen)) == 1){
          plot <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[1]), '')
        } else if (length(unique(input$grade_Age_Sygen)) == 2){
          plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[1]), paste(input$grade_Age_Sygen[1]))
          plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[2]), paste(input$grade_Age_Sygen[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Age_Sygen)) == 3){
          plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[1]), paste(input$grade_Age_Sygen[1]))
          plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[2]), paste(input$grade_Age_Sygen[2]))
          plot.3 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[3]), paste(input$grade_Age_Sygen[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Age_Sygen)) == 4){
          #print('test')
          plot.1 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[1]), paste(input$grade_Age_Sygen[1]))
          plot.2 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[2]), paste(input$grade_Age_Sygen[2]))
          plot.3 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[3]), paste(input$grade_Age_Sygen[3]))
          plot.4 <- plot_base_Age_Sygen(subset(data_modified, ais1 == input$grade_Age_Sygen[4]), paste(input$grade_Age_Sygen[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Age_Sygen))) == 2){
        if (length(unique(input$paralysis_Age_Sygen)) == 2){
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Age_Sygen(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Age_Sygen(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Age_Sygen))){
            plot <- plot_base_Age_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Age_Sygen))){
            plot <- plot_base_Age_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function

  output$title_Sex_Sygen <- renderText({paste("<h2><b>", "Sex distribution for patients enrolled in Sygen dataset", "</b>")})
  
  output$plot_Sex_Sygen <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Sex_Sygen)[1]:unique(input$year_Sex_Sygen)[2])
    nb_bins <- unique(input$binyear_Sex_Sygen)[1]
    
    data_modified <- data_sygen_epi[data_sygen_epi$yeardoi %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$yeardoi, 
                                   seq(input$year_Sex_Sygen[1],
                                       input$year_Sex_Sygen[2]+input$year_Sex_Sygen[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    if (input$checkbox_Sex_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_Sex_Sygen(data_modified, '') # display basic plot with all patients, and user selected stSexs
    }
    else if (input$checkbox_Sex_Sygen == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Sex_Sygen))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Sex_Sygen))))
        if (length(unique(input$grade_Sex_Sygen)) == 1){
          plot <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[1]), '')
        } else if (length(unique(input$grade_Sex_Sygen)) == 2){
          plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[1]), paste(input$grade_Sex_Sygen[1]))
          plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[2]), paste(input$grade_Sex_Sygen[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Sex_Sygen)) == 3){
          plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[1]), paste(input$grade_Sex_Sygen[1]))
          plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[2]), paste(input$grade_Sex_Sygen[2]))
          plot.3 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[3]), paste(input$grade_Sex_Sygen[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Sex_Sygen)) == 4){
          #print('test')
          plot.1 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[1]), paste(input$grade_Sex_Sygen[1]))
          plot.2 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[2]), paste(input$grade_Sex_Sygen[2]))
          plot.3 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[3]), paste(input$grade_Sex_Sygen[3]))
          plot.4 <- plot_base_Sex_Sygen(subset(data_modified, ais1 == input$grade_Sex_Sygen[4]), paste(input$grade_Sex_Sygen[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Sex_Sygen))) == 2){
        if (length(unique(input$paralysis_Sex_Sygen)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Sex_Sygen_paralysis(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Sex_Sygen_paralysis(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Sex_Sygen))){
            plot <- plot_base_Sex_Sygen_paralysis(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Sex_Sygen))){
            plot <- plot_base_Sex_Sygen_paralysis(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  
  output$title_AIS_Sygen <- renderText({paste("<h2><b>", "AIS distribution for patients enrolled in Sygen dataset", "</b>")})
  
  output$plot_AIS_Sygen <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_AIS_Sygen)[1]:unique(input$year_AIS_Sygen)[2])
    nb_bins <- unique(input$binyear_AIS_Sygen)[1]
    
    data_modified <- data_sygen_epi[data_sygen_epi$yeardoi %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$yeardoi,
                                   seq(input$year_AIS_Sygen[1],
                                       input$year_AIS_Sygen[2]+input$year_AIS_Sygen[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    if (input$checkbox_AIS_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_AIS_Sygen(data_modified, '') # display basic plot with all patients, and user selected stAISs
    }
    else if (input$checkbox_AIS_Sygen == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_AIS_Sygen))) == 2){
        if (length(unique(input$paralysis_AIS_Sygen)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_AIS_Sygen(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_AIS_Sygen(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_AIS_Sygen))){
            plot <- plot_base_AIS_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_AIS_Sygen))){
            plot <- plot_base_AIS_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  output$title_NLI_Sygen <- renderText({paste("<h2><b>", "NLI distribution for patients enrolled in Sygen dataset", "</b>")})
  
  output$plot_NLI_Sygen <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_NLI_Sygen)[1]:unique(input$year_NLI_Sygen)[2])
    nb_bins <- unique(input$binyear_NLI_Sygen)[1]
    
    data_modified <- data_sygen_epi[data_sygen_epi$yeardoi %in% years, ]
    data_modified$YEARDOI_cat<-cut(data_modified$yeardoi,
                                   seq(input$year_NLI_Sygen[1],
                                       input$year_NLI_Sygen[2]+input$year_NLI_Sygen[2]%%nb_bins,
                                       nb_bins),
                                   include.lowest = T, right = F)
    
    
    if (input$checkbox_NLI_Sygen == 1){ # if user chooses to display all data
      plot <- plot_base_NLI_Sygen(data_modified, '') # display basic plot with all patients, and user selected stNLIs
    }
    else if (input$checkbox_NLI_Sygen == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_NLI_Sygen))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_NLI_Sygen))))
        if (length(unique(input$grade_NLI_Sygen)) == 1){
          plot <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[1]), '')
        } else if (length(unique(input$grade_NLI_Sygen)) == 2){
          plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[1]), paste(input$grade_NLI_Sygen[1]))
          plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[2]), paste(input$grade_NLI_Sygen[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_NLI_Sygen)) == 3){
          plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[1]), paste(input$grade_NLI_Sygen[1]))
          plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[2]), paste(input$grade_NLI_Sygen[2]))
          plot.3 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[3]), paste(input$grade_NLI_Sygen[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_NLI_Sygen)) == 4){
          #print('test')
          plot.1 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[1]), paste(input$grade_NLI_Sygen[1]))
          plot.2 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[2]), paste(input$grade_NLI_Sygen[2]))
          plot.3 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[3]), paste(input$grade_NLI_Sygen[3]))
          plot.4 <- plot_base_NLI_Sygen(subset(data_modified, ais1 == input$grade_NLI_Sygen[4]), paste(input$grade_NLI_Sygen[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_NLI_Sygen))) == 2){
        if (length(unique(input$paralysis_NLI_Sygen)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_NLI_Sygen(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_NLI_Sygen(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_NLI_Sygen))){
            plot <- plot_base_NLI_Sygen(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_NLI_Sygen))){
            plot <- plot_base_NLI_Sygen(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot})
  
  output$title_Age_SCI_rehab <- renderText({paste("<h2><b>", "Age at injury for patients enrolled in SCI rehab dataset", "</b>")})
  
  output$plot_Age_SCI_rehab <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Age_SCI_rehab)[1]:unique(input$year_Age_SCI_rehab)[2])
    #nb_bins <- unique(input$binyear_Age_SCI_rehab)[1]
    
    data_modified <- data_SCI_rehab_epi[data_SCI_rehab_epi$YEARDOI %in% years, ]
    # data_modified$YEARDOI_cat<-cut(data_modified$YEARDOI, 
    #                                seq(input$year_Age_SCI_rehab[1],
    #                                    input$year_Age_SCI_rehab[2]+input$year_Age_SCI_rehab[2]%%nb_bins,
    #                                    nb_bins),
    #                                include.lowest = T, right = F)
    data_modified$YEARDOI_cat <- data_modified$YEARDOI
    
    if (input$checkbox_Age_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Age_SCI_rehab(data_modified, '') # display basic plot with all patients, and user selected stages
    }
    else if (input$checkbox_Age_SCI_rehab == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Age_SCI_rehab))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Age_SCI_rehab))))
        if (length(unique(input$grade_Age_SCI_rehab)) == 1){
          plot <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[1]), '')
        } else if (length(unique(input$grade_Age_SCI_rehab)) == 2){
          plot.1 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[1]), paste(input$grade_Age_SCI_rehab[1]))
          plot.2 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[2]), paste(input$grade_Age_SCI_rehab[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Age_SCI_rehab)) == 3){
          plot.1 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[1]), paste(input$grade_Age_SCI_rehab[1]))
          plot.2 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[2]), paste(input$grade_Age_SCI_rehab[2]))
          plot.3 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[3]), paste(input$grade_Age_SCI_rehab[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Age_SCI_rehab)) == 4){
          #print('test')
          plot.1 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[1]), paste(input$grade_Age_SCI_rehab[1]))
          plot.2 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[2]), paste(input$grade_Age_SCI_rehab[2]))
          plot.3 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[3]), paste(input$grade_Age_SCI_rehab[3]))
          plot.4 <- plot_base_Age_SCI_rehab(subset(data_modified, AIS == input$grade_Age_SCI_rehab[4]), paste(input$grade_Age_SCI_rehab[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Age_SCI_rehab))) == 2){
        if (length(unique(input$paralysis_Age_SCI_rehab)) == 2){
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Age_SCI_rehab(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Age_SCI_rehab(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Age_SCI_rehab))){
            plot <- plot_base_Age_SCI_rehab(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Age_SCI_rehab))){
            plot <- plot_base_Age_SCI_rehab(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  output$title_Sex_SCI_rehab <- renderText({paste("<h2><b>", "Sex distribution for patients enrolled in SCI rehab dataset", "</b>")})
  
  output$plot_Sex_SCI_rehab <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_Sex_SCI_rehab)[1]:unique(input$year_Sex_SCI_rehab)[2])

    data_modified <- data_SCI_rehab_epi[data_SCI_rehab_epi$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat <- data_modified$YEARDOI
    
    if (input$checkbox_Sex_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_Sex_SCI_rehab(data_modified, '') # display basic plot with all patients, and user selected stSexs
    }
    else if (input$checkbox_Sex_SCI_rehab == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_Sex_SCI_rehab))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_Sex_SCI_rehab))))
        if (length(unique(input$grade_Sex_SCI_rehab)) == 1){
          plot <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[1]), '')
        } else if (length(unique(input$grade_Sex_SCI_rehab)) == 2){
          plot.1 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[1]), paste(input$grade_Sex_SCI_rehab[1]))
          plot.2 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[2]), paste(input$grade_Sex_SCI_rehab[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_Sex_SCI_rehab)) == 3){
          plot.1 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[1]), paste(input$grade_Sex_SCI_rehab[1]))
          plot.2 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[2]), paste(input$grade_Sex_SCI_rehab[2]))
          plot.3 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[3]), paste(input$grade_Sex_SCI_rehab[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_Sex_SCI_rehab)) == 4){
          #print('test')
          plot.1 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[1]), paste(input$grade_Sex_SCI_rehab[1]))
          plot.2 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[2]), paste(input$grade_Sex_SCI_rehab[2]))
          plot.3 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[3]), paste(input$grade_Sex_SCI_rehab[3]))
          plot.4 <- plot_base_Sex_SCI_rehab(subset(data_modified, AIS == input$grade_Sex_SCI_rehab[4]), paste(input$grade_Sex_SCI_rehab[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_Sex_SCI_rehab))) == 2){
        if (length(unique(input$paralysis_Sex_SCI_rehab)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_Sex_SCI_rehab_paralysis(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_Sex_SCI_rehab_paralysis(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_Sex_SCI_rehab))){
            plot <- plot_base_Sex_SCI_rehab_paralysis(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_Sex_SCI_rehab))){
            plot <- plot_base_Sex_SCI_rehab_paralysis(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  
  output$title_AIS_SCI_rehab <- renderText({paste("<h2><b>", "AIS distribution for patients enrolled in SCI rehab dataset", "</b>")})
  
  output$plot_AIS_SCI_rehab <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_AIS_SCI_rehab)[1]:unique(input$year_AIS_SCI_rehab)[2])

    data_modified <- data_SCI_rehab_epi[data_SCI_rehab_epi$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat <- data_modified$YEARDOI
    
    if (input$checkbox_AIS_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_AIS_SCI_rehab(data_modified, '') # display basic plot with all patients, and user selected stAISs
    }
    else if (input$checkbox_AIS_SCI_rehab == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_AIS_SCI_rehab))) == 2){
        if (length(unique(input$paralysis_AIS_SCI_rehab)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_AIS_SCI_rehab(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_AIS_SCI_rehab(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_AIS_SCI_rehab))){
            plot <- plot_base_AIS_SCI_rehab(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_AIS_SCI_rehab))){
            plot <- plot_base_AIS_SCI_rehab(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot}) # end of the output plot function
  
  output$title_NLI_SCI_rehab <- renderText({paste("<h2><b>", "NLI distribution for patients enrolled in SCI rehab dataset", "</b>")})
  
  output$plot_NLI_SCI_rehab <- renderPlot({ # create output function for plot of interest
    years <- c(unique(input$year_NLI_SCI_rehab)[1]:unique(input$year_NLI_SCI_rehab)[2])

    data_modified <- data_SCI_rehab_epi[data_SCI_rehab_epi$YEARDOI %in% years, ]
    data_modified$YEARDOI_cat <- data_modified$YEARDOI
    
    
    if (input$checkbox_NLI_SCI_rehab == 1){ # if user chooses to display all data
      plot <- plot_base_NLI_SCI_rehab(data_modified, '') # display basic plot with all patients, and user selected stNLIs
    }
    else if (input$checkbox_NLI_SCI_rehab == 0){ # if user chooses filters
      if (as.numeric(as.vector(unique(input$checkGroup_NLI_SCI_rehab))) == 1){
        #print(as.numeric(as.vector(unique(input$grade_NLI_SCI_rehab))))
        if (length(unique(input$grade_NLI_SCI_rehab)) == 1){
          plot <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[1]), '')
        } else if (length(unique(input$grade_NLI_SCI_rehab)) == 2){
          plot.1 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[1]), paste(input$grade_NLI_SCI_rehab[1]))
          plot.2 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[2]), paste(input$grade_NLI_SCI_rehab[2]))
          plot <- grid.arrange(plot.1, plot.2, ncol=2)
        } else if (length(unique(input$grade_NLI_SCI_rehab)) == 3){
          plot.1 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[1]), paste(input$grade_NLI_SCI_rehab[1]))
          plot.2 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[2]), paste(input$grade_NLI_SCI_rehab[2]))
          plot.3 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[3]), paste(input$grade_NLI_SCI_rehab[3]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, ncol=2, nrow=2)
        } else if (length(unique(input$grade_NLI_SCI_rehab)) == 4){
          #print('test')
          plot.1 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[1]), paste(input$grade_NLI_SCI_rehab[1]))
          plot.2 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[2]), paste(input$grade_NLI_SCI_rehab[2]))
          plot.3 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[3]), paste(input$grade_NLI_SCI_rehab[3]))
          plot.4 <- plot_base_NLI_SCI_rehab(subset(data_modified, AIS == input$grade_NLI_SCI_rehab[4]), paste(input$grade_NLI_SCI_rehab[4]))
          plot <- grid.arrange(plot.1, plot.2, plot.3, plot.4, ncol=2, nrow=2)
        }
        
        
      } else if (as.numeric(as.vector(unique(input$checkGroup_NLI_SCI_rehab))) == 2){
        if (length(unique(input$paralysis_NLI_SCI_rehab)) == 2){
          #print('test')
          data_modified.tetra <-subset(data_modified, plegia =='tetra')
          data_modified.para <-subset(data_modified, plegia =='para')
          plot_tetra <- plot_base_NLI_SCI_rehab(data_modified.tetra, 'Tetraplegic')
          plot_para <- plot_base_NLI_SCI_rehab(data_modified.para, 'Paraplegic')
          plot <- grid.arrange(plot_tetra, plot_para, ncol=2)
        } else {
          if ('paraplegia' %in% as.vector(unique(input$paralysis_NLI_SCI_rehab))){
            plot <- plot_base_NLI_SCI_rehab(subset(data_modified, plegia =='para'), 'Paraplegic')
          } else if ('tetraplegia' %in% as.vector(unique(input$paralysis_NLI_SCI_rehab))){
            plot <- plot_base_NLI_SCI_rehab(subset(data_modified, plegia =='tetra'), 'Tetraplegic')
          }
          
        }
      }
    }
    
    plot})
  
  output$title_predict <- renderText({paste("<h2><b>", "Monitoring of individual patient", "</b>")})
  
  output$plot_predict <- renderPlot({ # create output function for plot of interest
    input_sex <- unique(input$select_sex)[1]
    input_age <- unique(input$select_age)[1]
    input_ais <- unique(input$select_ais)[1]
    input_nli <- unique(input$select_nli)[1]
    input_score <- unique(input$select_score)[1]
    input_indscore <- unique(input$input_compscore)[1]
    
    data_temp <- data_emsci_epi
    data_temp$UEMS <- as.numeric(as.character(data_temp$UEMS))
    data_temp$LEMS <- as.numeric(as.character(data_temp$LEMS))
    data_temp$TMS <- as.numeric(as.character(data_temp$TMS))
    
    data_modified <- data_temp
    
    if (!(input_sex == 'Unknown')){
      data_modified <- data_modified[data_modified$Sex %in% input_sex, ]
    }
    
    if (!(input_ais == 'Unknown')){
      data_modified <- data_modified[data_modified$AIS %in% input_ais, ]
    }
    
    if (!(input_nli == 'Unknown')){
      if (input_nli == 'Cervical'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'cervical', ]
      } else if (input_nli == 'Thoracic'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'thoracic', ]
      } else if (input_nli == 'Lumbar'){
        data_modified <- data_modified[data_modified$NLI_level %in% 'lumbar', ]
      } else {
        data_modified <- data_modified[data_modified$NLI %in% input_nli, ]
      }
    }
    
    if (!(input_age == 'Unknown')){
      if (input_age == '0-19'){
        data_modified <- data_modified[data_modified$AgeAtDOI>0 & data_modified$AgeAtDOI<20, ]
      } else if (input_age == '20-39'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=20 & data_modified$AgeAtDOI<40, ]
      } else if (input_age == '40-59'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=40 & data_modified$AgeAtDOI<60, ]
      } else if (input_age == '60-79'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=60 & data_modified$AgeAtDOI<80, ]
      } else if (input_age == '80+'){
        data_modified <- data_modified[data_modified$AgeAtDOI>=80, ]
      }
    }
    
    data_modified <- data_modified[data_modified$UEMS != 'NA', ]
    data_modified <- data_modified[data_modified$UEMS != 'NT', ]
  
    if (dim(data_modified)[1] == 0){
      plot <- plot_error_data()
    } else if (input_indscore == "Enter value..." || input_indscore == "") {
      plot <- plot_predict_emsci(data_modified, input_score)
    } else {
      value <- as.numeric(as.character(input_indscore))
      if (value < 0 || value > 50){
        plot <- plot_error_value()
      } else {
        #plot <- plot_error_value()
        plot <- plot_predict_emsci_NN(data_modified, input_score, value)
      }
    }
    
    
    
    plot})
  
  output$video <- renderUI({
    tags$video(src = "raw_video.mov", type = "video/mov", autoplay = NA, controls = NA, height = 500, width = 1000)
  })
  
  
}




# Run app ----
shinyApp(ui, server)