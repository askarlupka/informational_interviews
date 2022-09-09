#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
## Packages #####

list.of.packages <- c("shiny", "tidyverse", "remotes", "readxl", "writexl", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#This library is coming from my repository since I've been making changes to the original package code
new.package <- "collapsibleTree" %in% installed.packages()[,"Package"]
if(length(new.packages) == 0) remotes::install_github("askarlupka/collapsibleTree",
                        upgrade = "never", ref = "9ab309c3") #you can switch this to never after the initial install. I've had issues with three of my packages (rlang, stringi, htmltools) just not updating

library(collapsibleTree) #tree design

#These libraries should be coming from CRAN
library(shiny) #making applications
library(tidyverse) #manipulating data
library(writexl) #saving data as excel sheet
library(DT)

## Data Manipulation ####

#For now you'll have to save the interview information in an excel file and have it in the same folder as the app file.
#In the future, there should be a drop down option in the app to load a file from a location
informational_interview <- readxl::read_excel("informational_interview.xlsx")

#Rename it to manipulate it
data <- informational_interview

#Create the data that should go into the tooltip
data <- data %>%
  mutate(tooltip = paste0("Title: ",
                          title,
                          "<br>OneNote: <a href=\"",
                          link_onenote,
                          "\">a link</a>" ))

#Create the pathString and hierarchy order for the tree building
data$pathString <- apply(data[,c(2:6,1)], 1, function(x) paste(na.omit(x), collapse = "/"))

#Create the data tree
data_as_tree <- data.tree::ToDataFrameTable(data, "pathString", "tooltip")
dataN <- data.tree::FromDataFrameTable(data_as_tree)


## User Interface ####

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Informational Interview Tracker"),

    tabsetPanel(type = "tabs",
      
      # Tab: Interactive tree graphic          
      tabPanel('Tree',
               
        # Side: Coloring       
        sidebarPanel(
           selectInput('fill', 'Color', c("None", "Agency", "IC", "Division", "Title", "Area_of_interest_1", "Area_of_interest_2"))),
        
        # Main: Tree
        mainPanel(
          collapsibleTreeOutput("plot", height = "500px"))),
      
      # Tab: New observation addition
      tabPanel("Addition of New Person",
               
         # Side: Form
         sidebarPanel(
           textInput('name', 'Full Name'),
           textInput('goal', 'Field'),
           textInput('agency', 'Agency'),
           textInput('ic', 'IC'),
           textInput('division', 'Division'),
           textInput('title', 'Title'),
           actionButton('submit', 'Submit')),
               
         # Main: Datatable
         mainPanel(DT::dataTableOutput("mydata")))
      ))
    
## Server Logic ####

server <- function(input, output) {
  
    # Tab1: Tree
    output$plot <- renderCollapsibleTree({
    
     collapsibleTree(dataN,
                     tooltip = TRUE,
                     tooltipHtml = 'tooltip',
                     collapsed = FALSE,
                     fill = "green") #,
                     #fillByLevel = FALSE)
    })
     
    # Tab2: Data table
    
   # observeEvent(input$submit, {data <- data.frame("full_name" = input$name, "goal" = input$goal,"industry" = "", "Agency" = input$agency, 
   #                                                    "ic" = input$ic, "division" = input$division, "title" = input$title, "area_of_interest" = "", "link_onenote" = "", "link_biography" = "")
    output$mydata <- DT::renderDataTable({
      DT::datatable(data)
    })
   #new_sheet <- rbind(informational_interview, data)
   # writexl::write_xlsx(new_sheet, 'informational_interview_edited.xlsx')})
    
    # generate bins based on input$bins from ui.R
    
    
}

## Run the application ####
shinyApp(ui = ui, server = server)


