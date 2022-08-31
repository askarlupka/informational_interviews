#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##
#if(installed.packages(collapsibleTree))

list.of.packages <- c("shiny", "tidyverse", "remotes", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#This library is coming from my repository since I've been making changes to the original package code
new.package <- "collapsibleTree" %in% installed.packages()[,"Package"]
if(length(new.packages)) remotes::install_github("askarlupka/collapsibleTree",
                        upgrade = "ask") #you can switch this to never after the intial install. I've had issues with three of my packages (rlang, stringi, htmltools) just not updating

library(collapsibleTree) #tree design

#These libraries should be coming from CRAN
library(shiny) #making applications
library(tidyverse) #manipulating data
library(writexl) #saving data as excel sheet

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




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Informational Interview Tracker"),

    tabsetPanel(type = "tabs",
      tabPanel('Tree',
          sidebarPanel(
           selectInput('fill', 'Color', c("None", "Agency", "IC", "Division", "Title", "Area_of_interest_1", "Area_of_interest_2")
          )
        ),

        # Show a tree diagram with the selected root node
        mainPanel(
          collapsibleTreeOutput("plot", height = "500px")
      )),
      tabPanel("Input Values",
               sidebarPanel(
                 textInput('name', 'Full Name'),
                 textInput('goal', 'Goal'),
                 textInput('agency', 'Agency'),
               textInput('ic', 'IC'),
               textInput('division', 'Division'),
               textInput('title', 'Title'),
               actionButton('submit', 'Submit')
               ),
               mainPanel(tableOutput('view'))
      )
    
      )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderCollapsibleTree({
    
 
     
        # generate bins based on input$bins from ui.R


     collapsibleTree(dataN,
                     tooltip = TRUE,
                     tooltipHtml = 'tooltip',
                     collapsed = FALSE)
                   

    })
    
    observeEvent(input$submit, {new_data <- data.frame("full_name" = input$name, "goal" = input$goal,"industry" = "", "Agency" = input$agency, 
                                                       "ic" = input$ic, "division" = input$division, "title" = input$title, "area_of_interest" = "", "link_onenote" = "", "link_biography" = "")
    output$view <- renderTable({new_data})
    new_sheet <- rbind(informational_interview, new_data)
    write_xlsx(new_sheet, 'informational_interview_edited.xlsx')})
}

# Run the application 
shinyApp(ui = ui, server = server)
