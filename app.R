#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(rdrop2)
library(lubridate)
library(dplyr)
fields <- c("reportdate","newclients", "male","female","x1825","x2635","x3645","x45more","nativeamerican","black","White","Unknown","twoormore","othersingle","hispanic","nonhispanic","unknownethnicity","hmiseligible","locationattempt","located","inelligible","nostay","otherprogram","prescreensconducted","eligible","ineligiblefromscreen","k6toohigh","sentforrandom","randomized","declined","timebetweenpullandscreen","timebetweenrandomandenrol","timebetweenapprovalandmoveout","roomateassessments","roommateplacements","newplacements","singleoccplacements","roommatehousingplacements","recievingsubsidies","awaitinghousing","avgsinglerent","avgroommaterent","avglength","avgbetweenapprovalandlandlord")
saveinfo <- c("name","clinician", "date")
# Save a response
# ---- This is one of the two functions we will change for every storage type ----
outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
  
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Homelessnes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input Monthly Data", tabName = "input"),
      menuItem("Viz", tabName = "viz")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input",
              DT::dataTableOutput("responses", width = 300), tags$hr(),
              dateInput("reportdate", "Month of Report", value = NULL, format = "yyyy-mm"),
              numericInput("newclients", "New Clients enrolled in HNJ", min= 0, max = 100, value=0),
              textOutput("Client Gender"),
              numericInput("male", "Male", min= 0, max = 100, value = 0),
              numericInput("female", "Female", min = 0, max=100, value = 0),
              numericInput("x1825", "18-25", min = 0, max=100, value = 0),
              numericInput("x2635", "26-35", min = 0, max=100, value = 0),
              numericInput("x3645", "36-45", min = 0, max=100, value = 0),
              numericInput("x45more", "45+", min = 0, max=100, value = 0),
              numericInput("nativeamerican", "Alaska Native/American Indian", min = 0, max=100, value = 0),
              numericInput("black", "Black/African American", min = 0, max=100, value = 0),
              numericInput("White", "White", min = 0, max=100, value = 0),
              numericInput("Unknown", "Unknown", min = 0, max=100, value = 0),
              numericInput("twoormore", "Two or More Races", min = 0, max=100, value = 0),
              numericInput("othersingle", "Other Single Race", min = 0, max=100, value = 0),
              numericInput("hispanic", "Hispanic/Latino", min = 0, max=100, value = 0),
              numericInput("nonhispanic", "non-Hispanic/Latino", min = 0, max=100, value = 0),
              numericInput("unknownethnicity", "Unknown", min = 0, max=100, value = 0),
              numericInput("hmiseligible", "Number Identified as eligible per HMIS data pull", min = 0, max=100, value = 0),
              numericInput("locationattempt", "Number of individuals TRH attempted to locate", min = 0, max=100, value = 0),
              numericInput("located", "Number of individuals located by TRH", min = 0, max=100, value = 0),
              numericInput("inelligible", "Number of individuals inelligible for prescreens and why", min = 0, max=100, value = 0),
              numericInput("nostay", "No stay 30 days", min = 0, max=100, value = 0),
              numericInput("otherprogram", "Engaged with different housing programs", min = 0, max=100, value = 0),
              numericInput("prescreensconducted", "Number of HNJ prescreens conducted", min = 0, max=100, value = 0),
              numericInput("eligible", "Number Screened as Eligible", min = 0, max=100, value = 0),
              numericInput("ineligiblefromscreen", "Number ineligibleafter prescreen and why", min = 0, max=100, value = 0),
              numericInput("k6toohigh", "Dast-10 Audit-C & K6 too high", min = 0, max=100, value = 0),
              numericInput("sentforrandom", "Number sent to UCJC for randomization", min = 0, max=100, value = 0),
              numericInput("randomized", "Number randomized and willing to participate in HNJ", min = 0, max=100, value = 0),
              numericInput("declined", "Number randomized into HNJ that did not want to enroll and why", min = 0, max=100, value = 0),
              numericInput("timebetweenpullandscreen", "Average length of time between HMIS pull and identification and prescreen", min = 0, max=100, value = 0),
              numericInput("timebetweenrandomandenrol", "Average length of time between randomization and enrollment/program approval", min = 0, max=100, value = 0),
              numericInput("timebetweenapprovalandmoveout", "Average length of time between approval and move out to housing", min = 0, max=100, value = 0),
              numericInput("roomateassessments", "Number of roommate placement assessments conducted", min = 0, max=100, value = 0),
              numericInput("roommateplacements", "Number of new roommate housing placements determined", min = 0, max=100, value = 0),
              numericInput("newplacements", "Number of new placements into housing", min = 0, max=100, value = 0),
              numericInput("singleoccplacements", "Number of new single occupancy housing placements", min = 0, max=100, value = 0),
              numericInput("roommatehousingplacements", "Number of new roommate housing placements", min = 0, max=100, value = 0),
              numericInput("recievingsubsidies", "Total number of individuals receiving rental subsidies", min = 0, max=100, value = 0),
              numericInput("awaitinghousing", "Total number of individuals in the HNJ program that are awaiting a housing placement", min = 0, max=100, value = 0),
              numericInput("avgsinglerent", "Average single placement rent/month ($)", min = 0, max=100, value = 0),
              numericInput("avgroommaterent", "Average roommate rent/month ($)", min = 0, max=100, value = 0),
              numericInput("avglength", "Average length of HNJ rental subsidy", min = 0, max=100, value = 0),
              numericInput("avgbetweenapprovalandlandlord", "Average length of time between HNJ approval and landlord locator", min = 0, max=100, value = 0),
              actionButton("submit", "Submit")
      ),# datainput
      tabItem(tabName = "viz",
              fluidRow(
                column(3,
                       h3("About this App"),
                       box(width=12, p("Housing the persistently homeless")),
                       
                       
                       dateRangeInput("daterange", 
                                      "Select the date range you want to examine",
                                      start = "2017-04-01"
                       ),
                       
                       br(),
                       
                       
                       
                       actionButton("goButton", "Re-Run the Model"),
                       p("This shows the amount of idividuals housed through HNJ")
                ),
                
                
                # Show a tabset that includes a plot, summary, and table view
                # of the generated distribution
                column(9,
                       tabsetPanel(type = "tabs", 
                                   tabPanel("Plot", 
                                            fluidRow(plotOutput("totalhomeless")),
                                            fluidRow(plotOutput("costplot")),
                                            fluidRow(plotOutput("housing")),
                                            tabPanel("Data", tableOutput("table")))
                       )
                )
              )
      )# end of viz
    )#tabitems
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  fullset <- loadData()
  #fullset$reportdate <- as.Date(fullset$reportdate, origin = "1970-01-01")
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    save <- sapply(saveinfo, function(x) input[[x]])
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  observeEvent(input$submit, {
    fullset <- loadData()
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
      })
  
  cost <- reactive({
    date1 <- input$daterange[1]
    date2 <- input$daterange[2]
    
    fullset %>% filter(reportdate >= date1 & reportdate <= date2) 
    
  })
  
  output$costplot <- renderPlot({
    cost2 <- cost()
    
    ggplot(data = cost2) +
      ggtitle("Cost of housing homies") + 
       geom_line(aes(x = reportdate, y = avgsinglerent, col = "Single" )) +
       geom_line(aes(x= reportdate, y = avgroommaterent, col = "Roommate")) +
       theme_classic() + 
       xlab("Report Date") +
       ylab("Average Rent") +
      scale_color_manual(values = c("Single" = "black", "Roommate" = "red"), name = "Living Situation")# +
      #  scale_x_date(breaks = cost$reportdate)
  })
  
  output$totalhomeless <- renderPlot({
    cost2 <- cost()
    ggplot(cost2, aes(reportdate, hmiseligible)) + 
      geom_line() + 
      #scale_x_date(breaks = cost$reportdate, name = "Month") + 
      # scale_y_continuous(name = 'Number Eligible', limits = c(min(cost$hmiseligible)-2, max(cost$hmiseligible)+2), expand = c(0,0)) + 
      ggtitle("Persistently Homeless per HMIS Data Pull") + 
     theme_classic()
  })
  
  output$housing <- renderPlot({
    cost2 <- cost()
    ggplot(cost2)+
      geom_line(aes(reportdate, randomized,color="Randomized"))+
      geom_line(aes(reportdate, awaitinghousing, color="Awaiting Housing"))+
      geom_line(aes(reportdate, newplacements, color= "New Placements"))+
      # scale_x_date(breaks = cost$reportdate, name = "Month")+
      # scale_y_continuous(name = 'Individuals', limits = c(min(cost$newplacements)-2, max(cost$awaitinghousing+2)))+
      scale_color_manual(name = "Housing Status",values=c("Randomized"="black","Awaiting Housing" = "red", "New Placements" = "blue"))+
      ggtitle("Treatment Group") + 
      theme_classic()
  })
}# close server

# Run the application 
shinyApp(ui = ui, server = server)