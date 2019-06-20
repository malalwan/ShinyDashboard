library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Admin Dashboard", 
                    dropdownMenuOutput("msgOutput"),
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "Notification 1",
                                   icon = icon("warning"),
                                   status = "warning"
                                   ),
                                 notificationItem(
                                   text = "Notification 2",
                                   icon = icon("dashboard"),
                                   status = "success"
                                   )          
                    ),
                    dropdownMenu(type = "tasks",
                                 taskItem(
                                   "Work 1", 15
                                 ),
                                 taskItem(
                                   "Work 2", color = "green", 35
                                 )
                                 )
    ),
    dashboardSidebar(
        sidebarMenu(
          sidebarSearchForm("sText", "sButton"),
          menuItem("Dashboard", badgeLabel = "Main", badgeColor = "blue", tabName = "dd", icon = icon("dashboard")),
          menuSubItem("Capital and Product Statistics", tabName = "cps", icon = icon("chart-pie")),
          menuSubItem("Dynamics and Operations", tabName = "do", icon = icon("business-time")),
          menuSubItem("Growth Analysis", tabName = "ga", icon = icon("chart-line")),
          menuItem("Members", tabName = "Members"),
          menuItem("KPI", tabName = "KPI"),
          textInput("tInput", "Search Startup", value = "All"),
          selectInput("data","Choose Startup",c("Cylinders"="cyl","Transmission"="am","Gears"="gear"),multiple = T,selected = "cyl"),
          div(style = 'padding-left:15px' , submitButton("Update!"))
          )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "cps",
                    fluidRow(
                      tabBox(width = 8,
                        tabPanel(title = "Test Time Series Data",
                                     status = "primary", solidHeader = T,
                                 div(style = 'overflow-y:scroll;height:340px;',plotOutput("histogram"))),
                        tabPanel(title = "Controls for Dashboard", status = "warning", solidHeader = T,
                                 div(style = 'overflow-y:scroll;height:340px;',
                                 "Use Controls to fine tune the dashboard",
                                 sliderInput("bins", "Number of Breaks",1,100,50),
                                 "Enter Value:",
                                 textInput("tInput", "Search Startup", value = "All"),
                                 textInput("Input","Search Cohort", value = "All"))
                        )
                      ),
                        infoBoxOutput("raisedCapital"),
                        infoBoxOutput("successRate"),
                        infoBoxOutput("grants"),
                        infoBoxOutput("patents")
                    ),
                    fluidRow(
                        valueBoxOutput("distinctProducts"),
                        valueBoxOutput("revProducts"),
                        valueBoxOutput("debtProducts")
                      )
                    ),
            tabItem(tabName = "do",
                    fluidRow(
                     valueBoxOutput("cac"),
                     valueBoxOutput("ltv"),
                     valueBoxOutput("crr")
                    ),
                    fluidRow(
                      uiOutput("gmv"),
                      uiOutput("runway"),
                      uiOutput("mrr"),
                      uiOutput("br")
                      )
                    ),
            tabItem(tabName = "KPI",
                    tabsetPanel(
                      tabPanel("Data",
                               div(style = 'overflow-y:scroll;height:500px;',
                                   tableOutput("mtcars"),downloadButton("downloadData","Download Data"))),
                      tabPanel("Summary",
                               div(style = 'overflow-y:scroll;height:500px;',verbatimTextOutput("summ"))),
                      tabPanel("Plot",
                               div(style = 'overflow-y:scroll;height:500px;',plotOutput("plot")))
                    )
                    ),
            tabItem(tabName = "Members",
                    h2("Member Data here"))
            )
        )
    ) 
    
# Server info 
server <- function(input, output) {
    output$histogram <- renderPlot({
        hist(faithful$eruptions, breaks = input$bins)    
    })
    output$msgOutput <- renderMenu({
        msgs <- apply(read.csv("update.csv"),1,function(row){
            messageItem(from = row[["from"]], message = row[["message"]])
        })
    dropdownMenu(type = "messages", .list = msgs)
    })
#this all for Capital and Product Dashboard
    
    output$raisedCapital <- renderInfoBox({
      infoBox(title = "Capital Raised", value = "10000$", icon = icon("wallet"))
    })
    
    output$successRate <- renderInfoBox({
      infoBox(title = "Success Rate", value = "100", icon = icon("bar-chart-o"))
    })
    
    output$grants <- renderInfoBox({
      infoBox(title = "Grants", value = "100", icon = icon("bar-chart-o"))
    })
    
    output$patents <- renderInfoBox({
      infoBox(title = "Patents", value = "100", icon = icon("bar-chart-o"))
    })
    
    output$distinctProducts <- renderValueBox({
      valueBox("1,000","Number of Distinct Products", icon = icon("wallet"))
    })
    
    output$revProducts <- renderValueBox({
      valueBox("100","Revenue Generating Products", icon = icon("wallet"))
    })
    
    output$debtProducts <- renderValueBox({
      valueBox("10","Debt Sharing Products", icon = icon("wallet"))
    })
    
#this all for Dynamics and Operations
    #----------row1
    output$cac <- renderValueBox({
      valueBox("100$","Customer Acquisition Cost", icon = icon("wallet"))
    })
    output$ltv <- renderValueBox({
      valueBox("1000$","Lifetime Value", icon = icon("wallet"))
    })
    output$crr <- renderValueBox({
      valueBox("10%","Customer Retention Rate", icon = icon("wallet"))
    })
    #-----------row2
    output$gmv <- renderUI({
      infoBox(title = "GMV/MAU", value = "100",width = 3,icon = icon("bar-chart-o"))
    })
    output$runway <- renderUI({
      infoBox(title = "Runway", value = "10 Years",width = 3,icon = icon("bar-chart-o"))
    })
    output$mrr <- renderUI({
      infoBox(title = "MRR", value = "1000$",width = 3,icon = icon("bar-chart-o"))
    })
    output$br <- renderUI({
      infoBox(title = "Burn Rate", value = "11%",width = 3,icon = icon("bar-chart-o"))
    })
    
#this all for Growth Analysis
    
    output$mtcars <- renderTable({
      mtcars[,c("mpg",input$data)]})
    output$summ <- renderPrint({
      summary(mtcars)
    })
    output$plot <- renderPlot({
      with(mtcars,boxplot(mpg~gear))
    })
    
    output$downloadData <- downloadHandler(
      filename = function(){
        paste("mtcars","csv",sep = ".")
      },
      content = function(file){
        write.csv(mtcars[,c("mpg",input$data)],file)
      }
    )
}
    
    
# Run the application 
shinyApp(ui = ui, server = server)
