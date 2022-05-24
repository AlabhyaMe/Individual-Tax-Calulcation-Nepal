#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#setwd("G:/Data Works GitHub/Tax Calculator Nepal in R")

#Required Packages
library(shiny)
library(shinydashboard)
library(tidyverse)

#Source for more inputs


source("Function_server.R")




ui <- dashboardPage( skin = "green",
  
  dashboardHeader(title = "Monthly Tax Figure Nepal"),
  
  dashboardSidebar( 
    
    menuItem("MonthlySalary", tabName = "monthlySalary", icon = icon("dashboard")), #Salary Input
    numericInput("Sal", "Salary", value = 40000, min = 0),                              #Salary Input Box
    checkboxInput("Married", "Check if Married", FALSE),
    actionButton("Input", "Enter")),
  
  dashboardBody(
    
    fluidRow(
      infoBoxOutput("info_Salary"),                           # infoBoxOutput, value "Output" passed from server
      infoBoxOutput("info_TotalTax"),
      infoBoxOutput("info_MonthlyTax")),
      
    fluidRow(
      box(
        title = "Monthly Total Tax", plotOutput("CumTax")),
      box(
        title = "Stacked Monthly Total Tax", plotOutput("StackedTax"))
      ),
    
    )
  )


server <- function(input, output) {
  
  
  observeEvent(input$Input, {
    Chart1 <- Salary_Function (input$Sal, input$Married, 1)
    Chart2 <- Salary_Function(input$Sal,input$Married,2)
    
                    
    output$CumTax <- renderPlot({ 
       #Cumulative Tax Table Return
      g<- ggplot(Chart1, aes(x=Month, y=TotalMonthlyTax,label=TotalMonthlyTax, fill=factor(TotalMonthlyTax))) 
      g+geom_bar(stat="identity")+geom_text(size = 3, position = position_stack(vjust = 0.7))
      }, res=75)
  
    output$StackedTax <- renderPlot({
        #Stacked Tax Table Return
      g<- ggplot(Chart2, aes(x=Month, y = Amount,fill = TaxBracket, label=Amount))
      g+ geom_bar(stat ="identity") + geom_text(size = 3, position = position_stack(vjust = 0.7))
      }, res =100)
    
    output$Table_Tax <- renderTable({
      Chart1
    }, res=100)
    
    output$info_Salary <- renderInfoBox({
      infoBox("Total Taxable Income", paste0(input$Sal*12), icon = icon("credit-card"))
    })
    
    output$info_TotalTax<- renderInfoBox({
      #Chart1 <- Salary_Function (input$Sal, input$Married, 1)
      TotalTax <- sum(Chart1$TotalMonthlyTax)
      infoBox("Total Tax Amount", paste0(TotalTax), icon = icon("list"))
    })
    
    output$info_MonthlyTax<- renderInfoBox({
      #Chart1 <- Salary_Function (input$Sal, input$Married, 1)
      TotalTax <- sum(Chart1$TotalMonthlyTax)
      infoBox("Average Tax in a Month", paste0(TotalTax/12), icon = icon("list"))
    })
    
    })
 
   
  
}

shinyApp(ui, server)
