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
    
    menuItem("Input Monthly Salary", tabName = "monthlySalary", icon = icon("dashboard")), #Salary Input
    numericInput("Sal", "Taxable Monthly Salary ", value =0, min = 0),                              #Salary Input Box
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
        title = "Monthly Total Tax Amount Distributed by Tax Bracket", plotOutput("StackedTax"))
      ),
    
    )
  )


server <- function(input, output) {
  
  
  observeEvent(input$Input, {                                                 #Calculation after pressing enter
    
    Chart1 <- Salary_Function (input$Sal, input$Married, 1)
    Chart2 <- Salary_Function(input$Sal,input$Married,2)
    TotalTax <- sum(Chart1$TotalMonthlyTax)
    Avg <- TotalTax/12
                    
    output$CumTax <- renderPlot({                                             #Plot1
       #Cumulative Tax Table Return
      g<- ggplot(Chart1, aes(x=Month, y=TotalMonthlyTax,label=TotalMonthlyTax, fill=-TotalMonthlyTax)) 
      g+geom_bar(stat="identity")+geom_text(size = 3, position = position_stack(vjust = 0.7),color="white")+
        geom_hline(yintercept=Avg,linetype="dashed", color = "green",size =1) + 
        annotate("text", x = 2, y=Avg+0.1*Avg, label = "Average Tax")
      }, res=100)
  
    output$StackedTax <- renderPlot({                                     #Plot2
        #Stacked Tax Table Return
      g<- ggplot(Chart2, aes(x=Month, y = Amount,fill = TaxBracket, label=Amount))
      g+ geom_bar(stat ="identity") + 
        geom_text(data= subset(Chart2,Amount !=0), size = 3, position = position_stack(vjust = 0.7),color="white")+
        scale_fill_discrete(name = "Yearly Income", labels = c("Less than 400K", "400K-500K", "500K-700K","700K-2000K","Greater than 2000K"))+
        geom_hline(yintercept=Avg, linetype="dashed", color = "green",size=1)+
        annotate("text", x = 2, y=Avg+0.1*Avg, label = "Average Tax")
      }, res =100)
    
   
    output$info_Salary <- renderInfoBox({
      infoBox("Total Taxable Yearly Income", paste0(input$Sal*12), icon = icon("credit-card"))
    })
    
    output$info_TotalTax<- renderInfoBox({
      TotalTax <- sum(Chart1$TotalMonthlyTax)
      infoBox("Total Liable Yearly Tax", paste0(TotalTax), icon = icon("list"))
    })
    
    output$info_MonthlyTax<- renderInfoBox({
      #Chart1 <- Salary_Function (input$Sal, input$Married, 1)
      
      infoBox("Average Monthly Tax", paste0(Avg), icon = icon("list"))
    })
    
    })
 
   
  
}

shinyApp(ui, server)
