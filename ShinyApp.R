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
    numericInput("Sal", "Monthly Salary ", value =0, min = 0),                              #Salary Input Box
    checkboxInput("Married", "Check if Married", FALSE),
    sliderInput("CIT", "CIT Contribution Monthly", 0, 25000, 0),
    sliderInput("Ins", "Total YEARLY Insurance", 0, 40000, 0),
    checkboxInput("PF", "Check if PF or SSF (10% contribution assumed)", FALSE),
    actionButton("Input", "Enter")),
  
  dashboardBody(
    
    fluidRow(
      infoBoxOutput("info_Salary"),                           # infoBoxOutput, value "Output" passed from server
      infoBoxOutput("info_TotalTax"),
      infoBoxOutput("info_MonthlyTax")),
    
   fluidRow(
     infoBoxOutput("info_SalaryD"),
     infoBoxOutput("info_TotalTaxD"),
     infoBoxOutput("info_MonthlyTaxD"),
     
   ),
      
    fluidRow(
      box(
        title = "Monthly Total Tax without deduction", plotOutput("CumTax")),
      box(
        title = "Monthly Total Tax with PF, CIT and Insurance", plotOutput("StackedTax"))
      ),
   
   
    
    )
  )


server <- function(input, output) {
  
  
  observeEvent(input$Input, {                                                 #Calculation after pressing enter
    
    Chart1 <- Salary_Function (input$Sal, input$Married,0, F, 0)
    Chart2 <- Salary_Function(input$Sal,input$Married, input$CIT, input$PF, input$Ins)
    
    TotalTax <- sum(Chart1$Amount)
    Avg <- round(TotalTax/12,2)
    
    TotalTax2 <- sum(Chart2$Amount)
    Avg2 <- round(TotalTax2/12,2)
                    
    output$CumTax <- renderPlot({                                             #Plot1
       #Cumulative Tax Table Return
      g<- ggplot(Chart1, aes(x=Month, y = Amount,fill = TaxBracket, label=Amount))
      g+ geom_bar(stat ="identity") + 
        geom_text(data= subset(Chart1,Amount !=0), size = 3, position = position_stack(vjust = 0.7),color="white")+
        scale_fill_discrete(name = "Yearly Income", labels = c("< 500K", "500K-700K", "700K-1000K","100K-2000K","> 2000K"))+
        geom_hline(yintercept=Avg, linetype="dashed", color = "green",size=1)+
        annotate("text", x = 2, y=Avg+0.1*Avg, label = "Average Tax")
    }, res =100)
  
    output$StackedTax <- renderPlot({                                     #Plot2
        #Stacked Tax Table Return
      g<- ggplot(Chart2, aes(x=Month, y = Amount,fill = TaxBracket, label=Amount))
      g+ geom_bar(stat ="identity") + 
        geom_text(data= subset(Chart2,Amount !=0), size = 3, position = position_stack(vjust = 0.7),color="white")+
        scale_fill_discrete(name = "Yearly Income", labels = c("< 500K", "500K-700K", "700K-1000K","100K-2000K","> 2000K"))+
        geom_hline(yintercept=Avg2, linetype="dashed", color = "green",size=1)+
        annotate("text", x = 2, y=Avg2+0.1*Avg2, label = "Average Tax")
      }, res =100)
    
   
    output$info_Salary <- renderInfoBox({
      infoBox("Total Taxable Yearly Income", paste0(input$Sal*12), icon = icon("credit-card"))
    })
    
    output$info_TotalTax<- renderInfoBox({
      infoBox("Total Liable Yearly Tax", paste0(TotalTax), icon = icon("list"))
    })
    
    output$info_MonthlyTax<- renderInfoBox({
      infoBox("Average Monthly Tax", paste0(Avg), icon = icon("list"))
    })
    
    output$info_SalaryD <- renderInfoBox({
      infoBox("Total Taxable Yearly Income After Deduction", paste0((Chart2[1,2])*12), icon = icon("credit-card"))
    })
    
    output$info_TotalTaxD<- renderInfoBox({
      infoBox("Total Liable Yearly Tax After Deduction", paste0(TotalTax2), icon = icon("list"))
    })
    
    output$info_MonthlyTaxD<- renderInfoBox({
      infoBox("Average Monthly Tax After Deduction", paste0(Avg2), icon = icon("list"))
    })
   
    })
 
   
  
}

shinyApp(ui, server)
