#setwd("C:/Users/Dell/Dropbox/Shiny")
rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)

Salary_Function <- function(SalaryM, Status, ret_tab){
  
  Salary <- rep(SalaryM,each=12)
  Month <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
  CummulativeIncome <- cumsum(Salary)
  
  #Slab1
  
  Tax1 <- rep(0,each=12)
  Tax2 <- rep(0,each=12)
  Tax3 <- rep(0,each=12)
  Tax4 <- rep(0, each=12)
  Tax5 <- rep(0,each=12)

  TB <- c(400000,500000,700000,2000000)
  TB2 <- TB + 50000
  
  if (Status == TRUE){
    TB<-TB2
  }
  i=1
  
  while(i<=12){
    if(CummulativeIncome[i]<=TB[1]){                     #Slab 1 Condition
        Tax1[i] <- Salary[i]*0.01                         #Slab 1 Tax
      }
    else if(CummulativeIncome[i]>TB[1] & CummulativeIncome[i]<=TB[2]) { #Slab 2 Condition
      if (abs(CummulativeIncome[i]-TB[1]) < Salary[i]){
        Tax2[i] <- abs(CummulativeIncome[i]-TB[1])*0.10
        Tax1[i] <- (Salary[i]-abs(CummulativeIncome[i]-TB[1]))*0.01
      }
      else{
        Tax2[i]<- Salary[i]*0.10
      }
    }
    else if(CummulativeIncome[i]>TB[2] & CummulativeIncome[i]<=TB[3]) { #Slab 3 Condition
      if (abs(CummulativeIncome[i]-TB[2]) < Salary[i]){
        Tax3[i] <- abs(CummulativeIncome[i]-TB[2])*0.20
        Tax2[i] <- (Salary[i]-abs(CummulativeIncome[i]-TB[2]))*0.1
      }
      else{
        Tax3[i]<- Salary[i]*0.20
      }
    }
    else if(CummulativeIncome[i]>TB[3] & CummulativeIncome[i]<=TB[4]) { #Slab 4 Condition
      
      if (abs(CummulativeIncome[i]-TB[3]) < Salary[i]){
        Tax4[i] <- abs(CummulativeIncome[i]-TB[3])*0.30
        Tax3[i] <- (Salary[i]-abs(CummulativeIncome[i]-TB[3]))*0.2
      }
      else{
        Tax4[i]<- Salary[i]*0.30
      }
    }  
    else if(CummulativeIncome[i]>TB[4]) { #Slab 4 Condition
      if (abs(CummulativeIncome[i]-TB[4]) < Salary[i]){
        Tax5[i] <- abs(CummulativeIncome[i]-TB[4])*0.36
        Tax4[i] <- (Salary[i]-abs(CummulativeIncome[i]-TB[4]))*0.3
      }
    else{
        Tax5[i]<- Salary[i]*0.36
      }
    } 
    i=i+1
  }
  
  Tax_Table <- data.frame(Month,Salary,CummulativeIncome,Tax1,Tax2,Tax3,Tax4,Tax5)
  Tax_Table$TotalMonthlyTax <- rowSums(Tax_Table[,c(4:8)])
  Tax_Table$IncomeAfterTax <-Tax_Table$CummulativeIncome - Tax_Table$TotalMonthlyTax
  
  
  #Mutate Month to order it
  Tax_Table <- Tax_Table %>% 
              mutate(Month = factor(Month, levels = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec" )))
  
  #Table for stacked
  Stacked_Table <- Tax_Table %>%
                      gather(key = "TaxBracket", value = "Amount", 4:8)
  
   
  if (ret_tab==1){
   return(Tax_Table)
  } else if (ret_tab ==2) {
   return(Stacked_Table) }
  else {
   return("Enter the value 1 or 2")}
  }
  
