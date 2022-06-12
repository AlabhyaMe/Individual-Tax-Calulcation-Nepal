#setwd("C:/Users/Dell/Dropbox/Shiny")
rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)

Salary_Function <- function(SalaryM, Status,CIT, PF,Ins){
  
  
  
  if (PF ==TRUE){
    SalaryM <- round(0.9*SalaryM,1)
  }
  
  Salary <- rep(SalaryM - CIT - (Ins/12),each=12)
  Month <- c("July","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June")
  
  CummulativeIncome <- cumsum(Salary)
  
  #Slab1
  
  Tax1 <- rep(0,each=12)
  Tax2 <- rep(0,each=12)
  Tax3 <- rep(0,each=12)
  Tax4 <- rep(0, each=12)
  Tax5 <- rep(0,each=12)

  TB <- c(500000,700000,1000000,2000000)
  TB2 <- TB + c(100000,100000,100000,0)
  
  if (Status == TRUE){
    TB<-TB2
  }
  
  
  i=1
  
  
  
  while(i<=12){
    if(CummulativeIncome[i]<=TB[1]){                     #Slab 1 Condition
        Tax1[i] <- round(Salary[i]*0.01,1)                         #Slab 1 Tax
      }
    else if(CummulativeIncome[i]>TB[1] & CummulativeIncome[i]<=TB[2]) { #Slab 2 Condition
      if (abs(CummulativeIncome[i]-TB[1]) < Salary[i]){
        Tax2[i] <- round(abs(CummulativeIncome[i]-TB[1])*0.10,1)
        Tax1[i] <- round((Salary[i]-abs(CummulativeIncome[i]-TB[1]))*0.01,1)
      }
      else{
        Tax2[i]<- round(Salary[i]*0.10,1)
      }
    }
    else if(CummulativeIncome[i]>TB[2] & CummulativeIncome[i]<=TB[3]) { #Slab 3 Condition
      if (abs(CummulativeIncome[i]-TB[2]) < Salary[i]){
        Tax3[i] <- round(abs(CummulativeIncome[i]-TB[2])*0.20,1)
        Tax2[i] <- round((Salary[i]-abs(CummulativeIncome[i]-TB[2]))*0.1,1)
      }
      else{
        Tax3[i]<- round(Salary[i]*0.20,1)
      }
    }
    else if(CummulativeIncome[i]>TB[3] & CummulativeIncome[i]<=TB[4]) { #Slab 4 Condition
      
      if (abs(CummulativeIncome[i]-TB[3]) < Salary[i]){
        Tax4[i] <- round(abs(CummulativeIncome[i]-TB[3])*0.30,1)
        Tax3[i] <- round((Salary[i]-abs(CummulativeIncome[i]-TB[3]))*0.2,1)
      }
      else{
        Tax4[i]<- round(Salary[i]*0.30,1)
      }
    }  
    else if(CummulativeIncome[i]>TB[4]) { #Slab 4 Condition
      if (abs(CummulativeIncome[i]-TB[4]) < Salary[i]){
        Tax5[i] <- round(abs(CummulativeIncome[i]-TB[4])*0.36,1)
        Tax4[i] <- round((Salary[i]-abs(CummulativeIncome[i]-TB[4]))*0.3,1)
      }
    else{
        Tax5[i]<- round(Salary[i]*0.36,1)
      }
    } 
    i=i+1
  }
  
  Tax_Table <- data.frame(Month,Salary,CummulativeIncome,Tax1,Tax2,Tax3,Tax4,Tax5)
  Tax_Table$TotalMonthlyTax <- rowSums(round(Tax_Table[,c(4:8)],2))
  Tax_Table$IncomeAfterTax <-Tax_Table$CummulativeIncome - Tax_Table$TotalMonthlyTax
  
  
  #Mutate Month to order it
  Tax_Table <- Tax_Table %>% 
              mutate(Month = factor(Month, levels = c("July","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","June" )))
  
  #Table for stacked
  Stacked_Table <- Tax_Table %>%
                      gather(key = "TaxBracket", value = "Amount", 4:8)
  
   
  
   return(Stacked_Table) 
  }
  
  
