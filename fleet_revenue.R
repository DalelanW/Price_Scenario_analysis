library(tidyverse)
library(readxl)

#import already cleaned dataset
ovrfleettotal <- read_excel("Ovrfleettotal.xlsx")

#creating a function for revenue
fleetrevenue <- function(yield, price){
  labour_cost = 0.13836  # aka $138.36 per ton of yield
  
  revenue = yield * price # in usd
  cost = labour_cost * yield # in usd
  total = revenue - cost
  return(total)
}

#testing with parameters
price = 9.16
fleetrevenue(1015021.134, price)

#it works, now for 
ovrfleettotal <- ovrfleettotal %>% 
  mutate(
    revenue = fleetrevenue(ovrfleettotal$totalcatch, price)
  )

#adding the net present value function
NPV <- function(revenue,i, time){
  discount = (1 + i)
  dfactor = discount ^ time
  value = revenue / dfactor
  return(value)
}


#mandatory testing
i = 0.08
NPV( 9157155.26,i, 1)

#seems to work fine

#adding the NPV table in the datasheet
ovrfleettotal <- ovrfleettotal %>% 
  mutate(
    NPV = NPV(ovrfleettotal$revenue,i,25)
  )


#some visualizations
library(viridis)
library(hrbrthemes)
#now looking at the potential total catch
ggplot(ovrfleettotal, aes( x = SSP, y = NPV, fill= time))+
  geom_bar(position = "dodge", stat = "identity", alpha = .9)+
  scale_fill_viridis(discrete = TRUE, option = "F")+
  ggtitle("Total Potential Catch of All Generations")+
  theme_ipsum()+
  xlab("")
