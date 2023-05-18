####manipulating the findings to determine effects of price elasticity

library(tidyverse)


##################
#condition totals

pivot_longer(cols = neartot:longtot, names_to = "time", values_to = "totalcatch")

price <- 9.16  #usd per kg at 2015 levels
nearterm_price <- 9.73
midterm_price <- 10.16
longterm_price <- 11.01

#using impact models predicted 0.4% change in annual growth rat of price

SSP_production <- rbind(
  SSP2tot,
  SSP3tot,
  SSP4tot,
  SSP5tot
) %>% 
  mutate(
    neartot_year = SSP_production$neartot/ 20,
    midtot_year = SSP_production$midtot/ 20,
    longtot_year = SSP_production$longtot/ 40
  )

#######################next steps############################
#redo the NPV equation using the elasticity including system
#using elasticity as well in considerations

#parameters
testvars <- list(
  E = -0.476,
  Ff = 1.04,
  D = 1.05,
  B = 0.98096
)
#############initial variables#############
#with price = 9.16usd based on customs average and growth basedon on fish to 2039 data

intvars <- list(
  E = -0.8,
  Ff = 1 + 0.4, #i.e. 0.4 increase annually in price for mollusks
  D =  1 + 0.08,  #discount factor composite factor
  B = (E * Ff) - E + 1  #elasticity and annual price change composite factor
)


####building a new NPV function#########################

NPV_e <- function(quant,price,N,vars){
  quant = quant
  price = price
  labour_cost = (0.13836 * 7)  # aka $138.36 per ton of yield X 7 Vessels in the fishery
  N = N
  vars = vars
  
  Ff = vars$Ff
  B = vars$B
  D = vars$D
  
  vars <-  (B * Ff)/D 
  top <-  (vars)^N - 1  #relative to time
  bot <-  (vars) - 1
  G <-  top / bot
  revenue <- quant * price
  cost <- quant * labour_cost  #aggregated cost for LAC area based on that paper
  
  output = (revenue - cost) * G  #quantity per year x price x other factors
  return(output)
}


###testing using other empirical data###
##using the german studies test variables, expected output is 152
NPV_e(quant = 168,price = 0.05,N = 25, vars = testvars)

#works fine, results showed a value of 151.8744
#note when making list, use '=' sign so that you can index in functions
#note changes made after test to include netprofit
#netprofit respective of a composite factor of revenue - cost (i.e. cost is labour cost factor * quant)

##now for my values###
###################################################################

#test before including in tables
#my values

ssp_NPV <- SSP_production %>% 
  mutate(
    near_NPVe = NPV_e(quant = SSP_production$neartot_year,
                      price = price, N = 20, vars = intvars),
    
    mid_NPVe = NPV_e(quant = SSP_production$midtot_year,
                     price = midterm_price, N = 20, vars = intvars),
    
    long_NPVe = NPV_e(quant = SSP_production$longtot_year,
                      price = longterm_price, N = 40, vars = intvars)
  )






