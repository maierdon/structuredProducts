#calculation of a volatility measurement that represents the implied volatility as
#good as possible
#in this file, a simple EWMA approach is chosen
#in a further step, the result for the apple stock is compared to the implied vola
#to find out if it is correlated at all... 
#when there is a high correlation this approach is used as it is only time series dependent
#and therefore more convenient when we simulate market scenarios

get_vola <- function(S, #underlying price
                     a, #factor see RE slides
                     valuation_date) #date the vola is needed
{
  first_observation_date <- valuation_date %m-% months(6)
  
  S <- log(S[timeSequence(from = first_observation_date,to = valuation_date, by = "day"), 4])
  
  daily_returns <- S[, 1] / lag.xts(S[, 1]) - 1
  daily_returns <- na.omit(daily_returns)
  vartoday <- as.numeric((1-a) * abs(daily_returns[1]))
  for (i in 2:length(daily_returns))
  {
    vartoday <- as.numeric((1-a) * abs(daily_returns[i]) + a * vartoday)
  }
  
  sdtoday <- sqrt(vartoday)
  
  return(sdtoday)
}