#valuation engine for options (blackscholes)
#we only use european options for structured products

option_valuation <- function(S, # = Stock Price
                          K, # = Strike Price at Expiration
                          r, # = Risk-free Interest Rate
                          q, # = dividend yield
                          sig = NA, # = volatility if known
                          maturity_date, # = date the option matures
                          valuation_date, # = date the option is valued
                          type) # = C for call or P for put
{
  K <- as.numeric(K)
  r <- as.numeric(r)
  q <- as.numeric(q)
  
  time_to_maturity <- julian(as.Date(as.character(maturity_date)),as.Date(valuation_date))[1]
  
  if (is.na(sig))
  {
    sig <- get_vola(S,0.95,valuation_date = as.Date(valuation_date)) # = Volatility of the Underlying asset
  }
  sig <- sig * sqrt(time_to_maturity)
  if (sig > 0.4) sig <- 0.4
  time_to_maturity <- time_to_maturity/365
  
  S <- as.numeric(S[valuation_date,4])
  
  #dividend included
  d1 <- (log(S / K) + ((r-q) + sig^2 / 2) * time_to_maturity) / (sig * sqrt(time_to_maturity))
  d2 <- d1 - sig * sqrt(time_to_maturity)
  if(type=="C"){
    value <- S * exp(-q * time_to_maturity) * pnorm(d1) - K * exp(-r * time_to_maturity) * pnorm(d2)
    return(value)}
  
  if(type=="P"){
    value <-  (K * exp(-r * time_to_maturity) * pnorm(-d2) - S * exp(-q * time_to_maturity) * pnorm(-d1))
    return(value)}
}

#various questions still open..
#influence of the volatility really that heavy?
#should we also include the dividend?
#should I leave the vola like this? that it is not an input (it's derived from timeseries)
#time convention? trading days?

