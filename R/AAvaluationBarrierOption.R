#valuation engine for barrier options with the fExoticOptions package
#we only use european options for structured products

barrier_option_valuation <- function(S, # = Stock
                                     K, # = Strike Price at Expiration
                                     r, # = Risk-free Interest Rate
                                     b = 0, # = cost-of-carry rate
                                     rebate = 0, # = possible rebate (if knocked-out or never knocked-in)
                                     barrier, # = Barrier level (absolute value)
                                     sig = NA, # = volatility if known
                                     maturity_date, # = date the option matures
                                     val_date, # = date the option is valued
                                     type) # = specific description (cdi, cui, cdo, cuo, pdi, pui, pdo, puo)
{
  val_date <- as.Date(as.character(val_date))
  # print(val_date)
  # print(maturity_date)
  maturity_date <- as.Date(as.character(maturity_date))
  evaluation_price <- as.numeric(S[val_date,4])
  time_to_maturity <- max(julian(maturity_date,val_date)[1],1)/365
  #print(paste(sig,"sig1"))
  if (is.na(sig)) # if a sigma has been set manually, take the one that has been set
  {
    sig <- get_vola(S,0.9,valuation_date = val_date) # = Volatility of the Underlying asset
  }
  sig <- sig * sqrt((time_to_maturity*365)) #auf laufzeit ausrechnen
  if (sig > 0.4) sig <- 0.4
  #is something needed that makes sure that the absolute value of the barrier is in
  #if not we should also change input of the our barrier function
  # print(paste(type,"type"))
  # print(paste(evaluation_price,"evaluation_price"))
  # print(paste(K,"K"))
  # print(paste(barrier,"barrier"))
  # print(paste(time_to_maturity,"ttm"))
  # print(paste(r,"r"))
  # print(paste(sig,"sig"))
  # print(paste(b,"b"))
  # print(paste(rebate,"rebate"))
  out <- StandardBarrierOption(TypeFlag = type,
                        S = evaluation_price,
                        X = K,
                        H = barrier,
                        Time = max(time_to_maturity,0.0001),
                        r = r,
                        sigma = max(sig,0.0001),
                        b = b,
                        K = rebate)@price
  #print(paste(out, "optionprice"))
  return(max(out,0))
  
}
