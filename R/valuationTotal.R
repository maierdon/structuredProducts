# script that takes product termsheet and some other parameters as an input and
# returns an xts table with all prices of the product and optionally creates a graph

# projRoot <- getwd()
# source(paste(projRoot,"R","valuation_product.R", sep="/"))

valuationTotal <- function(product_termsheet)
{
  #this is downloaded way too much in my opinion.. is there a possibility to reduce?
  #I think it would make a big difference
  underlying <- getSymbols(as.character(product_termsheet["Underlying",]), auto.assign = F)[,1:4]
  #ask wolfgang what is more important, the possibility to choose every underlying
  #or speed? 
  #or think of other ways to increase speed.
  
  risk_free_rate <- as.numeric(product_termsheet["RiskFreeRate",])
  
  expected_dividends <- as.numeric(product_termsheet["Dividend",])
  
  vola <- as.numeric(product_termsheet["Volatility",])
  
  product_term <- index(underlying[timeSequence(from = product_termsheet["IssueDate",],
                                                to = product_termsheet["MaturityDate",],by = "d"),])
  
  valuationts <- rep(NA, length(product_term))
  
  first_valuation <- product_valuation(product_termsheet = product_termsheet,
                                       underlying = underlying,
                                       risk_free_rate = risk_free_rate,
                                       expected_dividends = expected_dividends,
                                       vola = vola,
                                       valuation_date = as.Date(product_term[1]))
  for (i in 1:length(product_term)){
    valuationts[i] <- product_valuation(product_termsheet = product_termsheet,
                                        underlying = underlying,
                                        risk_free_rate = risk_free_rate,
                                        expected_dividends = expected_dividends,
                                        vola = vola,
                                        valuation_date = as.Date(product_term[i])) / 
      first_valuation * 100
  }
  valuationtsxts <- as.xts(valuationts,
                           order.by = product_term)
  
  valuationtsxts[is.nan(valuationtsxts)] <- 0
  
  underlying <- underlying[product_term,4]
  starting_price <- as.numeric(underlying[1])
  for (j in 1:length(product_term))
  {
    underlying[j] <- underlying[j] / starting_price * 100
  }
  valuationtsxts <- cbind(valuationtsxts, underlying)  
  return(valuationtsxts)
}
