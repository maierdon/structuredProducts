# script that takes product termsheet and some other parameters as an input and
# returns an xts table with all prices of the product and optionally creates a graph

# projRoot <- getwd()
# source(paste(projRoot,"R","valuation_product.R", sep="/"))

valuation_complete_lifecycle <- function(product_termsheet,
                                         underlying = NA,
                                         risk_free_rate = NA,
                                         expected_dividends = NA,
                                         chart = T,
                                         vola = NA)
{
  # maybe give possibility to input / create timeseries with changing risk_free_rate, expected dividends, volatility
  # so the characteristics will change even more during lifecycle, payoff at maturity would stay
  
  if (is.na(underlying)){
    underlying <- getSymbols(as.character(product_termsheet["Underlying",]), auto.assign = F)[,1:4]
  }
  
  if (is.na(risk_free_rate)){
    risk_free_rate <- as.numeric(product_termsheet["RiskFreeRate",])
  }

  if (is.na(expected_dividends)){
    expected_dividends <- as.numeric(product_termsheet["Dividend",])
  }

  if (is.na(vola)){
     vola <- as.numeric(product_termsheet["Volatility",])
  }
  
  product_term <- index(underlying[timeSequence(from = product_termsheet["IssueDate",],
                                                to = product_termsheet["MaturityDate",],by = "d"),])

  valuationts <- rep(NA, length(product_term))

  
  #define first price so the actual price can be normalized starting at 100
  
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
  
  
  if (chart)
  {
    underlying <- underlying[product_term,4]
    
    starting_price <- as.numeric(underlying[1])
    
    for (j in 1:length(product_term))
    {
      underlying[j] <- underlying[j] / starting_price * 100
    }
    
    minvalue <- min(min(as.numeric(valuationtsxts)),min(as.numeric(underlying)))
    maxvalue <- max(max(as.numeric(valuationtsxts)),max(as.numeric(underlying)))
    yrange <- c(minvalue*0.8, maxvalue*1.2)
    
    #chart_object <- 
    #print(c(tail(valuationtsxts,5)))  
    plot(valuationtsxts,ylim = yrange,
                         order.by = product_term,
                         main = colnames(product_termsheet),
                         ylab = "Normalized prices",
                         type = "l", lwd = 2)
    lines(underlying, lwd = 1)
    # if ("BarrierEventDate" %in% rownames(product_termsheet)){
    #   vert_line <- which(index(valuationtsxts) == as.Date(as.character(product_termsheet["BarrierEventDate",])))
    #   print(vert_line)
    #   print(index(valuationtsxts))
    #   abline(v = .index(valuationtsxts)[vert_line], col = "red")
    # }
    addLegend("topleft",on=1,
              legend.names = c(colnames(product_termsheet),"Underlying"), lty = c(1,1), lwd = c(2,1))
    #return(I(list(chart_object,valuationtsxts)))
  }
}




#define the input
# test_prod <- OutperformanceCert(Nominal = 100000,
#                                 Underlying = "AAPL",
#                                 IssueDate = as.Date("2021-01-01"),
#                                 Maturity = "1 year",
#                                 ParticipationRatio = 2,
#                                 StrikePerc = 1,
#                                 CapPerc = 1.05)
# 
# 
# valuation_complete_lifecycle(product_termsheet = test_prod,
#                              chart = T)
