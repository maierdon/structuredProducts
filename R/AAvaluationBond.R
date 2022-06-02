# zero bond creation / valuation
# can I take the FEMS? dont think so

bond_valuation <- function(nominal,
                           interest_rate,
                           valuation_date,
                           maturity_date){
  time_to_maturity <- julian(as.Date(as.character(maturity_date)),as.Date(as.character(valuation_date)))[1]/365
  out <- as.numeric(nominal) * exp(-as.numeric(interest_rate)*time_to_maturity)
  return(out)
}
