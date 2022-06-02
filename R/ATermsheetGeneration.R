# file that creates termsheets for every structured product

# Outperformance Certificate ------------------------------
#cap needs to be doubled to neutralize both long positions
OutperformanceCert <- function(Nominal, #Nominal = Investes Ammount
                               Underlying, #Underlying = Character (symbol of underlying)
                               IssueDate = Sys.Date(), #Issue date of the product default today
                               #otherwise character in %yyyy%-%mm%-%dd% format
                               Maturity = "1 year", #Maturity = lifetime of the certificate in years!
                               #ParticipationRatio = participation ratio for upside > 1 if = 1 ==> Tracker Certificate
                               #needs to be derived
                               StrikePerc = 1, #strike level default = 1 ; 1 means strike = IssuePrice ; Above the strike one participates
                               CapPerc = NA,
                               risk_free_rate = NA, #current risk free interest rate (for black scholes)
                               expected_dividends = NA, #expected dividends of the underlying
                               vola_at_issue = NA) #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity))
  {
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign = F)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific character input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]), numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next day
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4])

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  Cap <- CapPerc * as.numeric(Underlying[IssueDate, 4]) #if cap is set this is the cap level, otherwise NA

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0))
             ,which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  LEPO_price <- option_valuation(S = Underlying,
                                 K = 0.01,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "C")
  if(LEPO_price > (as.numeric(Underlying[IssueDate, 4]))) warning("LEPO more expensive than share price!")
  numb <- Nominal / as.numeric(Underlying[IssueDate, 4])
  amount <- numb * LEPO_price #secures participation factor of 1

  option_price <- option_valuation(S = Underlying,
                                   K = Strike,
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola_at_issue,
                                   maturity_date = MaturityDate,
                                   valuation_date = IssueDate,
                                   type = "C")
  numb_opt <- (Nominal - amount) / option_price

  if (!(is.na(CapPerc)))
  {
    option_price1 <- option_valuation(S = Underlying,
                                      K = Cap,
                                      r = risk_free_rate,
                                      q = expected_dividends,
                                      sig = vola_at_issue,
                                      maturity_date = MaturityDate,
                                      valuation_date = IssueDate,
                                      type = "C")
    amount2 <- numb * option_price1
    #outperffactor <- ((Nominal - amount + numb_opt * option_price1) / option_price) / numb
    #outperffactor <- numb_opt / numb
    numb_opt <- (Nominal - amount + amount2) / (option_price - option_price1)
    # add_opt <- 1000
    # while (add_opt > 1){
    #   add_opt <- ((Nominal - amount + numb_opt * option_price1) / option_price) - numb_opt
    #   numb_opt <- numb_opt + add_opt
    # }
    #((as.numeric(Underlying[IssueDate, 4]) - LEPO_price + option_price1) / option_price)
  }

  outperffactor <- numb_opt / numb
  ParticipationRatio <- 1 + outperffactor

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  TermSheet$Cap <- as.character(Cap)
  TermSheet$ParticipationFactor <- as.character(round(ParticipationRatio,4))
  TermSheet$Currency <- "USD"
  if (is.na(Cap)) TermSheet$Components <- I(list(c("LEPO", "Long Call"))) else TermSheet$Components <- I(list(c("LEPO", "Long Call", "Short Call", "Short Call")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$LEPOS <- as.character(round(numb,4))
  TermSheet$Options <- as.character(round(numb_opt,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet <- as.data.frame(TermSheet)
  colnames(TermSheet) <- "Outperformance Certificate"
  return(TermSheet)
}


# Capital Protection Certificate with participation------------------------------------

CapitalProtection <- function(Nominal,
                              Underlying,
                              IssueDate = Sys.Date(),
                              Maturity,
                              ProtectionFactor = 1,
                              #ParticipationRatio = 1,
                              StrikePerc = 1,
                              CapPerc = NA,
                              risk_free_rate = NA, #current risk free interest rate (for black scholes)
                              expected_dividends = NA, #expected dividends of the underlying
                              vola_at_issue = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity)))
                              barrier = F, BarrierPerc = NA)
  {
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign=FALSE)

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]),numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next date
  }

  ProtectionLevel <- ProtectionFactor * Nominal #amount of protected capital (multiply nominal with pretection factor)

  Cap <- CapPerc * as.numeric(Underlying[IssueDate, 4]) #calculate Cap with the Cap_percentage

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4]) # calculate strike with Strike_percentage
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4]) #calculate barrier level if set

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date
  #extract number from string and
  #add to issue date in years
  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  bond_price <- ProtectionLevel - (ProtectionLevel * (risk_free_rate + 0.04))

  if (barrier){
    option_price <- barrier_option_valuation(S = Underlying,
                                             K = Strike,
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola_at_issue,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "cuo")
  }
  else {
    option_price <- option_valuation(S = Underlying,
                                     K = Strike,
                                     r = risk_free_rate,
                                     q = expected_dividends,
                                     sig = vola_at_issue,
                                     maturity_date = MaturityDate,
                                     valuation_date = IssueDate,
                                     type = "C")
  }

  numb <- Nominal / as.numeric(Underlying[IssueDate,4])
  numb_opt <- (Nominal - bond_price) / option_price
  if (!(is.na(CapPerc)))
  {
    option_price1 <- option_valuation(S = Underlying,
                                      K = Cap,
                                      r = risk_free_rate,
                                      q = expected_dividends,
                                      sig = vola_at_issue,
                                      maturity_date = MaturityDate,
                                      valuation_date = IssueDate,
                                      type = "C")
    numb_opt <- (Nominal - bond_price) / (option_price - option_price1)

    # add_opt <- 1000
    # while (add_opt > 1){
    #   add_opt <- ((Nominal - bond_price + numb_opt * option_price1) / option_price) - numb_opt
    #   numb_opt <- numb_opt + add_opt
    # }
    #ParticipationRatio <- ((Nominal - bond_price + option_price1) / option_price)
  }

  ParticipationRatio <- numb_opt / numb

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(round(risk_free_rate,4))
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$ProtectedAmount <- as.character(ProtectionLevel)
  TermSheet$BondComponent <- as.character(ProtectionLevel)
  TermSheet$Strike <- as.character(round(Strike,2))
  TermSheet$Cap <- as.character(Cap)
  TermSheet$ParticipationFactor <- as.character(round(ParticipationRatio,4))
  TermSheet$Currency <- "USD"
  if (is.na(Cap)){
    if (barrier){
      TermSheet$Components <- I(list(c("Zero Bond", "Long BUO Call")))
    }
    else TermSheet$Components <- I(list(c("Zero Bond", "Long Call")))
  }
  else{
    if (barrier){
      TermSheet$Components <- I(list(c("Zero Bond", "Long BUO Call", "Short Call")))
    }
    else TermSheet$Components <- I(list(c("Zero Bond", "Long Call", "Short Call")))
  }
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Options <- as.character(round(numb_opt,4))
  TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet = as.data.frame(TermSheet)
  colnames(TermSheet) <- "Capital Protection Certificate"
  return(TermSheet)

}


# Discount Certificate ------------------------------------

DiscountCertificate <- function(Nominal,
                              Underlying,
                              IssueDate = Sys.Date(),
                              Maturity,
                              StrikePerc = 1,
                              risk_free_rate = NA, #current risk free interest rate (for black scholes)
                              expected_dividends = NA, #expected dividends of the underlying
                              vola_at_issue = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity)))
                              barrier = F, BarrierPerc = NA)
{
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign=FALSE)

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]),numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next date
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4]) # calculate strike with Strike_percentage
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4])

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date
  #extract number from string and
  #add to issue date in years
  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  LEPO <- option_valuation(S = Underlying,
                           K = 0.01,
                           r = risk_free_rate,
                           q = expected_dividends,
                           sig = vola_at_issue,
                           maturity_date = MaturityDate,
                           valuation_date = IssueDate,
                           type = "C")

  option_price <- option_valuation(S = Underlying,
                                     K = Strike,
                                     r = risk_free_rate,
                                     q = expected_dividends,
                                     sig = vola_at_issue,
                                     maturity_date = MaturityDate,
                                     valuation_date = IssueDate,
                                     type = "C")
  numb <- Nominal / (LEPO - option_price)
  discount <- numb * (as.numeric(Underlying[IssueDate, 4]) - LEPO + option_price)
  discount_percent <- discount / Nominal

  if (barrier) {
    option_price1 <- barrier_option_valuation(S = Underlying,
                                             K = Strike,
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola_at_issue,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "pdo")
    numb <- Nominal / (LEPO - option_price + option_price1)
    discount <- numb * (as.numeric(Underlying[IssueDate, 4]) - LEPO + option_price - option_price1)
    discount_percent <- discount / Nominal
  }

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  TermSheet$Cap <- as.character(Strike) # because its capped by the short call!
  TermSheet$Currency <- "USD"
  if (barrier){
    TermSheet$Components <- I(list(c("LEPO", "Short Call", "Long BDO Put")))
  }
  else TermSheet$Components <- I(list(c("LEPO", "Short Call")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Discount <- as.character(round(discount_percent,4))
  if (barrier) TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet = as.data.frame(TermSheet)
  colnames(TermSheet) <- "Discount Certificate"
  return(TermSheet)
}

# Barrier discount certificate term -------------------------------------
BarrierDiscountCertificate <- function(Nominal,
                                Underlying,
                                IssueDate = Sys.Date(),
                                Maturity,
                                StrikePerc = 1,
                                BarrierPerc,
                                risk_free_rate = NA, #current risk free interest rate (for black scholes)
                                expected_dividends = NA, #expected dividends of the underlying
                                vola_at_issue = NA) #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity))))
{
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign=FALSE)

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]),numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next date
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4]) # calculate strike with Strike_percentage
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4])

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date
  #extract number from string and
  #add to issue date in years
  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  LEPO <- option_valuation(S = Underlying,
                           K = 0.01,
                           r = risk_free_rate,
                           q = expected_dividends,
                           sig = vola_at_issue,
                           maturity_date = MaturityDate,
                           valuation_date = IssueDate,
                           type = "C")

  short_call <- option_valuation(S = Underlying,
                                 K = Strike,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "C")

  long_put_b <- barrier_option_valuation(S = Underlying,
                                         K = Strike,
                                         r = risk_free_rate,
                                         b = 0,
                                         rebate = 0,
                                         barrier = BarrierLevel,
                                         sig = vola_at_issue,
                                         maturity_date = MaturityDate,
                                         val_date = IssueDate,
                                         type = "pdo")

  numb <- Nominal / (LEPO - short_call + long_put_b)
  discount <- numb * (as.numeric(Underlying[IssueDate, 4]) - LEPO + short_call - long_put_b)
  discount_percent <- discount / Nominal

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  TermSheet$Cap <- as.character(Strike)
  TermSheet$Currency <- "USD"
  TermSheet$Components <- I(list(c("LEPO", "Short Call", "Long BDO Put")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Discount <- as.character(round(discount_percent,4))
  TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet <- as.data.frame(TermSheet)
  colnames(TermSheet) <- "Barrier Discount Certificate"
  return(TermSheet)
}

# Reverse Convertible ------------------------------------

#hard because one part is a bond, the other an put option (which is sold)
#coupon is guaranteed, but at only at maturity (cashflow wise!)
#so more than the nominal goes into the zero bond
#if underlying price goes down, one has to buy at the strike price
#--> money from maturing zero bond is taken - guaranteed coupon to buy the shares
#we only do cash settlement though

#the model is a bit wrong: for simplicity reasons we model it as a zero bond
#and a short put option. the difference between the nominal and the invested
#amount for the zero bond we just ignore. Same for the realized premium for the
#sold put option

ReverseConvertible <- function(Nominal,
                               Underlying,
                               IssueDate = Sys.Date(),
                               Maturity,
                               StrikePerc = 1,
                               risk_free_rate = NA, #current risk free interest rate (for black scholes)
                               expected_dividends = NA, #expected dividends of the underlying
                               vola_at_issue = NA,  #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity)))))
                               barrier = F, BarrierPerc = NA)
{
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign=FALSE)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]),numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next date
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4]) # calculate strike with Strike_percentage
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4])

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date
  #extract number from string and
  #add to issue date in years
  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  bond_price <- Nominal - (Nominal * (risk_free_rate + 0.04))

  if (barrier){
    option_price <- barrier_option_valuation(S = Underlying,
                                             K = Strike,
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola_at_issue,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "pdi")
  }
  else{
    option_price <- option_valuation(S = Underlying,
                                     K = Strike,
                                     r = risk_free_rate,
                                     q = expected_dividends,
                                     sig = vola_at_issue,
                                     maturity_date = MaturityDate,
                                     valuation_date = IssueDate,
                                     type = "P")
  }

  #calculate how many options I can sell for the nominal amount
  numb <- Nominal / Strike
  Coupon <- (Nominal - bond_price) + option_price * numb
  coupon_percent <- Coupon / Nominal
    #(bond_price - numb * option_price)

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- Symbol
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$BondComponent <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  TermSheet$Currency <- "USD"
  if (barrier){
    TermSheet$Components <- I(list(c("Zero Bond", "Short BDI Put")))
  }
  else TermSheet$Components <- I(list(c("Zero Bond", "Short Put")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Coupon <- as.character(round(coupon_percent,4))
  if (barrier) TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet = as.data.frame(TermSheet)
  colnames(TermSheet) <- "Reverse Convertible"
  return(TermSheet)
}

# Tracker Certificate -----------------------------
#open-ended?
TrackerCertificate <- function(Nominal,
                               Underlying,
                               IssueDate = Sys.Date(),
                               Maturity,
                               risk_free_rate = NA, #current risk free interest rate (for black scholes)
                               expected_dividends = NA, #expected dividends of the underlying
                               vola_at_issue = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity))))))
                               CapPerc = NA, Strike = NA)
{
  #maybe just take the underlying if no maturity has been set?
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign=FALSE)

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]),numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next date
  }

  if (!(is.na(CapPerc))){
    Cap <- CapPerc * as.numeric(Underlying[IssueDate, 4])
  }

  else{
    Cap <- NA
  }

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date
  #extract number from string and
  #add to issue date in years
  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  #need to implement a Cap in the Tracker ???? so an additional short call
  LEPO <- option_valuation(S = Underlying,
                           K = 0.01,
                           r = risk_free_rate,
                           q = expected_dividends,
                           sig = vola_at_issue,
                           maturity_date = MaturityDate,
                           valuation_date = IssueDate,
                           type = "C")
  numb <- Nominal / LEPO

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- Symbol
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  if (!(is.na(Strike))) TermSheet$Strike <- as.character(Strike)
  TermSheet$UnderlyingPrice <- as.character(as.numeric(Underlying[IssueDate, 4]))
  TermSheet$Cap <- as.character(Cap)
  TermSheet$Currency <- "USD"
  if (!(is.na(Cap))){
    TermSheet$Components <- I(list(c("LEPO","Short Call")))
  }
  else TermSheet$Components <- I(list(c("LEPO")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$LEPOS <- as.character(round(numb,4))
  if (!(is.na(Strike))) TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet = as.data.frame(TermSheet)
  colnames(TermSheet) <- "Tracker Certificate"
  return(TermSheet)
}


# Warrant ----------------------------------
Warrant <- function(Nominal, #Nominal = Invested Ammount
                    Underlying, #Underlying = Character (symbol of underlying)
                    IssueDate = Sys.Date(), #Issue date of the product default today otherwise character in %yyyy%-%mm%-%dd% format
                    Maturity = "1 year", #Maturity = lifetime of the certificate in years!
                    StrikePerc = 1,
                    type,  #Call or Put
                    risk_free_rate = NA, #current risk free interest rate (for black scholes)
                    expected_dividends = NA, #expected dividends of the underlying
                    vola_at_issue = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity))))))
                    barrier = F, BarrierPerc = NA)
  {
  if (!(type %in% c("Call","Put" ))) {
    stop("Type has to be either 'Call' (Call Warrant) or 'Put' (Put Warrant)!")#prevent wrong input values
  }

  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign = F)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific character input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]), numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next day
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4])
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4])
  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  if (type == "Call")
  {
    if (barrier) {
      option <- barrier_option_valuation(S = Underlying,
                                         K = Strike,
                                         r = risk_free_rate,
                                         b = 0,
                                         rebate = 0,
                                         barrier = BarrierLevel,
                                         sig = vola_at_issue,
                                         maturity_date = MaturityDate,
                                         val_date = IssueDate,
                                         type = "cdo")
    }
    else{
      option <- option_valuation(S = Underlying,
                                 K = Strike,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "C")
    }
  }
  else
  {
    if (barrier){
      option <- barrier_option_valuation(S = Underlying,
                                         K = Strike,
                                         r = risk_free_rate,
                                         b = 0,
                                         rebate = 0,
                                         barrier = BarrierLevel,
                                         sig = vola_at_issue,
                                         maturity_date = MaturityDate,
                                         val_date = IssueDate,
                                         type = "puo")
    }
    else {
      option <- option_valuation(S = Underlying,
                                 K = Strike,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "P")
    }
  }
  numb <- Nominal / option

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- Symbol
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  if (barrier) TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet$Currency <- "USD"
  if (type == "Call") TermSheet$Type <- "Call" else TermSheet$Type <- "Put"
  if (type == "Call"){
    if (barrier){
      TermSheet$Components <- I(list("Long BDO Call"))
    }
    else TermSheet$Components <- I(list("Long Call"))
  }
  else{
    if (barrier){
      TermSheet$Components <- I(list("Long BUO Put"))
    }
    else TermSheet$Components <- I(list("Long Put"))
  }
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet <- as.data.frame(TermSheet)
  colnames(TermSheet) <- "Warrant"
  return(TermSheet)

}

# Spread Warrant ----------------------------------
SpreadWarrant <- function(Nominal, #Nominal = Invested Amount
                    Underlying, #Underlying = Character (symbol of underlying)
                    IssueDate = Sys.Date(), #Issue date of the product default today otherwise character in %yyyy%-%mm%-%dd% format
                    Maturity = "1 year", #Maturity = lifetime of the certificate in years!
                    StrikePerc = 1, #strike of the long warrant
                    CapPerc, #strike of the short warrant
                    type,  #Bull or Bear
                    risk_free_rate = NA, #current risk free interest rate (for black scholes)
                    expected_dividends = NA, #expected dividends of the underlying
                    vola_at_issue = NA) #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity)
{
  if (!(type %in% c("Bull","Bear" ))) {
    stop("Type has to be either 'Bull' (Bull Spread Warrant) or 'Bear' (Bear Spread Warrant)!")#prevent wrong input values
  }

  if (type == "Bull") {
    if (CapPerc < StrikePerc) stop("Cap should be higher than Strike for the Bull Spread Warrant!")#prevent wrong input values
  }
  else {if (CapPerc > StrikePerc) stop("Cap should be lower than Strike for the Bear Spread Warrant!")}

  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign = F)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific character input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]), numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next day
  }

  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4])

  Cap <- CapPerc * as.numeric(Underlying[IssueDate, 4])

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  if (type == "Bull")
  {
    option <- option_valuation(S = Underlying,
                                 K = Strike,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "C")

    option1 <- option_valuation(S = Underlying,
                               K = Cap,
                               r = risk_free_rate,
                               q = expected_dividends,
                               sig = vola_at_issue,
                               maturity_date = MaturityDate,
                               valuation_date = IssueDate,
                               type = "C")
  }
  else
  {
    option <- option_valuation(S = Underlying,
                                 K = Strike,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola_at_issue,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "P")
    option1 <- option_valuation(S = Underlying,
                               K = Cap,
                               r = risk_free_rate,
                               q = expected_dividends,
                               sig = vola_at_issue,
                               maturity_date = MaturityDate,
                               valuation_date = IssueDate,
                               type = "P")
  }

  numb <- Nominal / (option - option1)

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- Symbol
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(Strike)
  TermSheet$Cap <- as.character(Cap)
  TermSheet$Type <- as.character(type)
  TermSheet$Currency <- "USD"
  if (type == "Bull") TermSheet$Type <- "Bull" else TermSheet$Type <- "Bear"
  if (type == "Bull") TermSheet$Components <- I(list(c("Long Call", "Short Call"))) else TermSheet$Components <- I(list(c("Long Put", "Short Put")))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet <- as.data.frame(TermSheet)
  colnames(TermSheet) <- "Spread Warrant"
  return(TermSheet)

}


# Zero Bond for Capital protection with knock-out ------------------------------------

ZeroBond <- function(Nominal,
                     Underlying,
                     IssueDate = Sys.Date(),
                     Maturity,
                     ProtectionFactor = 1,
                     risk_free_rate = NA,
                     expected_dividends = NA,
                     vola_at_issue = NA,
                     StrikePerc = NA,
                     BarrierPerc = NA,
                     numb = NA,
                     type = "")
{
  Symbol <- Underlying
  Underlying <- getSymbols(Symbol, auto.assign = F)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]), numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next day
  }

  ProtectionLevel <- ProtectionFactor * Nominal #amount of protected capital (multiply nominal with pretection factor)
  Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4]) # calculate strike with Strike_percentage
  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4]) # calculate barrier level with barrier_percentage
  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity ))) # calculate Maturity date

  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola_at_issue))
  {
    vola_at_issue <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  if (type == "BRC"){
    bond_price <- Nominal - (Nominal * (risk_free_rate + 0.04))

    option_price <- barrier_option_valuation(S = Underlying,
                                             K = Strike,
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola_at_issue,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "pdi")

    #calculate how many options I can sell for the nominal amount
    numb <- Nominal / Strike
    Coupon <- (Nominal - bond_price) + option_price * numb
    coupon_percent <- Coupon / Nominal
  }
  else if (type == "CPKO"){
    bond_price <- ProtectionLevel - (ProtectionLevel * (risk_free_rate + 0.04))
    option_price <- barrier_option_valuation(S = Underlying,
                                             K = Strike,
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola_at_issue,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "cuo")
    numb <- Nominal / as.numeric(Underlying[IssueDate,4])
    numb_opt <- (Nominal - bond_price) / option_price
    ParticipationRatio <- numb_opt / numb
    numb <- numb_opt
  }
  # else if (type == "W"){
  #   if (BarrierLevel < Strike)
  #   {
  #     option <- barrier_option_valuation(S = Underlying,
  #                                        K = Strike,
  #                                        r = risk_free_rate,
  #                                        b = 0,
  #                                        rebate = 0,
  #                                        barrier = BarrierLevel,
  #                                        sig = vola_at_issue,
  #                                        maturity_date = MaturityDate,
  #                                        val_date = IssueDate,
  #                                        type = "cdo")
  #   }
  #   else{
  #     option <- barrier_option_valuation(S = Underlying,
  #                                        K = Strike,
  #                                        r = risk_free_rate,
  #                                        b = 0,
  #                                        rebate = 0,
  #                                        barrier = BarrierLevel,
  #                                        sig = vola_at_issue,
  #                                        maturity_date = MaturityDate,
  #                                        val_date = IssueDate,
  #                                        type = "puo")
  #   }
  #
  # }

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$ProtectedAmount <- as.character(ProtectionLevel)
  TermSheet$BondComponent <- as.character(ProtectionLevel)
  TermSheet$Strike <- as.character(Strike)
  if (type == "CPKO") TermSheet$ParticipationFactor <- as.character(round(ParticipationRatio,4))
  TermSheet$Currency <- "USD"
  TermSheet$Components <- I(list("Zero Bond"))
  TermSheet$Volatility <- as.character(round(vola_at_issue,4))
  if (type == "BRC") TermSheet$Coupon <- as.character(round(coupon_percent,4))
  TermSheet$Options <- as.character(round(numb,4))
  TermSheet$BarrierLevel <- as.character(round(BarrierLevel,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet = as.data.frame(TermSheet)
  colnames(TermSheet) <- "Zero Bond"
  return(TermSheet)
}

# bonus certificate termsheet -----------------------
BonusCertificateTerm <- function(Nominal, #Nominal = Investes Ammount
            Underlying, #Underlying = Character (symbol of underlying)
            IssueDate = Sys.Date(), #Issue date of the product default today
            #otherwise character in %yyyy%-%mm%-%dd% format
            Maturity = "1 year", #Maturity = lifetime of the certificate in years!
            #ParticipationRatio = participation ratio for upside > 1 if = 1 ==> Tracker Certificate
            #needs to be derived
            #StrikePerc = 1, #strike level default = 1 ; 1 means strike = IssuePrice ; Above the strike one participates
            CapPerc = NA,
            risk_free_rate = NA, #current risk free interest rate (for black scholes)
            expected_dividends = NA, #expected dividends of the underlying
            vola = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity))
            BarrierPerc = NA)
{
  Symbol <- Underlying

  Underlying <- getSymbols(Symbol, auto.assign = F)[,1:4]

  IssueDate <- as.Date(IssueDate) # In case of a specific character input change type to DateTime object

  while(identical(as.numeric(Underlying[IssueDate, 4]), numeric(0)) == TRUE){ #If the time series contains no data at issue date,
    IssueDate <- IssueDate  %m-% days(1)                                      # take next day
  }

  #Strike <- StrikePerc * as.numeric(Underlying[IssueDate, 4])

  BarrierLevel <- BarrierPerc * as.numeric(Underlying[IssueDate, 4])

  if(!is.na(CapPerc)){
    Cap <- CapPerc * as.numeric(Underlying[IssueDate, 4])
  }
  else {Cap <- NA}

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  while(identical(as.numeric(Underlying[MaturityDate,4]),numeric(0)) == TRUE){ #if time series contains no price at maturity date (Weekend)
    MaturityDate <- MaturityDate  %m-% days(1)                                  # set maturity date to next day until time series contains a price
  }

  if (is.na(risk_free_rate)){
    riskFreeRates <- na.approx(getSymbols("^IRX",auto.assign = F))
    risk_free_rate <- as.numeric(riskFreeRates[IssueDate,4]) / 100
  }

  if (is.na(expected_dividends)){
    expected_dividends <- getDividends(Symbol = Symbol, to = IssueDate)
    expected_dividends_y <- apply.yearly(expected_dividends,sum)
    expected_dividends <- round((as.numeric(expected_dividends_y[(
      ifelse(identical(which(year(expected_dividends_y) == year(IssueDate)),integer(0)),
             which(year(expected_dividends_y) == year(IssueDate)-1),
             which(year(expected_dividends_y) == year(IssueDate))))])) / Underlying[IssueDate,4],4)
  }

  if (is.na(vola))
  {
    vola <- get_vola(Underlying,0.95,valuation_date = IssueDate) # = Volatility of the Underlying asset
  }

  LEPO_price <- option_valuation(S = Underlying,
                                 K = 0.01,
                                 r = risk_free_rate,
                                 q = expected_dividends,
                                 sig = vola,
                                 maturity_date = MaturityDate,
                                 valuation_date = IssueDate,
                                 type = "C")

  # option_price <- barrier_option_valuation(S = Underlying,
  #                                          K = Strike,
  #                                          r = risk_free_rate,
  #                                          b = 0,
  #                                          rebate = 0,
  #                                          barrier = BarrierLevel,
  #                                          sig = vola,
  #                                          maturity_date = MaturityDate,
  #                                          val_date = IssueDate,
  #                                          type = "pdo")

  if (!(is.na(Cap))){
    cap_price <- option_valuation(S = Underlying,
                                  K = Cap,
                                  r = risk_free_rate,
                                  q = expected_dividends,
                                  sig = vola,
                                  maturity_date = MaturityDate,
                                  valuation_date = IssueDate,
                                  type = "C")

    #numb_opt <- Nominal / (LEPO_price + option_price - cap_price)
  }
  # else{
  #   numb_opt <- Nominal / (LEPO_price + option_price)
  # }
  #with current strike, this participation factor is possible
  numb <- (Nominal / as.numeric(Underlying[IssueDate, 4]))
  #Participation <- numb_opt / numb
  #Participation_theory <- Participation

  # what strike is needed for a participation factor of 1?
  available_amount <- Nominal - (numb * LEPO_price)
  init_price <- as.numeric(Underlying[IssueDate, 4])
  strikes <- seq(1.2 * init_price,(0.2 * init_price),-0.1)
  for (i in 1:length(strikes)){
    max_strike <- -1
    option_price <- barrier_option_valuation(S = Underlying,
                                             K = strikes[i],
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = BarrierLevel,
                                             sig = vola,
                                             maturity_date = MaturityDate,
                                             val_date = IssueDate,
                                             type = "pdo")

    if (!(is.na(Cap))) numb_opt <- Nominal / (LEPO_price + option_price - cap_price)
    else numb_opt <- Nominal / (LEPO_price + option_price)

    if (numb_opt >= numb){
      numb_opt <- numb
      max_strike <- strikes[i]
      break
    }
  }
  # else{
  #   Participation <- 1
  #   numb_opt <- numb_opt
  #   max_strike <- Strike
  # }

  TermSheet <- data.frame(NA)
  TermSheet$Issuer <- "Sample Bank"
  TermSheet$Rating <- "AA-"
  TermSheet$RiskFreeRate <- as.character(risk_free_rate)
  TermSheet$Underlying <- as.character(Symbol)
  TermSheet$Dividend <- as.character(expected_dividends)
  TermSheet$IssueDate <- as.character(IssueDate)
  TermSheet$Maturity <- Maturity
  TermSheet$MaturityDate <- as.character(MaturityDate)
  TermSheet$Nominal <- as.character(Nominal)
  TermSheet$Strike <- as.character(round(max_strike,4))
  TermSheet$UnderlyingPrice <- as.character(as.numeric(Underlying[IssueDate, 4]))
  TermSheet$Cap <- as.character(Cap)
  TermSheet$Currency <- "USD"
  if (is.na(Cap)){
    TermSheet$Components <- I(list(c("LEPO", "Long BDO Put")))
  }
  else TermSheet$Components <- I(list(c("LEPO", "Long BDO Put", "Short Call")))
  TermSheet$Volatility <- as.character(vola)
  TermSheet$BarrierLevel <- as.character(BarrierLevel)
  TermSheet$LEPOS <- as.character(round(numb,4))
  TermSheet$Options <- as.character(round(numb_opt,4))
  TermSheet <- TermSheet[,-1]
  TermSheet <- t(TermSheet)
  TermSheet <- as.data.frame(TermSheet)
  colnames(TermSheet) <- if(is.na(Cap)){"Bonus Certificate"} else {"Bonus Certificate with Cap"}
  return(TermSheet)
}


# Bonus Certificate---------
BonusCertificate  <- function(Nominal,
                              Underlying,
                              IssueDate,
                              Maturity,
                              #StrikePerc = 1,
                              CapPerc = NA,
                              BarrierPerc = 0.8,
                              risk_free_rate = NA,
                              expected_dividends = NA,
                              vola = NA){

  IssueDate <- as.Date(IssueDate)

  if(BarrierPerc > 1) warning("BarrierPerc has to be <1 because down and Out Option")

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  contractOne <- BonusCertificateTerm(Nominal = Nominal,
                                      Underlying = Underlying,
                                      IssueDate = IssueDate,
                                      Maturity = Maturity,
                                      CapPerc = CapPerc,
                                      risk_free_rate = risk_free_rate,
                                      expected_dividends = expected_dividends,
                                      vola = vola,
                                      BarrierPerc = BarrierPerc)#LEPO long down and out put

  colnames(contractOne) <- "Bonus Certificate"

  contractTwo <- TrackerCertificate(Nominal = Nominal,
                                    Underlying = Underlying,
                                    IssueDate = IssueDate,
                                    Maturity = Maturity,
                                    risk_free_rate = risk_free_rate,
                                    expected_dividends = expected_dividends,
                                    vola_at_issue = vola,
                                    CapPerc = CapPerc,
                                    Strike = contractOne["Strike",])

  colnames(contractTwo) <- "Bonus Certificate"

  out <- barrier_contract(risk_factor = Underlying,
                          contract_1 = contractOne,
                          contract_2 = contractTwo,
                          barrier_level = BarrierPerc,
                          barrier_type = "Out",
                          IssueDate = IssueDate,
                          MaturityDate = MaturityDate)

  return(out)

}
# Capital Protection with Barrier------------
CapitalProtectionKnockOut <- function(Nominal,
                                      Underlying,
                                      IssueDate,
                                      Maturity,
                                      ProtectionFactor,
                                      StrikePerc = 1,
                                      BarrierPerc = 1.2,
                                      risk_free_rate = NA,
                                      expected_dividends = NA,
                                      vola_at_issue = NA){
  IssueDate <- as.Date(IssueDate)

  if(BarrierPerc < 1) warning("BarrierPerc has to be >1 because Up and Out Option")

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  contractOne <- CapitalProtection(Nominal = Nominal,
                                   Underlying = Underlying,
                                   IssueDate = IssueDate,
                                   Maturity = Maturity,
                                   ProtectionFactor = ProtectionFactor,
                                   StrikePerc = StrikePerc,
                                   CapPerc = NA,
                                   risk_free_rate = risk_free_rate,
                                   expected_dividends = expected_dividends,
                                   vola_at_issue = vola_at_issue,
                                   barrier = T,
                                   BarrierPerc = BarrierPerc)

  colnames(contractOne) <- "Capital Protection with Knock Out"

  contractTwo <- ZeroBond(Nominal = Nominal,
                          Underlying = Underlying,
                          IssueDate = IssueDate,
                          Maturity = Maturity,
                          ProtectionFactor = ProtectionFactor,
                          risk_free_rate = risk_free_rate,
                          expected_dividends = expected_dividends,
                          vola_at_issue = vola_at_issue,
                          StrikePerc = StrikePerc,
                          BarrierPerc = BarrierPerc,
                          type = "CPKO")

  colnames(contractTwo) <- "Capital Protection with Knock Out"


  out <- barrier_contract(risk_factor = Underlying, #think about the case of the price
                          contract_1 = contractOne,
                          contract_2 = contractTwo,
                          barrier_level = BarrierPerc,
                          barrier_type = "Out",
                          IssueDate = IssueDate,
                          MaturityDate = MaturityDate)

  return(out)

}

# Discount Certificate with Knock Out-------
DiscountCertificateWithKnockOut  <- function(Nominal,
                                             Underlying,
                                             IssueDate,
                                             Maturity,
                                             StrikePerc = 1,
                                             BarrierPerc = 0.8,
                                             risk_free_rate = NA,
                                             expected_dividends = NA,
                                             vola_at_issue = NA){
  IssueDate <- as.Date(IssueDate)

  if(BarrierPerc > 1) warning("BarrierPerc has to be <1 because down and Out Option")

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  contractOne <- BarrierDiscountCertificate(Nominal = Nominal,
                                            Underlying = Underlying,
                                            IssueDate = IssueDate,
                                            Maturity = Maturity,
                                            StrikePerc = StrikePerc,
                                            BarrierPerc = BarrierPerc,
                                            risk_free_rate = risk_free_rate,
                                            expected_dividends = expected_dividends,
                                            vola_at_issue = vola_at_issue)

  colnames(contractOne) <- "Barrier Discount Certificate"

  contractTwo <- DiscountCertificate(Nominal = Nominal,
                                     Underlying = Underlying,
                                     IssueDate = IssueDate,
                                     Maturity = Maturity,
                                     StrikePerc = StrikePerc,
                                     risk_free_rate = risk_free_rate,
                                     expected_dividends = expected_dividends,
                                     vola_at_issue = vola_at_issue,
                                     barrier = T, BarrierPerc = BarrierPerc)

  colnames(contractTwo) <- "Barrier Discount Certificate"

  out <- barrier_contract(risk_factor = Underlying, #think about the case of the price
                          contract_1 = contractOne,
                          contract_2 = contractTwo,
                          barrier_level = BarrierPerc,
                          barrier_type = "Out",
                          IssueDate = IssueDate,
                          MaturityDate = MaturityDate)

  return(out)

}

# Barrier Reverse Convertible---------
BarrierReverseConvertible  <- function(Nominal,
                                       Underlying,
                                       IssueDate,
                                       Maturity,
                                       StrikePerc = 1,
                                       BarrierPerc = 0.8,
                                       risk_free_rate = NA,
                                       expected_dividends = NA,
                                       vola_at_issue = NA){
  IssueDate <- as.Date(IssueDate)

  if(BarrierPerc > 1) warning("BarrierPerc has to be <1 because down and Out Option")

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))

  contractTwo <- ReverseConvertible(Nominal = Nominal,
                                    Underlying = Underlying,
                                    IssueDate = IssueDate,
                                    Maturity = Maturity,
                                    StrikePerc = StrikePerc,
                                    risk_free_rate = risk_free_rate,
                                    expected_dividends = expected_dividends,
                                    vola_at_issue = vola_at_issue,
                                    barrier = T,
                                    BarrierPerc = BarrierPerc)

  colnames(contractTwo) <- "Barrier Reverse Convertible"

  contractOne <- ZeroBond(Nominal = Nominal,
                          Underlying = Underlying,
                          IssueDate = IssueDate,
                          Maturity = Maturity,
                          ProtectionFactor = 1,
                          risk_free_rate = risk_free_rate,
                          StrikePerc = StrikePerc,
                          expected_dividends = expected_dividends,
                          vola_at_issue = vola_at_issue,
                          BarrierPerc = BarrierPerc,
                          type = "BRC")

  colnames(contractOne) <- "Barrier Reverse Convertible"

  out <- barrier_contract(risk_factor = Underlying, #think about the case of the price
                          contract_1 = contractOne,
                          contract_2 = contractTwo,
                          barrier_level = BarrierPerc,
                          barrier_type = "Out",
                          IssueDate = IssueDate,
                          MaturityDate = MaturityDate)

  return(out)

}
# Warrant with Knock-Out------
WarrantWithKnockOut <- function(Nominal, #Nominal = Invested Ammount
                                Underlying, #Underlying = Character (symbol of underlying)
                                IssueDate = Sys.Date(), #Issue date of the product default today otherwise character in %yyyy%-%mm%-%dd% format
                                Maturity = "1 year", #Maturity = lifetime of the certificate in years!
                                StrikePerc = 1,
                                type,  #Call or Put
                                BarrierPerc,
                                risk_free_rate = NA, #current risk free interest rate (for black scholes)
                                expected_dividends = NA, #expected dividends of the underlying
                                vola_at_issue = NA){
  IssueDate <- as.Date(IssueDate)

  if(BarrierPerc < 1 & type == "Put") warning("BarrierPerc has to be >1 because up and Out Option")
  if(BarrierPerc > 1 & type == "Call") warning("BarrierPerc has to be <1 because down and Out Option")

  MaturityDate <- IssueDate %m+% years(as.numeric(gsub("([0-9]+).*$", "\\1", Maturity )))
  #if not breached
  contractOne <- Warrant(Nominal = Nominal,
                         Underlying = Underlying,
                         IssueDate = IssueDate,
                         Maturity = Maturity,
                         StrikePerc = StrikePerc,
                         type = type,
                         risk_free_rate = risk_free_rate,
                         expected_dividends = expected_dividends,
                         vola_at_issue = vola_at_issue,
                         barrier = T,
                         BarrierPerc = BarrierPerc)
  IssueDate <- as.Date(as.character(contractOne["IssueDate",]))
  MaturityDate <- as.Date(as.character(contractOne["MaturityDate",]))
  colnames(contractOne) <- "Knock Out Warrant"
  #if Barrier is breached
  contractTwo <- ZeroBond(Nominal = Nominal,
                          Underlying = Underlying,
                          IssueDate = IssueDate,
                          Maturity = Maturity,
                          ProtectionFactor = 0,
                          risk_free_rate = risk_free_rate,
                          expected_dividends = expected_dividends,
                          vola_at_issue = vola_at_issue,
                          StrikePerc = StrikePerc,
                          BarrierPerc = BarrierPerc,
                          numb = as.numeric(contractOne["Options",])
                          )

  colnames(contractTwo) <- "Knock Out Warrant"

  out <- barrier_contract(risk_factor = Underlying, #think about the case of the price
                          contract_1 = contractOne,
                          contract_2 = contractTwo,
                          barrier_level = BarrierPerc,
                          barrier_type = "Out",
                          IssueDate = IssueDate,
                          MaturityDate = MaturityDate)

  return(out)

}


