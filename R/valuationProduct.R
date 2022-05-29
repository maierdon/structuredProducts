# valuation function that gives every product a value on every day

# Idea: Build a function that takes the termsheet of the structured product
# and the valuation date as an input and the output the value at this day
# optional inputs as riskfactors: underlying, risk-free-rate, expected dividends
# volatility

# split the product in single parts and value the parts separately before
# adding it
# bond valuation is already implemented in FEMS - however cannot be used
# --> new file valuation_bond_self.R

product_valuation <- function(product_termsheet, #termsheet of the product that needs to be evaluated
                              underlying = NA, #xts of respective underlying (open, low, high, close)
                              risk_free_rate = NA, #current risk free interest rate (for black scholes)
                              expected_dividends = NA, #expected dividends of the underlying
                              vola = NA, #optional, if not set a proxy for the historical volatility is taken (and normalized to maturity)
                              valuation_date) #date on which the product should be valued
  {

  valuation_date <- as.Date(valuation_date)
  # if (is.na(underlying)){
  #   underlying <- getSymbols(as.character(product_termsheet["Underlying",]), auto.assign = F)[,1:4]
  # }
  if (is.na(risk_free_rate)){
    risk_free_rate <- as.numeric(product_termsheet["RiskFreeRate",])
  }
  if (is.na(expected_dividends)){
    expected_dividends <- as.numeric(product_termsheet["Dividend",])
  }
  if (is.na(vola))
  {
    vola <- as.numeric(product_termsheet["Volatility",]) # = Volatility of the Underlying asset
  }

  Nominal <- as.numeric(product_termsheet["Nominal",])

  #outperformance certificate-------
  if (colnames(product_termsheet) == "Outperformance Certificate")
  {
    #as the allocation ration is 0.1 (10:1), the participation factor is transformed that it can be used later
    #participation_factor <- (as.numeric(product_termsheet["ParticipationFactor",]) - 1)
    LEPO <- option_valuation(S = underlying,
                             K = 0.01,
                             r = risk_free_rate,
                             q = expected_dividends,
                             sig = vola,
                             maturity_date = product_termsheet["MaturityDate",],
                             valuation_date = valuation_date,
                             type = "C")
    participation_call <- option_valuation(S = underlying,
                             K = as.numeric(product_termsheet["Strike",]),
                             r = risk_free_rate,
                             q = expected_dividends,
                             sig = vola,
                             maturity_date = product_termsheet["MaturityDate",],
                             valuation_date = valuation_date,
                             type = "C") # * participation_factor
    amount <- LEPO * as.numeric(product_termsheet["LEPOS",])
    amount1 <- participation_call * as.numeric(product_termsheet["Options",])

    if (!(is.na(product_termsheet["Cap",]))){
      cap_limit_perf <- option_valuation(S = underlying,
                                         K = as.numeric(product_termsheet["Cap",]),
                                         r = risk_free_rate,
                                         q = expected_dividends,
                                         sig = vola,
                                         maturity_date = product_termsheet["MaturityDate",],
                                         valuation_date = valuation_date,
                                         type = "C") # * (participation_factor + 1)
      amount2 <- cap_limit_perf * (as.numeric(product_termsheet["Options",]) + as.numeric(product_termsheet["LEPOS",]))
      product_price <- (amount + amount1 - amount2)
      #normalized_product_price <- (LEPO + participation_call - cap_limit_perf) / as.numeric(underlying[as.Date(product_termsheet["IssueDate",]),4]) * 100
    }
    else product_price <- (amount + amount1)
    #else normalized_product_price <- (LEPO + participation_call) / as.numeric(underlying[as.Date(product_termsheet["IssueDate",]),4]) * 100
    return(product_price)
  }

  #capital protection certificate------
  if (colnames(product_termsheet) == "Capital Protection Certificate with Participation")
  {

    bond_part <- bond_valuation(nominal = as.numeric(product_termsheet["BondComponent",]),
                                interest_rate = as.numeric(product_termsheet["RiskFreeRate",]) + 0.04, #alternativ riskfreerates nehmen? mit getsymbols ^IRX an valuation date
                                maturity_date = as.character(product_termsheet["MaturityDate",]),
                                valuation_date = valuation_date)
    #participation_factor <- (as.numeric(product_termsheet["ParticipationFactor",]))

    participation_call <- option_valuation(S = underlying,
                                           K = as.numeric(product_termsheet["Strike",]),
                                           r = risk_free_rate,
                                           q = expected_dividends,
                                           sig = vola,
                                           maturity_date = product_termsheet["MaturityDate",],
                                           valuation_date = valuation_date,
                                           type = "C")

    amount1 <- participation_call * as.numeric(product_termsheet["Options",])
    # number_of_options <- (as.numeric(product_termsheet["BondComponent",]) - bond_part) / participation_call
    # resulting_participation_ratio <- number_of_options / (product_termsheet["BondComponent",] / participation_call)
    if (!(is.na(product_termsheet["Cap",]))){
      cap_limit_perf <- option_valuation(S = underlying,
                                         K = as.numeric(product_termsheet["Cap",]),
                                         r = risk_free_rate,
                                         q = expected_dividends,
                                         sig = vola,
                                         maturity_date = as.character(product_termsheet["MaturityDate",]),
                                         valuation_date = valuation_date,
                                         type = "C")
      amount2 <- cap_limit_perf * as.numeric(product_termsheet["Options",])
      product_price <- bond_part + amount1 - amount2
    }
    else product_price <- bond_part + amount1
    return(product_price)
  }

  #discount certificate--------
  if (colnames(product_termsheet) == "Discount Certificate")
  {
    LEPO <- option_valuation(S = underlying,
                             K = 0.01,
                             r = risk_free_rate,
                             q = expected_dividends,
                             sig = vola,
                             maturity_date = product_termsheet["MaturityDate",],
                             valuation_date = valuation_date,
                             type = "C")
    short_call <- option_valuation(S = underlying,
                                   K = as.numeric(product_termsheet["Strike",]),
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola,
                                   maturity_date = product_termsheet["MaturityDate",],
                                   valuation_date = valuation_date,
                                   type = "C")
    product_price <- as.numeric(product_termsheet["Options",]) * (LEPO - short_call)

    #maybe here just an if statement for the barrier, depends how we do it with the barrier

    return(product_price)
  }

  #barrier discount certificate
  if (colnames(product_termsheet) == "Discount Certificate with Knock Out")
  {
    LEPO <- option_valuation(S = underlying,
                             K = 0.01,
                             r = risk_free_rate,
                             q = expected_dividends,
                             sig = vola,
                             maturity_date = product_termsheet["MaturityDate",],
                             valuation_date = valuation_date,
                             type = "C")
    short_call <- option_valuation(S = underlying,
                                   K = as.numeric(product_termsheet["Strike",]),
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola,
                                   maturity_date = product_termsheet["MaturityDate",],
                                   valuation_date = valuation_date,
                                   type = "C")
    if ("BarrierEventDate" %in% rownames(product_termsheet)){
      EventDate <- as.Date(as.character(product_termsheet["BarrierEventDate",]))
      if (valuation_date < EventDate){
        long_put_b <- barrier_option_valuation(S = underlying,
                                               K = as.numeric(product_termsheet["Strike",]),
                                               r = risk_free_rate,
                                               b = 0,
                                               rebate = 0,
                                               barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                               sig = vola,
                                               maturity_date = product_termsheet["MaturityDate",],
                                               val_date = valuation_date,
                                               type = "pdo")

        product_price <- as.numeric(product_termsheet["Options",]) * (LEPO - short_call + long_put_b)
      }
      else product_price <- as.numeric(product_termsheet["Options",]) * (LEPO - short_call)
    }
    else{
      long_put_b <- barrier_option_valuation(S = underlying,
                                             K = as.numeric(product_termsheet["Strike",]),
                                             r = risk_free_rate,
                                             b = 0,
                                             rebate = 0,
                                             barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                             sig = vola,
                                             maturity_date = product_termsheet["MaturityDate",],
                                             val_date = valuation_date,
                                             type = "pdo")
      product_price <- as.numeric(product_termsheet["Options",]) * (LEPO - short_call + long_put_b)
    }
    return(product_price)
  }

  #reverse convertible
  if (colnames(product_termsheet) == "Reverse Convertible")
  {
    bond_part <- bond_valuation(nominal = product_termsheet["Nominal",],
                                interest_rate = as.numeric(product_termsheet["RiskFreeRate",]) + 0.04,
                                maturity_date = product_termsheet["MaturityDate",],
                                valuation_date = valuation_date)

    short_put <- option_valuation(S = underlying,
                                  K = as.numeric(product_termsheet["Strike",]),
                                  r = risk_free_rate,
                                  q = expected_dividends,
                                  sig = vola,
                                  maturity_date = product_termsheet["MaturityDate",],
                                  valuation_date = valuation_date,
                                  type = "P")

    short_put_issue <- option_valuation(S = underlying,
                                  K = as.numeric(product_termsheet["Strike",]),
                                  r = risk_free_rate,
                                  q = expected_dividends,
                                  sig = vola,
                                  maturity_date = product_termsheet["MaturityDate",],
                                  valuation_date = as.character(product_termsheet["IssueDate",]),
                                  type = "P")

    product_price <- bond_part - (as.numeric(product_termsheet["Options",]) * (short_put - short_put_issue))

    return(product_price)
  }

  #tracker certificate
  if (colnames(product_termsheet) == "Tracker Certificate")
  {
    LEPO <- option_valuation(S = underlying,
                             K = 0.01,
                             r = risk_free_rate,
                             q = expected_dividends,
                             sig = vola,
                             maturity_date = product_termsheet["MaturityDate",],
                             valuation_date = valuation_date,
                             type = "C")
    total <- LEPO * as.numeric(product_termsheet["LEPOS",])
    return(total)
  }

  #warrant
  if (colnames(product_termsheet) == "Warrant")
  {
    if (product_termsheet["Type",] == "Call")
    {
      participation_call <- option_valuation(S = underlying,
                                             K = as.numeric(product_termsheet["Strike",]),
                                             r = risk_free_rate,
                                             q = expected_dividends,
                                             sig = vola,
                                             maturity_date = product_termsheet["MaturityDate",],
                                             valuation_date = valuation_date,
                                             type = "C")
      return(participation_call)
    }
    else
    {
      participation_put <- option_valuation(S = underlying,
                                            K = as.numeric(product_termsheet["Strike",]),
                                            r = risk_free_rate,
                                            q = expected_dividends,
                                            sig = vola,
                                            maturity_date = product_termsheet["MaturityDate",],
                                            valuation_date = valuation_date,
                                            type = "P")
      return(participation_put)
    }
  }

  #Spread Warrant
  if (colnames(product_termsheet) == "Spread Warrant")
  {
    if (product_termsheet["Type",] == "Bull")
    {
      participation_call <- option_valuation(S = underlying,
                                             K = as.numeric(product_termsheet["Strike",]),
                                             r = risk_free_rate,
                                             q = expected_dividends,
                                             sig = vola,
                                             maturity_date = product_termsheet["MaturityDate",],
                                             valuation_date = valuation_date,
                                             type = "C")

      cap_part <- option_valuation(S = underlying,
                                   K = as.numeric(product_termsheet["Cap",]),
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola,
                                   maturity_date = product_termsheet["MaturityDate",],
                                   valuation_date = valuation_date,
                                   type = "C")
      product_price <- participation_call - cap_part
    }

    else
    {
      participation_put <- option_valuation(S = underlying,
                                            K = as.numeric(product_termsheet["Strike",]),
                                            r = risk_free_rate,
                                            q = expected_dividends,
                                            sig = vola,
                                            maturity_date = product_termsheet["MaturityDate",],
                                            valuation_date = valuation_date,
                                            type = "P")

      cap_part <- option_valuation(S = underlying,
                                   K = as.numeric(product_termsheet["Cap",]),
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola,
                                   maturity_date = product_termsheet["MaturityDate",],
                                   valuation_date = valuation_date,
                                   type = "P")

      product_price <- participation_put - cap_part
    }

    return(product_price)
  }

  # Capital Protection with Barrier
  if (colnames(product_termsheet) == "Capital Protection with Knock Out")
  {
    bond_part <- bond_valuation(nominal = as.numeric(product_termsheet["BondComponent",]),
                                interest_rate = as.numeric(product_termsheet["RiskFreeRate",]) + 0.04, #alternativ riskfreerates nehmen? mit getsymbols ^IRX an valuation date
                                maturity_date = as.character(product_termsheet["MaturityDate",]),
                                valuation_date = valuation_date)
    Strike <- as.numeric(product_termsheet["Strike",])
    if ("BarrierEventDate" %in% rownames(product_termsheet)){
      EventDate <- as.Date(as.character(product_termsheet["BarrierEventDate",]))
      if (valuation_date < EventDate){
        participation_call <- barrier_option_valuation(S = underlying,
                                                       K = Strike,
                                                       r = risk_free_rate,
                                                       b = 0,
                                                       rebate = 0,
                                                       barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                       sig = vola,
                                                       maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                       val_date = valuation_date,
                                                       type = "cuo")
        amount1 <- participation_call * as.numeric(product_termsheet["Options",])
        product_price <- bond_part + amount1
        return(product_price)
      }
      else return(bond_part)
    }
    else{
      participation_call <- barrier_option_valuation(S = underlying,
                                                     K = Strike,
                                                     r = risk_free_rate,
                                                     b = 0,
                                                     rebate = 0,
                                                     barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                     sig = vola,
                                                     maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                     val_date = valuation_date,
                                                     type = "cuo")
      amount1 <- participation_call * as.numeric(product_termsheet["Options",])
      product_price <- bond_part + amount1
      return(product_price)
    }
  }

  # Barrier Reverse Convertible
  if (colnames(product_termsheet) == "Barrier Reverse Convertible")
  {
    bond_part <- bond_valuation(nominal = product_termsheet["Nominal",],
                                interest_rate = as.numeric(product_termsheet["RiskFreeRate",]) + 0.04,
                                maturity_date = product_termsheet["MaturityDate",],
                                valuation_date = valuation_date)
    Strike <- as.numeric(product_termsheet["Strike",])
    short_put_issue <- barrier_option_valuation(S = underlying,
                                                K = Strike,
                                                r = risk_free_rate,
                                                b = 0,
                                                rebate = 0,
                                                barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                sig = vola,
                                                maturity_date = product_termsheet["MaturityDate",],
                                                val_date = valuation_date,
                                                type = "pdi")
    if ("BarrierEventDate" %in% rownames(product_termsheet)){
      EventDate <- as.Date(as.character(product_termsheet["BarrierEventDate",]))
      if (valuation_date < EventDate){
        short_put <- barrier_option_valuation(S = underlying,
                                              K = Strike,
                                              r = risk_free_rate,
                                              b = 0,
                                              rebate = 0,
                                              barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                              sig = vola,
                                              maturity_date = product_termsheet["MaturityDate",],
                                              val_date = valuation_date,
                                              type = "pdi")
        product_price <- bond_part - (as.numeric(product_termsheet["Options",]) * short_put)#- short_put_issue))
        return(product_price)
      }
      else{
        short_put <- option_valuation(S = underlying,
                                      K = Strike,
                                      r = risk_free_rate,
                                      q = expected_dividends,
                                      sig = vola,
                                      maturity_date = product_termsheet["MaturityDate",],
                                      valuation_date = valuation_date,
                                      type = "P")
        product_price <- bond_part - (as.numeric(product_termsheet["Options",]) * short_put)# - short_put_issue))
        return(product_price)
      }
    }
    else{
      short_put <- barrier_option_valuation(S = underlying,
                                            K = Strike,
                                            r = risk_free_rate,
                                            b = 0,
                                            rebate = 0,
                                            barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                            sig = vola,
                                            maturity_date = product_termsheet["MaturityDate",],
                                            val_date = valuation_date,
                                            type = "pdi")
      product_price <- bond_part - (as.numeric(product_termsheet["Options",]) * short_put)# - short_put_issue))
      return(product_price)
    }
  }

  # Bonus Certificate
  if (colnames(product_termsheet) == "Bonus Certificate")
  {
    LEPO_price <- option_valuation(S = underlying,
                                   K = 0.01,
                                   r = risk_free_rate,
                                   q = expected_dividends,
                                   sig = vola,
                                   maturity_date = product_termsheet["MaturityDate",],
                                   valuation_date = valuation_date,
                                   type = "C")
    if (!(is.na(product_termsheet["Cap",]))){
      cap_price <- option_valuation(S = underlying,
                                    K = as.numeric(product_termsheet["Cap",]),
                                    r = risk_free_rate,
                                    q = expected_dividends,
                                    sig = vola,
                                    maturity_date = product_termsheet["MaturityDate",],
                                    valuation_date = valuation_date,
                                    type = "C")
    }
    if ("BarrierEventDate" %in% rownames(product_termsheet)){
      EventDate <- as.Date(as.character(product_termsheet["BarrierEventDate",]))
      if (valuation_date < EventDate){
        option_price <- barrier_option_valuation(S = underlying,
                                                 K = as.numeric(product_termsheet["Strike",]),
                                                 r = risk_free_rate,
                                                 b = 0,
                                                 rebate = 0,
                                                 barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                 sig = vola,
                                                 maturity_date = product_termsheet["MaturityDate",],
                                                 val_date = valuation_date,
                                                 type = "pdo")
        if (!(is.na(product_termsheet["Cap",]))) total <- (LEPO_price * as.numeric(product_termsheet["LEPOS",])) +
                                                          (option_price * as.numeric(product_termsheet["Options",])) -
                                                          (as.numeric(product_termsheet["LEPOS",])  * cap_price)

        else total <- (LEPO_price * as.numeric(product_termsheet["LEPOS",])) + (option_price * as.numeric(product_termsheet["Options",]))
      }
      else{
        if (!(is.na(product_termsheet["Cap",]))) total <- (LEPO_price * as.numeric(product_termsheet["LEPOS",])) -
                                                          (as.numeric(product_termsheet["LEPOS",]) * cap_price)

        else total <- LEPO_price * as.numeric(product_termsheet["LEPOS",])
      }
    }
    else{
      option_price <- barrier_option_valuation(S = underlying,
                                               K = as.numeric(product_termsheet["Strike",]),
                                               r = risk_free_rate,
                                               b = 0,
                                               rebate = 0,
                                               barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                               sig = vola,
                                               maturity_date = product_termsheet["MaturityDate",],
                                               val_date = valuation_date,
                                               type = "pdo")
      if (!(is.na(product_termsheet["Cap",]))) total <- (LEPO_price * as.numeric(product_termsheet["LEPOS",])) +
                                                        (option_price * as.numeric(product_termsheet["Options",])) -
                                                        (as.numeric(product_termsheet["LEPOS",]) * cap_price)
      else total <- (LEPO_price * as.numeric(product_termsheet["LEPOS",])) + (option_price * as.numeric(product_termsheet["Options",]))
    }
    return(total)
  }

  # Warrant with knock-out
  if (colnames(product_termsheet) == "Warrant with Knock Out")
  {
    Strike <- as.numeric(product_termsheet["Strike",])

    if ("BarrierEventDate" %in% rownames(product_termsheet)){

      EventDate <- as.Date(as.character(product_termsheet["BarrierEventDate",]))
      if (valuation_date < EventDate){
        if (as.numeric(product_termsheet["BarrierLevel",]) < as.numeric(product_termsheet["Strike",])){
          product_price <- barrier_option_valuation(S = underlying,
                                                    K = Strike,
                                                    r = risk_free_rate,
                                                    b = 0,
                                                    rebate = 0,
                                                    barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                    sig = vola,
                                                    maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                    val_date = valuation_date,
                                                    type = "cdo")

        }
        else{

          product_price <- barrier_option_valuation(S = underlying,
                                                    K = Strike,
                                                    r = risk_free_rate,
                                                    b = 0,
                                                    rebate = 0,
                                                    barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                    sig = vola,
                                                    maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                    val_date = valuation_date,
                                                    type = "puo")
        }
      }
      else product_price <- 0
    }

    else{

      if (as.numeric(product_termsheet["BarrierLevel",]) < as.numeric(product_termsheet["Strike",])){

        product_price <- barrier_option_valuation(S = underlying,
                                                  K = Strike,
                                                  r = risk_free_rate,
                                                  b = 0,
                                                  rebate = 0,
                                                  barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                  sig = vola,
                                                  maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                  val_date = valuation_date,
                                                  type = "cdo")
      }

      else{

        product_price <- barrier_option_valuation(S = underlying,
                                                  K = Strike,
                                                  r = risk_free_rate,
                                                  b = 0,
                                                  rebate = 0,
                                                  barrier = as.numeric(product_termsheet["BarrierLevel",]),
                                                  sig = vola,
                                                  maturity_date = as.character(product_termsheet["MaturityDate",]),
                                                  val_date = valuation_date,
                                                  type = "puo")
      }
    }

    amount <- product_price * as.numeric(product_termsheet["Options",])
    return(amount)

    }
}
