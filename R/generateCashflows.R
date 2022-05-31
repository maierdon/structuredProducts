generateCashflows <- function(portfolio , termsheet){
 # general variables--------
  cap <- FALSE

  product <- colnames(termsheet)

  serverURL <- "https://demo.actusfrf.org:8080/"

  Date_Term_Names <<- c(
    "statusDate","contractDealDate","initialExchangeDate",
    "maturityDate","cycleAnchorDateOfInterestCalculationBase",
    "amortizationDate","contractDealDate","cycleAnchorDateOfPrincipalRedemption",
    "arrayCycleAnchorDateOfPrincipalRedemption","purchaseDate",
    "terminationDate","cycleAnchorDateOfScalingIndex",
    "cycleAnchorDateOfRateReset","cycleAnchorDateOfInterestPayment",
    "capitalizationEndDate")

  if(!is.na(termsheet["Cap",])) cap <- TRUE
  # product discount certificate--------
  if(product == "Discount Certificate"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff + output[[2]][[1]]$Event_Payoff)

    output <- output[[1]]

    output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

    output <- as.data.frame(output)

    output <- output[order(output$Event_Date),]

    return(output)
  }
  # product CapitalProtection-------
  if(product == "Capital Protection Certificate with Participation"){

    events <- generateEvents(portfolio,serverURL = serverURL)

    output <- create_proper_table(events)

    if(cap){
      num_optns <- as.numeric(termsheet["Options",])

      output[[2]][[1]]$Event_Payoff <- output[[2]][[1]]$Event_Payoff + output[[3]][[1]]$Event_Payoff

      yield <- num_optns * sum(output[[2]][[1]]$Event_Payoff) + sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[2]][[1]]

      output$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- output[order(output$Event_Date),]

      return(output)
    }
    else{
      num_optns <- as.numeric(termsheet["Options",])

      yield  <- num_optns * sum(output[[2]][[1]]$Event_Payoff) + sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[2]][[1]]

      output$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- output[order(output$Event_Date),]

      return(output)

    }
  }
  # product Discount Certificate with Knock Out------
  if(product == "Discount Certificate with Knock Out"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if("BarrierEventDate" %in% rownames(termsheet)){
      #Cashflowpattern für aktivierte Barriere (normal DC)
      yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff + output[[2]][[1]]$Event_Payoff)+ (as.numeric(termsheet["Options",])*output[[3]][[1]]$Event_Payoff[1])

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
    else{
      #Cashflowpattern für BarrierDC #muss angeschaut werden!!!!
      yield <- (as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff + output[[2]][[1]]$Event_Payoff + output[[3]][[1]]$Event_Payoff))

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
  }
  # product Outperformance Certificate------
  if(product == "Outperformance Certificate"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if(is.na(termsheet["Cap",])){

      yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff) +
                                                       as.numeric(termsheet["Options",])*
                                                       sum(output[[2]][[1]]$Event_Payoff)

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
    else{
      #hier liegt der fehler es sind zuviele lepos und zuwenig short calls (participation reatio einbauen????)
      yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff + output[[3]][[1]]$Event_Payoff) +
                                                       as.numeric(termsheet["Options",])*
                                                       sum(output[[2]][[1]]$Event_Payoff +
                                                       output[[4]][[1]]$Event_Payoff)

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
  }
  # product Reverse Convertible ------
  if(product == "Reverse Convertible"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    yield <- (as.numeric(termsheet["Options",])*sum(output[[2]][[1]]$Event_Payoff))+sum(output[[1]][[1]]$Event_Payoff)

    output <- output[[2]]

    output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

    output <- as.data.frame(output)

    output <- output[order(output$Event_Date),]

    return(output)
  }
  # product tracker certificate--------
  if(product == "Tracker Certificate"){

    if(is.na(termsheet["Cap",])){
      events <- generateEvents(ptf = portfolio, serverURL = serverURL)

      output <- create_proper_table(events)


      yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]
    }
    else {
      events <- generateEvents(ptf = portfolio, serverURL = serverURL)

      output <- create_proper_table(events)


      yield <- as.numeric(termsheet["LEPOS",])*(sum(output[[1]][[1]]$Event_Payoff +
                                                  output[[2]][[1]]$Event_Payoff))

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]
    }

    return(output)
  }
  # product Warrant----------
  if(product == "Warrant"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)


    yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff)

    output <- output[[1]]

    output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),ifelse(as.numeric(termsheet["Nominal",])+yield < 0,0,as.numeric(termsheet["Nominal",])+yield))

    output <- as.data.frame(output)

    output <- output[order(output$Event_Date),]

    return(output)
  }
  # product ZeroBond -----
  if(product == "Zero Bond"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)


    #yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff)

    output <- output[[1]]

    output[[1]]$Event_Payoff[c(1,2)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",]))

    output <- as.data.frame(output)

    output <- output[order(output$Event_Date),]

    return(output)
  }
  # product Bonus Certificate----
  if(product == "Bonus Certificate"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if("BarrierEventDate" %in% rownames(termsheet)){
      #print(1)
      if(is.na(termsheet["Cap",])){
        #print(paste(output[[1]][[1]]),"output")
        yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff)
        #print(paste(yield,"yield"))
        output <- output[[1]]

        output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),
                                              as.numeric(termsheet["Nominal",])+yield)

        output <- as.data.frame(output)

        output <- output[order(output$Event_Date),]

        return(output)
      }
      else{
        yield <- as.numeric(termsheet["LEPOS",])*sum(output[[1]][[1]]$Event_Payoff +
                                                       output[[2]][[1]]$Event_Payoff) #funktioniert noch nicht da tracker noch kein Cap implementiert hat!!!!

        output <- output[[1]]

        output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),
                                              as.numeric(termsheet["Nominal",])+yield)

        output <- as.data.frame(output)

        output <- output[order(output$Event_Date),]

        return(output)
      }
    }
    else{

      if(is.na(termsheet["Cap",])){

        yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff +
                                                         output[[2]][[1]]$Event_Payoff)


          output <- output[[1]]

          output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),
                                                as.numeric(termsheet["Nominal",])+yield)

          output <- as.data.frame(output)

          output <- output[order(output$Event_Date),]

          return(output)
      }
      else{

        yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff +
                                                         output[[2]][[1]]$Event_Payoff +
                                                         output[[3]][[1]]$Event_Payoff)


        output <- output[[1]]

        output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),
                                              as.numeric(termsheet["Nominal",])+yield)

        output <- as.data.frame(output)

        output <- output[order(output$Event_Date),]

        return(output)
      }
    }
  }
  # product Capital Protection with Knock Out-------
  if(product == "Capital Protection with Knock Out"){

    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if("BarrierEventDate" %in% rownames(termsheet)){

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,2)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["ProtectedAmount",]))

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
    else{
      yield <- as.numeric(termsheet["Options",])*sum(output[[2]][[1]]$Event_Payoff) + sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[2]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
  }
  # product Barrier Reverse Convertible--------
  if(product == "Barrier Reverse Convertible"){

    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if("BarrierEventDate" %in% rownames(termsheet)){

      yield <- (as.numeric(termsheet["Options",])*sum(output[[2]][[1]]$Event_Payoff))+sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,2)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)

    }
    else{

      yield <- as.numeric(termsheet["Nominal",]) * as.numeric(termsheet["Coupon",])

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,2)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }

  }
  # product Warrant with Knock Out------
  if(product == "Warrant with Knock Out"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    if("BarrierEventDate" %in% rownames(termsheet)){

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,2)] <- c(-as.numeric(termsheet["Nominal",]),0)

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)

    }
    else{

      yield <- as.numeric(termsheet["Options",]) * sum(output[[1]][[1]]$Event_Payoff)

      output <- output[[1]]

      output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),ifelse(as.numeric(termsheet["Nominal",])+yield < 0,0,as.numeric(termsheet["Nominal",])+yield))

      output <- as.data.frame(output)

      output <- output[order(output$Event_Date),]

      return(output)
    }
  }
  # product spread warrant ----------
  if(product == "Spread Warrant"){
    events <- generateEvents(ptf = portfolio, serverURL = serverURL)

    output <- create_proper_table(events)

    yield <- as.numeric(termsheet["Options",])*sum(output[[1]][[1]]$Event_Payoff + output[[2]][[1]]$Event_Payoff)

    output <- output[[1]]

    output[[1]]$Event_Payoff[c(1,4)] <- c(-as.numeric(termsheet["Nominal",]),as.numeric(termsheet["Nominal",])+yield)

    output <- as.data.frame(output)

    output <- output[order(output$Event_Date),]

    return(output)

  }

}




