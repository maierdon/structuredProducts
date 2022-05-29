#create a function that returns the three things we need:
#termsheet at position 1
#cashflows at position 2
#chart at position 3
# rm(list= ls())
# projRoot <- getwd()
# source(paste(projRoot,"R","import_lib_func.R",sep = "/"))

testidea <- function(type,
                     Nominal,
                     Underlying,
                     IssueDate,
                     Maturity,
                     StrikePerc,
                     CapPerc = NA,
                     risk_free_rate,
                     expected_dividends,
                     vola_at_issue,
                     ProtectionFactor = NA, #optional
                     BarrierPerc = NA, #optional
                     optionType = NA, #optional
                     spreadWarrantType = NA) #optional
  {
  #Outperformance Certificate ----------------------------------
  if (type == "Outperformance Certificate"){

    out_termsheet <- OutperformanceCert(Nominal = Nominal,
                                    Underlying = Underlying,
                                    IssueDate = IssueDate,
                                    Maturity = Maturity,
                                    StrikePerc = StrikePerc,
                                    CapPerc = CapPerc,
                                    risk_free_rate = risk_free_rate,
                                    expected_dividends = expected_dividends,
                                    vola_at_issue = vola_at_issue)
  }
  #Capital Protection with Participation ---------------------
  else if (type == "Capital Protection with Participation"){
    out_termsheet <- CapitalProtection(Nominal = Nominal,
                                       Underlying = Underlying,
                                       IssueDate = IssueDate,
                                       Maturity = Maturity,
                                       ProtectionFactor = ProtectionFactor,
                                       StrikePerc = StrikePerc,
                                       CapPerc = CapPerc,
                                       risk_free_rate = risk_free_rate,
                                       expected_dividends = expected_dividends,
                                       vola_at_issue = vola_at_issue)
  }

  #Discount Certificate -------------------------------------
  else if (type == "Discount Certificate"){
    out_termsheet <- DiscountCertificate(Nominal = Nominal,
                                         Underlying = Underlying,
                                         IssueDate = IssueDate,
                                         Maturity = Maturity,
                                         StrikePerc = StrikePerc,
                                         risk_free_rate = risk_free_rate,
                                         expected_dividends = expected_dividends,
                                         vola_at_issue = vola_at_issue)
  }
  #Discount Certificate with Knock Out------------------
  else if (type == "Discount Certificate with Knock Out"){
    out_termsheet <- DiscountCertificateWithKnockOut(Nominal = Nominal,
                                                     Underlying = Underlying,
                                                     IssueDate = IssueDate,
                                                     Maturity = Maturity,
                                                     StrikePerc = StrikePerc,
                                                     BarrierPerc = BarrierPerc,
                                                     risk_free_rate = risk_free_rate,
                                                     expected_dividends = expected_dividends,
                                                     vola_at_issue = vola_at_issue)
  }
  #Reverse Convertible ---------------------------------
  else if (type == "Reverse Convertible"){
    out_termsheet <- ReverseConvertible(Nominal = Nominal,
                                        Underlying = Underlying,
                                        IssueDate = IssueDate,
                                        Maturity = Maturity,
                                        StrikePerc = StrikePerc,
                                        risk_free_rate = risk_free_rate,
                                        expected_dividends = expected_dividends,
                                        vola_at_issue = vola_at_issue)
  }
  #Tracker Certificate -------------------------------
  else if (type == "Tracker Certificate"){
    out_termsheet <- TrackerCertificate(Nominal = Nominal,
                                        Underlying = Underlying,
                                        IssueDate = IssueDate,
                                        Maturity = Maturity,
                                        risk_free_rate = risk_free_rate,
                                        expected_dividends = expected_dividends,
                                        vola_at_issue = vola_at_issue)
  }
  #Warrant --------------------------------
  else if (type == "Warrant"){
    out_termsheet <- Warrant(Nominal = Nominal,
                             Underlying = Underlying,
                             IssueDate = IssueDate,
                             Maturity = Maturity,
                             StrikePerc = StrikePerc,
                             type = optionType,
                             risk_free_rate = risk_free_rate,
                             expected_dividends = expected_dividends,
                             vola_at_issue = vola_at_issue)
  }
  #Spread Warrant ---------------------------------
  else if (type == "Spread Warrant"){
    out_termsheet <- SpreadWarrant(Nominal = Nominal,
                                   Underlying = Underlying,
                                   IssueDate = IssueDate,
                                   Maturity = Maturity,
                                   StrikePerc = StrikePerc,
                                   CapPerc = CapPerc,
                                   type = spreadWarrantType,
                                   risk_free_rate = risk_free_rate,
                                   expected_dividends = expected_dividends,
                                   vola_at_issue = vola_at_issue)
  }
  #Capital Protection with Knock Out -----------------------------
  else if (type == "Capital Protection with Knock Out"){
    out_termsheet <- CapitalProtectionKnockOut(Nominal = Nominal,
                                               Underlying = Underlying,
                                               IssueDate = IssueDate,
                                               Maturity = Maturity,
                                               ProtectionFactor = ProtectionFactor,
                                               StrikePerc = StrikePerc,
                                               BarrierPerc = BarrierPerc,
                                               risk_free_rate = risk_free_rate,
                                               expected_dividends = expected_dividends,
                                               vola_at_issue = vola_at_issue)
  }

  #Barrier Reverse Convertible ----------------------------------
  else if (type == "Barrier Reverse Convertible"){
    out_termsheet <- BarrierReverseConvertible(Nominal = Nominal,
                                               Underlying = Underlying,
                                               IssueDate = IssueDate,
                                               Maturity = Maturity,
                                               StrikePerc = StrikePerc,
                                               BarrierPerc = BarrierPerc,
                                               risk_free_rate = risk_free_rate,
                                               expected_dividends = expected_dividends,
                                               vola_at_issue = vola_at_issue)
  }
  #Bonus Certificate ----------------------------------
  else if (type == "Bonus Certificate"){
    out_termsheet <- BonusCertificate(Nominal = Nominal,
                                      Underlying = Underlying,
                                      IssueDate = IssueDate,
                                      Maturity = Maturity,
                                      CapPerc = CapPerc,
                                      BarrierPerc = BarrierPerc,
                                      risk_free_rate = risk_free_rate,
                                      expected_dividends = expected_dividends,
                                      vola = vola_at_issue)
  }
  #Warrant with Knock Out ---------------------------
  else if (type == "Warrant with Knock Out"){
    out_termsheet <- WarrantWithKnockOut(Nominal = Nominal,
                                         Underlying = Underlying,
                                         IssueDate = IssueDate,
                                         Maturity = Maturity,
                                         StrikePerc = StrikePerc,
                                         type = optionType,
                                         BarrierPerc = BarrierPerc,
                                         risk_free_rate = risk_free_rate,
                                         expected_dividends = expected_dividends,
                                         vola_at_issue = vola_at_issue)
  }
  #else -------------
  else stop(paste(type,"Please choose an available structured product."))
  #maybe return something that shiny can use to create no output at all...(- blank page)

  #cashflowcreation ----------------------
  ptf_1 <- generate_portfolio_multiple_options(termsheet = out_termsheet)
  out_cashflows <- generateCashflows(ptf_1,termsheet = out_termsheet)

  #valuation time series creation -------------------------
  out_ts <- valuationTotal(product_termsheet = out_termsheet)
  #print(out_ts)
  out_termsheet["Components",] <- paste(unlist(out_termsheet["Components",]),collapse = ", ")
  out_all <- list(out_termsheet,out_cashflows,out_ts)

  return(out_all)
}
# x <- testidea(type = "Barrier Reverse Convertible",
#          Nominal = 10000,
#          Underlying = "AAPL",
#          IssueDate = "2020-01-01",
#          Maturity = "1 years",
#          StrikePerc = 1,
#          BarrierPerc = 0.8,
#          risk_free_rate = 0.01,
#          expected_dividends = 0.02,
#          vola_at_issue = 0.015)

#rownames(x[[1]])
#x[[2]] #stimmen noch nicht ganz..
#plot(x[[3]][,2])
#lines(x[[3]][,1])
#einbauen in shiny..
#conditional inputs based on which product is chosen
#give the possibility to tick NA option (Cap eg.)
#decide which inputs start with a default value and which not
#could make it easier and we could erase some code (e.g. vola function)




