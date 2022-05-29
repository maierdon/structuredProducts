#####function that generates portfolio ----------------------

generate_portfolio_multiple_options <- function(termsheet){
  
  projRoot <- getwd()
  
  optionNames <- c("contractType" ,"contractID" , "contractRole", "contrStrucObj.marketObjectCode", "contrStruc.referenceType", 
                   "contrStruc.referenceRole", "currency", "calendar", "contractDealDate", "statusDate", "purchaseDate", 
                   "priceAtPurchaseDate", "maturityDate", "optionExerciseType", "optionType", "optionStrike1",                 
                   "settlementPeriod", "deliverySettlement", "description")
  
  option01 <- structure(c("OPTNS", "option01", "BUY", "AAPL", "MOC", "UDL", "USD", "NC",
                          "2020-01-01", "2020-01-01","2020-01-02","10","2020-03-30","E",
                          "C", "80", "P0D","S","European call option on underlying MOC AAPL
                        - actus-test-option.json option01;2020-03-30 maturity out of money"),
                        .Names = optionNames)
  
  bondNames <- c("calendar", "businessDayConvention", "endOfMonthConvention", "contractType", "statusDate", "contractRole",                             
                 "legalEntityIDRecordCreator", "contractID", "legalEntityIDCounterparty", "cycleAnchorDateOfInterestPayment",         
                 "cycleOfInterestPayment", "arrayCycleAnchorDateOfInterestPayment", "arrayCycleOfInterestPayment", "nominalInterestRate", 
                 "dayCountConvention", "accruedInterest", "capitalizationEndDate", "cycleAnchorDateOfInterestCalculationBase", 
                 "cycleOfInterestCalculationBase", "interestCalculationBase", "interestCalculationBaseAmount", "cyclePointOfInterestPayment",              
                 "currency", "amortizationDate", "contractDealDate", "initialExchangeDate", "premiumDiscountAtIED", "maturityDate",                             
                 "notionalPrincipal", "cycleAnchorDateOfPrincipalRedemption", "cycleOfPrincipalRedemption", "nextPrincipalRedemptionPayment",           
                 "arrayCycleAnchorDateOfPrincipalRedemption", "arrayCycleOfPrincipalRedemption", "arrayNextPrincipalRedemptionPayment", 
                 "arrayIncreaseDecrease", "purchaseDate", "priceAtPurchaseDate", "terminationDate", "priceAtTerminationDate",                   
                 "marketObjectCodeOfScalingIndex", "scalingIndexAtStatusDate", "cycleAnchorDateOfScalingIndex", "cycleOfScalingIndex",                      
                 "scalingEffect", "cycleAnchorDateOfRateReset", "cycleOfRateReset", "rateSpread", "arrayCycleAnchorDateOfRateReset", 
                 "arrayCycleOfRateReset", "arrayRate", "arrayFixedVariable", "marketObjectCodeOfRateReset", "cyclePointOfRateReset",                    
                 "fixingDays", "rateMultiplier", "description", "contrStrucObj.marketObjectCode", "contrStruc.referenceType", 
                 "contrStruc.referenceRole" )
  
  bondValues <- c("NOCALENDAR", "NULL", "SD", "PAM", "2015-01-01", "RPA", "NULL", "101",
                  "TEST_LEI_CP", "NULL", "NULL", "NULL", "NULL", "0.00", "30E360", "-999999999", "NULL",
                  "NULL", "NULL", "NULL", "NULL", "E", "EUR", "NULL", "2015-01-01", "2015-01-02", "0", "2015-04-02",
                  "1000", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "-999999999", "NULL", "-999999999", 
                  "NULL", "-999999999", "NULL", "NULL", "NULL", "NULL", "NULL", "0.00","NULL", "NULL", "NULL", "NULL", "NULL", "B",
                  "NULL", "1", "Long position of a 3-month Zero Coupon Bond starting at 01/02/2015 with Notional of 1000", "NULL", "NULL", "NULL")
  
  bond01 <- structure(bondValues,
                      .Names = bondNames)
  
  numIter <- length(termsheet["Components",][[1]])
  
  if(termsheet["Components",][[1]][1] == "Zero Bond") {
    
    test <- as.data.frame(matrix(nrow = numIter,ncol = 65))
    colnames(test) <- unique(c(names(option01), names(bond01)))
    test[1,names(bond01)] <- as.data.frame(t(as.data.frame(bond01)))
    
    
    if(numIter > 1){
      test[2:numIter,names(option01)] <- as.data.frame(t(as.data.frame(option01)))
      rownames(test) <- as.character(seq(from = 1, to = numIter, by = 1))
    }
    else{
      rownames(test) <- c("1")
    }
  }
  
  else if(termsheet["Components",][[1]][1] != "Zero Bond"){
    
    test <- as.data.frame(t(as.data.frame(option01)))
    
    colnames(test) <- names(option01)
    
    test[1:numIter,names(option01)] <- as.data.frame(t(as.data.frame(option01)))
    
    rownames(test) <- as.character(seq(from = 1, to = numIter, by = 1))
  }
  
  ts_temp <- getSymbols(Symbols = as.character(termsheet["Underlying",]), auto.assign = F)[,1:4]
  
  for(i in 1:numIter){
    if(termsheet["Components",][[1]][i] == "Zero Bond"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(termsheet["IssueDate",])
      test[i,"initialExchangeDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"premiumDiscountAtIED"] <- as.character(ifelse("ProtectedAmount" %in% rownames(termsheet),
                                                       as.numeric(termsheet["ProtectedAmount",])* -(as.numeric(termsheet["RiskFreeRate",])+0.04),
                                                       as.numeric(termsheet["BondComponent",])* -(as.numeric(termsheet["RiskFreeRate",])+0.04)))
      
      test[i,"maturityDate"] <- termsheet["MaturityDate",]
      test[i,"notionalPrincipal"] <- termsheet["BondComponent",]
      test[i,"currency"] <- termsheet["Currency",]
    }
    
    if(termsheet["Components",][[1]][i] == "LEPO"){
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"contractID"] <- paste("option0",as.character(i),sep = "")
      test[i,"optionStrike1"] <- "0.01"
      test[i,"optionType"] <- "C"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- option_valuation(S = ts_temp ,
                                                         K = 0.01 ,
                                                         r = as.numeric(termsheet["RiskFreeRate",]),
                                                         q = as.numeric(termsheet["Dividend",]),
                                                         maturity_date = as.character(termsheet["MaturityDate",]),
                                                         valuation_date = as.character(termsheet["IssueDate",]),
                                                         type = "C",
                                                         sig = as.numeric(termsheet["Volatility",]))
    }
    
    if(termsheet["Components",][[1]][i] == "Long Call"){
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",]) 
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "C"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- option_valuation(S = ts_temp ,
                                                         K = as.numeric(termsheet["Strike",]) ,
                                                         r = as.numeric(termsheet["RiskFreeRate",]),
                                                         q = as.numeric(termsheet["Dividend",]),
                                                         maturity_date = as.character(termsheet["MaturityDate",]) ,
                                                         valuation_date = as.character(termsheet["IssueDate",]),
                                                         type = "C",
                                                         sig = as.numeric(termsheet["Volatility",]))
    }
    if(termsheet["Components",][[1]][i] == "Long Put"){
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- option_valuation(S = ts_temp ,
                                                         K = as.numeric(termsheet["Strike",]),
                                                         r = as.numeric(termsheet["RiskFreeRate",]),
                                                         q = as.numeric(termsheet["Dividend",]),
                                                         maturity_date = as.character(termsheet["MaturityDate",]) ,
                                                         valuation_date = as.character(termsheet["IssueDate",]),
                                                         type = "P",
                                                         sig = as.numeric(termsheet["Volatility",]))
    }
    if(termsheet["Components",][[1]][i] == "Short Call"){
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(ifelse(is.na(termsheet["Cap",]),termsheet["Strike",],termsheet["Cap",]))
      test[i,"optionType"] <- "C"
      test[i,"contractRole"] <- "SEL"
      test[i,"priceAtPurchaseDate" ] <- option_valuation(S = ts_temp ,
                                                         K = as.numeric(ifelse(is.na(termsheet["Cap",]),termsheet["Strike",],termsheet["Cap",])) ,
                                                         r = as.numeric(termsheet["RiskFreeRate",]),
                                                         q = as.numeric(termsheet["Dividend",]),
                                                         maturity_date = as.character(termsheet["MaturityDate",]) ,
                                                         valuation_date = as.character(termsheet["IssueDate",]),
                                                         type = "C",
                                                         sig = as.numeric(termsheet["Volatility",]))
    }
    if(termsheet["Components",][[1]][i] == "Short Put"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "SEL"
      test[i,"priceAtPurchaseDate" ] <- option_valuation(S = ts_temp ,
                                                         K = as.numeric(termsheet["Strike",]) ,
                                                         r = as.numeric(termsheet["RiskFreeRate",]),
                                                         q = as.numeric(termsheet["Dividend",]),
                                                         maturity_date = as.character(termsheet["MaturityDate",]) ,
                                                         valuation_date = as.character(termsheet["IssueDate",]),
                                                         type = "P",
                                                         sig = as.numeric(termsheet["Volatility",]))
    }
    if(termsheet["Components",][[1]][i] == "Long BUO Call"){

      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "C"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                         K = as.numeric(termsheet["Strike",]) ,
                                                         r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                         barrier = as.numeric(termsheet["BarrierLevel",]),
                                                         sig = as.numeric(termsheet["Volatility",]),
                                                         maturity_date = termsheet["MaturityDate",],
                                                         val_date = termsheet["IssueDate",],
                                                         type = "cuo"
                                                        )
    }
    if(termsheet["Components",][[1]][i] == "Long BDO Call"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "C"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                                 K = as.numeric(termsheet["Strike",]) ,
                                                                 r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                                 barrier = as.numeric(termsheet["BarrierLevel",]),
                                                                 sig = as.numeric(termsheet["Volatility",]),
                                                                 maturity_date = termsheet["MaturityDate",],
                                                                 val_date = termsheet["IssueDate",],
                                                                 type = "cdo"
      )
    }
    if(termsheet["Components",][[1]][i] == "Long BDO Put"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                                 K = as.numeric(termsheet["Strike",]) ,
                                                                 r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                                 barrier = as.numeric(termsheet["BarrierLevel",]),
                                                                 sig = as.numeric(termsheet["Volatility",]),
                                                                 maturity_date = termsheet["MaturityDate",],
                                                                 val_date = termsheet["IssueDate",],
                                                                 type = "pdo"
      )
    }
    if(termsheet["Components",][[1]][i] == "Long BDI Put"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                                 K = as.numeric(termsheet["Strike",]) ,
                                                                 r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                                 barrier = as.numeric(termsheet["BarrierLevel",]),
                                                                 sig = as.numeric(termsheet["Volatility",]),
                                                                 maturity_date = termsheet["MaturityDate",],
                                                                 val_date = termsheet["IssueDate",],
                                                                 type = "pdi"
      )
    }
    if(termsheet["Components",][[1]][i] == "Short BDI Put"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "SEL"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                                 K = as.numeric(termsheet["Strike",]) ,
                                                                 r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                                 barrier = as.numeric(termsheet["BarrierLevel",]),
                                                                 sig = as.numeric(termsheet["Volatility",]),
                                                                 maturity_date = termsheet["MaturityDate",],
                                                                 val_date = termsheet["IssueDate",],
                                                                  type = "pdi"
      )
    }
    if(termsheet["Components",][[1]][i] == "Long BUO Put"){
      
      test[i,"statusDate"] <- as.character(termsheet["IssueDate",])
      test[i,"contractDealDate"] <- as.character(termsheet["IssueDate",])
      test[i,"purchaseDate"] <- as.character(as.Date(as.character(termsheet["IssueDate",])) %m+% days(1))
      test[i,"maturityDate"] <- as.character(termsheet["MaturityDate",])
      test[i,"optionStrike1"] <- as.character(termsheet["Strike",])
      test[i,"optionType"] <- "P"
      test[i,"contractRole"] <- "BUY"
      test[i,"priceAtPurchaseDate" ] <- barrier_option_valuation(S = ts_temp ,
                                                                 K = as.numeric(termsheet["Strike",]) ,
                                                                 r = as.numeric(termsheet["RiskFreeRate",]),b = 0,rebate = 0,
                                                                 barrier = as.numeric(termsheet["BarrierLevel",]),
                                                                 sig = as.numeric(termsheet["Volatility",]),
                                                                 maturity_date = termsheet["MaturityDate",],
                                                                 val_date = termsheet["IssueDate",],
                                                                 type = "puo"
      )
    }
    test[i,is.na(test[i,])] <- "NULL"
  }
  #write.csv(test,"C:/Users/janni/Desktop/Projekt/BA_structured_products/Francis_Suff/data/OptionsPortfolio_dev1.csv", row.names = FALSE)
  write.csv(test,"/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/OptionsPortfolio_dev1.csv", row.names = FALSE)
  #optionsDataFile2 <- "C:/Users/janni/Desktop/Projekt/BA_structured_products/Francis_Suff/data/OptionsPortfolio_dev1.csv"
  optionsDataFile2 <- "/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/OptionsPortfolio_dev1.csv"
  
  RiskFactor_to_manipulate <- file2RiskFactors_df(fname = "/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_dev.csv",sep = ",")
  #RiskFactor_to_manipulate <- file2RiskFactors_df(fname = "C:/Users/janni/Desktop/Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_dev.csv",sep = ",")
  #RiskFactor_to_manipulate <- file2RiskFactors_df(fname = "/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_dev.csv",sep = ",")
  RiskFactor_to_manipulate[2,c(6,8)] <- c(as.numeric(ts_temp[as.character(termsheet["IssueDate",]),4]),as.numeric(ts_temp[as.character(termsheet["MaturityDate",]),4]))
  RiskFactor_to_manipulate[1,c(5,7)] <- c(as.character(termsheet["IssueDate",]),as.character(termsheet["MaturityDate",]))
  RiskFactor_to_manipulate[2,c(5,7)] <- c(as.character(termsheet["IssueDate",]),as.character(termsheet["MaturityDate",]))
  RiskFactor_to_manipulate[3,c(5,7)] <- c(as.character(termsheet["IssueDate",]),as.character(termsheet["MaturityDate",]))
  RiskFactor_to_manipulate[1,c(6,8)] <- c(as.numeric(termsheet["RiskFreeRate",]),as.numeric(termsheet["RiskFreeRate",]))
  RiskFactor_to_manipulate[3,c(6,8)] <- c(1+as.numeric(termsheet["RiskFreeRate",]),1+as.numeric(termsheet["RiskFreeRate",]))
  #View(RiskFactor_to_manipulate)
  #write.csv(RiskFactor_to_manipulate,file = "Francis_Suff/data/RiskFactors_devManipulated.csv" ,row.names = FALSE)
  write.csv(RiskFactor_to_manipulate,file = "/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_devManipulated.csv" ,row.names = FALSE)
  #write.csv(RiskFactor_to_manipulate,file = "C:/Users/janni/Desktop/Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_devManipulated.csv" ,row.names = FALSE)
  #riskFactorsDataFile1 <-"Francis_Suff/data/RiskFactors_devManipulated.csv"
  riskFactorsDataFile1 <-"/Users/user/ZHAW/Semester 6/BA/R_Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_devManipulated.csv"
  #ßriskFactorsDataFile1 <-"C:/Users/janni/Desktop/Projekt/BA_structured_products/Francis_Suff/data/RiskFactors_devManipulated.csv"
  port_temp <- samplePortfolio(optionsDataFile2,riskFactorsDataFile1)
  return(port_temp)
}
