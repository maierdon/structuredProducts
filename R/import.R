# import.R                                                                                                                                                                                                                                                                                                                                                                                                                            ...# import.R - working development version of import.R
# fnp Feb 2022
# function to (1) defaults for separator, valuation engine
#  (2) read csv file named in  parameter source into dataframe
loadFromText <- function(source, pars) {
  ## check for specified separator other than ","
  if(!"sep" %in% names(pars)) {
    separator <- ","
  } else {
    separator <- pars[["sep"]]
  }
  contracts_df <- read.csv(source,header=TRUE,sep = separator)
  return(contracts_df)
}
# **************************************
# file2Contracts_df( filename )
#     simplified version of sampleContracts_df  FNP 10th April
# **************************************
file2Contracts_df <- function(fname, sep = ",") {
    contracts_df <- loadFromText(fname,list())
    # convert all missing data into text null
    contracts_df[is.na(contracts_df)] <- "NULL"
    return(contracts_df)
}
# **************************************
# file2riskFactors_df( filename )
#     simplified version of
# **************************************
file2RiskFactors_df <- function(fname, sep = ",") {
  riskFactors_df <- loadFromText(fname,list())
  # convert all missing data into text null
  riskFactors_df[is.na(riskFactors_df)] <- "NULL"
  return(riskFactors_df)
}


# ************************************
# contracts_df2list(contracts_df)
#   build list of contracts from df
#   expanded cleaned version of df2contracts_list
#  Split df: terms, legs, descriptp (once)
#  for each row: createContract(terms, legs, irow)
#      append into returned list  fnp  10 Apr 2022
#    -- improved version df2contracts_list()
# ************************************************

contracts_df2list<- function(contracts_df){
  nonTermCols <- c("description","contrStrucObj.marketObjectCode",
                   "contrStruc.referenceType", "contrStruc.referenceRole")
  terms_df <-contracts_df[!names(contracts_df) %in% nonTermCols]
  legs_df <-data.frame(
         marketObjectCode = contracts_df["contrStrucObj.marketObjectCode"],
         referenceType = contracts_df["contrStruc.referenceType"],
         referenceRole = contracts_df["contrStruc.referenceRole"]
         )
  outlist <- list()
  for ( irow in 1:nrow(contracts_df)){
    outlist <- append (outlist, datarow2Contract(terms_df,legs_df,irow) )
  }
  return (outlist)
}

# ************************************
# riskFactors_df2list(riskFactors_df)
#   input: dataframe riskFactor data,
#   returns list of riskFactor objects
#   convert date, value pairs in risk Factor row
#   all riskFactors are referenceIndex for now
# ************************************************
riskFactors_df2list <- function(riskFactors_df){
  rflist <- list()
  nhdrs <- 4        # rfType, moc, base, dataPairCount are " row headers"
  for ( irow in 1:nrow(riskFactors_df)){
      rfRow <- riskFactors_df[irow,]
      tset <- as.character(rfRow[nhdrs-1+(1:rfRow$dataPairCount)*2])
          # vector of dates
      vset <- as.numeric(rfRow[nhdrs+(1:rfRow$dataPairCount)*2])
           # vector of numeric values
      rflist <-append(rflist,
                       Index(rfRow$marketObjectCode,rfRow$base,,tset,vset))
      }
  return(rflist)
}

# ***********************************************
# datarow2Contract ( ) -  create contract object
#    inputs:  terms_df, legs_df, descr, irow :
#    contractType; object <- new("contractType)
#    constructors will set isStructured but not populate terms or legs
#    if isStructured: insertLegs
#    insertTerms ( both simple and strutured cases )
# *************************************
datarow2Contract<- function(terms_df, legs_df,irow){
  contractTypeName <- longName(tolower(terms_df$contractType[irow]))
  contract <- CT(contractTypeName)
  #FNP  avoid validity check for now 10Apr2022; test PAM,OPTNS
  if (contractTypeName == "Option"){
     contract$contractStructure<-list(
               CLeg(legs_df$contrStrucObj.marketObjectCode[irow])
               )
     contract$isCompound <- TRUE
  } else {
     contract$isCompound <- FALSE
  }
  # insert terms - skipping term validy checks for now FNP Apr 2022
  ContractTerms <- as.list(t(terms_df[irow,]))
  names(ContractTerms) <- colnames(terms_df)  # reattach column names

  ## drop all NULL elements. sapply operates on a list but returns a vector
  ContractTerms <- ContractTerms[sapply(ContractTerms, function(x) x != "NULL")]

  set(object = contract, what = ContractTerms)
  return(contract)
}

