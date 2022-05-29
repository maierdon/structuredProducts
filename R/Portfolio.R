# Portfolio.R  FEMS dev code by Francis Parr Feb 2022
# Edits/subset of Portfolio.R in main FEMS branch
# Licensing and Copyright notices from there
# **************************************************
# defines: class Portfolio, Portfolio() constructor,
#    add(<portfolio>,<contract_list>), set(<portfolio>,<rfconn>)
#    generateEvents(<portfolio>, <serverURL>)
#    samplePortfolio(<contractDataFilename>)

# TODO => FNP 13Apr2022 (1) convert samplePortfolio<cdfn>) to
# Portfolio(<cdfn>,<mdfn>, <validate> ) ;  rfConn to <rf_list>
#
# concept: Portfolio contains all information needed to validata and issue
#    a cashflow generation request to server running Actus core libraries
# *********************************************************************

# define the class Portfolio
setRefClass("Portfolio",
            fields = list(
              contracts = "list",   # contracts are instances of ContractType
              riskFactors = "list"  # riskFactors ReferenceIndex with moc
            ))

# define generic function Portfolio()
setGeneric(name = "Portfolio",
           def = function(...){
             standardGeneric("Portfolio")
           })
# and instantiate for the no parameters case
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
             return(new("Portfolio"))
          })

# ************************************************************************
# generateEvents(<Portfolio>,<serverURL>)
#
# calls specified Actus server <serverURL> to generate cashflow events
# for all contracts in a Portfolio using provided RiskFactor scenario data

setGeneric(name = "generateEvents",
           def = function(ptf,serverURL){
             standardGeneric("generateEvents")
           })

setMethod (f = "generateEvents", signature = c("Portfolio","character") ,
          definition = function(ptf, serverURL){
            # send contract and risk factors to the Server as valid JSON

# Functional programming construction of preJson for Portfolio
            contractDefs <- lapply(ptf$contracts,preJcontract)
            riskFactors <-  preJSONrfs(ptf$riskFactors)
            fin_list <- list(contracts = contractDefs,
                             riskFactors = riskFactors)

            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = FALSE)

            # issue POST command to have server generate cashflows
            response_events <- POST(paste0(serverURL, "eventsBatch"),
                                    body = request_body,
                                    content_type_json())
            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(response_content$error)
              stop("ErrorIn::ContractType:: API response error; Check if all necessary ContractTerms were set correctly!!!")
            }
            return(response_content)
        })

# ************************************************************
# samplePortfolio(contractDataFileName)  cdfn  FNP 13 April 2022
#  builds sample portfolio object using cdfn contractDataFileName and
#  rfdfn riskFactorDataFileName - both are csv files
#   tested data/BondPortfolio_dev.csv, data/OptionsPortfolio_dev.csv
#   => toDO : add rfdfn
# ************************************************************
samplePortfolio <- function(cdfn, rfdfn) {
  ptf <- Portfolio()            # create portfolio object no agtributes set
                                # read in contract and riskFactor date from
                                # named files; convert to lists of contract
                                # and riskFactor objects
                                # riskfactors first - contract.moc valid check
  ptf$riskFactors <- riskFactors_df2list( file2RiskFactors_df(rfdfn) )
  ptf$contracts <- contracts_df2list( file2Contracts_df(cdfn) )
                                # portfolio is now initialized and ready for
                                # cashflow generation
  return(ptf)
}
