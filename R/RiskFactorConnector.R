# RiskFactorConnector.R has code from FEMS/R/RiskFactorConnector.R
# FEMSdev edits Francis Parr MArch 2022
# defines ref class RiskFactorConnectors as riskFactorList

setRefClass("RiskFactorConnector",
            fields = list(
              riskfactors = "list"
            ))

# create a constructor RFConn
setGeneric(name = "RFConn",
           def = function(...){
             standardGeneric("RFConn")
           })

# set up  generic add()
setGeneric(name = "add", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("add")
           })

# add a new Risk Factor into the RFConnector
setMethod("add", signature = c("RiskFactorConnector","RiskFactor"),
           def = function(object,what){
            object$riskfactors <- append(object$riskfactors,what)
      # get() code assumes names will provide index from labels into riskfactors
            names(object$riskfactors) <-
                lapply(object$riskfactors,function(x){return(x$label)} )
           })

# instantiate RFConn for an empty parameter list
setMethod(f = "RFConn", signature = c(),
          definition = function(){
            object = new("RiskFactorConnector")
#            pars = list(...)
#            if (length(pars) > 0)
#              add(object, what = pars)

            return(object)
          })

# validate by creating an empty riskFactorConnector object
# rfc <-RFConn()
