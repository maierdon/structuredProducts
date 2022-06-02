#function that creates a better overview of the cashflows
#input is the option_cfl_rslts from the main test file

create_proper_table <- function(object)
{
  out <- vector("list", length(object))
  for (i in 1:length(object))
  {
    Event_Type <- rep(NA,length(object[[i]]$events))
    Event_Date <- rep(NA,length(object[[i]]$events))
    Event_Payoff <- rep(NA,length(object[[i]]$events))
    Event_Currency <- rep(NA,length(object[[i]]$events))
    
    
    for (j in 1:length(object[[i]]$events))
    {
      Event_Type[j] <- object[[i]]$events[[j]]$type
      Event_Date[j] <- object[[i]]$events[[j]]$time
      Event_Payoff[j] <- object[[i]]$events[[j]]$payoff
      Event_Currency[j] <- object[[i]]$events[[j]]$currency
    }
    Event_Date <- format(as.Date(Event_Date), "%Y-%m-%d")
    out[[i]] <- list(data.frame(Event_Date,Event_Type,Event_Currency,Event_Payoff))
  }
  return(out)
}
