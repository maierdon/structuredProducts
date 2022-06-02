#barrier function 0.2

#barrier function------------------------------------
barrier_contract <- function(risk_factor,          #time series of the underlying which has to be evaluated
                             contract_1,           #default contract (what we start with). Exception: Out options
                             contract_2 = NULL,    #optional second contract (in case barrier is triggered)
                             barrier_level,        #relative percentage of the barrier level (e.g. 0.8 = 80% of start price)
                             barrier_type,
                             IssueDate,
                             MaturityDate){              #Barrier Type, does the trigger active or deactive? (konck in or knock out barrier)
  if (!(barrier_type %in% c( "Out","In" ))) {
    stop("Barrier type (BType) has to be either 'Out' (deactivating) or 'In' (activating)!")#prevent wrong input values
    }
  if (barrier_level < 0 | barrier_level > 2) {
    warning("Barrier level should be a percentage written as a decimal number!")#prevent wrong input values
  }
  
  risk_factor <- getSymbols(risk_factor, auto.assign = F)[timeSequence(IssueDate,MaturityDate,by = "d"),1:4]
  trigger_point <- as.numeric(risk_factor[1, 4] * barrier_level) #define the specific trigger point (point where barrier gets triggered)
  ##set output possibilities to simplify the return later on (so only outputpos[1] or [2] needs to be set)
  if (is.null(contract_2) & barrier_type == "In"){
    output_possibility <- list(0, contract_1) #'knock-in' barrier -> 0 or contract_1
  }
  else if (is.null(contract_2) & barrier_type == "Out") {
    output_possibility <- list(contract_1, 0) #'knock-out' barrier -> contract_1 or 0
  }
  else output_possibility <- list(contract_1, contract_2) #two contracts (e.g. convertible bonds)

  if(trigger_point < as.numeric(risk_factor[1, 4])){ #Check if trigger_point is below starting price s0
    i <- 1
    while(as.numeric(risk_factor[i, 3]) > trigger_point){ #function to check if barrier got triggered (take daily low) (down)
      i <- i + 1 #iteration step
      if (i == length(risk_factor[, 1])) {
        #print("not activated")
        output_possibility[1][[1]]["Barrier",] <- "yes"
        out <- as.data.frame(output_possibility[1])
        colnames(out) <- gsub("[.]"," ",colnames(out))
        return(out)#return output_possibilty at the end of timeframe if barrier has never been triggered
      }
      
    }
    #print(c("barrier activated at:",as.numeric(risk_factor[i, 3])))
    date <- index(risk_factor)[i]
    output_possibility[2][[1]]["Barrier",] <- "yes"
    output_possibility[2][[1]]["BarrierLevel",] <- as.character(round(trigger_point,4))
    output_possibility[2][[1]]["BarrierEventDate",] <- as.character(date)
    # BarrierEventDate <- IssueDate %m+% days(i)
    # df_to_manipulate <- output_possibility[2][1][[1]][[1]]
    # df_to_manipulate[nrow(df_to_manipulate) + 1,] <- c(as.character(BarrierEventDate), "BarrierEvent", 0)
    # df_to_manipulate <- df_to_manipulate[order(df_to_manipulate["Dates"]),]
    # output_possibility[2][1][[1]][[1]] <- df_to_manipulate
    out <- as.data.frame(output_possibility[2])
    colnames(out) <- gsub("[.]"," ",colnames(out))
    return(out) #return value if barrier has been triggered
  }
  else { #barrier_level is bigger than 1 therefore triggerpoint above starting price (Up)
    i <- 1
    while(as.numeric(risk_factor[i, 2]) < trigger_point){ #function to check if barrier has been triggered (take daily high)
      i <- i + 1 #iteration step
      if (i == length(risk_factor[, 1])){
        #print("not activated")
        output_possibility[1][[1]]["Barrier",] <- "yes"
        out <- as.data.frame(output_possibility[1])
        colnames(out) <- gsub("[.]"," ",colnames(out))
        return(out) #return value at the end of term if barrier has never been triggered
      }
    }
    #print(
    date <- index(risk_factor)[i]
    output_possibility[2][[1]]["Barrier",] <- "yes"
    output_possibility[2][[1]]["BarrierLevel",] <- as.character(round(trigger_point,4))
    output_possibility[2][[1]]["BarrierEventDate",] <- as.character(date)
    # BarrierEventDate <- IssueDate %m+% days(i)
    # df_to_manipulate <- output_possibility[2][1][[1]][[1]]
    # df_to_manipulate[nrow(df_to_manipulate) + 1,] <- c(as.character(BarrierEventDate), "BarrierEvent", 0)
    # df_to_manipulate <- df_to_manipulate[order(df_to_manipulate["Dates"]),]
    # output_possibility[2][1][[1]][[1]] <- df_to_manipulate
    out <- as.data.frame(output_possibility[2])
    colnames(out) <- gsub("[.]"," ",colnames(out))
    return(out) #return value at the end of term if barrier has been triggered
  }
}




