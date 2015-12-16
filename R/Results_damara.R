#------------------------------------------------------------------------------#
#    Additional summary functions for DAMARA taking account of
#    new covars for fleet economics
#   
# Paul Dolder / Simon Mardle
# Created: 14/10/2015
# Changed: 
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# ecoSum data.frame[year, quarter, stock, fleet, iter, ||,|| 
#        profits, capacity, costs, discards, effort, landings] 
#------------------------------------------------------------------------------#
ecoSum_damara <- function (fleets, flnms = "all", years, covars = NULL)
{
    if (flnms[1] == "all")
        flnms <- names(fleets)
    Dim <- dim(fleets[[1]]@effort[, years, ])[c(2, 4, 6)]
    Dimnm <- dimnames(fleets[[1]]@effort[, years, ])
    n <- prod(Dim) * length(flnms)
    res <- data.frame(year = rep(years, prod(Dim[2:3]) * length(flnms)),
        quarter = rep(rep(Dimnm[[4]], each = Dim[1]), Dim[3] *
            length(flnms)), fleet = rep(flnms, each = prod(Dim)),
        iter = rep(rep(1:Dim[3], each = prod(Dim[1:2])), length(flnms)),
        capacity = numeric(n), costs = numeric(n), effort = numeric(n),
        profits = numeric(n),               ## Adaptation from here to bring in additional covars
        DAS_FocusArea=numeric(n),DAS_Elsewhere=numeric(n),
        revenueFocusArea=numeric(n),revenueElsewhere=numeric(n),
        totalRevenue=numeric(n),crewCosts=numeric(n),fuelCosts=numeric(n),
        variableCosts=numeric(n),fixedCosts=numeric(n),
        depreciationCosts=numeric(n),investmentCosts=numeric(n),
        GCF=numeric(n),GVA=numeric(n),netProfit=numeric(n),BER=numeric(n),
        employment=numeric(n),numberVessels=numeric(n))
    k <- 1
    for (f in flnms) {
        fl <- fleets[[f]]
        mts <- names(fl@metiers)
        res[k:(k + prod(Dim) - 1), "capacity"] <- c(fl@capacity[,
            years, ])
        res[k:(k + prod(Dim) - 1), "effort"] <- c(fl@effort[,
            years, ])
        if(!is.null(covars)) res[k:(k + prod(Dim) - 1), "costs"] <- c(costs_flbeia(fl, covars, f)[,years, ])
        res[k:(k + prod(Dim) - 1), "profits"] <- c(revenue_flbeia(fl)[,
            years, ]) - res[k:(k + prod(Dim) - 1), "costs"]
        ## Additional DAMARA outputs    
        if(!is.null(covars)) {
        ## fleet based costs (i.e. fixed and capital)
        res[k:(k + prod(Dim) - 1), "fixedCosts"] <- res[k:(k + prod(Dim) - 1), "fixedCosts"] + c(covars[["FixedCost"]][f,years,] * covars[["NumbVessels"]][f, years,])
        res[k:(k + prod(Dim) - 1), "depreciationCosts"] <- res[k:(k + prod(Dim) - 1), "depreciationCosts"] + c(covars[["DepreciationCost"]][f, years,] * covars[["NumbVessels"]][f, years,])
        res[k:(k + prod(Dim) - 1), "investmentCosts"] <- res[k:(k + prod(Dim) - 1), "investmentCosts"] + c(covars[["InvestShare"]][f, years,] * covars[["CapitalValue"]][f, years,] * covars[["NumbVessels"]][f, years,])
        ##metier based costs (i.e. fuel and other variable costs)
        for (mt in mts) {   ##CHECK ALL EFFORT IS ACCOUNTED FOR!
          res[k:(k + prod(Dim) - 1), "fuelCosts"] <- res[k:(k + prod(Dim) - 1), "fuelCosts"] + c(covars[["FuelCost"]][f, years,] * fl@effort[,years,] * fl@metiers[[mt]]@effshare[,years,])
          res[k:(k + prod(Dim) - 1), "variableCosts"] <- res[k:(k + prod(Dim) - 1), "variableCosts"] + c(covars[["VariableCost"]][f, years,] * fl@effort[,years,] * fl@metiers[[mt]]@effshare[,years,])
          #calc DAS elsewhere???
          res[k:(k + prod(Dim) - 1), "DAS_FocusArea"] <- res[k:(k + prod(Dim) - 1), "DAS_FocusArea"] + (c(fl@effort[,years,] * fl@metiers[[mt]]@effshare[,years,]) / c(covars[["NumbVessels"]][f,years,]*covars[["AvgKwPerVessel"]][f,years,])*c(covars[["NumbVessels"]][f,years,]))
          ##revenues
          m <- fl@metiers[[mt]]
          sts <- catchNames(fl)
          for (st in sts) {
             if (!(st %in% catchNames(m))) 
                next
             dat <- m@catches[[st]]
             res[k:(k + prod(Dim) - 1), "revenueFocusArea"] <- res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + 
                                c(apply(dat@landings.n[,years,] * dat@landings.wt[,years,] * dat@price[,years,], c(2, 4, 6), sum, na.rm = T))  
          }
        res[k:(k + prod(Dim) - 1), "revenueFocusArea"] <- res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + c(covars[["OtherRevenueFocusArea"]][f, years,] * fl@effort[,years,] * fl@metiers[[mt]]@effshare[,years,])
        }
        ## Need to add in additional revenue from outside 7B-K and other species - check FG and BK
        res[k:(k + prod(Dim) - 1), "revenueElsewhere"] <- res[k:(k + prod(Dim) - 1), "revenueElsewhere"] + c(covars[["NumbVessels"]][f, years,] * covars[["OtherRevenueElsewhere"]][f, years,])
        res[k:(k + prod(Dim) - 1), "totalRevenue"] <- (res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + res[k:(k + prod(Dim) - 1), "revenueElsewhere"])
        ##crewCosts
        res[k:(k + prod(Dim) - 1), "crewCosts"] <- res[k:(k + prod(Dim) - 1), "totalRevenue"] * c(fl@crewshare[,years,])
        ##profits and BER
        res[k:(k + prod(Dim) - 1), "GCF"] <- res[k:(k + prod(Dim) - 1), "totalRevenue"] - res[k:(k + prod(Dim) - 1), "fixedCosts"] - res[k:(k + prod(Dim) - 1), "fuelCosts"] - res[k:(k + prod(Dim) - 1), "variableCosts"] - res[k:(k + prod(Dim) - 1), "crewCosts"]
        res[k:(k + prod(Dim) - 1), "GVA"] <- res[k:(k + prod(Dim) - 1), "GCF"] + res[k:(k + prod(Dim) - 1), "crewCosts"]
        res[k:(k + prod(Dim) - 1), "netProfit"] <- res[k:(k + prod(Dim) - 1), "GCF"] - res[k:(k + prod(Dim) - 1), "depreciationCosts"] 
        res[k:(k + prod(Dim) - 1), "BER"] <- (res[k:(k + prod(Dim) - 1), "crewCosts"] + res[k:(k + prod(Dim) - 1), "fixedCosts"] + res[k:(k + prod(Dim) - 1), "depreciationCosts"])/
                                             (1-(res[k:(k + prod(Dim) - 1), "fuelCosts"]/ res[k:(k + prod(Dim) - 1), "totalRevenue"]) - (res[k:(k + prod(Dim) - 1), "variableCosts"])/res[k:(k + prod(Dim) - 1), "totalRevenue"])
        res[k:(k + prod(Dim) - 1), "employment"] <- res[k:(k + prod(Dim) - 1), "employment"] + c(covars[["EmploymentPerVessel"]][f,years,] * covars[["NumbVessels"]][f,years,])
        res[k:(k + prod(Dim) - 1),"numberVessels"] <-res[k:(k + prod(Dim) - 1), "numberVessels"] + c(covars[["NumbVessels"]][f,years,])
        }
        k <- k + prod(Dim)
    }
    return(res)
}
    

#-------------------------------------------------------------------------------
# revenue_FocusArea(fleet, covars, years)
# This function calculates the total revenue in the focus area 
# (Revenue species modelled + Other Revenue Focus Area)
#-------------------------------------------------------------------------------
revenue_FocusArea <- function(fleet, covars){
    flnm<-fleet@name
    sts <- catchNames(fleet)
    mts <- names(fleet@metiers)
    
    res <- FLQuant(0, dimnames = dimnames(fleet@effort))
    
    for(mt in mts){
        m <- fleet@metiers[[mt]]
        for(st in sts){
            if(!(st %in% catchNames(m))) next
            dat <- m@catches[[st]]
            res <- res + apply(dat@landings.n*dat@landings.wt*dat@price, c(2,4,6),sum,na.rm=T)
        }
    }
    # Additional revenue from other species in FocusArea, occurs at the fleet level
    res<- res + (fleet@effort * covars[["OtherRevenueFocusArea"]][flnm,])
    return(res)               
}

#-------------------------------------------------------------------------------
# revenue_OutsideFocusArea (fleet, covars, years)
# This function calculates the total revenue outside the FocusArea 
# (Other Revenue OutsideFocus Area * No Vessels)
#-------------------------------------------------------------------------------

revenue_OutsideFocusArea <- function(fleet, covars){
    flnm<-fleet@name
  
    res <- FLQuant(0, dimnames = dimnames(fleet@effort))
    
    # Additional revenue from species from elsewhere (outside FocusArea), occurs at the fleet level
    res<- res + (covars[["NumbVessels"]][flnm,] * covars[["OtherRevenueElsewhere"]][flnm,])
    return(res)               
}

#-------------------------------------------------------------------------------
# totvcost_FocusArea(fleet, years)
# Redefined to include crew costs for other species in focus area
#-------------------------------------------------------------------------------
totvcost_FocusArea <- function(fleet, covars){
    
    mts <- names(fleet@metiers)
    
    res <- FLQuant(0, dimnames = dimnames(fleet@effort))
    
    for(mt in mts){
        res <- res + fleet@metiers[[mt]]@vcost*fleet@effort*fleet@metiers[[mt]]@effshare
    }
    Rev <- revenue_FocusArea(fleet, covars)*fleet@crewshare
    
    res <- res + Rev
    
    return(res)               
}

#-------------------------------------------------------------------------------
# costs_damara(fleet, years)
# Redefined to account for crew costs which include 
# other species in focus area
#-------------------------------------------------------------------------------
costs_damara <- function(fleet, covars, flnm = NULL){
    
    res <- totvcost_damara(fleet) + totfcost_flbeia(fleet, covars, flnm)
    
    return(res)               
}




###########################################################################################
###########################################################################################
## Revised SCD function to take account of revenue from outside of the modelled
## Area
###########################################################################################


SCD_damara <- function(fleets, covars, fleets.ctrl, flnm, year = 1, season = 1,...){
    
   
    
    fleet <- fleets[[flnm]]
    
    ny <- dim(fleet@effort)[2]
    ns <- dim(fleet@effort)[4]
    it <- dim(fleet@effort)[6]
    
    # VaC - total for Focus Area
    VaC <- seasonSums(totvcost_FocusArea(fleet,covars)[,year]) # total anual variable costs
    # FxC - total for both areas
    FxC <- (covars[["NumbVessels"]][flnm, ] * seasonSums(fleet@fcost))[, year]
    # FuC  # per unit of effort, we asume common cost for all the metiers.
    FuC <- (covars[['FuelCost']][flnm,]*seasonSums(fleet@effort))[,year]
    # CaC # per number of vessels
    CaC <- (covars[['CapitalCost']][flnm,]*covars[["NumbVessels"]][flnm, ])[,year]
    # Revenue - inside the focus area
    Rev1 <- seasonSums(revenue_FocusArea(fleet,covars)[,year])
    Rev1 <- ifelse(Rev1 == 0, 1e-16, Rev1)
    
    # Revenue - outside the focus area
    Rev2 <- seasonSums(revenue_OutsideFocusArea(fleet,covars)[,year])
    Rev2 <- ifelse(Rev2 == 0, 1e-16, Rev2)
    
    # CrC - focus area
    CrC <- (Rev1*seasonMeans(fleet@crewshare[,year]))  +  covars[['Salaries']][flnm,year]
    # CrC - outside focus area
    CrC2<- (Rev2*seasonMeans(fleet@crewshare[,year]))  +  covars[['Salaries']][flnm,year]
    
    # VaC2
    VaC2<-(covars[["NumbVessels"]][flnm,] * covars[["OtherAreaVariableCost/Vessel"]][flnm,])[,year]
    
    #x1 <- FuC/Rev
    #x2 <- VaC/Rev
    
    a <- CrC + CrC2 + FxC + CaC
    #b <- 1 - x1 - x2
    # Recalculate b based on both revenue inside the focus area and revenue
    # outside the focus area
    b<- ((Rev1 - (FuC + (VaC-CrC))) + (Rev2 - (VaC2-CrC2)))/(Rev1 + Rev2)
    
    BER <- a/b
    
    # Redefine Rev = Rev + Rev2
    Rev<- Rev1 + Rev2
    
    Inv <- c((Rev - BER)/Rev)*c(covars[['InvestShare']][flnm,year])
    
    Ks <- seasonSums(fleet@capacity[,year])[drop=T]    # seasonal capacity [ns,ni]
    K  <- c(seasonSums(fleet@capacity[,year])) # annual capacity. [ni]

    # pKs How annual capacity is distributed along seasons.
    if(ns == 1)      pKs <- rep(1,it) #[ni]
    else  if(it > 1) pKs <- sweep(Ks,2,K,"/")    # ns > 1 [ns,ni]
          else       pKs <- Ks/K    # [ns]
    
    w1 <- c(covars[['w1']][flnm,year]) 
    w2 <- c(covars[['w2']][flnm,year]) 
    
    
#    # Translate Inv in number of vessels.
#    Inv_ves <- ifelse(Inv>0, Inv/c(covars[['NewVessPrice']][flnm, year,]), Inv/c(covars[['OldVessPrice']][flnm, year,]))
  
    omega <- ifelse(Inv < 0, 
                        ifelse(-Inv < w1, Inv*K, -w1*K),  # Inv < 0
                        ifelse(Inv < w2, Inv*K, w2*K))    # Inv >= 0  
                        
  #  print(omega)
                
    # Investment in new vessels only occur if the operational days of existing vessesl is equal to capacity and investment saving is >0.
    # In iters where effort == capacity?    
    # In iterSel although the money for investment is >0 there is no investment.
    Ef <- c(fleet@effort[,year])
    iterSel <- which(omega > 0 & Ef < 0.99*K)              
    
    omega[iterSel] <- 0
    
    # If year is not last year Update capacity  in year [year+1].
    if (year < ny){
        fleets[[flnm]]@capacity[, year + 1] <- Ks + omega*pKs
        covars[['NumbVessels']][flnm,year+1,] <- (K + omega)/covars[['MaxDays']][flnm,year+1,]
    }

    
    return(list(fleets = fleets, covars = covars))
}
