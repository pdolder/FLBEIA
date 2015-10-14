#------------------------------------------------------------------------------#
#    Additional summary function for DAMARA taking account of
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
        employment=numeric(n))
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
        res[k:(k + prod(Dim) - 1), "fixedCosts"] <- covars[["FixedCost"]][f, ] * covars[["NumbVessels"]][f, ]
        res[k:(k + prod(Dim) - 1), "depreciationCosts"] <- covars[["DepreciationCost"]][f, ] * covars[["NumbVessels"]][f, ]
        res[k:(k + prod(Dim) - 1), "investmentCosts"] <- covars[["InvestShare"]][f, ] * covars[["CapitalCost"]][f, ] * covars[["NumbVessels"]][f, ]
        ##metier based costs (i.e. fuel and other variable costs)
        for (mt in mts) {   ##CHECK ALL EFFORT IS ACCOUNTED FOR!
          res[k:(k + prod(Dim) - 1), "fuelCosts"] <- res[k:(k + prod(Dim) - 1), "fuelCosts"] + (covars[["fuelCost"]][f, ] * fleet@effort * fleet@metiers[[mt]]@effshare)
          res[k:(k + prod(Dim) - 1), "variableCosts"] <- res[k:(k + prod(Dim) - 1), "variableCosts"] + (covars[["VariableCost"]][f, ] * fleet@effort * fleet@metiers[[mt]]@effshare)
          #calc DAS elsewhere???
          res[k:(k + prod(Dim) - 1), "DAS_FocusArea"] <- res[k:(k + prod(Dim) - 1), "DAS_FocusArea"] + (fleet@effort * fleet@metiers[[mt]]@effshare / (covars[["NumbVessels"]][f, ]*covars[["AvgKwPerVessel"]][f, ]))
          ##revenues
          m <- fl@metiers[[mt]]
          sts <- catchNames(fl)
          for (st in sts) {
             if (!(st %in% catchNames(m))) 
                next
             dat <- m@catches[[st]]
             res[k:(k + prod(Dim) - 1), "revenueFocusArea"] <- res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + 
                                apply(dat@landings.n * dat@landings.wt * dat@price, c(2, 4, 6), sum, na.rm = T)  
          }
        }
        ## Need to add in additional revenue from outside 7B-K and other species - check FG and BK
        res[k:(k + prod(Dim) - 1), "revenueFocusArea"] <- res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + (covars[["OtherRevenue7BK"]][f, ] * fleet@effort * fleet@metiers[[mt]]@effshare)
        res[k:(k + prod(Dim) - 1), "revenueElsewhere"] <- res[k:(k + prod(Dim) - 1), "revenueElsewhere"] + (covars[["NumbVessels"]][f, ] * covars[["OtherRevenueElsewhere"]][f, ])
        res[k:(k + prod(Dim) - 1), "totalRevenue"] <- (res[k:(k + prod(Dim) - 1), "revenueFocusArea"] + res[k:(k + prod(Dim) - 1), "revenueElsewhere"])
        ##crewCosts
        res[k:(k + prod(Dim) - 1), "crewCosts"] <- res[k:(k + prod(Dim) - 1), "totalRevenue"] * fleet@crewshare
        ##profits and BER
        res[k:(k + prod(Dim) - 1), "GCF"] <- res[k:(k + prod(Dim) - 1), "totalRevenue"] - res[k:(k + prod(Dim) - 1), "fixedCosts"] - res[k:(k + prod(Dim) - 1), "fuelCosts"] - res[k:(k + prod(Dim) - 1), "variableCosts"] - res[k:(k + prod(Dim) - 1), "crewCosts"]
        res[k:(k + prod(Dim) - 1), "GVA"] <- res[k:(k + prod(Dim) - 1), "GCF"] + res[k:(k + prod(Dim) - 1), "crewCosts"]
        res[k:(k + prod(Dim) - 1), "netProfit"] <- res[k:(k + prod(Dim) - 1), "GCF"] - res[k:(k + prod(Dim) - 1), "depreciationCosts"] - res[k:(k + prod(Dim) - 1), "investmentCosts"]
        res[k:(k + prod(Dim) - 1), "BER"] <- (res[k:(k + prod(Dim) - 1), "crewCosts"] + res[k:(k + prod(Dim) - 1), "fixedCosts"] + res[k:(k + prod(Dim) - 1), "depreciationCosts"] + res[k:(k + prod(Dim) - 1), "investmentCosts"]) /
                                             ((res[k:(k + prod(Dim) - 1), "GCF"] + res[k:(k + prod(Dim) - 1), "crewCosts"]) / res[k:(k + prod(Dim) - 1), "totalRevenue"])
        res[k:(k + prod(Dim) - 1), "employment"] <- covars[["EmploymentPerVessel"]][f, ] * covars[["NumbVessels"]][f, ]
        }
        k <- k + prod(Dim)
    }
    return(res)
}
    
