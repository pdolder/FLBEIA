#######################################################################
######## Functions to support DAMARA Celtic DST tool ## ###############
####### Created by Paul J Dolder, Cefas 21/08/14 ######################
#######################################################################

#########################################################
###Function to get range of data from FLStock objects ###
#########################################################

loadStockRange<-function(dir=file.path(data.path,'stocks')) {
  
  stks<-as.list(list.files(dir))
  stock.nam<-unlist(strsplit(unlist(stks), "\\.")) # names from RData files
  stock.nam<-stock.nam[stock.nam !="RData"]
  names(stks)<-stock.nam # as a list
  
  # load in the stocks and get the data range from all
  stocks <- FLStocks(lapply(stks, function(x) {
    load(file.path(dir,x))
    res<-get("stock")
    name(res) <- gsub(".RData","",x)
    res}))
    return(c(range(stocks)[["minyear"]],range(stocks)[["maxyear"]]))
}


#########################################################
######## Function to load FLStock objects ###############
#########################################################

loadStocks<-function(dir=file.path(data.path,'stocks')) {
  
  stks<-as.list(list.files(dir))
  stock.nam<-unlist(strsplit(unlist(stks), "\\.")) # names from RData files
  stock.nam<-stock.nam[stock.nam !="RData"]
  names(stks)<-stock.nam # as a list
  
  # load in the stocks and get the data range from all
  stocks <- FLStocks(lapply(stks, function(x) {
    load(file.path(dir,x))
    res<-get("stock")
    name(res) <- gsub(".RData","",x)
    res}))
  return(stocks)
}

##################################################################################################
### Function to implement closed areas in FLBEIA #################################################
##################################################################################################

close.area<-function(area) {
  
  fleets<-unclass(fleets)
  
  mets.close<-grep(area,unique(stack(sapply(fleets,function(x) x@metiers@names))$values),value=T)
  
  for (fl in 1:length(fleets)) {
    
    if (any(fleets[[fl]]@metiers@names %in% mets.close)) {
      
      # metiers to close for fleet
      mets<-grep(area,fleets[[fl]]@metiers@names,value=T) 
      
      # set the effshare and catch.q to zero
      for (mt in 1:length(mets)) {
        m.<-mets[mt]
        fleets[[fl]]@metiers[[m.]]@effshare[,ac(proj.yrs)]<-0
        s.<-catchNames(fleets[[fl]][[m.]])
        for (st in 1:length(s.)) {
          fleets[[fl]]@metiers[[m.]]@catches[[st]]@catch.q[,ac(proj.yrs)]<-0
          fleets[[fl]]@metiers[[m.]]@catches[[st]]@landings.sel[,ac(proj.yrs)]<-0
          fleets[[fl]]@metiers[[m.]]@catches[[st]]@discards.sel[,ac(proj.yrs)]<-0
        }
        
      }
      
      # reallocate effort share to the remaining metiers
      mets.fl<-fleets[[fl]]@metiers@names # all metiers
      # calculate the sum of eff share in open metiers/areas
      eff.sum<-colSums(t(sapply(mets.fl,function(mt) as.vector(fleets[[fl]]@metiers[[mt]]@effshare[,ac(proj.yrs)]))))
      # raise the effort shares so sum to one...
      for (mt in 1:length(mets.fl)) {
        fleets[[fl]]@metiers[[mt]]@effshare[,ac(proj.yrs)]<-fleets[[fl]]@metiers[[mt]]@effshare[,ac(proj.yrs)]/eff.sum
      }
    }
  }
  fleets<-FLFleetsExt(fleets)
  return(fleets)
}

##################################################################################################

##################################################################################################
## Function to extract fleet level data
##################################################################################################

# ----------slot.fleet method -------
setGeneric("slot.fleet", function(fleets,slot.){
  standardGeneric("slot.fleet")
})
setMethod("slot.fleet", signature(fleets="FLFleetsExt"),
          function(fleets,slot.)
          {
            sl. <- eval(parse("",text=slot.))
            res <-lapply(fleets, function (x) {
              mt. <- lapply(x@metiers, function(x1) {
                res. <- as.data.frame(sl.(x1))
                names(res.)[which(names(res.)=="data")]   <- slot.
                res.$fleet  <- x@name
                res.$metier <- x1@name
                return(res.)})
              mt. <- eval(parse(text=paste('rbind(mt.[[',paste(seq(length(mt.)),collapse=']],mt.[['),']])',sep='')))
            })
            res <- eval(parse(text=paste('rbind(res[[',paste(seq(length(res)),collapse=']],res[['),']])',sep='')))
            
            return(res)
          })

##################################################################################################
##################################################################################################

##################################################################################################
##################################################################################################
## Function to extract the price at age from the catches slot of the first metier catching the stock
## Used in DAMARA for filling the base parameters for the elasticPrices function
##################################################################################################
##################################################################################################

getPrice<-function (fleet, stock) {
  mts <- fleet@metiers@names
  totLa <- landWStock.f(fleet, stock)
  res <- FLQuant(0, dimnames = dimnames(totLa))
  for (mt in mts) {
    m <- fleet@metiers[[mt]]
    if (!stock %in% catchNames(m)) 
      next
    dat <- m@catches[[stock]]
    res <- res + apply(dat@landings.n * dat@landings.wt * 
                         dat@price, c(1, 2, 4, 6), sum, na.rm = T)
    }
  res <- res/totLa
  res[is.na(res)]<-0
  return(res)
}


###########################################################################################################
## Function to summarise the summary indicator outputs from FLBEIA as quantiles from a multi-iteration run
###########################################################################################################

SumIndQuantiles<-function(data=NULL) {
  
  if(is.null(data)) stop("Must specify the data summary object you want to use, i.e. results from bioSum,ecoSum etc..")
  if(any(!c("reshape2","dplyr") %in% rownames(installed.packages()))) stop("Must have dplyr and reshape2 installed in your library")
  require(dplyr);   require(reshape2)
  
  # If its the fleet summary, eco summary etc... need to reorientate the data
  data<-melt(data,id=colnames(data)[!colnames(data) %in% c("landings","discards","price","tacshare",                            # fleet  indicators
                                                           "capacity","costs","effort","profits",                               # eco indicators
                                                           "DAS_FocusArea","DAS_Elsewhere","revenueFocusArea",                  # Additional DAMARA eco indicatorsa
                                                           "revenueElsewhere","totalRevenue","crewCosts","fuelCosts",
                                                           "variableCosts","fixedCosts","depreciationCosts","investmentCosts",
                                                           "GCF","GVA","netProfit","BER","employment","numberVessels",
                                                           "effshare","effort")])                                               # metier level indicators
  
  
  # The list of variables to summarise across (excluding iter and value)
  variables<-colnames(data)[!(colnames(data) %in% c("iter","value"))]
  variables <- lapply(variables, as.symbol)
  
  x<-group_by_(data,.dots=variables) %>% summarise(q05=quantile(value,prob=0.05,na.rm=T),
                                                   q10=quantile(value,prob=0.10,na.rm=T),
                                                   q25=quantile(value,prob=0.25,na.rm=T),
                                                   q50=quantile(value,prob=0.50,na.rm=T),
                                                   q75=quantile(value,prob=0.75,na.rm=T),
                                                   q90=quantile(value,prob=0.90,na.rm=T),
                                                   q95=quantile(value,prob=0.95,na.rm=T)) %>% ungroup()
  colnames(x)[colnames(x)=="variable"]<-"indicator"
  return(as.data.frame(x))
}
