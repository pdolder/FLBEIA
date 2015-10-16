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
    name(res) <- x
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
    name(res) <- x
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

