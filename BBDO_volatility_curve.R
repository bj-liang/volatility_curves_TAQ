#if "<-" it's lines of code from precept; if "=" it's lines I made

closeAllConnections()
rm(list=ls())
library(data.table)
setwd("C:\\Users\\benny\\OneDrive\\Desktop\\github\\volatility_curves_TAQ")

C <- fread("TAQ.gz", header=TRUE)
C$DATE <- as.Date(as.character(C$DATE),format='%Y%m%d')

copy = C #Just a way for me to check data

library(chron)
C$TIME_M <- chron(times=C$TIME_M)
C <- C[C$EX == 'N' | C$EX == 'T' | C$EX == 'P' | C$EX == 'Z']
C <- C[C$TIME_M >= chron(times='09:29:00') & C$TIME_M <= chron(times='16:00:00')] # filter only trading hours
#spare a minute to fill the first bin

#C <- C[C$SYM_SUFFIX == ' ']
#I trust TAQ's data ordering
C$EX <- factor(C$EX) # make exchanges a categorical variable 


get_nbbo <- function(type, df, time) {
  arr <- c()
  
  for (ex in levels(df$EX)){
    if (type == 'bid') {
      tmp <- df[df$EX == ex & df$TIME_M <= time] 
      nbbo <- tail(tmp, 1)$BID
    } else if (type == 'offer') {
      tmp <- df[df$EX == ex & df$TIME_M <= time]
      nbbo <- tail(tmp, 1)$ASK
    }
    arr <- c(arr, nbbo)
  }
  
  if (length(arr) > 1) {
    if (type == 'bid') {
      max(arr)
    } else if (type == 'offer') {
      min(arr)
    }
  }
  else {
    arr
  }
}

dirtyOneDays <- as.Date(seq(from=chron(dates='2019-05-01', format = c(dates = "y-m-d")), to=chron(dates='2019-05-31', format = c(dates = "y-m-d"))))
tradingDays = dirtyOneDays[-c(4,5,11,12,18,19,25,26,27)] #this will save me time... hopefully
dailySnapshot = data.frame()
nbbo <- data.table()

for (d in tradingDays) {
  dailySnapshot = C[DATE == d]
  memoryMinuteBins = data.table()
  memoryMinuteBins$TIME <- seq(from=chron(times='09:30:00'), to=chron(times='16:00:00'), by=chron(times='00:01:00'))
  memoryMinuteBins$OFFER <- lapply(memoryMinuteBins$TIME, {function (x) get_nbbo('offer', dailySnapshot, x)})
  memoryMinuteBins$BID <- lapply(memoryMinuteBins$TIME, {function (x) get_nbbo('bid', dailySnapshot, x)})
  memoryMinuteBins$DATE = d
  memoryMinuteBins$MIDPOINT = (as.numeric(memoryMinuteBins$BID)+as.numeric(memoryMinuteBins$OFFER))/2
  nbbo = rbind(nbbo,memoryMinuteBins)
}
copynbbo = nbbo #keep data on hand
nbbo = nbbo[ , deltaOFFER := as.numeric(OFFER) - shift(as.numeric(OFFER)), by = DATE]  
nbbo = nbbo[ , deltaBID := as.numeric(BID) - shift(as.numeric(BID)), by = DATE] 

sum_bins <- function(type, binNumb, dt ) {
  
  fifteenBin <- seq(from=chron(times='09:30:00'), to=chron(times='16:00:00'), by=chron(times='00:15:00'))
  t <- fifteenBin[binNumb]
  minutes <- dt[dt$TIME >= t & dt$TIME < t+chron(times='00:15:00')]
  if (type == 'offer') {
  sum(minutes$deltaOFFER^2, na.rm = TRUE) 
  } else if (type == 'bid') {
    sum(minutes$deltaBID^2, na.rm = TRUE)   
  }
}

varianceNBBOOffer = c()
varianceNBBOBid = c()


for (i in c(1:26)) {
  varianceNBBOOffer = c(varianceNBBOOffer, sum_bins('offer', i, nbbo))
  varianceNBBOBid = c(varianceNBBOBid, sum_bins('bid', i, nbbo))
}
varianceNBBOOffer = varianceNBBOOffer/length(tradingDays)
  varianceNBBOBid = varianceNBBOBid/length(tradingDays)
  
#plot this 
pdf("varianceNBBOOffer.pdf")
plot(varianceNBBOOffer, type = "b", xlab = "bins", ylab = "Variance of ASK NBBO",
     col = "blue")
dev.off()
pdf("varianceNBBOBid.pdf")
plot(varianceNBBOBid, type = "b", xlab = "bins", ylab = "Variance of BID NBBO",
     col = "blue")
dev.off()