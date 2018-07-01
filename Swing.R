swing <- function(x,y = "A") {
  
    up <- x[x$voteShare > x$voteShare_prev,]
    down <- x[x$voteShare < x$voteShare_prev,]
    same <- x[x$voteShare == x$voteShare_prev,]
    
    up$swing <- -log(1 - ((up$voteShare - up$voteShare_prev) / (100 - up$voteShare_prev)))
    down$swing <- log(1 - (1 - down$voteShare / down$voteShare_prev))
    
    if(y != "T") {
    same$swing <- NA
    }
    
    swingData <- rbind(up,down)
    
    swingData$swing[swingData$voteShare == 0] <- NA
    swingData$swing[swingData$voteShare_prev == 0] <- NA
    
    return(swingData)

}

voteSwing <- swing(votes)
totalSwing <- swing(voteTotals,"T")

swingData <- merge(voteSwing,totalSwing[c("Party","Year","swing")],by=c("Party","Year"),suffixes = c("","_nat"))