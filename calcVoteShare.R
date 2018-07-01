library(reshape)

tidy <- function(varlist,varname,removestring,year) {
  
  x <- data[c(1,varlist)]
  x <- melt(as.data.frame(x), 1)
  
  names(x) <- c("pano","Party",varname)
  x$Party <- gsub(removestring, "", x$Party)
  x$Year <- year
  
  return(x)
  
}

votes17 <- tidy(18:25,"votes","Vote17","2017")
votes15 <- tidy(c(47:53,55),"votes","Vote15","2015")
votes10 <- tidy(c(77:83,85),"votes","Vote10","2010")

votes <- rbind(votes10,votes15,votes17)
votes[is.na(votes)] <- 0

totals <- votes[votes$Party == "Total",]
parties <- votes[votes$Party != "Total",]

allParties <- aggregate(parties$votes, by=list(votes=parties$Year,parties$pano), FUN=sum)
names(allParties) <- c("Year","pano","partyVotes")

other <- merge(allParties,totals,by=c("pano","Year"))
other$votes <- other$votes - other$partyVotes
other$Party <- "Other"
other <- other[c("pano","Year","Party","votes")]

votes <- rbind(votes,other)
votes <- merge(votes,totals,by=c("pano","Year"))
votes$voteShare <- votes$votes.x / votes$votes.y * 100
votes <- votes[,c(1:4,7)]
names(votes) <- c("pano","Year","Party","votes","voteShare")

partyTotals <- aggregate(votes ~ Party + Year, data=votes, sum, subset = Party != "Total")
voteTotals <- aggregate(votes ~ Party + Year, data=votes, sum, subset = Party == "Total")

voteTotals <- merge(partyTotals, voteTotals, by = "Year")
voteTotals$voteShare <- voteTotals$votes.x / voteTotals$votes.y * 100
voteTotals <- voteTotals[,c(1:3,6)]
names(voteTotals) <- c("Year","Party","votes","voteShare")

