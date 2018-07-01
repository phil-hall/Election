Year <- c("2010","2015","2017")
lastYear <- c("2005","2010","2015")

yearMatch <- cbind(Year,lastYear)

votes <- merge(votes, yearMatch, by="Year")
votes <- merge(votes, votes[-6], by.x=c("pano","Party","lastYear"), by.y=c("pano","Party","Year"), suffixes = c("","_prev"))

votes <- votes[-c(3,9)]

voteTotals <- merge(voteTotals, yearMatch, by="Year")
voteTotals <- merge(voteTotals, voteTotals[-5], by.x=c("Party","lastYear"), by.y=c("Party","Year"), suffixes = c("","_prev"))

voteTotals <- voteTotals[-2]