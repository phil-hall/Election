partyLong <- c("Conservative","Green","Labour","Liberal Democrat","Plaid Cymru","Scottish National Party","Speaker")
partyShort <- c("Con","Green","Lab","LD","PC","SNP","Spk")

partyLook <- cbind(partyLong,partyShort)

Inc15 <- merge(data[c("pano","Winner10")],partyLook,by.x = "Winner10", by.y = "partyLong")
Inc15$Year <- "2015"
Inc15 <- Inc15[-1]

Inc17 <- merge(data[c("pano","Winner15")],partyLook,by.x = "Winner15", by.y = "partyLong")
Inc17$Year <- "2017"
Inc17 <- Inc17[-1]

Inc <- rbind(Inc15,Inc17)

names(Inc) <- c("pano","Incumbent","Year")

swingData <- merge(swingData,Inc,by = c("pano","Year"))

swingData$Incumbent = ifelse(swingData$Party == swingData$Incumbent,1,0)