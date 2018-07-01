varlist <- c("pano",
             "ConstituencyName",
             "c11PopulationDensity",
             "c11Male",
             "c11BornUK",
             "leaveHanretty",
             "c11HouseOwned",
             "c11Employed",
             "c11Degree",
             "Region",
             "c11DeprivedNone",
             "c11Deprived4",
             "Winner15",
             "Country",
             "c11BornEngland",
             "c11HouseholdAll65plus",
             "c11Retired",
             "c11HealthVeryGood",
             "c11HealthGood",
             "c11EconomicActive",
             "c11Unemployed16to24",
             "c11NSSECHigherManager",
             "c11NSSECHigherProfessional",
             "c11QualLevel3",
             "c11QualLevel4")

modelData <- merge(swingData,data[varlist],by="pano")

modelData <- modelData[!is.na(modelData$swing),]

modelData$Health <- modelData$c11HealthVeryGood + modelData$c11HealthGood
modelData$NSSEC_High <- modelData$c11NSSECHigherManager + modelData$c11NSSECHigherProfessional
modelData$Education <- modelData$c11QualLevel3 + modelData$c11QualLevel4

fit <- lm(swing ~ swing_nat + Incumbent*Party + c11Unemployed16to24*Party + NSSEC_High*Party + Education*Party, data=modelData)
summary(fit)

modelData$P <- predict(fit)

up <- modelData[modelData$P > 0,]
down <- modelData[modelData$P < 0,]

up$pVote <- (1-exp(-up$P))*(100-up$voteShare_prev)+up$voteShare_prev
down$pVote <- down$voteShare_prev * (1-(1-exp(down$P)))

Preds <- rbind(up,down)

x <- aggregate(pVote ~ pano + Year, data=Preds, max)

x <- merge(x,Preds,by=c("pano","Year"))

maxVotes <- x$pVote.x == x$pVote.y

winners <- x[maxVotes,]

table(winners$Party,winners$Year)