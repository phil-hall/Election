library(readxl)
library(sqldf)
library(xlsx)

con <- "http://www.britishelectionstudy.com/wp-content/uploads/2017/07/BES-2017-General-Election-results-file-v1.0.xlsx"

download.file(con,"election2017Data.xlsx",method = "curl")

data <- read_excel("election2017Data.xlsx",sheet = "Data")

con <- "http://www.electoralcalculus.co.uk/electdata_2005nb.txt"

download.file(con,"election2005Data.xlsx",method = "curl")

data2005 <- read.csv("election2005Data.xlsx",sep = ";")

data2005 <- data2005[data2005$Area != 1,]

data2005$Votes05 <- data2005$CON + data2005$LAB + data2005$LIB + data2005$NAT + data2005$MIN + data2005$OTH
data2005$Con05 <- data2005$CON / data2005$Votes05 * 100
data2005$Lab05 <- data2005$LAB / data2005$Votes05 * 100
data2005$LD05 <- data2005$LIB / data2005$Votes05 * 100

data$ConstituencyName <- gsub(",", "", data$ConstituencyName)
data$ConstituencyName <- gsub("-", " ", data$ConstituencyName)
data2005$Name <- gsub("-", " ", data2005$Name)
data2005$Name <- gsub("Hull", "Kingston upon Hull", data2005$Name)

mgsub <- function(pattern, replacement, x, ...) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, ...)
  }
  result
}

data2005$Name <- mgsub(c("North East","North West","South East","South West"),
                      c("NE","NW","SE","SW"), data2005$Name)

mpos <- function(pattern, x) {
  result <- data.frame(matrix(nrow = length(x),ncol = length(pattern)))
  for(i in 1:length(pattern)) {
    result[,i] <- regexpr(pattern[i], x)
  }
  names(result) <- pattern
  result
}

pos <- mpos(c("North","East","South","West","NE","NW","SE","SW","Mid","Central"),data2005$Name)

dirlength <- c(5,4,5,4,2,2,2,2,3,7)

strlen <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:length(dirlength)) {
  strlen[,i] <- dirlength[i]
}

posend <- (pos + strlen - 1) * ((pos + strlen) > strlen)

constlength <- nchar(data2005$Name)

posconst <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:ncol(posend)) {
  posconst[,i] <- constlength == posend[,i]
}

dirs <- c("North","East","South","West","NE","NW","SE","SW","Mid","Central")

spaceconst <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:ncol(posend)) {
  spaceconst[,i] <- substr(data2005$Name, posend[,i]+1, posend[,i]+1) == " "  
}

andconst <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:ncol(posend)) {
  andconst[,i] <- substr(data2005$Name, pos[,i]-4, pos[,i]-2) != "and"  
}

valid <- (spaceconst | posconst) & andconst & pos != 1

pos <- pos * valid

posend <- (pos + strlen - 1) * ((pos + strlen) > strlen)

start <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))
end <- start

for (i in 1:ncol(start)) {
  start[,i] <- substring(data2005$Name,1,pos[,i]-2)
  end[,i] <- substring(data2005$Name,posend[,i]+1)
}

dirmatrix <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:ncol(dirmatrix)) {
  for (j in 1:nrow(dirmatrix)) {
    if (pos[j,i] > 0) {
      dirmatrix[j,i] <- dirs[i] 
    } else {
      dirmatrix[j,i] <- ""
    }
  }
}

final <- data.frame(matrix(nrow = nrow(pos),ncol = ncol(pos)))

for (i in 1:ncol(final)) {
  for (j in 1:nrow(final)) {
    if (pos[j,i] > 0) {
      final[j,i] <- paste0(dirmatrix[j,i]," ",start[j,i],end[j,i]) 
    } else {
      final[j,i] <- ""
    }
  }
}

final <- apply(final,1,max)

final <- mgsub(c("NE","NW","SE","SW"),c("North East","North West","South East","South West"),final)
data2005$Name <- mgsub(c("NE","NW","SE","SW"),c("North East","North West","South East","South West"),data2005$Name)



for (i in 1:length(final)) {
  if (final[i] == "") {
    final[i] <- data2005$Name[i]
  }
}

a <- as.data.frame(cbind(data2005$Name,final))
names(a) <- c("orig","mod")

b <- sqldf("SELECT a.orig, a.mod, b.ConstituencyName FROM a LEFT JOIN data as b on 
          b.ConstituencyName = a.orig or b.ConstituencyName = a.mod" )

v<-b[is.na(b$ConstituencyName),]
w<-as.data.frame(data$ConstituencyName)

write.xlsx(v,"Unmatched.xlsx")

updates <- read.xlsx2("Manual Updates.xlsx",1)

a <- sqldf("SELECT a.*, b.mod as mod2 FROM a as a LEFT JOIN updates as b on 
          a.orig = b.orig" )

b <- sqldf("SELECT a.orig, a.mod, a.mod2, b.ConstituencyName FROM a LEFT JOIN data as b on 
          b.ConstituencyName = a.orig or b.ConstituencyName = a.mod or b.ConstituencyName = a.mod2" )

v<-b[is.na(b$ConstituencyName),]
w<-as.data.frame(data$ConstituencyName)

