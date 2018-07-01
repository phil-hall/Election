library(readxl)

con <- "http://www.britishelectionstudy.com/wp-content/uploads/2017/07/BES-2017-General-Election-results-file-v1.0.xlsx"

download.file(con,"electionData.xlsx",method = "curl")

data <- read_excel("electionData.xlsx",sheet = "Data")

data <- data[data$Country == "England",]