setwd('C:/Users/Owner/Documents/GitHub/Fantasy-Football')
all_hist <- read.csv('prior runs.csv',stringsAsFactors=F)
write.csv(all_hist[-c(1:10),],'prior runs.csv',row.names=F)