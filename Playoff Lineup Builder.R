library(XML)
setwd('C:/Users/Owner/Documents/GitHub/Fantasy-Football')

all_games <- lapply(dir('FB Ref',full.name=T), function(x) {
team_tree <- htmlTreeParse(x, useInternal=T)
data.frame(cbind(substr(x,8,11),t(matrix(sapply(xpathSApply(team_tree, paste0('//tr/td[',c(1,5,10,12,18,22,25),']')),function(x) xmlValue(x, trim=T)),7))),stringsAsFactors=F)
})

all_games_df <- do.call(rbind,all_games)
names(all_games_df) <- c('Year','Player','Team','Week','Points_Dec','Passing','Rushing','Receiving')
for (y in c(1,4:8)) all_games_df[,y] <- as.numeric(all_games_df[,y])
all_games_df$decimal_adj <- ((all_games_df$Passing %% ifelse(all_games_df$Passing<0,-25,25))/25) + ((all_games_df$Rushing %% ifelse(all_games_df$Rushing<0,-10,10))/10) + ((all_games_df$Receiving %% ifelse(all_games_df$Receiving<0,-10,10))/10)
all_games_df$Points_Rnd <- round(all_games_df$Points_Dec - all_games_df$decimal_adj,1)
all_games_df$match <- paste0(all_games_df$Player,'-',all_games_df$Year,'-',all_games_df$Week)

lineups <- read.csv('old lineups.csv',stringsAsFactors=F)

lineups$weeks_used <- NA
lineups$Week1 <- NA
lineups$Week2 <- NA
lineups$Both <- NA

for (yr in 2013:2016) {

week_14_mat <- all_games_df$Points_Rnd[match(paste0(lineups$Name[which(lineups$Week=='P1' & lineups$Year==yr)],'-',yr,'-14'),all_games_df$match)]
week_15_mat <- all_games_df$Points_Rnd[match(paste0(lineups$Name[which(lineups$Week=='P1' & lineups$Year==yr)],'-',yr,'-15'),all_games_df$match)]
p1_mat <- week_14_mat + week_15_mat
all_mat <- data.frame('Actual'=lineups$Points[which(lineups$Week=='P1' & lineups$Year==yr)],'w1'=week_14_mat,'w2'=week_15_mat,'both'=p1_mat)
matches <- apply(all_mat,1,function(x) match(x[1],x[2:4]))
lineups$weeks_used[which(lineups$Week=='P1' & lineups$Year==yr)] <- matches
lineups[which(lineups$Week=='P1' & lineups$Year==yr),c('Week1','Week2','Both')] <- all_mat[,2:4]


week_16_mat <- all_games_df$Points_Rnd[match(paste0(lineups$Name[which(lineups$Week=='P2' & lineups$Year==yr)],'-',yr,'-16'),all_games_df$match)]
week_17_mat <- all_games_df$Points_Rnd[match(paste0(lineups$Name[which(lineups$Week=='P2' & lineups$Year==yr)],'-',yr,'-17'),all_games_df$match)]
p2_mat <- week_16_mat + week_17_mat
all_mat <- data.frame('Actual'=lineups$Points[which(lineups$Week=='P2' & lineups$Year==yr)],'w1'=week_16_mat,'w2'=week_17_mat,'both'=p2_mat)
matches <- apply(all_mat,1,function(x) match(x[1],x[2:4]))
lineups$weeks_used[which(lineups$Week=='P2' & lineups$Year==yr)] <- matches
lineups[which(lineups$Week=='P2' & lineups$Year==yr),c('Week1','Week2','Both')] <- all_mat[,2:4]
}

write.csv(lineups[which(lineups$Week=='P1' | lineups$Week=='P2'),],'playoff_split.csv',row.names=F)

table(lineups$Pos[which(is.na(lineups$weeks_used[which(lineups$Week=='P1')]))])

head(all_games_df)

all_games_df[which(all_games_df$Player=='Davante Adams'),]

lineups[which(lineups$Week=='P1' & lineups$Year==2016),]

data.frame(week_13_mat,week_13_mat
head(week_14_mat)
head(p1_mat)

head(lineups)

-2 %% -10