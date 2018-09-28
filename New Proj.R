library(png)
library(XML)

owners <- c('scott','cory','aj','devon','comp','chad','lucas','perry','matty','seth')
setwd('C:/Users/Owner/Documents/GitHub/Fantasy-Football')
############## Get teams
team_tree <- htmlTreeParse('Teams.txt', useInternal=T)

roster_page <- data.frame(matrix(c(rep(1,3),rep(2,3),rep(3,3),4,1:3,1:3,1:3,1),10,2))
rownames(roster_page) <- owners

rosters <- lapply(1:10, function(x) {
players <- xpathSApply(team_tree, paste0('//tr[',roster_page$X1[x],']/td[',roster_page$X2[x],']/table/tr/td/a/text()'))
cbind(rownames(roster_page)[x],sapply(players, function(x) xmlValue(x)))
})

roster_list <- data.frame(do.call(rbind,rosters),stringsAsFactors=F)
names(roster_list) <- c('Owner','Player')

trades <- read.table('AE__pending trade.txt',stringsAsFactors=F,head=T)
for (j in 1:nrow(trades)) roster_list$Owner[which(roster_list$Player==trades$Player[j])] <- trades$Owner[j]

#######Get all postions
all_files <- dir('Players',full=T)
length(all_files)

empty_chk <- sapply(all_files, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
rnk <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td/text()'))
rnk <- sapply(rnk, function(x) xmlValue(x, trim=T))
rnk[1]
})

proj_files <- names(empty_chk[which(empty_chk!='Sorry there are no weekly projections for this player yet. Please check back shortly.')])
length(proj_files)

all_pos <- sapply(proj_files, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)
substr(xmlValue(xpathSApply(the_tree, '//span[@class="player-info--basic__team"]/text()')[[1]],trim=T),1,2)
})


DEF <- names(which(all_pos=='D,'))
KICK <- names(which(all_pos=='K,'))
FLEX <- names(which(all_pos=='QB' | all_pos=='RB' | all_pos=='WR' | all_pos=='TE'))

#######Get Defense
DEF_Proj <- lapply(DEF, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)

range_c <- match('68% Confidence Interval', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
range <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',range_c,']/text()'))
range <- sapply(range, function(x) xmlValue(x, trim=T))

pts_all_c <- match('Points Allowed', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
pts_all <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',pts_all_c,']/text()'))
pts_all <- sapply(pts_all, function(x) xmlValue(x, trim=T))

fum_c <- match('Fumble Recoveries', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
fum <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',fum_c,']/text()'))
fum <- sapply(fum, function(x) xmlValue(x, trim=T))

sacks_c <- match('Sacks', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
sacks <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',sacks_c,']/text()'))
sacks <- sapply(sacks, function(x) xmlValue(x, trim=T))

INT_c <- match('Interceptions', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
INT <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',INT_c,']/text()'))
INT <- sapply(INT, function(x) xmlValue(x, trim=T))

SAF_c <- match('Safeties', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
SAF <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',SAF_c,']/text()'))
SAF <- sapply(SAF, function(x) xmlValue(x, trim=T))

TD_c <- match('Touchdowns', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
TD <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',TD_c,']/text()'))
TD <- sapply(TD, function(x) xmlValue(x, trim=T))

rnk_c <- match('Opponent Offensive Ranking', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
rnk <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',rnk_c,']/text()'))
rnk <- sapply(rnk, function(x) xmlValue(x, trim=T))

wk <- xpathSApply(the_tree,'//div[1]/table[1]/tbody/tr/td[2]')
wk <- sapply(wk, function(x) xmlValue(x, trim=T))

cbind(wk, gsub('Players/','',substr(x,1,nchar(x)-4)), range, pts_all, sacks, INT, fum, SAF, TD, rnk)
})

DEF_All <- data.frame(do.call(rbind, DEF_Proj),stringsAsFactors=F)
names(DEF_All)[1:2] <- c('Week', 'Player')
DEF_All$Player <- gsub('DST','D/ST',DEF_All$Player)

split <- sapply(gregexpr('-',DEF_All$range),function(x) tail(x,1))
DEF_All$max <- as.numeric(substr(DEF_All$range,split+1,nchar(DEF_All$range)))
DEF_All$min <- as.numeric(substr(DEF_All$range,1,split-1))
DEF_All$std <- (DEF_All$max-DEF_All$min)/2
DEF_All$Var <- DEF_All$std ^ 2

DEF_All[,4:9] <- sapply(DEF_All[,4:9],as.numeric)

pts_cuts <- c(0,.5,6.5,13.5,17.5,26.5,34.5,45.5,Inf)
pts_scores <- c(5,4,3,1,0,-1,-3,-5)

DEF_All$pts_all_grp <- as.numeric(cut(DEF_All$pts_all, pts_cuts, labels=1:8))
DEF_All$pts_all_score <- pts_scores[DEF_All$pts_all_grp]

DEF_All$yrds_est<-DEF_All$pts_all*5.2+230
yrds_cuts <- c(0,99.5,199.5,299.5,349.5,399.5,449.5,499.5,549.5,Inf)
yrds_scores <- c(5,3,2,0,-1,-3,-5,-6,-7)

DEF_All$yrds_all_grp <- as.numeric(cut(DEF_All$yrds_est, yrds_cuts, labels=1:9))
DEF_All$yrds_all_score <- yrds_scores[DEF_All$yrds_all_grp]

DEF_All$Full_Proj <- DEF_All$yrds_all_score+DEF_All$pts_all_score+DEF_All$TD*6+DEF_All$SAF*2+DEF_All$fum*2+DEF_All$INT*2+DEF_All$sacks

DEF_Final <- DEF_All[order(-DEF_All$Full_Proj),]



#######Get Kickers
KCK_Proj <- lapply(KICK, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)

range_c <- match('68% Confidence Interval', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
range <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',range_c,']/text()'))
range <- sapply(range, function(x) xmlValue(x, trim=T))

FGM_c <- match('Field Goals Made', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
FGM <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',FGM_c,']/text()'))
FGM <- sapply(FGM, function(x) xmlValue(x, trim=T))

FGA_c <- match('Field Goals Attempted', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
FGA <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',FGA_c,']/text()'))
FGA <- sapply(FGA, function(x) xmlValue(x, trim=T))

XPM_c <- match('Extra Points Made', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
XPM <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',XPM_c,']/text()'))
XPM <- sapply(XPM, function(x) xmlValue(x, trim=T))

XPA_c <- match('Extra Points Attempted', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
XPA <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',XPA_c,']/text()'))
XPA <- sapply(XPA, function(x) xmlValue(x, trim=T))

proj_c <- match('Standard Fantasy Scoring', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
proj <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',proj_c,']/text()'))
proj <- sapply(proj, function(x) xmlValue(x, trim=T))

rnk_c <- match('Opponent Defensive Ranking', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
rnk <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',rnk_c,']/text()'))
rnk <- sapply(rnk, function(x) xmlValue(x, trim=T))

wk <- xpathSApply(the_tree,'//div[1]/table[1]/tbody/tr/td[2]')
wk <- sapply(wk, function(x) xmlValue(x, trim=T))

cbind(wk, gsub('Players/','',substr(x,1,nchar(x)-4)), range, FGM, FGA, XPM, XPA, proj, rnk)
})

KCK_All <- data.frame(do.call(rbind, KCK_Proj),stringsAsFactors=F)
names(KCK_All)[1:2] <- c('Week', 'Player')

KCK_All[,4:8] <- sapply(KCK_All[,4:8],as.numeric)
KCK_All$XP_miss <- KCK_All$XPA - KCK_All$XPM
KCK_All$FG_miss <- KCK_All$FGA - KCK_All$FGM
KCK_All$Full_Proj <- KCK_All$proj - KCK_All$FG_miss*3 - KCK_All$XP_miss

split <- sapply(gregexpr('-',KCK_All$range),function(x) tail(x,1))
KCK_All$max <- as.numeric(substr(KCK_All$range,split+1,nchar(KCK_All$range)))
KCK_All$min <- as.numeric(substr(KCK_All$range,1,split-1))
KCK_All$std <- (KCK_All$max-KCK_All$min)/2
KCK_All$Var <- KCK_All$std ^ 2

KCK_Final <- KCK_All[order(-KCK_All$Full_Proj),]



#######Get Everyone else
ALL_Proj <- lapply(FLEX, function(x) {
the_tree <- htmlTreeParse(x, useInternal=T)

range_c <- match('68% Confidence Interval', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
range <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',range_c,']/text()'))
range <- sapply(range, function(x) xmlValue(x, trim=T))

proj_c <- match('Standard Fantasy Scoring', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
proj <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',proj_c,']/text()'))
proj <- sapply(proj, function(x) xmlValue(x, trim=T))

rnk_c <- match('Opponent Defensive Ranking', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
rnk <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',rnk_c,']/text()'))
rnk <- sapply(rnk, function(x) xmlValue(x, trim=T))

pos <- substr(xmlValue(xpathSApply(the_tree, '//span[@class="player-info--basic__team"]/text()')[[1]],trim=T),1,2)

wk <- xpathSApply(the_tree,'//div[1]/table[1]/tbody/tr/td[2]')
wk <- sapply(wk, function(x) xmlValue(x, trim=T))

cbind(wk, gsub('Players/','',substr(x,1,nchar(x)-4)), range, proj, rnk, pos)
})


FLEX_All <- data.frame(do.call(rbind, ALL_Proj),stringsAsFactors=F)
names(FLEX_All)[1:2] <- c('Week', 'Player')

FLEX_All[,4] <- sapply(FLEX_All[,4],as.numeric)

split <- sapply(gregexpr('-',FLEX_All$range),function(x) tail(x,1))
FLEX_All$max <- as.numeric(substr(FLEX_All$range,split+1,nchar(FLEX_All$range)))
FLEX_All$min <- as.numeric(substr(FLEX_All$range,1,split-1))
FLEX_All$std <- (FLEX_All$max-FLEX_All$min)/2
FLEX_All$Var <- FLEX_All$std ^ 2

FLEX_Final <- FLEX_All[order(-FLEX_All$proj),]

MIA <- names(which(is.na(sapply(roster_list$Player, function(x) match(x,FLEX_Final$Player)))))
MIA <- names(which(is.na(sapply(MIA, function(x) match(x,DEF_Final$Player)))))
MIA <- names(which(is.na(sapply(MIA, function(x) match(x,KCK_Final$Player)))))
MIA

#######Get current scores
pos_order <- c('QB-1','RB-1','RB-2','WR/RB','WR-1','WR-2','WR-3','TE-1','D/ST','K')
options(warn=-1)
done_scores <- lapply(dir('Teams',full=T), function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

#x <- dir('Teams',full=T)[8]
#xpathSApply(the_tree,'//table[1]/tr/td[5]/text()')

scores <- xpathSApply(the_tree,'//table[1]/tr/td[5]/span/text()')
if (is.null(scores)) {
scores<-xpathSApply(the_tree,'//table[1]/tr/td[5]/text()')
scores[[1]]<-NULL
}
players <- xpathSApply(the_tree, '//table[1]/tr/td[@class="playertablePlayerName"]/a/text()')

scores <- sapply(scores, function(x) xmlValue(x, trim=T))
players <- sapply(players, function(x) xmlValue(x, trim=T))

cbind(gsub('Teams/','',substr(x,1,nchar(x)-4)), pos_order, players[1:10], as.numeric(scores[1:10]))
})
options(warn=0)
#############replace invalid starters
owner_wk_order <- sapply(done_scores, function(x) x[1,1])

find_OwWk <- function(x) which(owner_wk_order==x)
repl_slot <- function(inx, player, slot, bye) {
	repl_item <- done_scores[[inx]]
	if (bye==TRUE) {
		repl_item[slot,3] <- player
		repl_item[slot,4] <- NA}
	else {
		repl_item[slot:10,3] <- c(player,done_scores[[inx]][slot:9,3])
		repl_item[slot:10,4] <- c(NA,done_scores[[inx]][slot:9,4])}
	repl_item
}

done_scores[[find_OwWk('perry-2')]] <- repl_slot(find_OwWk('perry-2'),NA,9,FALSE)
done_scores[[find_OwWk('perry-2')]][10,4] <- -1
done_scores[[find_OwWk('perry-2')]][9,4] <- 0

done_scores[[find_OwWk('lucas-4')]] <- repl_slot(find_OwWk('lucas-4'),'Bears D/ST',9,FALSE)
done_scores[[find_OwWk('aj-4')]] <- repl_slot(find_OwWk('aj-4'),'Ryan Succop',10,TRUE)
#############

results <- data.frame(do.call(rbind, done_scores),stringsAsFactors=F)
names(results) <- c('OwnerWeek', 'Pos', 'Player', 'Final')
split <- sapply(gregexpr('-',results$OwnerWeek),function(x) tail(x,1))
results$Week <- substr(results$OwnerWeek, split+1, nchar(results$OwnerWeek))

resultsF <- merge(results[which(results$Pos!='K' & results$Pos!='D/ST'),], FLEX_Final[,c('Player','Week','proj','std','Var')], by=cbind('Player','Week'), all.x=T)
resultsK <- merge(results[which(results$Pos=='K'),], KCK_Final[,c('Player','Week','proj','std','Var')], by=cbind('Player','Week'), all.x=T)
resultsD <- merge(results[which(results$Pos=='D/ST'),], DEF_Final[,c('Player','Week','Full_Proj','std','Var')], by.x=cbind('Player','Week'), all.x=T)
names(resultsD)[6] <- 'proj'
results_Final <- rbind(resultsF,resultsK,resultsD)

done <- which(!is.na(results_Final$Final))
results_Final$proj[done] <- results_Final$Final[done]
results_Final$std[done] <- 0
results_Final$Var[done] <- 0
results_Final$OwnerWeekPos <- paste0(results_Final$OwnerWeek,results_Final$Pos)


######create proj by week
proj_sq <- function(x) {
w_proj <- x[,4]
names(w_proj) <- x[,1]
miss <- which(is.na(match(1:17,x[,1])))
names(miss) <- miss
miss[1:length(miss)] <- NA
rm_bye <- c(miss,w_proj)
rm_bye <- rm_bye[which(names(rm_bye)!='BYE')]
rm_bye <- rm_bye[which(names(rm_bye)!='')]
as.numeric(rm_bye[order(as.numeric(names(rm_bye)))])
}


KCK_Proj2 <- lapply(unique(KCK_Final$Player), function(x) KCK_Final[KCK_Final$Player==x,c('Week','Player','range','Full_Proj')])
DEF_Proj2 <- lapply(unique(DEF_Final$Player), function(x) DEF_Final[DEF_Final$Player==x,c('Week','Player','range','Full_Proj')])

options(warn=-1)
proj_by_wk_f <- sapply(ALL_Proj, proj_sq)
proj_by_wk_k <- sapply(KCK_Proj2, proj_sq)
proj_by_wk_d <- sapply(DEF_Proj2, proj_sq)
options(warn=0)

pl_names <- c(gsub('Players/','',substr(FLEX,1,nchar(FLEX)-4)),unique(KCK_Final$Player),unique(DEF_Final$Player))
pl_pos <- c(all_pos[which(all_pos!='K,' & all_pos!='D,')],rev(sort(ifelse(all_pos[which(all_pos=='K,' | all_pos=='D,')]=='K,','K','D'))))
proj_by_wk <- cbind(proj_by_wk_f, proj_by_wk_k, proj_by_wk_d)

proj_by_wk <- data.frame(Player=pl_names,Pos=pl_pos,t(proj_by_wk),stringsAsFactors=F)

DEF_Final <- DEF_Final[which(DEF_Final$Week!='BYE'),]
KCK_Final <- KCK_Final[which(KCK_Final$Week!='BYE'),]
FLEX_Final <- FLEX_Final[which(FLEX_Final$Week!='BYE'),]


#save historical
time <- gsub(':','_',file.info('Teams.txt')$mtime)
write.table(DEF_Final, paste0('Historical Proj/D_proj_',time,'.txt'),row.names=F)
write.table(KCK_Final, paste0('Historical Proj/K_proj_',time,'.txt'),row.names=F)
write.table(FLEX_Final, paste0('Historical Proj/FLEX_proj_',time,'.txt'),row.names=F)
write.table(proj_by_wk, paste0('Historical Proj/Grid_proj_',time,'.txt'),row.names=F)

#write for linked files
write.table(DEF_Final, 'D_proj.txt',row.names=F)
write.table(KCK_Final, 'K_proj.txt',row.names=F)
write.table(FLEX_Final, 'FLEX_proj.txt',row.names=F)
write.table(results_Final, 'results.txt',row.names=F)
write.table(roster_list, 'current_rosters.txt',row.names=F)
write.table(proj_by_wk, 'Grid_proj.txt',row.names=F)


###sim output
FLEX_Final$Owner <- roster_list$Owner[match(FLEX_Final$Player,roster_list$Player)]
KCK_Final$Owner <- roster_list$Owner[match(KCK_Final$Player,roster_list$Player)]
DEF_Final$Owner <- roster_list$Owner[match(DEF_Final$Player,roster_list$Player)]

#one replacement
#FA_qb <- t(sapply(1:17, function(w) c('Pos'=paste0('QB-',w),FLEX_Final[which(FLEX_Final$pos=='QB' & FLEX_Final$Week==w)[14],c('Player','proj','Var')])))
#FA_rb <- t(sapply(1:17, function(w) c('Pos'=paste0('RB-',w),FLEX_Final[which(FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[35],c('Player','proj','Var')])))
#FA_wr <- t(sapply(1:17, function(w) c('Pos'=paste0('WR-',w),FLEX_Final[which(FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[45],c('Player','proj','Var')])))
#FA_te <- t(sapply(1:17, function(w) c('Pos'=paste0('TE-',w),FLEX_Final[which(FLEX_Final$pos=='TE' & FLEX_Final$Week==w)[14],c('Player','proj','Var')])))
#FA_d <- t(sapply(1:17, function(w) c('Pos'=paste0('D-',w),DEF_Final[which(DEF_Final$Week==w)[13],c('Player','Full_Proj','Var')])))
#FA_k <- t(sapply(1:17, function(w) c('Pos'=paste0('K-',w),KCK_Final[which(KCK_Final$Week==w)[11],c('Player','Full_Proj','Var')])))

#mix replacement
FA_qb <- t(sapply(1:17, function(w) c('Pos'=paste0('QB-',w),mean(FLEX_Final$proj[which(FLEX_Final$pos=='QB' & FLEX_Final$Week==w)[12:17]]),mean(FLEX_Final$Var[which(FLEX_Final$pos=='QB' & FLEX_Final$Week==w)[12:17]]))))
FA_rb <- t(sapply(1:17, function(w) c('Pos'=paste0('RB-',w),mean(FLEX_Final$proj[which(FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[30:40]]),mean(FLEX_Final$Var[which(FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[30:40]]))))
FA_wr <- t(sapply(1:17, function(w) c('Pos'=paste0('WR-',w),mean(FLEX_Final$proj[which(FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[40:50]]),mean(FLEX_Final$Var[which(FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[40:50]]))))
FA_te <- t(sapply(1:17, function(w) c('Pos'=paste0('TE-',w),mean(FLEX_Final$proj[which(FLEX_Final$pos=='TE' & FLEX_Final$Week==w)[13:18]]),mean(FLEX_Final$Var[which(FLEX_Final$pos=='TE' & FLEX_Final$Week==w)[13:18]]))))
FA_d <- t(sapply(1:17, function(w) c('Pos'=paste0('D-',w),mean(DEF_Final$Full_Proj[which(DEF_Final$Week==w)[12:17]]),mean(DEF_Final$Var[which(DEF_Final$Week==w)[12:17]]))))
FA_k <- t(sapply(1:17, function(w) c('Pos'=paste0('K-',w),mean(KCK_Final$Full_Proj[which(KCK_Final$Week==w)[10:15]]),mean(KCK_Final$Var[which(KCK_Final$Week==w)[10:15]]))))

FA_df <- rbind(FA_qb,FA_rb,FA_wr,FA_te,FA_d,FA_k)
FA_df <- data.frame(matrix(unlist(FA_df),17*6,3),stringsAsFactors=F)
names(FA_df) <- c('WkPos','proj','Var')
FA_df$proj <- as.numeric(FA_df$proj)
FA_df$Var <- as.numeric(FA_df$Var)


full_lineup <- lapply(owners, function(o) {
start_k <- t(sapply(1:17, function(w) c('Pos'='K','Week'=w,'Owner'=o,KCK_Final[which(KCK_Final$Owner==o & KCK_Final$Week==w)[1],c('Player','Full_Proj','Var')])))
start_d <- t(sapply(1:17, function(w) c('Pos'='D','Week'=w,'Owner'=o,DEF_Final[which(DEF_Final$Owner==o & DEF_Final$Week==w)[1],c('Player','Full_Proj','Var')])))

start_qb <- t(sapply(1:17, function(w) c('Pos'='QB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='QB' & FLEX_Final$Week==w)[1],c('Player','proj','Var')])))
start_te <- t(sapply(1:17, function(w) c('Pos'='TE','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='TE' & FLEX_Final$Week==w)[1],c('Player','proj','Var')])))
start_rb <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='RB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[1:2],c('Player','proj','Var')])))
start_wr <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='WR','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[1:3],c('Player','proj','Var')])))

flex_rb <- t(sapply(1:17, function(w) c('Pos'='RB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[3],c('Player','proj','Var')])))
flex_wr <- t(sapply(1:17, function(w) c('Pos'='RB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[4],c('Player','proj','Var')])))

fl_rb_proj <- ifelse(is.na(unlist(flex_rb[,'proj'])),0,unlist(flex_rb[,'proj']))
fl_wr_proj <- ifelse(is.na(unlist(flex_wr[,'proj'])),0,unlist(flex_wr[,'proj']))

flex_pick <- ifelse(fl_rb_proj >= fl_wr_proj, 'RB', 'WR')
start_fl <- rbind(flex_rb[which(flex_pick=='RB'),],flex_wr[which(flex_pick=='WR'),])

colnames(start_d)[5] <- 'proj'
colnames(start_k)[5] <- 'proj'

all_df <- rbind(start_qb,start_rb,start_fl,start_wr,start_te,start_d,start_k)
all_df  <- data.frame(apply(all_df,2,unlist),stringsAsFactors=F)
all_df$proj <- as.numeric(all_df$proj)
all_df$Var <- as.numeric(all_df$Var)
all_df$Week <- as.numeric(all_df$Week)

pos_week <- paste0(all_df$Pos,'-',all_df$Week)
free_agents <- pos_week[which(all_df$proj<FA_df$proj[match(pos_week,FA_df$WkPos)] | is.na(all_df$proj))]
all_df$Player[which(all_df$proj<FA_df$proj[match(pos_week,FA_df$WkPos)] | is.na(all_df$proj))] <- 'Free Agent'

all_df[which(all_df$proj<FA_df$proj[match(pos_week,FA_df$WkPos)] | is.na(all_df$proj)),c('proj','Var')] <- FA_df[match(free_agents,FA_df$WkPos),c('proj','Var')]
all_df
})

full_lineup <- do.call(rbind,full_lineup)
full_lineup$Owner <- match(full_lineup$Owner,owners)
full_lineup <- full_lineup[order(full_lineup$Week),]
full_lineup <- full_lineup[order(full_lineup$Owner),]

results_Final$Owner <- match(sapply(strsplit(results_Final$OwnerWeek,'-'),function(j) j[1]),owners)
thurs_check <- ifelse(as.numeric(format(Sys.time(),'%u')) >= 4 |  as.numeric(format(Sys.time(),'%u')) == 1,0,1)
for (o in 1:length(owners)) {for (w in 1:max(results_Final$Week)-thurs_check) full_lineup[which(full_lineup$Owner==o & full_lineup$Week==w),c('Player','proj','Var')] <- results_Final[which(results_Final$Owner==o & results_Final$Week==w),c('Player','proj','Var')]}
full_lineup$proj <- as.numeric(full_lineup$proj)

#all_scores <- aggregate(cbind(proj,Var)~Owner+Week,full_lineup,sum)
#all_scores[match(sched$tm_wk, paste0(all_scores$Owner,'-',all_scores$Week)),c('proj','Var')]

sched <- read.table('sched.txt',stringsAsFactors=F,sep=';',head=T)
sched$Tm_Inx <- (sched$team-1) * 17 + sched$wk
sched$Opp_Inx <- (sched$opp-1) * 17 + sched$wk

sim_cnt <- 50000

full_proj <- sapply(1:nrow(full_lineup), function(x) {
pts <- rnorm(sim_cnt ,full_lineup$proj[x],sqrt(full_lineup$Var[x]))
gap <- mean(ifelse(pts<0,0,pts)) - full_lineup$proj[x]
final_pts <- ifelse(pts - gap<0,0,pts - gap)
final_pts
})

proj_totals <- sapply(seq(1,1700,10), function(z) {
apply(full_proj[,z:(z+9)],1,sum)
})

wins <- sapply(1:nrow(sched), function(y) {
ifelse(proj_totals[,sched$Tm_Inx[y]] > proj_totals[,sched$Opp_Inx[y]],1,0)
})

sched$win_prob <- apply(wins,2,mean)
sched$PF <- apply(proj_totals,2,mean)[sched$Tm_Inx]
sched$PA <- apply(proj_totals,2,mean)[sched$Opp_Inx]

point_totals <- sapply(seq(1,nrow(sched),13), function(z) {
apply(proj_totals[,sched$Tm_Inx[z:(z+12)]],1,sum)
})

win_totals <- sapply(seq(1,130,13), function(z) {
apply(wins[,z:(z+12)],1,sum)
})

#playoffs
dp_tm <- c(1,4,6,8,10)
hk_tm <- c(2,3,5,7,9)

div_dp_stnd <- win_totals[,dp_tm] + point_totals[,dp_tm]/10000
div_hk_stnd <- win_totals[,hk_tm] + point_totals[,hk_tm]/10000

div_dp_winner <- apply(apply(div_dp_stnd,1,rank),2,function(x) which(x==5))
div_hk_winner <- apply(apply(div_hk_stnd,1,rank),2,function(x) which(x==5))

div_dp_score <- apply(div_dp_stnd,1,max)
div_hk_score <- apply(div_hk_stnd,1,max)

seed_1 <- ifelse(div_dp_score>div_hk_score,dp_tm[div_dp_winner],hk_tm[div_hk_winner])
seed_2 <- ifelse(div_dp_score>div_hk_score,hk_tm[div_hk_winner],dp_tm[div_dp_winner])

wc_seeding <- win_totals+point_totals/10000
for (x in 1:sim_cnt) {
wc_seeding[x,seed_1[x]] <- NA
wc_seeding[x,seed_2[x]] <- NA
}

seed_3 <- apply(apply(wc_seeding,1,rank),2,function(x) which(x==8))
seed_4 <- apply(apply(wc_seeding,1,rank),2,function(x) which(x==7))

playoffs_1 <- sapply(seq(14,170,17), function(z) {
apply(proj_totals[,c(z,z+1)],1,sum)
})

playoffs_2 <- sapply(seq(16,170,17), function(z) {
apply(proj_totals[,c(z,z+1)],1,sum)
})

top_semi <- sapply(1:sim_cnt, function(y) ifelse(playoffs_1[y,seed_1[y]] > playoffs_1[y,seed_4[y]], seed_1[y], seed_4[y]))
low_semi <- sapply(1:sim_cnt, function(y) ifelse(playoffs_1[y,seed_2[y]] > playoffs_1[y,seed_3[y]], seed_2[y], seed_3[y]))
champ <- sapply(1:sim_cnt, function(y) ifelse(playoffs_2[y,top_semi[y]] > playoffs_2[y,low_semi[y]], top_semi[y], low_semi[y]))

playoff <- table(factor(c(seed_1,seed_2,seed_3,seed_4),c(1:10)))
semi_win <- table(factor(c(top_semi,low_semi),c(1:10)))
champ_prob <- table(factor(champ,c(1:10)))
po_proj <- cbind(playoff,semi_win,champ_prob)/sim_cnt

po_1_avg <- apply(playoffs_1,2,mean)
po_2_avg <- apply(playoffs_2,2,mean)

win_proj <- round(apply(win_totals,2,mean),0)
avg_pf <- apply(point_totals,2,mean)
q_standings <- win_proj+avg_pf/10000

hk_top <- hk_tm[order(q_standings[hk_tm])[5]]
dp_top <- dp_tm[order(q_standings[dp_tm])[5]]

q_wc_standings <- q_standings
q_wc_standings[hk_top] <- NA
q_wc_standings[dp_top] <- NA

wc_1 <- order(-q_wc_standings)[1]
wc_2 <- order(-q_wc_standings)[2]

div_1 <- ifelse(q_standings[hk_top] > q_standings[dp_top], hk_top, dp_top)
div_2 <- ifelse(q_standings[hk_top] > q_standings[dp_top], dp_top, hk_top)

proj_WL <- paste0(win_proj,'-',13-win_proj)
proj_PF <- round(sapply(1:length(owners), function(o) sum(sched$PF[which(sched$team==o)]))/13,1)
proj_PA <- round(sapply(1:length(owners), function(o) sum(sched$PA[which(sched$team==o)]))/13,1)

hk_order <- hk_tm[order(q_standings[hk_tm])]
dp_order <- dp_tm[order(q_standings[dp_tm])]


#dev.new(width=900, height=1600)
png('projection.png',width=900, height=1600)
par(mar=c(1,5,1,1))
pic_mx <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11), 4, 3, byrow = TRUE)
layout(pic_mx)

for (i in 1:10) {
barplot(rev(c(sched$win_prob[which(sched$team==i)],po_proj[i,])),horiz=T,names.arg=rev(c(owners[sched$opp[which(sched$team==i)]],'playoffs','semi','champ')),las=1,xlim=c(0,1),main=owners[i],axes=F,border=NA,col='lightGreen',cex.main=2,cex.names=1.3)
for (j in 16:1) text(.1,seq(18.7,0,-1.2)[j],paste0(round(c(sched$win_prob[which(sched$team==i)],po_proj[i,])[j]*100,1),'%'),cex=1.5)
for (j in 13:1) text(.45,seq(18.7,0,-1.2)[j],paste0(round(c(sched$PF[which(sched$team==i)])[j],1)),cex=1.5,,adj = c(1,.5))
for (j in 13:1) text(.5,seq(18.7,0,-1.2)[j],paste0(round(c(sched$PA[which(sched$team==i)])[j],1)),cex=1.5,adj = c(0,.5))
for (j in 13:1) text(.48,seq(18.7,0,-1.2)[j],'-',cex=1.5,adj = c(.6,.4))
text(.48,1.9,round(po_1_avg[i],1),cex=1.5)
text(.48,.7,round(po_2_avg[i],1),cex=1.5)
arrows(0,0,1,0,len=0)
arrows(0,3.7,1,3.7,len=0)
}

plot(0,type='n',axes=FALSE,ann=FALSE,xlim=c(0,20),ylim=c(.5,12))
text(c(rep(1,5),rep(2.5,5),rep(4.5,5),rep(7,5)),rep(1:5,4),c(owners[hk_order],proj_WL[hk_order],proj_PF[hk_order],proj_PA[hk_order]),cex=2)
text(c(rep(1,5),rep(2.5,5),rep(4.5,5),rep(7,5)),rep(7:11,4),c(owners[dp_order],proj_WL[dp_order],proj_PF[dp_order],proj_PA[dp_order]),cex=2)
text(c(2.5,4.5,7),rep(11.7,3),c('W-L','PF','PA'),cex=1.2)
text(c(2.5,4.5,7),rep(5.7,3),c('W-L','PF','PA'),cex=1.2)

arrows(10,10,15,10,len=0)
arrows(10,7,15,7,len=0)
arrows(10,4,15,4,len=0)
arrows(10,1,15,1,len=0)
arrows(15,8,20,8,len=0)
arrows(15,3,20,3,len=0)
arrows(16,5,20,5,len=0)

arrows(15,10,15,7,len=0)
arrows(15,4,15,1,len=0)
arrows(20,8,20,3,len=0)

text(11,11,owners[div_1],cex=2)
text(11,8,owners[wc_2],cex=2)
text(11,5,owners[wc_1],cex=2)
text(11,2,owners[div_2],cex=2)

q_top <- ifelse(po_1_avg[div_1] > po_1_avg[wc_2],div_1,wc_2)
q_low <- ifelse(po_1_avg[div_2] > po_1_avg[wc_1],div_2,wc_1)
q_champ <- ifelse(po_2_avg[q_top] > po_2_avg[q_low],q_top,q_low)

text(16,9,owners[q_top],cex=2)
text(16,4,owners[q_low],cex=2)
text(17,6,owners[q_champ],cex=2.5)

text(15,12,paste0('As of ',format(file.info('Teams.txt')$mtime, "%a %b %d %r")),cex=2)


dev.off()

library(twitteR)
setup_twitter_oauth('3OFet2Lrb7SM3P5xRIS4mYcYT', 'lUY751Alq4eneCod71LwCG3L4PT4L20PMET8sGqKgovrZOfbdg', access_token='981969608277135360-7w95AsxJKN6gbCBogRcLiawZ2znXoIs', access_secret='JEmeS2Gs2TavP5DEwwJgJWGH0ryzjHZBpGqpZG6jk9ZIK')


last_twt <- updateStatus('New Simulation Complete! @reinhurdler @CompTwinB',mediaPath='projection.png')


#apply(win_totals,2,mean)
#table(win_totals[,6])
#table(apply(win_totals, 1, sum))
#apply(point_totals,2,mean)