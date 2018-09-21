library(XML)

owners <- c('scott','cory','aj','devon','comp','chad','lucas','perry','matty','seth')
############## Get teams
team_tree <- htmlTreeParse('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Teams.txt', useInternal=T)

roster_page <- data.frame(matrix(c(rep(1,3),rep(2,3),rep(3,3),4,1:3,1:3,1:3,1),10,2))
rownames(roster_page) <- owners

rosters <- lapply(1:10, function(x) {
players <- xpathSApply(team_tree, paste0('//tr[',roster_page$X1[x],']/td[',roster_page$X2[x],']/table/tr/td/a/text()'))
cbind(rownames(roster_page)[x],sapply(players, function(x) xmlValue(x)))
})

roster_list <- data.frame(do.call(rbind,rosters),stringsAsFactors=F)
names(roster_list) <- c('Owner','Player')

#######Get all postions
setwd('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Players/')

all_files <- dir()
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
#wk <- wk[-c(length(wk))]

cbind(wk, substr(x,1,nchar(x)-4), range, pts_all, sacks, INT, fum, SAF, TD, rnk)
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
#wk <- wk[-c(length(wk))]

cbind(wk, substr(x,1,nchar(x)-4), range, FGM, FGA, XPM, XPA, proj, rnk)
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
#wk <- wk[-c(length(wk))]

cbind(wk, substr(x,1,nchar(x)-4), range, proj, rnk, pos)
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
setwd('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Teams')
pos_order <- c('QB-1','RB-1','RB-2','WR/RB','WR-1','WR-2','WR-3','TE-1','D/ST','K')
options(warn=-1)
done_scores <- lapply(dir(), function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

scores <- xpathSApply(the_tree,'//table[1]/tr/td[5]/span/text()')
if (is.null(scores)) {
scores<-xpathSApply(the_tree,'//table[1]/tr/td[5]/text()')
scores[[1]]<-NULL
}
players <- xpathSApply(the_tree, '//table[1]/tr/td[@class="playertablePlayerName"]/a/text()')

scores <- sapply(scores, function(x) xmlValue(x, trim=T))
players <- sapply(players, function(x) xmlValue(x, trim=T))

cbind(substr(x,1,nchar(x)-4), pos_order, players[1:10], as.numeric(scores[1:10]))
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

#done_scores[[find_OwWk('scott-6')]][,4] <- c(done_scores[[find_OwWk('scott-6')]][1:5,4],9,done_scores[[find_OwWk('scott-6')]][6:9,4])
#done_scores[[find_OwWk('matty-11')]] <- repl_slot(find_OwWk('matty-11'),NA,7,TRUE)
#done_scores[[find_OwWk('aj-8')]]
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

#ALL_Proj[[78]]<-NULL
#pl_pos<-pl_pos[-c(78)]
#pl_names<-pl_names[-c(78)]

options(warn=-1)
proj_by_wk_f <- sapply(ALL_Proj, proj_sq)
proj_by_wk_k <- sapply(KCK_Proj2, proj_sq)
proj_by_wk_d <- sapply(DEF_Proj2, proj_sq)
options(warn=0)

#which(sapply(proj_by_wk_f,length)==18)
#ALL_Proj[[128]]

pl_names <- c(substr(FLEX,1,nchar(FLEX)-4),unique(KCK_Final$Player),unique(DEF_Final$Player))
pl_pos <- c(all_pos[which(all_pos!='K,' & all_pos!='D,')],rev(sort(ifelse(all_pos[which(all_pos=='K,' | all_pos=='D,')]=='K,','K','D'))))
proj_by_wk <- cbind(proj_by_wk_f, proj_by_wk_k, proj_by_wk_d)

proj_by_wk <- data.frame(Player=pl_names,Pos=pl_pos,t(proj_by_wk),stringsAsFactors=F)
#current_wk <- max(as.numeric(results_Final$Week))

#proj_by_wk$Full_avg <- apply(proj_by_wk[,3:19], 1, mean, na.rm=T)
#proj_by_wk$RORS_avg <- apply(proj_by_wk[,(3+current_wk):15], 1, mean, na.rm=T)
#proj_by_wk$Playoff_avg <- apply(proj_by_wk[,16:19], 1, mean, na.rm=T)

#pos_order <- c('QB','RB','WR','TE','D','K')
#pos_avg_st <- c(5, 13, 18, 5, 5, 5)
#proj_by_wk$PRnk_RORS <- NA

#re_rank <- function(x) which(proj_by_wk$Pos==x)[order(-proj_by_wk$RORS_avg[which(proj_by_wk$Pos==x)])]
#for (x in pos_order) {proj_by_wk$PRnk_RORS[re_rank(x)] <- 1:length(re_rank(x))}

#RORS_P_avg <- sapply(1:6, function(x) proj_by_wk$RORS_avg[which(proj_by_wk$Pos==pos_order[x] & proj_by_wk$PRnk_RORS==pos_avg_st[x])])
#proj_by_wk$RORS_P_avg <- RORS_P_avg[match(proj_by_wk$Pos,pos_order)]

#row.names(proj_by_wk) <- NULL

#str(proj_by_wk)


DEF_Final <- DEF_Final[which(DEF_Final$Week!='BYE'),]
KCK_Final <- KCK_Final[which(KCK_Final$Week!='BYE'),]
FLEX_Final <- FLEX_Final[which(FLEX_Final$Week!='BYE'),]

#save historical
setwd('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Players')
time <- gsub(':','_',file.info(dir())$mtime[1])
write.table(DEF_Final, paste0('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Historical Proj/D_proj_',time,'.txt'),row.names=F)
write.table(KCK_Final, paste0('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Historical Proj/K_proj_',time,'.txt'),row.names=F)
write.table(FLEX_Final, paste0('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Historical Proj/FLEX_proj_',time,'.txt'),row.names=F)
write.table(proj_by_wk, paste0('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Historical Proj/Grid_proj_',time,'.txt'),row.names=F)

#write for linked files
write.table(DEF_Final, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/D_proj.txt',row.names=F)
write.table(KCK_Final, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/K_proj.txt',row.names=F)
write.table(FLEX_Final, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/FLEX_proj.txt',row.names=F)
write.table(results_Final, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/results.txt',row.names=F)
write.table(roster_list, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/current_rosters.txt',row.names=F)
write.table(proj_by_wk, 'C:/Users/A097092/Desktop/Extra/Fantasy FB research/Grid_proj.txt',row.names=F)
