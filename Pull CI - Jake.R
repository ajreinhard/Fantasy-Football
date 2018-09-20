library(XML)

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

all_proj <- lapply(proj_files, function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

col <- match('68% Confidence Interval', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
range <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',col,']/text()'))
final <- sapply(range, function(x) xmlValue(x, trim=T))

col2 <- match('FanDuel Fantasy Points', xpathSApply(the_tree,'//table[2]/thead/tr/th/@title'))
FDpts <- xpathSApply(the_tree, paste0('//table[2]/tbody/tr/td[',col2,']/text()'))
finalFD <- sapply(FDpts, function(x) xmlValue(x, trim=T))

pos <- substr(xmlValue(xpathSApply(the_tree, '//span[@class="player-info--basic__team"]/text()')[[1]],trim=T),1,2)

wk <- xpathSApply(the_tree,'//div[1]/table[1]/tbody/tr/td[2]')
wk <- sapply(wk, function(x) xmlValue(x, trim=T))

cbind(wk, substr(x,1,nchar(x)-4), pos, final, finalFD)
})

all_proj_df <- data.frame(do.call(rbind, all_proj),stringsAsFactors=F)
names(all_proj_df) <- c('Week', 'Player','Pos', 'Proj', 'FD_Proj')

split <- sapply(gregexpr('-',all_proj_df$Proj),function(x) tail(x,1))
all_proj_df$max <- as.numeric(substr(all_proj_df$Proj,split+1,nchar(all_proj_df$Proj)))
all_proj_df$min <- as.numeric(substr(all_proj_df$Proj,1,split-1))
all_proj_df$std <- (all_proj_df$max-all_proj_df$min)/2
all_proj_df$mean <- all_proj_df$max-all_proj_df$std

all_proj_df$Pos[which(all_proj_df$Pos=='K,')] <- 'K'
all_proj_df$Pos[which(all_proj_df$Pos=='D,')] <- 'D'
all_proj_df$Player <- gsub('DST','D/ST',all_proj_df$Player)

#score D as FanDuel
all_proj_df$mean <- as.numeric(ifelse(all_proj_df$Pos=='D', all_proj_df$FD_Proj, all_proj_df$mean))

######view proj other weeks
#view <- all_proj_df[which(all_proj_df$Pos=='D' & all_proj_df$Week==2),]
#view[order(-view$mean),]

team_tree <- htmlTreeParse('C:/Users/A097092/Desktop/Extra/Fantasy FB research/Teams Jake.txt', useInternal=T)

roster_page <- data.frame(matrix(c(rep(1,3),rep(2,3),rep(3,3),4,1:3,1:3,1:3,1),10,2))
rownames(roster_page) <- c('andrew','jake','breanna','joseph','alyssa','chris','jordan','alex','colin','paul')

rosters <- lapply(1:10, function(x) {
players <- xpathSApply(team_tree, paste0('//tr[',roster_page$X1[x],']/td[',roster_page$X2[x],']/table/tr/td/a/text()'))
cbind(rownames(roster_page)[x],sapply(players, function(x) xmlValue(x)))
})

roster_list <- data.frame(do.call(rbind,rosters),stringsAsFactors=F)
names(roster_list) <- c('Owner','Player')

##############trade
#roster_list<-roster_list[which(roster_list$Player!='Trey Burton'),]
#roster_list$Owner[which(roster_list$Player=='Leonard Fournette')] <- 'aj'

proj_full <- merge(roster_list,all_proj_df,by='Player',all.x=T)

#proj_full$Owner[which(proj_full$Player=='Jarvis Landry' & proj_full$Week==8)] <- 'cory'
#proj_full$Owner[which(proj_full$Player=='Sterling Shepard' & proj_full$Week==8)] <- 'devon'
#proj_full$Owner[which(proj_full$Player=='Doug Martin' & proj_full$Week==8)] <- 'devon'

#na check for players on ESPN roster, but not NumberFire
proj_full[which(is.na(proj_full$mean)==T),]

#build starting lineups
pos_order <- c('RB','WR','QB','TE','D','K')
pos_start <- c(3,3,1,2,1,1)
pos_min <- c(2,2,1,1,1,1)
rep_rnk <- c(35, 35, 15, 15, 15, 12)
names(pos_start) <- pos_order

#proj_full <- proj_full[which(proj_full$Week!='BYE'),]

bye_list <- lapply(rownames(roster_page), function(o) sapply(4:12, function(w) sapply(pos_order, function(p) nrow(proj_full[which(proj_full$Week==w & proj_full$Owner==o & proj_full$mean!=0 & proj_full$Pos==p),]))))
need_list <- lapply(bye_list, function(t) ifelse(pos_min-t<0,0,pos_min-t))
all_needs <- lapply(need_list, function(t) rbind(ceiling(which(t==1)/6)+3,which(t==1)-(floor((which(t==1)-1)/6)*6)))

###########manual add needs
#all_needs[[9]] <- cbind(all_needs[[9]],matrix(c(11,2),2,1,byrow=T))
#all_needs[[5]] <- cbind(all_needs[[5]],matrix(c(2:17,rep(4,16)),2,16,byrow=T))
#all_needs[[5]] <- cbind(all_needs[[5]],matrix(c(1:3,13:17,rep(6,8)),2,8,byrow=T))
#all_needs[[8]] <- cbind(all_needs[[8]],matrix(c(11,2),2,1,byrow=T))
all_needs[[3]] <- cbind(all_needs[[3]],matrix(c(3,6),2,1,byrow=T))
all_needs[[6]] <- cbind(all_needs[[6]],matrix(c(3,1),2,1,byrow=T))

#start FA
for (o in 1:10) { 
for (n in 1:ncol(all_needs[[o]])) {

if (length(all_needs[[o]])!=0) {
wk <- all_needs[[o]][1,n]
pos <- pos_order[all_needs[[o]][2,n]]
rnk <- rep_rnk[all_needs[[o]][2,n]]
}

all_list <- all_proj_df[which(all_proj_df$Pos==pos & all_proj_df$Week==wk),]
rep_plyr <- all_list[order(-all_list$mean)[rnk],]
rep_plyr$Owner <- rownames(roster_page)[o]
rep_plyr$Player <- paste0(rep_plyr$Player,'^')
proj_full <- rbind(rep_plyr, proj_full)
}}


order_list <- lapply(1:10,function(o) lapply(pos_order, function(p) lapply(1:17, function(w) {
pos_own <- proj_full[which(proj_full$Owner==rownames(roster_page)[o] & proj_full$Pos==p & proj_full$Week==w),]
pos_own <- pos_own[order(-pos_own$mean),]
pos_own <- pos_own[seq(1,pos_start[p]),]
pos_own$mean[which(is.na(pos_own$mean))] <- 0
pos_own
})))

#choose flex
for (o in 1:10) { 
for (w in 1:17) {
if ((order_list[[o]][[1]][[w]]$mean[3]>order_list[[o]][[2]][[w]]$mean[3]) & ((order_list[[o]][[1]][[w]]$mean[3]>order_list[[o]][[4]][[w]]$mean[2]) | (is.na(order_list[[o]][[4]][[w]]$mean[2]))))
	{order_list[[o]][[2]][[w]] <- order_list[[o]][[2]][[w]][1:2,]
	order_list[[o]][[4]][[w]] <- order_list[[o]][[4]][[w]][1,]}
else if ((order_list[[o]][[2]][[w]]$mean[3]>order_list[[o]][[1]][[w]]$mean[3]) & ((order_list[[o]][[2]][[w]]$mean[3]>order_list[[o]][[4]][[w]]$mean[2]) | (is.na(order_list[[o]][[4]][[w]]$mean[2]))))

	{order_list[[o]][[1]][[w]] <- order_list[[o]][[1]][[w]][1:2,]
	order_list[[o]][[4]][[w]] <- order_list[[o]][[4]][[w]][1,]}
else
	{order_list[[o]][[1]][[w]] <- order_list[[o]][[1]][[w]][1:2,]
	order_list[[o]][[2]][[w]] <- order_list[[o]][[2]][[w]][1:2,]}
}}

starters_df <- do.call(rbind,do.call(rbind,do.call(rbind,order_list)))
lapply(which(starters_df$mean==0),function(x) starters_df[c((x-1):(x+1)),])

#begin full sim
smple <- 100000

scores <- lapply(rownames(roster_page), function(o) sapply(1:17, function(w) {
apply(sapply(1:9, function(x) {
pl_est <- rnorm(smple, starters_df$mean[which(starters_df$Owner==o & starters_df$Week==w)[x]], starters_df$std[which(starters_df$Owner==o & starters_df$Week==w)[x]])
ifelse(pl_est<0,0,pl_est)
}),1,sum)
}))


makescore <- function(x) sapply(x, function(y) rep(y,smple))

scores[[1]][,1:2] <- makescore(c(114.54,98.2))
scores[[2]][,1:2] <- makescore(c(107.76,76.3))
scores[[3]][,1:2] <- makescore(c(88.2,80.7))
scores[[4]][,1:2] <- makescore(c(92.96,122))
scores[[5]][,1:2] <- makescore(c(79.32,127.1))
scores[[6]][,1:2] <- makescore(c(94.34,89.5))
scores[[7]][,1:2] <- makescore(c(111.06,84.2))
scores[[8]][,1:2] <- makescore(c(106.92,103.1))
scores[[9]][,1:2] <- makescore(c(131.78,78.8))
scores[[10]][,1:2] <- makescore(c(113.54,74))

#bring sched and score reg season
sched <- read.table('C:/Users/A097092/Desktop/Extra/Fantasy FB research/sched - Jake.txt',stringsAsFactors=F,sep=';',head=T)

win_loss <- lapply(1:10, function(t) sapply(1:13, function(g) ifelse(scores[[sched$opp[which(sched$team==t)[g]]]][,g]<scores[[t]][,g],1,0)))
w_l_hist <- do.call(rbind,lapply(win_loss, function(x) table(factor(apply(x,1,sum),levels=0:13))/smple))
win_pct <- do.call(rbind,lapply(win_loss, function(x) apply(x,2,mean)))

win_tot <- lapply(win_loss, function(x) apply(x,1,sum))
pt_tot <- lapply(scores, function(x) apply(x[,1:13],1,sum))

#playoffs
dp_tm <- c(1,2,3,4,5)
hk_tm <- c(6,7,8,9,10)

div_dp_w <- rbind(win_tot[[1]],win_tot[[4]],win_tot[[6]],win_tot[[8]],win_tot[[10]])
div_hk_w <- rbind(win_tot[[2]],win_tot[[3]],win_tot[[5]],win_tot[[7]],win_tot[[9]])
div_dp_p <- rbind(pt_tot[[1]],pt_tot[[4]],pt_tot[[6]],pt_tot[[8]],pt_tot[[10]])
div_hk_p <- rbind(pt_tot[[2]],pt_tot[[3]],pt_tot[[5]],pt_tot[[7]],pt_tot[[9]])

#two<-ifelse(div_dp_stnd[2,]>div_dp_stnd[3,],div_dp_stnd[2,],div_dp_stnd[3,])
#table(ifelse(div_dp_stnd[5,]>two,1,0))/smple
#table(ifelse(div_dp_stnd[1,]>div_dp_stnd[5,],1,0))/smple
#table(ifelse(div_dp_stnd[2,]>div_dp_stnd[5,],1,0))/smple
#table(ifelse(div_dp_stnd[3,]>div_dp_stnd[5,],1,0))/smple
#table(ifelse(div_dp_stnd[4,]>div_dp_stnd[5,],1,0))/smple
#table(div_dp_winner)/smple

div_dp_stnd <- div_dp_w+div_dp_p/10000
div_hk_stnd <- div_hk_w+div_hk_p/10000

div_dp_winner <- apply(apply(div_dp_stnd,2,rank),2,function(x) which(x==5))
div_hk_winner <- apply(apply(div_hk_stnd,2,rank),2,function(x) which(x==5))

div_dp_score <- apply(div_dp_stnd,2,max)
div_hk_score <- apply(div_hk_stnd,2,max)

seed_1 <- ifelse(div_dp_score>div_hk_score,dp_tm[div_dp_winner],hk_tm[div_hk_winner])
seed_2 <- ifelse(div_dp_score>div_hk_score,hk_tm[div_hk_winner],dp_tm[div_dp_winner])

wc_seeding <- do.call(rbind,win_tot)+do.call(rbind,pt_tot)/10000
for (x in 1:smple) {
wc_seeding[seed_1[x],x] <- NA
wc_seeding[seed_2[x],x] <- NA
}

seed_3 <- apply(apply(wc_seeding,2,rank),2,function(x) which(x==8))
seed_4 <- apply(apply(wc_seeding,2,rank),2,function(x) which(x==7))

top_semi <- sapply(1:smple,function(x) ifelse(sum(scores[[seed_1[x]]][x,14:15])>sum(scores[[seed_4[x]]][x,14:15]),seed_1[x],seed_4[x]))
low_semi <- sapply(1:smple,function(x) ifelse(sum(scores[[seed_2[x]]][x,14:15])>sum(scores[[seed_3[x]]][x,14:15]),seed_2[x],seed_3[x]))
champ <- sapply(1:smple,function(x) ifelse(sum(scores[[top_semi[x]]][x,16:17])>sum(scores[[low_semi[x]]][x,16:17]),top_semi[x],low_semi[x]))

repl_zero <- function(y) sapply(1:10,function(x) ifelse(length(which(names(y)==x))==1,y[which(names(y)==x)],0))

#write final results
winner <- repl_zero(table(champ)/smple)
final_g <- repl_zero(table(c(top_semi,low_semi))/smple)
playoff <- repl_zero(table(c(seed_1,seed_2,seed_3,seed_4))/smple)
div_ch <- repl_zero(table(c(seed_1,seed_2))/smple)
results <- cbind(w_l_hist,playoff,div_ch,final_g,winner)

write.table(results,'C:/Users/A097092/Desktop/Extra/Fantasy FB research/game_results_jake.txt',row.names=F)
write.table(win_pct,'C:/Users/A097092/Desktop/Extra/Fantasy FB research/game_pred_jake.txt',row.names=F)
write.table(starters_df,'C:/Users/A097092/Desktop/Extra/Fantasy FB research/all_starters_jake.txt',row.names=F)

length(c(which(top_semi==4 & low_semi==3),which(top_semi==3 & low_semi==4)))/smple*100

apply(win_loss[[3]],2,mean)
winner
playoff

(repl_zero(table(seed_2)/smple)+repl_zero(table(seed_3)/smple))/playoff


