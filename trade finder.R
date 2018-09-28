fuller_lineup <- lapply(owners, function(o) {

start_k <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='K','Week'=w,'Owner'=o,KCK_Final[which(KCK_Final$Owner==o & KCK_Final$Week==w)[1:3],c('Player','Full_Proj','Var')])))
start_d <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='D','Week'=w,'Owner'=o,DEF_Final[which(DEF_Final$Owner==o & DEF_Final$Week==w)[1:3],c('Player','Full_Proj','Var')])))

start_qb <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='QB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='QB' & FLEX_Final$Week==w)[1:3],c('Player','proj','Var')])))
start_te <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='TE','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='TE' & FLEX_Final$Week==w)[1:3],c('Player','proj','Var')])))
start_rb <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='RB','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='RB' & FLEX_Final$Week==w)[1:5],c('Player','proj','Var')])))
start_wr <- do.call(rbind,lapply(1:17, function(w) cbind('Pos'='WR','Week'=w,'Owner'=o,FLEX_Final[which(FLEX_Final$Owner==o & FLEX_Final$pos=='WR' & FLEX_Final$Week==w)[1:6],c('Player','proj','Var')])))

colnames(start_d)[5] <- 'proj'
colnames(start_k)[5] <- 'proj'

all_df <- rbind(start_qb,start_rb,start_wr,start_te,start_d,start_k)
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

fuller_lineup <- do.call(rbind,fuller_lineup)
fuller_lineup$Owner <- match(fuller_lineup$Owner,owners)
fuller_lineup <- fuller_lineup[order(-fuller_lineup$proj),]
pre_filter_FL <- fuller_lineup

all_scores <- aggregate(cbind(proj,Var)~Week+Owner,pre_filter_FL,sum, subset= starter==1)
sched[,c('team_mean','team_var')] <- all_scores[sched$Tm_Inx,c('proj','Var')]
sched[,c('opp_mean','opp_var')] <- all_scores[sched$Opp_Inx,c('proj','Var')]
sched$win_est <- 1-pnorm(0,sched$spread,sqrt(sched$team_var + sched$opp_var))





part_1 <- 'aj'
part_2 <- 'scott'

part_id_1 <- match(part_1,owners)
part_id_2 <- match(part_2,owners)

fuller_lineup <- pre_filter_FL[which(pre_filter_FL$Owner==part_id_1 | pre_filter_FL$Owner==part_id_2),]

team_1 <- roster_list$Player[which(roster_list$Owner==part_1)]
team_2 <- roster_list$Player[which(roster_list$Owner==part_2)]
team_1_cmb <- combn(team_1,2)
team_2_cmb <- combn(team_2,2)
all_posib <- expand.grid(1:ncol(team_1_cmb),1:ncol(team_2_cmb))

team_1_rows <- sapply(team_1, function(x) c(which(fuller_lineup$Player==x),rep(NA,18-length(which(fuller_lineup$Player==x)))))
team_2_rows <- sapply(team_2, function(x) c(which(fuller_lineup$Player==x),rep(NA,18-length(which(fuller_lineup$Player==x)))))


###being testing scenerios
begin <- Sys.time()
trade_combos <- sapply(1:nrow(all_posib), function(i) {
fuller_lineup$Owner[which(fuller_lineup$Player==team_1_cmb[1,all_posib[i,1]])] <- match(part_2,owners)
fuller_lineup$Owner[which(fuller_lineup$Player==team_1_cmb[2,all_posib[i,1]])] <- match(part_2,owners)
fuller_lineup$Owner[which(fuller_lineup$Player==team_2_cmb[1,all_posib[i,2]])] <- match(part_1,owners)
fuller_lineup$Owner[which(fuller_lineup$Player==team_2_cmb[2,all_posib[i,2]])] <- match(part_1,owners)

fuller_lineup$Uni_ID <- paste0(fuller_lineup$Owner,'-',fuller_lineup$Pos,'-',fuller_lineup$Week)
fuller_lineup <- fuller_lineup[order(fuller_lineup$Uni_ID),]
mat_rows <- c(match(unique(fuller_lineup$Uni_ID),fuller_lineup$Uni_ID),nrow(fuller_lineup)+1)
fuller_lineup$indiv_rnk <- unlist(lapply(1:(length(mat_rows)-1),function(x) 1:(mat_rows[x+1] - mat_rows[x])))
fuller_lineup$lookup <- paste0(fuller_lineup$Uni_ID,'-',fuller_lineup$indiv_rnk)

fuller_lineup$starter <- 0
fuller_lineup$starter[which(fuller_lineup$indiv_rnk==1 & fuller_lineup$Pos %in% c('QB','TE','D','K'))] <- 1
fuller_lineup$starter[which(fuller_lineup$indiv_rnk<=3 & fuller_lineup$Pos=='WR')] <- 1
fuller_lineup$starter[which(fuller_lineup$indiv_rnk<=2 & fuller_lineup$Pos=='RB')] <- 1
flex_wr_lines <- match(paste0(rep(c(part_id_1,part_id_2),17),'-WR-',1:17,'-4'),fuller_lineup$lookup)
flex_rb_lines <- match(paste0(rep(c(part_id_1,part_id_2),17),'-RB-',1:17,'-3'),fuller_lineup$lookup)
fuller_lineup$starter[ifelse(fuller_lineup$proj[flex_wr_lines] > fuller_lineup$proj[flex_rb_lines],flex_wr_lines,flex_rb_lines)] <- 1

all_scores_mine <- aggregate(cbind(proj,Var)~Week+Owner,fuller_lineup,sum, subset= starter==1 & Week<=13)
sched[which(sched$team==part_id_1),c('team_mean','team_var')] <- all_scores_mine[which(all_scores_mine$Owner==part_id_1),c('proj','Var')]
sched[which(sched$team==part_id_2),c('team_mean','team_var')] <- all_scores_mine[which(all_scores_mine$Owner==part_id_2),c('proj','Var')]
sched$spread <- sched$team_mean - sched$opp_mean
sched$win_est <- 1-pnorm(0,sched$spread,sqrt(sched$team_var + sched$opp_var))

return(aggregate(win_est~team,sched,sum)[c(part_id_1,part_id_2),2])
})
Sys.time() - begin


summary(trade_combos[3,])