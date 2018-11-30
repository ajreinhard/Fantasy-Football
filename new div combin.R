all_comb <- combn(10,5)
w_pct <- c(0.624,0.575,0.539,0.527,0.517,0.476,0.472,0.470,0.449,0.398)
w_pct_mx <- matrix(w_pct[all_comb],5)

top_rnk <- which(rank(abs(mean(w_pct)-apply(w_pct_mx,2,mean)))==1)
all_comb[,top_rnk]
mean(w_pct_mx[,top_rnk])

wins <- c(429,263,369,301,356,324,324,319,305,88)
losses <- c(257,194,315,270,332,357,363,360,375,134)
ties <- c(7,2,9,5,5,12,6,14,13,3)

base_avg <- (sum(wins) + sum(ties)/2) / sum(wins+losses+ties)

wins_mx <- matrix(wins[all_comb],5)
losses_mx <- matrix(losses[all_comb],5)
ties_mx <- matrix(ties[all_comb],5)

wins_all <- apply(wins_mx,2,sum)
ties_all <- apply(ties_mx,2,sum)
losses_all <- apply(losses_mx,2,sum)

all_wtd <- (wins_all + ties_all/2) / (wins_all + ties_all + losses_all)

all_comb[,which(rank(abs(all_wtd-base_avg))==1)]


