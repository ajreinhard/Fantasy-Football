options(warn=-1)
done_scores <- lapply(dir('Teams',full=T), function(x) {

the_tree <- htmlTreeParse(x, useInternal=T)

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

#done_scores[[find_OwWk('lucas-4')]] <- repl_slot(find_OwWk('lucas-4'),'Bears D/ST',9,FALSE)
#done_scores[[find_OwWk('aj-4')]] <- repl_slot(find_OwWk('aj-4'),'Ryan Succop',10,TRUE)
#############