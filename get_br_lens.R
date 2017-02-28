library(ape)

tr_files <- read.tree(file = grep('.tre', dir(), value = T), tree.names = NULL)
ladder_trees <- sapply(tr_files, ladderize)
edge_lens <- ladder_trees[4,]
br_lens_info <- as.data.frame(edge_lens)
write.csv(br_lens_info, file = 'br_lens_info.csv')


