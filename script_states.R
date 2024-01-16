# Quick example for plotting states at the nodes of a phylogeny

tree <- pbtree(b = 0.2, n = 10) %>% ladderize()

data <- data.frame(node = names(branching.times(tree)), A = c(1,0,1,1,1,0,0,1,1), B = c(1,0,1,1,1,0,0,0,0))


states_df <- data.frame(node = names(branching.times(tree)), age = branching.times(tree))

states_df$states <- c(rep("A", 5), "A_B_C_D", "A_C", "A_D", "A_C")

legend_df <- data.frame(states = c("A", "B", "C", "D"),
                        col = c("darkorange", "dodgerblue3", "darkolivegreen4", "orchid4"))
distri <- c(rep("A", 5), "B_C", "B_C", "B_C", "C", "B")

library(RPANDA)
plot(tree, label.offset = 0.5)
add.gts(-0.3, is.phylo = T, names = c("Q", "Pli.", "Miocene"))
par(new = T)
plot(tree, label.offset = 0.5)

add.states(decx_states = states_df$states, distri = c(distri),
           col = legend_df$col, cex = 0.2, leg = F)
