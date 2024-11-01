grp_l[[2]] <- find.clusters(X, method = 'kmeans', stat = 'BIC', center = FALSE,
                            scale = FALSE, perc.pca = 90, pca.info = TRUE, n.iter = 10000,
                            pca.select = "percVar", n.clust = 4)

grp_l[[2]]$size
group.maj <- which.max(grp_l[[i]]$size) #finds index of max, i.e. major group (cyan)
group.maj
group.min <- which.min(grp_l[[i]]$size) #minimum group is going to be.... ***small 4th cluster (violet)
group.min

size.remaingroup <- grp_l[[i]]$size[c(-group.maj,-group.min)]
remaingroup.min <- which.min(size.remaingroup)
size.remaingroup.min <- size.remaingroup[remaingroup.min]
remaingroup.max <- which.max(size.remaingroup)
size.remaingroup.max <- size.remaingroup[remaingroup.max]

group.remainder.max <- which(grp_l[[i]]$size == size.remaingroup.max)
group.remainder.min <- which(grp_l[[i]]$size == size.remaingroup.min) 

index.grps_l <- list()

index.grps_l[[1]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.maj]) #returns index of all group 1s...major (1 = cyan)
index.grps_l[[4]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.min]) #returns index of all group 4s...minor (4 = violet)
index.grps_l[[2]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.remainder.max]) #returns index of all group 3s...remainder (3 = magenta)
index.grps_l[[3]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.remainder.min]) #returns index of all group 3s...remainder (3 = green)


grp_l[[i]]$grp[index.grps_l[[1]]] <- 1
grp_l[[i]]$grp[index.grps_l[[2]]] <- 2
grp_l[[i]]$grp[index.grps_l[[3]]] <- 3
grp_l[[i]]$grp[index.grps_l[[4]]] <- 4 

dapc_l[[2]] <- dapc(X, grp_l[[2]]$grp, n.pca = 10, center = TRUE, scale = FALSE, n.da = 3)

scatter(dapc_l[[2]])
compoplot(dapc_l[[2]])

my_df <- as.data.frame(dapc_l[[i]]$ind.coord)
my_df$Cluster <- paste("Cluster -", dapc_l[[i]]$grp)
head(my_df)
cum.variance_l[[i]] <- round(100*dapc_l[[i]]$pca.eig[1:dapc_l[[i]]$n.pca]/sum(dapc_l[[i]]$pca.eig),digits=2)

tmp <- as.data.frame(dapc_l[[1]]$DAPC$posterior)
tmp$K <- my_k[1]
tmp$Individual <- rownames(tmp)
tmp <- melt(tmp, id = c("Individual", "K"))
names(tmp)[3:4] <- c("Group", "Posterior")
tmp$Population <- factor(pop(ROST_1997to2018[[1]]), c("1997-Warm", "2016-Warm", "2018-Cold"))
my_df <- tmp
my_df

tmp <- as.data.frame(dapc_l[[i]]$posterior)
tmp$K <- 4
tmp$Individual <- rownames(tmp)
tmp <- melt(tmp, id = c("Individual", "K"))
names(tmp)[3:4] <- c("Group", "Posterior")
tmp$Population <- factor(pop(ROST_1997to2018[[1]]), c("1997-Warm", "2016-Warm", "2018-Cold"))

my_df <- rbind(my_df, tmp)

grp.labs <- paste("K =", c(2,4))
names(grp.labs) <- c(2,4)


p2and4 <- ggplot(my_df, aes(x = Individual, y = Posterior, fill = Group))
p2and4 <- p2and4 + geom_bar(stat = "identity")
p2and4 <- p2and4 + facet_grid(K ~ Population, scales = "free_x", space = "free", 
                      labeller = labeller(K = grp.labs))
p2and4 <- p2and4 + theme_bw()
p2and4 <- p2and4 + ylab("Posterior Membership Probability")
p2and4 <- p2and4 + theme(legend.position='none')
p2and4 <- p2and4 + scale_fill_manual(values=c(colors.bright))
p2and4 <- p2and4 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
p2and4 <- p2and4 + theme(text = element_text(size = 12, family = 'sans'), axis.text.x = element_blank())
p2and4

#compoplot (k=2 and k =4), p2and4
#dapc (k = 2), p2_dapc
#compoplot (k =3), loc prior, p3
#dapc (k = 3), loc prior, plots_l
p2and4 <- p2and4 + xlab("")
p3 <- p3 + ylab("")

contemp.plocvsnoloc <- ggarrange(ggarrange(p2_dapc, plots_l,
                                      ncol = 2, widths = c(1, 1),
                                      labels = c("A", "B")),
                            p2and4, p3,
                            nrow = 3, labels = c("", "C", "D"),
                            widths = c(1.5, 3, 3),
                            heights = c(2, 3, 2), font.label = list(size = 12, color = 'black'))
contemp.plocvsnoloc

ggsave("dapc_1997to2018_combined.png", contemp.plocvsnoloc, scale = 1, width = 7.0, units = "in", dpi = 1200, limitsize = FALSE)

