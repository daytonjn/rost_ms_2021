source("ROST_requiredPackages.R") #Required packges

#Command to save
#ggsave("dapc_1997to2018_k2.png", plots_l[[3]], scale = 1, width = 3.5, units = "in", dpi = 1200, limitsize = FALSE)

#Cyan, Yellow, Magenta Green

#cyan, magenta, orange, green, violet
colors.bright <- c("#EE7733", "#EE3377",  "#33BBEE")


name <- "ROST_1997to2018_locprior"
my_k <- 3
grp_l <- vector(mode = "list", length = length(my_k))
dapc_l <- vector(mode = "list", length = length(my_k))
cum.variance_l <- vector(mode = "list", length = length(my_k))
plots_l <- list()
admix.output_l <- list()


X <- scaleGen(ROST_1997to2018[[1]], NA.method="mean", center=FALSE)

i <- 1

grp <- pop(ROST_1997to2018[[1]])
#For each value k, cycle through...and choose optimal number of PCs and n.da - 1

dapc_l <- xvalDapc(X, grp , n.pca.max = 100, training.set = 0.80,
                          result="groupMean", center = TRUE, scale = FALSE,
                          n.pca = NULL, n.rep=500, xval.plot = FALSE, n.da = NULL,
                          parallel = "snow", ncpus = 5)
  
  
  my_df <- as.data.frame(dapc_l$DAPC$ind.coord)
  my_df$Cluster <- paste("Cluster -", dapc_l$DAPC$grp)
  head(my_df)
  
  cum.variance_l <- round(100*dapc_l$DAPC$pca.eig[1:dapc_l$DAPC$n.pca]/sum(dapc_l$DAPC$pca.eig),digits=2)
  
  ######
  sink.file.name <- paste0("xvaldapc_", name, "_k", my_k,".txt" )
  sink(sink.file.name)
  cat("Output for xvaldapc analysis\n")
  cat("=============================\n")
  paste("Dataset:", name, "\n", "Number of Clusters:", my_k) %>% print()
  cat("Mean Successful Assignment by Number of PCs of PCA\n")
  print(dapc_l$`Mean Successful Assignment by Number of PCs of PCA`)
  cat("Root Mean Squared Error by Number of PCs of PCA\n")
  print(dapc_l$`Root Mean Squared Error by Number of PCs of PCA`)
  cat("=============================\n")
  dapc.title <- paste("PCs Retained:", dapc_l$DAPC$n.pca, "... DFs Retained:", dapc_l$DAPC$n.da)
  print(dapc.title)
  cat("Cumulative Variance of Retained PCs")
  print(cum.variance_l)
  print(sum(cum.variance_l))
  sink()
  #######
  
  
  plots_l <- ggplot(my_df, aes(x = LD1, y = LD2, color = Cluster, fill = Cluster))
  plots_l  <- plots_l  + geom_point(size = 4, alpha = 0.9) + stat_ellipse(level = 0.80, size = 1) +
    scale_color_manual(values = colors.bright) + geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) + 
    theme_bw() + theme(text = element_text(size = 12, family = "sans"),legend.title=element_blank(),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line= element_line(colour = "black"),
                       legend.position = 'none') +
    ylab("LD-2") + xlab("LD-1")
  
plots_l
##########
####NOW VISUALIZE CLUSTERS AS COMPOPLOTS IN ORDER TO SEE IF WE NEED TO SWITCH COLORS AROUND....
tmp <- as.data.frame(dapc_l$DAPC$posterior)
tmp$K <- my_k[1]
tmp$Individual <- rownames(tmp)
tmp <- melt(tmp, id = c("Individual", "K"))
names(tmp)[3:4] <- c("Group", "Posterior")
tmp$Population <- factor(pop(ROST_1997to2018[[1]]), c("1997-Warm", "2016-Warm", "2018-Cold"))
my_df <- tmp
my_df

grp.labs <- paste("K =", my_k)
names(grp.labs) <- my_k



p3 <- ggplot(my_df, aes(x = Individual, y = Posterior, fill = Group))
p3 <- p3 + geom_bar(stat = "identity")
p3 <- p3 + facet_grid(K ~ Population, scales = "free_x", space = "free", 
                      labeller = labeller(K = grp.labs))
p3 <- p3 + theme_bw()
p3 <- p3 + ylab("Posterior Membership Probability")
p3 <- p3 + theme(legend.position='none')
p3 <- p3 + scale_fill_manual(values=c(colors.bright))
p3 <- p3 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
p3 <- p3 + theme(text = element_text(size = 12, family = 'sans'), axis.text.x = element_blank())
p3


####

ggsave("dapc_1997to2018_locpior.png", plots_l, scale = 1, width = 3.5, height = 3.19, units = "in", dpi = 1200, limitsize = FALSE)
ggsave("dapc_1997to2018_locprior_compop.png", p3, scale = 1, width = 7.0, height = 2.13, units = "in", dpi = 1200, limitsize = FALSE)

