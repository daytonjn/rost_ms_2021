source("ROST_requiredPackages.R") #Required packges

#Command to save
#ggsave("dapc_1997to2018_k2.png", plots_loc_l[[3]], scale = 1, width = 3.5, units = "in", dpi = 1200, limitsize = FALSE)

#Cyan, Yellow, Magenta Green

colors.bright <- c("#EE3377", "#33BBEE", "#228833","#F7F056")


name <- "ROST_1870sto2016_locprior"
my_k <- 4
grp_l <- vector(mode = "list", length = length(my_k))
dapc_l <- vector(mode = "list", length = length(my_k))
cum.variance_l <- vector(mode = "list", length = length(my_k))
plots_loc_l <- list()
admix.output_l <- list()


X <- scaleGen(ROST_1870sto2016[[1]], NA.method="mean", center=FALSE)


grp <- pop(ROST_1870sto2016[[1]])
#For each value k, cycle through...and choose optimal number of PCs and n.da - 1

dapc_l <- xvalDapc(X, grp , n.pca.max = 100, training.set = 0.80,
                          result="groupMean", center = TRUE, scale = FALSE,
                          n.pca = NULL, n.rep=500, xval.plot = FALSE, n.da = NULL,
                          parallel = "snow", ncpus = 5)
  
  
  my_df <- as.data.frame(dapc_l$DAPC$ind.coord)
  my_df$Cluster <- paste(dapc_l$DAPC$grp)
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
  
  
  plots_loc_l <- ggplot(my_df, aes(x = LD1, y = LD2, color = Cluster, fill = Cluster))
  plots_loc_l  <- plots_loc_l  + geom_point(size = 4, alpha = 0.9) + stat_ellipse(level = 0.80, size = 1) +
    scale_color_manual(values = c( "#F7F056", "#228833", "#33BBEE", "#EE3377")) + geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) + 
    theme_bw() + theme(text = element_text(size = 12, family = "sans"),legend.title=element_blank(),
                       panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line= element_line(colour = "black")) +
    ylab("LD-2") + xlab("LD-1")
  plots_loc_l <- plots_loc_l  + theme(legend.position = "none")
  plots_loc_l
##########
####NOW VISUALIZE CLUSTERS AS COMPOPLOTS IN ORDER TO SEE IF WE NEED TO SWITCH COLORS AROUND....
tmp <- as.data.frame(dapc_l$DAPC$posterior)
tmp$K <- my_k[1]
tmp$Individual <- rownames(tmp)
tmp <- melt(tmp, id = c("Individual", "K"))
names(tmp)[3:4] <- c("Group", "Posterior")
tmp$Population <- factor(pop(ROST_1870sto2016[[1]]), c("1870s-Warm", "1970s-Warm", "1997-Warm", "2016-Warm"))
my_df <- tmp
my_df

levels(my_df$Population)[levels(my_df$Population) == "1870s-Warm"] <- "1870s-W"
levels(my_df$Population)[levels(my_df$Population) == "1970s-Warm"] <- "1970s-W"

grp.labs <- paste("K =", my_k)
names(grp.labs) <- my_k

p_loc <- ggplot(my_df, aes(x = Individual, y = Posterior, fill = Group))
p_loc <- p_loc + geom_bar(stat = "identity")
p_loc <- p_loc + facet_grid(K ~ Population, scales = "free_x", space = "free", 
                      labeller = labeller(K = grp.labs))
p_loc <- p_loc + theme_bw()
p_loc <- p_loc + ylab(NULL)
p_loc <- p_loc + theme(legend.position='none')
p_loc <- p_loc + scale_fill_manual(values=c(  "#EE3377", "#33BBEE", "#228833", "#F7F056"))
p_loc <- p_loc + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12))
p_loc <- p_loc + theme(text = element_text(size = 10, family = 'sans'), axis.text.x = element_blank())
p_loc


####
ggsave("dapc_1870sto2016_locprior.png", plots_loc_l, scale = 1, width = 3.5, height = 3.19, units = "in", dpi = 1200, limitsize = FALSE)
ggsave("dapc_1870sto2016_locprior_compo.png", p_loc, scale = 1, width = 7.0, height = 2.13, units = "in", dpi = 1200, limitsize = FALSE)
#ggsave("dapc_1997to2018_k2.png", plots_loc_l[[3]], scale = 1, width = 3.5, units = "in", dpi = 1200, limitsize = FALSE)


p.noloc.vs.loc <- ggarrange(ggarrange(plots_l[[3]], p3,
                                      ncol = 2, widths = c(3, 9)),
                            ggarrange(plots_loc_l, p_loc,
                                      ncol = 2, widths = c(3, 9)),
                                      nrow = 2,
                                      labels = c("A", "B"),
                                      heights = c(2, 2), font.label = list(size = 12, color = 'black'))
p.noloc.vs.loc

ggsave("dapc_1870sto2016_k2to4.png", p3, scale = 1, width = 7.0, units = "in", dpi = 1200, limitsize = FALSE)

ggsave("dapc_1870sto2016_combined.png", p.comb, scale = 1, width = 7.0, units = "in", dpi = 1200, limitsize = FALSE)
