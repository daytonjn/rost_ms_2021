source("ROST_requiredPackages.R") #Required packges

#Command to save
#ggsave("dapc_1997to2018_k2.png", plots_l[[3]], scale = 1, width = 3.5, units = "in", dpi = 1200, limitsize = FALSE)

#Cyan, Yellow, Magenta Green

#cyan, magenta, orange, green, violet
colors.bright <- c( "#33BBEE",  "#EE3377", "#EE7733", "#228833", "#AA4499")


name <- "ROST_1997to2018"
my_k <- c(2,4)
grp_l <- vector(mode = "list", length = length(my_k))
dapc_l <- vector(mode = "list", length = length(my_k))
cum.variance_l <- vector(mode = "list", length = length(my_k))
plots_l <- list()
admix.output_l <- list()


X <- scaleGen(ROST_1997to2018[[1]], NA.method="mean", center=FALSE)

i <- 1

#For each value k, cycle through...and choose optimal number of PCs and n.da - 1
for(i in 1:length(dapc_l)){
  set.seed(9)
  grp_l[[i]] <- find.clusters(X, method = 'kmeans', stat = 'BIC', center = FALSE,
                              scale = FALSE, perc.pca = 90, pca.info = TRUE, n.iter = 10000,
                              pca.select = "percVar", n.clust = my_k[i])
  
  #Modify group assignments so that major group (1) = cyan and minor group (2) = yellow
  if (my_k[i] == 2){
    # group.maj <- which.max(grp_l[[i]]$size) #finds index of max, i.e. major group
    # group.min <- which.min(grp_l[[i]]$size)
    index.grps_l <- list()
    
    
    index.grps_l[[1]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == 1]) #returns index of all group 1s...
    index.grps_l[[2]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == 2]) #returns index of all group 2s...
    
    
    if (length(grp_l[[i]]$grp[index.grps_l[[1]]]) > length(grp_l[[i]]$grp[index.grps_l[[2]]])) {
      #if length of group 1 is longer than group 2...then numbers are all good!
      grp_l[[i]]$grp[index.grps_l[[1]]] <- 1
      grp_l[[i]]$grp[index.grps_l[[2]]] <- 2
      
      
    } else { #if not...then reassign
      grp_l[[i]]$grp[index.grps_l[[1]]] <- 2
      grp_l[[i]]$grp[index.grps_l[[2]]] <- 1
      
      grp_l[[i]]$size[1] <- length(index.grps_l[[2]])
      grp_l[[i]]$size[2] <- length(index.grps_l[[1]])
      
    }
    
    
    
  }
  
  #Modify group assignments so that major group (1) = cyan and minor group (2) = yellow
  if (my_k[i] == 3){
    group.maj <- which.max(grp_l[[i]]$size) #finds index of max, i.e. major group
    group.min <- which.min(grp_l[[i]]$size)
    size.remaingroup <- grp_l[[i]]$size[c(-group.maj,-group.min)]
    group.remainder <- which(grp_l[[i]]$size == size.remaingroup)
    
    index.grps_l <- list()
    
    index.grps_l[[1]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.maj]) #returns index of all group 1s...major (1 = cyan)
    index.grps_l[[2]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.min]) #returns index of all group 2s...minor (2 = yellow)
    index.grps_l[[3]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.remainder]) #returns index of all group 3s...remainder (3 = magenta)
    
    grp_l[[i]]$grp[index.grps_l[[1]]] <- 1
    grp_l[[i]]$grp[index.grps_l[[2]]] <- 2
    grp_l[[i]]$grp[index.grps_l[[3]]] <- 3
    
    grp_l[[i]]$size <- c(length(grp_l[[i]]$grp[index.grps_l[[1]]]), length(grp_l[[i]]$grp[index.grps_l[[2]]]), length(grp_l[[i]]$grp[index.grps_l[[3]]]))
    
  }
  
  
  if (my_k[i] == 4){
    group.maj <- which.max(grp_l[[i]]$size) #finds index of max, i.e. major group (cyan)
    group.min <- which.min(grp_l[[i]]$size) #minimum group is going to be.... ***small 4th cluster (green)
    
    size.remaingroup <- grp_l[[i]]$size[c(-group.maj,-group.min)]
    remaingroup.min <- which.min(size.remaingroup)
    size.remaingroup.min <- size.remaingroup[remaingroup.min]
    remaingroup.max <- which.max(size.remaingroup)
    size.remaingroup.max <- size.remaingroup[remaingroup.max]
    
    group.remainder.max <- which(grp_l[[i]]$size == size.remaingroup.max)
    group.remainder.min <- which(grp_l[[i]]$size == size.remaingroup.min) 
    
    index.grps_l <- list()
    
    index.grps_l[[1]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.maj]) #returns index of all group 1s...major (1 = cyan)
    index.grps_l[[4]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.min]) #returns index of all group 2s...minor (2 = yellow)
    index.grps_l[[3]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.remainder.max]) #returns index of all group 3s...remainder (3 = magenta)
    index.grps_l[[2]] <- which(grp_l[[i]]$grp %in% grp_l[[i]]$grp[grp_l[[i]]$grp == group.remainder.min]) #returns index of all group 3s...remainder (3 = magenta)
    
    
    grp_l[[i]]$grp[index.grps_l[[1]]] <- 1
    grp_l[[i]]$grp[index.grps_l[[2]]] <- 2
    grp_l[[i]]$grp[index.grps_l[[3]]] <- 3
    grp_l[[i]]$grp[index.grps_l[[4]]] <- 4 
    
    
    grp_l[[i]]$size <- c(length(grp_l[[i]]$grp[index.grps_l[[1]]]), length(grp_l[[i]]$grp[index.grps_l[[2]]]), length(grp_l[[i]]$grp[index.grps_l[[3]]]), length(grp_l[[i]]$grp[index.grps_l[[4]]]))
    
  }
  
  
  dapc_l[[i]] <- xvalDapc(X, grp_l[[i]]$grp, n.pca.max = 100, training.set = 0.80,
                          result="groupMean", center = TRUE, scale = FALSE,
                          n.pca = NULL, n.rep=500, xval.plot = FALSE, n.da = NULL,
                          parallel = "snow", ncpus = 5)
  
  
  my_df <- as.data.frame(dapc_l[[i]]$DAPC$ind.coord)
  my_df$Cluster <- paste("Cluster -", dapc_l[[i]]$DAPC$grp)
  head(my_df)
  
  cum.variance_l[[i]] <- round(100*dapc_l[[i]]$DAPC$pca.eig[1:dapc_l[[i]]$DAPC$n.pca]/sum(dapc_l[[i]]$DAPC$pca.eig),digits=2)
  
  ######
  sink.file.name <- paste0("xvaldapc_", name, "_k", my_k[i],".txt" )
  sink(sink.file.name)
  cat("Output for xvaldapc analysis\n")
  cat("=============================\n")
  paste("Dataset:", name, "\n", "Number of Clusters:", my_k[i]) %>% print()
  print(grp_l[[i]]$size)
  cat("Mean Successful Assignment by Number of PCs of PCA\n")
  print(dapc_l[[i]]$`Mean Successful Assignment by Number of PCs of PCA`)
  cat("Root Mean Squared Error by Number of PCs of PCA\n")
  print(dapc_l[[i]]$`Root Mean Squared Error by Number of PCs of PCA`)
  cat("=============================\n")
  dapc.title <- paste("PCs Retained:", dapc_l[[i]]$DAPC$n.pca, "... DFs Retained:", dapc_l[[i]]$DAPC$n.da)
  print(dapc.title)
  cat("Cumulative Variance of Retained PCs")
  print(cum.variance_l[[i]])
  print(sum(cum.variance_l[[i]]))
  sink()
  #######
  
  #PRINT CLUSTER MEMBERSHIP PROBABILITIES TO A FILE FOR UPLOAD INTO pophelperShiny
  admix.output_l[[i]] <- round(dapc_l[[i]]$DAPC$posterior, digits = 3)
  file.name <- paste0("admix_", name, "_k", my_k[i],".txt" )
  write.table(admix.output_l[[i]], file=file.name, row.names = FALSE, col.names = FALSE)
  
  if (my_k[i] == 2){
    
    temp <- as.data.frame(dapc_l[[i]]$DAPC$ind.coord)
    temp$Group <- factor(dapc_l[[i]]$DAPC$grp, levels = c("1", "2"))
    
    plots_l[[i]] <- ggplot(temp, aes(x=LD1, fill = Group)) + geom_density(alpha = 0.7) +
      scale_color_manual(values = colors.bright) + theme_bw() +
      theme(text = element_text(size = 12, family = "sans"),legend.title=element_blank(),
            panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line= element_line(colour = "black"),
            legend.position = "none") +  ylab("Density") + xlab("LD-1") +
      xlim(min(dapc_l[[i]]$DAPC$ind.coord) - 0.5, max(dapc_l[[i]]$DAPC$ind.coord) + 1)
    
  }
  
  if (my_k[i] > 2) {
    my_pal <- colors.bright
    
    plots_l[[i]] <- ggplot(my_df, aes(x = LD1, y = LD2, color = Cluster, fill = Cluster))
    plots_l[[i]]  <- plots_l[[i]]  + geom_point(size = 4, alpha = 0.9) + stat_ellipse(level = 0.80, size = 1) +
      scale_color_manual(values = colors.bright) + geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) + 
      theme_bw() + theme(text = element_text(size = 12, family = "sans"),legend.title=element_blank(),
                         panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line= element_line(colour = "black")) +
      ylab("LD-2") + xlab("LD-1")
    plots_l[[i]] <- plots_l[[i]]  + theme(legend.position = "none")
  }
  
  
  print(i)
  i = i+1
}



##########
####NOW VISUALIZE CLUSTERS AS COMPOPLOTS IN ORDER TO SEE IF WE NEED TO SWITCH COLORS AROUND....
tmp <- as.data.frame(dapc_l[[1]]$DAPC$posterior)
tmp$K <- my_k[1]
tmp$Individual <- rownames(tmp)
tmp <- melt(tmp, id = c("Individual", "K"))
names(tmp)[3:4] <- c("Group", "Posterior")
tmp$Population <- factor(pop(ROST_1997to2018[[1]]), c("1997-Warm", "2016-Warm", "2018-Cold"))
my_df <- tmp
my_df
i = 1
for(i in 2:length(dapc_l)){
  tmp <- as.data.frame(dapc_l[[i]]$DAPC$posterior)
  tmp$K <- my_k[i]
  tmp$Individual <- rownames(tmp)
  tmp <- melt(tmp, id = c("Individual", "K"))
  names(tmp)[3:4] <- c("Group", "Posterior")
  tmp$Population <- pop(ROST_1997to2018[[1]])
  
  my_df <- rbind(my_df, tmp)
  i = i+1
}

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

ggsave("dapc_1997to2018_k2.png", plots_l[[1]], scale = 1, width = 3.5, height = 3.19, units = "in", dpi = 1200, limitsize = FALSE)
ggsave("dapc_1997to2018_k4.png", plots_l[[3]], scale = 1, width = 3.5, height = 3.19, units = "in", dpi = 1200, limitsize = FALSE)
ggsave("dapc_1997to2018_k2to2.png", p3, scale = 1, width = 7.0, height = 2.13, units = "in", dpi = 1200, limitsize = FALSE)
#ggsave("dapc_1997to2018_k2.png", plots_l[[3]], scale = 1, width = 3.5, units = "in", dpi = 1200, limitsize = FALSE)

