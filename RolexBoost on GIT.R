################################################################################################################################################
### Project  : RolexBoost
### Script   : RolexBoost on GIT.R
### Contents : A flexible boosting algorithm with adaptive loss functions
################################################################################################################################################

################################################################################################################################################
### Setting up environment
################################################################################################################################################

# Load library
pkgs <- c("rpart", "ada", "caTools", "ggplot2", "reshape", "dplyr", "PairedData", "car", "reshape", "Matrix", "stringr", "randomForest", "ECoL", "FSA", "gridExtra")
sapply(pkgs, install.packages, character.only = T) 
sapply(pkgs, require, character.only = T)

# Load Datasets
load("datasets.RData")

# Load Functions
load("functions.RData")

################################################################################################################################################
### Analysis
################################################################################################################################################

## Table 1. Description of the datasets 
table.1.tmp <- matrix(NA, length(df.all), 2, dimnames = list(names(df.all), c("No.Instances", "No.Attributes")))
for (i in 1:nrow(table.1.tmp)){table.1.tmp[i,] <- dim(df.all[[i]]) - c(0, 1)}
table.1     <- table.1.tmp[-c(17:18, 20:21, 23:24, 26:27, 29:30, 32:34, 36:40),]
table.1     <- cbind(table.1, No.Classes = c(rep(2, 15), rep(3, 5), rep(4, 1), rep(6, 1)))

print(table.1)

## Table 2. Performance benchmarks 
res.ranks <- as.matrix(res.acc.all[, c(3:9)])
for (i in 1:nrow(res.ranks)){res.ranks[i,] <- rank(-res.acc.all[i, c(3:9)], ties.method = "min")}
res.mrank           <- round(colMeans(unlist(res.ranks)), 2)
acc.mean            <- aggregate(x = res.acc.all[, c(3:9)], by = list(res.acc.all[, 2]), mean)
table.2.tmp         <- rbind(acc.mean, lapply(acc.mean, mean), res.mrank[c(7, 1:6)])
table.2.tmp[, 2:8]  <- round(table.2.tmp[, 2:8], 4)
table.2             <- table.2.tmp[c(1, 9:11, 15:18, 22:24, 34:37, 6:8, 12:14, 19:21, 31:33, 38:40, 2:5, 25:30, 41:42),]
table.2[, 1]        <- c(na.omit(labels(df.all)), "Mean.acc", "Mean.rank")

print(table.2)

## Table 3. Post-hoc test results (p-value) 
res.fm.ph <- friedman.post.hoc(value ~ X2 | X1, data = melt(res.ranks))
res.dunn  <- dunnTest(melt(res.ranks)[, 3], melt(res.ranks)[, 2], method = 'bonferroni')$res
p.val.vec <- as.matrix(res.dunn[4])
res.p.val <- c(rep(NA, 7),                                              
               res.fm.ph$PostHoc.Test[2], rep(NA, 6),                    
               res.fm.ph$PostHoc.Test[c(6, 15)], rep(NA, 5),            
               res.fm.ph$PostHoc.Test[c(3, 12, 18)], rep(NA, 4),         
               res.fm.ph$PostHoc.Test[c(5, 14, 21, 17)], rep(NA, 3),     
               res.fm.ph$PostHoc.Test[c(1, 7, 11, 8, 10)], rep(NA, 2),  
               res.fm.ph$PostHoc.Test[c(4, 13, 20, 16, 19, 9)], NA,      # Nemenyi test
               p.val.vec[c(7, 9, 20, 10, 15, 8)], NA)                    # Bonferroni-Dunn test

table.3   <- as.data.frame(matrix(matrix(round(res.p.val, 3), 7, 8), nrow = 7, ncol = 8, 
                                  dimnames = list(c(names(res.mrank)), c(names(res.mrank), names(res.mrank)[7]))))

print(table.3)

## Figure 2. Ratio of each algorithm included in the Top-n rank on 30 UCI datasets
windowsFonts("Times New Roman" = windowsFont("Times New Roman"))
res.rank.ratio.tmp <- c()
for (i in 1:6){ res.rank.ratio.tmp <- cbind(res.rank.ratio.tmp, apply(res.ranks, 2, function(x){ length(which(x == i)) / length(x) })) }
res.rank.ratio.tmp <- matrix(res.rank.ratio.tmp, 42, 1)
for (i in 1:35){res.rank.ratio.tmp[i + 7,] <- res.rank.ratio.tmp[i + 7,] + res.rank.ratio.tmp[i,]}
res.rank.ratio     <- data.frame(Top_n     = c(rep("Top1", 7), rep("Top2", 7), rep("Top3", 7), rep("Top4", 7), rep("Top5", 7), rep("Top6", 7)),
                                 Algorithm = rep(c(1:7), 6),
                                 Ratio     = res.rank.ratio.tmp)
res.rank.ratio$adj.y            <- res.rank.ratio$Ratio
res.rank.ratio$adj.y[c(34, 40)] <- res.rank.ratio$adj.y[c(34, 40)] - 0.04

figure.2 <- ggplot(data = res.rank.ratio, aes(x = Top_n, y = Ratio, fill = factor(Algorithm))) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0.0,1.05)) +
  theme_bw() +
  theme(legend.position = c(0.06, 0.82), legend.title = element_text(size = 10), 
        legend.text = element_text(size = 5)) +
  labs(x = "Top-n", y = "Ratio", fill = "Algorithm") +
  theme(axis.title.x = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.title.y = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.text.x  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(axis.text.y  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(plot.title   = element_text(family = 'sans' , face = 2, color = 'black', size = 13)) +
  theme(legend.text  = element_text(family = 'Times New Roman', size = 10)) +
  theme(legend.title = element_blank()) +
  geom_text(aes(y = adj.y, label = round(Ratio, 2)), color = "black", vjust = -0.5, position = position_dodge(0.69), family = 'Times New Roman', size = 4) +
  scale_fill_manual(values = c("lightgrey", "powderblue", "lightsteelblue", "khaki", "burlywood", "lightpink", "darkseagreen"),
                    labels = c("AdaBoost", "GentlBoost", "RotationForest", "RandomForest", "RotationBoost", "FlexBoost", "RolexBoost")) +
  guides(fill = guide_legend(keywidth = 0.2, keyheight = 0.2, default.unit = "inch"))

print(figure.2)


## Figure 3. Relationship between the degree of complexity change through rotation and the performance improvement from FlexBoost to RolexBoost 
## 40 UCI datasets (left)
figure.3.left <- ggplot(res.dis$uci, aes(x = res.dis$uci[ ,1], y = res.dis$uci[, 2], color = factor(res.dis$uci[, 3]))) + 
  geom_point() +
  labs(x = 'Degree of complexity change (in F3) through rotation', y = 'Performance improvement from FlexBoost to RolexBoost') +
  theme_bw() +
  theme(legend.text = element_text(size = 15), legend.title = element_blank(), 
        legend.position = c(0.12, 0.91), legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  theme(axis.title.x = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.title.y = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.text.x  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(axis.text.y  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  guides(colour = guide_legend(override.aes = list(size = 5), reverse = TRUE)) +
  geom_vline(xintercept = 0, color = 'grey')

## 800 synthetic data (right)
figure.3.right <- ggplot(res.dis$art, aes(x = res.dis$art[ ,1], y = res.dis$art[, 2], color = factor(res.dis$art[, 3]))) + 
  geom_point() +
  labs(x = 'Degree of complexity change (in F3) through rotation', y = 'Performance improvement from FlexBoost to RolexBoost') +
  theme_bw() +
  theme(legend.text = element_text(size = 15), legend.title = element_blank(), 
        legend.position = c(0.12, 0.901), legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  theme(axis.title.x = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.title.y = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.text.x  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(axis.text.y  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  guides(colour = guide_legend(override.aes = list(size = 5), reverse = TRUE)) +
  geom_vline(xintercept = 0, color = 'grey') +
  scale_x_continuous(breaks = seq(-0.4, 0.4, 0.2)) +
  scale_y_continuous(breaks = seq(-0.05, 0.1, 0.05))

figure.3 <- grid.arrange(figure.3.left, figure.3.right, nrow = 1, ncol = 2)

print(figure.3)


################################################################################################################################################
### Appendix. Experiment
################################################################################################################################################
# Input
seed        <- 1
data        <- Iris2
independent <- 1:(ncol(data) - 1)
dependent   <- ncol(data)  
X           <- data[, independent]
y           <- data[, dependent]
iter        <- 50

# Hyper-parameter that controls the sensitivity of exponential loss used in FlexBoost and RolexBoost
par_k       <- 0.5

## K-fold AdaBoost
kfold.ada(iteration = iter)

## K-fold GentleBoost
kfold.gentle(iteration = iter)

## K-fold RotationForest
kfold.rotationForest(iteration = iter) 

## K-fold RandomForest
kfold.randomForest(iteration = iter)

## K-fold RotationBoost
kfold.rotboost(df = data, rot_iter = 1, boost_iter = iter)

## K-fold FlexBoost 
kfold.flex(df = data, flex_iter = iter, par.k = par_k) 

## K-fold RolexBoost
kfold.rolex(df = data, n.subset = 3, rot_iter = 1, flex_iter = iter, par.k = par_k)


