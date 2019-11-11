#########################################################################################################################
### Project  : RolexBoost
### Script   : RolexBoost on GIT.R
### Contents : A flexible boosting algorithm with adaptive loss functions
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################
# remove previous history
rm(list = ls())

# Load library
pkgs <- c("rpart", "ada", "caTools", "ggplot2", "reshape", "dplyr", "PairedData", "car", "reshape", "Matrix", "stringr", "randomForest")
sapply(pkgs, require, character.only = T)

# Load Datasets
load("datasets.RData")

# Load Functions
load("functions.RData")

#########################################################################################################################
### Analysis
#########################################################################################################################
## Load the accuracy tables
res.acc.all  <- read.csv(url("http://bit.ly/ROLEXBOOST"), header = TRUE)

## Table 1. Description of the datasets 
table.1.tmp <- matrix(NA, length(df.all), 2, dimnames = list(names(df.all), c("No.Instances", "No.Attributes")))
for (i in 1:nrow(table.1.tmp)){table.1.tmp[i,] <- dim(df.all[[i]]) - c(0, 1)}
table.1     <- table.1.tmp[-c(17:18, 20:21, 23:24, 26:27, 29:30),]
table.1     <- cbind(table.1, No.Classes = c(rep(2, 15), rep(3, 5)))

print(table.1)

## Performance benchmarks (accuracy)
acc.mean            <- aggregate(x = res.acc.all[, c(3:9)], by = list(res.acc.all[, 2]), mean)
table.2.tmp         <- rbind(acc.mean, lapply(acc.mean, mean), c(NA, table(as.factor(c(1,2,3,4,5,6,7))[apply(acc.mean[ ,-1][-15, ], 1, which.max)])))
table.2.tmp[, 2:8]  <- round(table.2.tmp[, 2:8], 4)
table.2             <- table.2.tmp[c(1, 5:7, 11:14, 18:20, 24:27, 2:4, 8:10, 15:17, 21:23, 28:32),]
table.2[, 1]        <- c(na.omit(labels(df.all)), "Avg.acc", "Number of win")

print(table.2)

## Friedman post hoc test results (p-value) on rankings and average accuracies
res.ranks <- as.matrix(res.acc.all[, c(3:9)])
for (i in 1:nrow(res.ranks)){res.ranks[i,] <- rank(-res.acc.all[i, c(3:9)], ties.method = "min")}
res.fm.ph <- friedman.post.hoc(value ~ X2 | X1, data = melt(res.ranks))
res.p.val <- c(rep(NA, 7),                                               # adaboost
               res.fm.ph$PostHoc.Test[2], rep(NA, 6),                    # gentleboost
               res.fm.ph$PostHoc.Test[c(6, 15)], rep(NA, 5),             # rotation forest
               res.fm.ph$PostHoc.Test[c(3, 12, 18)], rep(NA, 4),         # random forest
               res.fm.ph$PostHoc.Test[c(5, 14, 21, 17)], rep(NA, 3),     # rotation boost
               res.fm.ph$PostHoc.Test[c(1, 7, 11, 8, 10)], rep(NA, 2),   # flexboost
               res.fm.ph$PostHoc.Test[c(4, 13, 20, 16, 19, 9)], NA)      # rolex boost  (rish alpha = 0.05 / 21 = 0.0024)    
res.mrank <- colMeans(unlist(res.ranks))
res.macc  <- as.character(round(colMeans(res.acc.all[,c(3:9)]), 4))
table.3   <- as.data.frame(matrix(rbind(matrix(round(res.p.val, 4), 7, 7), as.character(round(colMeans(res.ranks), 2)), res.macc), 
                                  nrow = 9, ncol = 7, dimnames = list(c(names(res.mrank), "Mean Rank", "Mean Accuracy"), names(res.mrank))))

print(table.3)

## Top-n rank plot
res.rank.ratio.tmp <- c()
for (i in 1:6){ res.rank.ratio.tmp <- cbind(res.rank.ratio.tmp, apply(res.ranks, 2, function(x){ length(which(x == i)) / length(x) })) }
res.rank.ratio.tmp <- matrix(res.rank.ratio.tmp, 42, 1)
for (i in 1:35){res.rank.ratio.tmp[i + 7,] <- res.rank.ratio.tmp[i + 7,] + res.rank.ratio.tmp[i,]}
res.rank.ratio     <- data.frame(Top_n     = c(rep("Top1", 7), rep("Top2", 7), rep("Top3", 7), rep("Top4", 7), rep("Top5", 7), rep("Top6", 7)),
                                 Algorithm = rep(c(1:7), 6),
                                 Ratio     = res.rank.ratio.tmp)

figure.2 <- ggplot(data = res.rank.ratio, aes(x = Top_n, y = Ratio, fill = factor(Algorithm))) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0.0,1.05)) +
  theme_bw() +
  theme(legend.position = c(0.075, 0.9), legend.title = element_text(size = 10), 
        legend.text = element_text(size = 5)) +
  labs(x = "Top-n", y = "Ratio", fill = "Algorithm") +
  theme(axis.title.x = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.title.y = element_text(family = 'sans' , face = 2, color = 'black', size = 15)) +
  theme(axis.text.x  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(axis.text.y  = element_text(family = 'sans' , face = 1, color = 'black', size = 13)) +
  theme(plot.title   = element_text(family = 'sans' , face = 2, color = 'black', size = 13)) +
  theme(legend.text  = element_text(family = 'Times New Roman', size = 10)) +
  theme(legend.title = element_blank()) +
  geom_text(aes(label = round(Ratio, 2)), color = "black", vjust = -0.5, position = position_dodge(0.69), family = 'Times New Roman', size = 4) +
  scale_fill_manual(values = c("lightgrey", "powderblue", "lightsteelblue", "khaki", "burlywood", "lightpink", "darkseagreen"),
                    labels = c("AdaBoost", "GentlBoost", "RotationForest", "RandomForest", "RotationBoost", "FlexBoost", "RolexBoost")) +
  guides(fill = guide_legend(keywidth = 0.2, keyheight = 0.2, default.unit = "inch"))

print(figure.2)

#########################################################################################################################
### Appendix. Experiment
#########################################################################################################################
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
kfold.rolex(df = data, rot_iter = 1, flex_iter = iter, par.k = par_k)
  
