pkgs <- c("readtext","DescTools","robusHD", "recosystem","StatPerMeCo","ggplot2","reshape","OpenMx","factoextra","dplyr")
pkgTest <- function(pkgs){ 
  for (i in pkgs){
    if (!require(i,character.only = TRUE)){
      install.packages(i,dep=TRUE)
    }
  }
}
pkgTest(pkgs)

library(data.table)
library(readtext)
library(DescTools)
library(robustHD)
library(StatPerMeCo)
library(ggplot2)
library(reshape)
library(OpenMx)
library(factoextra)
library(matlab)
library(ggplot2)

############################################ Data  #####################################################
### Set brainD15 as working directory
setwd("~/Downloads/brainD15 ")
## Extract list of file names from the folder
file = dir(pattern = "\\.txt$")
## Import data
data_ori <- lapply(file, fread)

## Or could use the following code to import data for the first time. 
## Then save the data in Rstudio
#sublist <- list()
#for(i in 1:length(txtfile)){
#sublist[[subject[i]]] <- read.delim(paste0("/Users/yangxiaofei/Desktop/PHP 2650/HW2/brainD15/",txtfile[i]), 
# header = FALSE, sep = " ")
#}
## save sublist in RData
# save(sublist, file = "sublist.RData")
# Import big data
#load("sublist.RData")


############################################ 1  #####################################################

# Compute the correlation matrix (15 regions by 15 regions) for each subject
corsub <- lapply(data_ori, cor)
# Perform Fisher’s Z transform of correlation matrices
fishersub <- lapply(corsub, FisherZ)
# Set diagonal entries of F to 0
for(i in 1:length(fishersub)){
  for(j in 1:nrow(fishersub[[i]])){
    fishersub[[i]][j,j] <- 0
  }
}

############################################ 2 ####################################################### 

# Compute the averages and variances of each entry over all the 820 Fs matrices.
Fv <- apply(simplify2array(fishersub), 1:2, mean)
Fn <- apply(simplify2array(fishersub), 1:2, var)

############################################ 3 ####################################################### 

# Check the order the subjects by their subject IDs
# The orginal files are arranged by ID number
# So we could use the imported data
file_ord <- sort(as.vector(file),decreasing = FALSE)

## Seperate the original dataset into training data and testing data
# Compute the average matrix Ftrain and Ftest seperately
fishersub_train <- fishersub[1:410]
Ftrain <- apply(simplify2array(fishersub_train), 1:2, mean)
fishersub_test <- fishersub[-c(1:410)]
Ftest <- apply(simplify2array(fishersub_test), 1:2, mean)

############################################ 4 #######################################################
## Normalize the data matrix (4800 x 15) for each subject such that each columne should have mean 0 and variance 1.
data_num <- lapply(data_ori, as.data.frame)
X <- list(NA)
Xs <- lapply(data_num, scale)
# Seperate the normalized matrix into training data and testing data
Xtrain <- Xs[1:410]
Xtest <- Xs[-c(1:410)]

# Turn Xtrain and Xtest to matrix having a dimension of (4800*410) x 15.
Xtrain <- as.data.frame(do.call(rbind, Xtrain))
Xtest <- as.data.frame(do.call(rbind, Xtest))

## Explore the Pattern for Xs
# Choose a random sample from the 820 tests
# Reshape Xs[[1]]
# For example, choose the 1st one
Xs_1d <- as.data.frame(Xs[[1]])

## Density plot
## The 15 variables in sample test 1 follow normal distribution 
Xs_1 <- melt(Xs_1d)
pattern_Xs1_1 <- ggplot(Xs_1, aes (value)) +
  geom_density() +
  theme_bw()+
  facet_wrap(~variable)
print(pattern_Xs1_1)


# QQplot
# The linearity of the points suggests that the data are normally distributed.
pattern_Xs1_2 <- ggplot(Xs_1d, aes(sample=V3)) + stat_qq()
print(pattern_Xs1_2)

matplot(Xs_1d[1:30,1:13], type="l")

## Explore the Pattern for Ftrain
pattern_Ftrain_1 <- image(Ftrain, col=jet.colors(100))
print(pattern_Ftrain_1)
pattern_Ftrain_2 <- heatmap(Ftrain, Colv = NA, Rowv = NA, scale="column", col = cm.colors(256))
print(pattern_Ftrain_2)

############################################ PCA #######################################################
Xpca <- prcomp(Xtrain, center = TRUE, scale. = TRUE)
# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component
# pca_pattern <- fviz_eig(Xpca)
# print(pca_pattern)

## matrix factorization
PC <- as.matrix(Xpca$x)

# Find 95% Cumulative Proportion of Variance
# summary(Xpca)
# The 95% cumulative proportion of variance is at 13th component
# So choose the first 13 components
vars <- apply(Xpca$x, 2, var)  
props <- vars / sum(vars)
cp <- cumsum(props)
components <- c(1:15)

cp_df <- as.data.frame(cbind(components,cp))


pca_95 <- ggplot(cp_df, aes(x = components, y = cp))+
  geom_bar(stat="identity", fill="lightblue")+
  geom_hline(yintercept = 0.95)+ # add reference line
  theme_bw()

# Select 13 principal components so that we have 95% Cumulative Proportion of Variance
U <- PC[,1:13]

# Calculate inverse matrix
inverse_r <- t(as.matrix(Xpca$rotation)) 
G <- inverse_r[1:13,]

# Calculate UG matrix
UG<-U%*%G

# Calculate variance
C_ug <- cov(UG)
C_train <- cov(Xtrain)
C_test <- cov(Xtest)
CUGCtest <- norm(C_ug-C_test, "F")

CtrainCtest <- norm(C_train-C_test, "F")

############################################ SVD #######################################################
#SVD <- svd(Xtrain)

#svd_u <- SVD$u

#svd_d <- SVD$d
#svd_d <- vec2diag(svd_d) # Convert vector to diaganal matrix


#svd_v <- SVD$v
#svd_v_t <- t(svd_v) # transpose svd_v
# Xtain = SVD$u%*%svd_d%*%svd_v_t

# Select 13 components
#svd_u <- svd_u[,1:13]
#svd_d <- svd_d[1:13,1:13]
#svd_v_t <- svd_v_t[1:13,]

#U_s <- svd_u%*%svd_d

#G_s <- svd_v_t

#UG_svd <- svd_u%*%svd_d%*%svd_v_t

#C_ug_s <- cov(UG_svd)

#CUGCtest_s <- norm(C_ug_s-C_test, "F") # same as PCA


## PCA and SVD give the same result.


# Export
write.csv(Fn, "Fn.csv")
write.csv(Fv, "Fv.csv")
write.csv(Ftrain, "Ftrain.csv")
write.csv(Ftest, "Ftest.csv")
write.csv(U, "U.csv")
write.csv(G, "G.csv")
write.csv(C_ug, "CUG.csv")
write.csv(CUGCtest, "CUGCtest.csv")
write.csv(CtrainCtest, "CtrainCtest.csv")
write.csv(C_test, "Ctest.csv")
write.csv(C_train, "Ctrain.csv")
