#Nama   : Adam Ramadhan
#NIM    : 1305621002
#Prodi  : Matematika 2021

#Package
library(readxl)
library(dplyr)
library(ppclust)
library(factoextra)
library(cluster)
library(fclust)
library(clusterSim)
library(ggplot2)
library(ggpubr)
library(pastecs)
library(clValid)
library(fpc)
library(AER)
library(MASS)
library(corrplot)
library(olsrr)

#Data
data_penelitian <- data_penelitian[,-1]
data_penelitian

#Statistik Deskriptif
summary(data_penelitian)

#VIF
cekVIF <- function(data){
  corr=as.matrix(cor(data))
  VIF=diag(solve(corr))
  
  return(VIF)
}
cekVIF(data_penelitian)

ivar <- data_penelitian
ivaricor <- ginv(cor(ivar))
colnames(ivaricor) <- colnames(ivar)
rownames(ivaricor) <- colnames(ivar)
ivaricor
corrplot(corr=ivaricor,method="number",is.corr=FALSE)

#Metric Distance
'euclidean distance'
jarak_euclidean <- dist(data_penelitian,method="euclidean")
print(jarak_euclidean)
'square euclidean distance'
jarak_square_euclidean <- jarak_euclidean^2
print(jarak_square_euclidean)

#Metode K-Medoids
'c = 3'
k_medoids_3 <- pam(data_penelitian,k=3,medoids=c(1,2,3),do.swap=TRUE)
summary(k_medoids_3)
'c = 4'
k_medoids_4 <- pam(data_penelitian,k=4,medoids=c(1,2,3,4),do.swap=TRUE)
summary(k_medoids_4)

#Metode Fuzzy Possibilistic C-Means
'c = 2'
fuzzy_c_means_2 <- fcm(x=data_penelitian,centers=2,m=2,dmetric="sqeuclidean",iter.max = 29)
fuzzy_possibilistic_c_means_2 <- fpcm(x=data_penelitian,centers=fuzzy_c_means_2$v,membership=fuzzy_c_means_2$u,m=2,eta=2,iter.max = 10)
summary(fuzzy_possibilistic_c_means_2)

#Silhouette (c optimal untuk K-Medoids)
'c = 3'
summary(k_medoids_3)
'c = 4'
summary(k_medoids_4)

#PE, PC dan MPC (c optimal untuk Fuzzy Possibilistic C-Means)
fpcm.indeks <- ppclust2(fuzzy_possibilistic_c_means_2,"fanny")
'Partition Entropy'
fpcm_indeks_pe <- PE(fpcm.indeks$membership)
fpcm_indeks_pe
'Partition Coefficient'
fpcm_indeks_pc <- PC(fpcm.indeks$membership)
fpcm_indeks_pc
'Modified Partition Coefficient'
fpcm_indeks_mpc <- MPC(fpcm.indeks$membership)
fpcm_indeks_mpc

#Davies-Bouldin Index
'K-Medoids c = 3'
dbi_3 <- index.DB(x=data_penelitian,d=jarak_euclidean,cl=k_medoids_3$clustering,centrotypes="medoids",p=2,q=1)
print(dbi_3)
'K-Medoids c = 4'
dbi_4 <- index.DB(x=data_penelitian,d=jarak_euclidean,cl=k_medoids_4$clustering,centrotypes="medoids",p=2,q=1)
print(dbi_4)
'FPCM c = 2'
dbi_2 <- index.DB(x=data_penelitian,cl=fuzzy_possibilistic_c_means_2$cluster,centrotypes="centroids",p=2,q=1)
print(dbi_2)