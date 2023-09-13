library(factoextra)
library(MASS)
library(cluster)
library(pdfCluster)
library(WGCNA) # for faster cor function
library(ggcorrplot)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

set.seed(123)

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std.csv')

#### Clustering ####
img_vars_names_final=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')
X=data[, img_vars_names_final]

# Calculate distance
dist_mat <- as.dist(1-cor(t(X)))

# Hierarchical clustering
hclust_res <- hclust(dist_mat, method = "average")

# Within sum of distances for all solutions
nclu_sum_dist=c()
for (nclu in 2:15){
  res=hcut(dist_mat,nclu, hc_method = 'average')
  clu_sum_dist=c()
  for (clu in 1:nclu) {
    temp=X[res$cluster==clu, ]
    dist=1-cor(t(temp))
    sum_dist=sum(dist[lower.tri(dist)])
    clu_sum_dist=append(clu_sum_dist, sum_dist)
  }
  nclu_sum_dist=append(nclu_sum_dist, sum(clu_sum_dist))
}
plot(2:15, nclu_sum_dist, type = "b", pch = 19, ylab = 'Sum of within cluster distances', xlab = 'Number of clusters')

#### Significance of clustering indexes vs. multinormal distribution (in line with Dinga et al.) #### 
nclu_test=c(2:15)
n_sim_rep=10000

# Generate parameters for simulation
covm=cov(X)
mu=colMeans(X)
n_sim_subs=nrow(X)

sils=c()
sil_ps=c()

# Loop through candidate number of clusters and store silhouette ps
for (clu in nclu_test){
  
  sil_all_sim=c()
  
  print('Running simulation')
  for (rep in 1:n_sim_rep){

    # Initialize matrix
    X_sim=matrix(nrow=nrow(X), ncol=ncol(X))
    
    # Generate subject values from multivariate normal
    X_sim=mvrnorm(n = n_sim_subs, mu, covm)
    
    # Cluster matrix
    dist_mat_sim <- as.dist(1-WGCNA::cor(t(X_sim)))
    hcres_sim=hcut(dist_mat_sim, clu, hc_method = 'average', isdiss=TRUE)
    
    # Calculate silhouette in simulation (CHI is not valid because distance is not euclidean)
    sil_sim_res=silhouette(x = hcres_sim$cluster, dist = dist_mat_sim)
    sil_all_sim=append(sil_all_sim, mean(sil_sim_res[, 3]))
    
  }
  
  # Calculate real silhouette
  hclust_real <- hcut(dist_mat, clu, hc_method = 'average', isdiss=TRUE)
  sil_real_res=silhouette(x = hclust_real$cluster, dist = dist_mat)
  sil_real_mean=mean(sil_real_res[, 3])
  sils=append(sils, sil_real_mean)
  
  # Get p-values of silhouette 
  p_sil=sum(sil_all_sim>sil_real_mean)/n_sim_rep
  sil_ps=append(sil_ps, p_sil)
  
  print(paste('nclu:', clu, 'sil:', sil_real_mean, 'p:', p_sil))
  
}

plot(nclu_test, sils, type = "b", pch = 19, ylab = 'Mean silhouette', xlab = 'Number of clusters')
plot(nclu_test, sil_ps, type = "b", pch = 19, ylab = 'Mean silhouette p-values', xlab = 'Number of clusters')

#### Significance of clustering indexes vs. data permutation #### 

nclu_test=c(2:15)
n_sim_rep=10000
sils=c()
sil_ps=c()

# Loop through candidate number of clusters and store silhouette ps
for (clu in nclu_test){
  
  sil_all_sim=c()
  
  print('Running simulation')
  for (rep in 1:n_sim_rep){
    
    # Initialize matrix
    X_sim=matrix(nrow=nrow(X), ncol=ncol(X))
    
    # Generate matrix by shuffling rows of X for each column
    for (col in 1:ncol(X)){
    X_sim[, col]=sample(X[, col], nrow(X), replace = F)
    }
    
    # Cluster matrix
    dist_mat_sim <- as.dist(1-WGCNA::cor(t(X_sim)))
    hcres_sim=hcut(dist_mat_sim, clu, hc_method = 'average', isdiss=TRUE)
    
    # Calculate silhouette in simulation (CHI is not valid because distance is not euclidean)
    sil_sim_res=silhouette(x = hcres_sim$cluster, dist = dist_mat_sim)
    sil_all_sim=append(sil_all_sim, mean(sil_sim_res[, 3]))
    
  }
  
  # Calculate real silhouette
  hclust_real <- hcut(dist_mat, clu, hc_method = 'average', isdiss=TRUE)
  sil_real_res=silhouette(x = hclust_real$cluster, dist = dist_mat)
  sil_real_mean=mean(sil_real_res[, 3])
  sils=append(sils, sil_real_mean)
  
  # Get p-values of silhouette 
  p_sil=sum(sil_all_sim>sil_real_mean)/n_sim_rep
  sil_ps=append(sil_ps, p_sil)
  
  print(paste('nclu:', clu, 'sil:', sil_real_mean, 'p:', p_sil))
  
}

plot(nclu_test, sils, type = "b", pch = 19, ylab = 'Mean silhouette', xlab = 'Number of clusters')
plot(nclu_test, sil_ps, type = "b", pch = 19, ylab = 'Mean silhouette p-values', xlab = 'Number of clusters', ylim=c(0, 0.5))

#### Stability of clustering with leave one out validation (in line with Dinga et al.) #### 

nclu_test_ari=c()
for (clu in nclu_test){

  ari_all=c()
    
  print('Running LOO')
  
  # Do clustering
  hclust_all  <- hcut(dist_mat, clu, hc_method = 'average', isdiss=TRUE)
  
  for (sam_idx in 1:nrow(X)){

    # Extract one sample of participants
    sam=X[-sam_idx, ]
    
    # Do the clustering 
    dist_mat_sam <- as.dist(abs(1-WGCNA::cor(t(sam))))
    hcres_sam=hcut(dist_mat_sam, clu, hc_method = 'average', isdiss=TRUE)
    clu_sam=hcres_sam$cluster
    
    # Get the original cluster assignments for the same participants
    clu_orig=hclust_all$cluster[-sam_idx]
    
    # Calculate overlap
    ari_all=append(ari_all, adj.rand.index(clu_sam, clu_orig))
  
  }
  
  # Save ARI
  nclu_test_ari=append(nclu_test_ari, mean(ari_all))
  print(paste('nclu:', clu, 'mean ARI:', mean(ari_all)))
  
}

plot(nclu_test, nclu_test_ari, type = "b", pch = 19, ylab = 'Adjusted Rand Index', xlab = 'Number of clusters')

#### Stability of clustering on a fraction of participants #### 

perc=0.8
nrep=1000
n_rows=nrow(X)

sameclu_mat=array(NA, dim = c(nrow(X), nrow(X), nrep))
nclu_test_ari=c()
nclu_same_clusters=c()
nclu_diff_clusters=c()
for (clu in nclu_test){
  
  print('Running resampling')
  
  # Do clustering
  hclust_all  <- hcut(dist_mat, clu, hc_method = 'average', isdiss=TRUE)
  ari_all=c()
  for (rep in 1:nrep){
    
    # Extract one sample of participants
    rows_to_keep <- sample(n_rows, round(0.8*n_rows), replace = FALSE)
    
    # Subset the dataframe
    sam <- X[rows_to_keep, ]
    
    # Do the clustering 
    dist_mat_sam <- as.dist(abs(1-WGCNA::cor(t(sam))))
    hcres_sam=hcut(dist_mat_sam, clu, hc_method = 'average', isdiss=TRUE)
    clu_sam=hcres_sam$cluster

    # Add cluster assignment to dataframe
    X_temp <- X
    X_temp[rows_to_keep, 'clu_sam']=clu_sam
    
    # Get the original cluster assignments for the same participants
    clu_orig=hclust_all$cluster[rows_to_keep]
    
    # Calculate overlap
    ari_all=append(ari_all, adj.rand.index(clu_sam, clu_orig))
    
  }
  
  # Save ARI
  nclu_test_ari=append(nclu_test_ari, mean(ari_all))
  print(paste('nclu:', clu, 'mean ARI:', mean(ari_all)))
  
}

plot(nclu_test, nclu_test_ari, type = "b", pch = 19, ylab = 'Adjusted Rand Index', xlab = 'Number of clusters')

#### Choose optimal solution ####
optclu=6
hcres=hcut(dist_mat, optclu, hc_method = 'average', isdiss=TRUE)
data$clu=hcres$cluster

# Rename clusters
data$clu=factor(data$clu, levels = c(3, 1, 2, 4, 5, 6), labels=c('Intact', 'Context insensitivity', 'Rest hyper-connectivity', 'Inattention', 'Cognitive dyscontrol hyper', 'Cognitive dyscontrol hypo'))

# Save data
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu.csv', row.names = FALSE)

