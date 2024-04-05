#### Libraries, imports and global variables ####

setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
source('analysis_main/custom_functions.R')
set.seed(123)
data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std.csv')

#### Clustering ####

img_vars=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')
X=data[, img_vars]

# Calculate distance
dist_mat = as.dist(1-cor(t(X)))

# Hierarchical clustering
hclust_res = hclust(dist_mat, method = "average")
 
#### Quantitative tests of clustering performance 

# Cluster numbers to test
nclu_test = c(2:15)
perms=10000

# Within sum of distances for all solutions
within_dist=withinclu_sum_dist(X, nclu_test)

plot(nclu_test, within_dist, type = "b", pch = 19, ylab = 'Sum of within cluster distances', xlab = 'Number of clusters')

# Significance of clustering indexes vs. multinormal distribution (in line with Dinga et al.)
mn_sils=multinorm_cluster_sig(X, nclu_test, n_sim = perms)

plot(nclu_test, mn_sils$sils, type = "b", pch = 19, ylab = 'Mean silhouette', xlab = 'Number of clusters')
plot(nclu_test, mn_sils$sil_ps, type = "b", pch = 19, ylab = 'Mean silhouette p-values', xlab = 'Number of clusters')

# Significance of clustering indexes vs. data permutation
perm_sils=perm_cluster_sig(X, nclu_test, n_perm = perms)

plot(nclu_test, perm_sils$sils, type = "b", pch = 19, ylab = 'Mean silhouette', xlab = 'Number of clusters')
plot(nclu_test, perm_sils$sil_ps, type = "b", pch = 19, ylab = 'Mean silhouette p-values', xlab = 'Number of clusters', ylim=c(0, 0.5))

# Stability of clustering with leave one out validation (in line with Dinga et al.)
loo_aris=ari_loo(X, nclu_test)
  
plot(nclu_test, loo_aris, type = "b", pch = 19, ylab = 'Adjusted Rand Index', xlab = 'Number of clusters')

# Stability of clustering on a fraction of participants
nclu_test_ari=ari_resampling(X, perc = 0.80, n_rep = perms, nclu_test = nclu_test)

plot(nclu_test, nclu_test_ari, type = "b", pch = 19, ylab = 'Adjusted Rand Index', xlab = 'Number of clusters')

#### Choose optimal solution ####

optclu=6
hcres=hcut(dist_mat, optclu, hc_method = 'average', isdiss=TRUE)
data$clu=hcres$cluster

# Rename clusters
data$clu=factor(data$clu, levels = c(3, 1, 2, 4, 5, 6), labels=c('Intact', 'Context insensitivity', 'Rest hyper-connectivity', 'Inattention', 'Cognitive dyscontrol hyper', 'Cognitive dyscontrol hypo'))

#### Save data ####

write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu.csv')



