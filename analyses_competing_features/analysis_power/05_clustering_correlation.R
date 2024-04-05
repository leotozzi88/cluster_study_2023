#### Libraries, imports and global variables ####

library(vroom)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
source('analysis_main/custom_functions.R')
set.seed(123)
data=as.data.frame(vroom('analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std.csv'))

#### Clustering ####

img_vars=grep("power_", names(data), value = TRUE)
X=data[, img_vars]

# Calculate distance
dist_mat = as.dist(1-cor(t(X)))

# Hierarchical clustering
hclust_res = hclust(dist_mat, method = "average")

#### Quantitative tests of clustering performance 

# Cluster numbers to test
nclu_test = c(2, 6)
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

#### Save data ####

write.csv(data, 'analyses_competing_features/analysis_power/out/dataset_merged_qc_imputed_combat_clin_std_clu.csv')



