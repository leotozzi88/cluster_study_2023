#### Libraries ####

library(cluster)
library(WGCNA)
library(MASS)
library(mclust)
library(effectsize)
library(factoextra)
library(rcompanion)

#### Within-cluster sum of distances ####

withinclu_sum_dist = function(X, nclu_range) {
  nclu_sum_dist = c()
  
  # Calculate distance matrix for the data
  dist_mat = as.dist(1 - WGCNA::cor(t(X)))
  
  for (nclu in nclu_range) {
    res = hcut(dist_mat, nclu, hc_method = 'average')
    clu_sum_dist = c()
    
    for (clu in 1:nclu) {
      temp = X[res$cluster == clu,]
      dist = 1 - WGCNA::cor(t(temp))
      sum_dist = sum(dist[lower.tri(dist)])
      clu_sum_dist = append(clu_sum_dist, sum_dist)
    }
    
    nclu_sum_dist = append(nclu_sum_dist, sum(clu_sum_dist))
  }
  
  return(nclu_sum_dist)
}

#### Significance of clustering indexes vs. multinormal distribution (in line with Dinga et al.) ####

multinorm_cluster_sig = function(X, nclu_test, n_sim) {
  # Generate parameters for simulation
  covm = cov(X)
  mu = colMeans(X)
  n_sim_subs = nrow(X)
  
  sils = c()
  sil_ps = c()
  
  # Calculate distance matrix for real data
  dist_mat = as.dist(1 - WGCNA::cor(t(X)))
  
  # Loop through candidate number of clusters and store silhouette ps
  for (clu in nclu_test) {
    sil_all_sim = c()
    
    print('Running simulation')
    
    for (rep in 1:n_sim) {
      # Generate subject values from multivariate normal
      X_sim = mvrnorm(n = n_sim_subs, mu, covm)
      
      # Cluster matrix
      dist_mat_sim = as.dist(1 - WGCNA::cor(t(X_sim)))
      hcres_sim = hcut(dist_mat_sim,
                       clu,
                       hc_method = 'average',
                       isdiss = TRUE)
      
      # Calculate silhouette in simulation
      sil_sim_res = silhouette(x = hcres_sim$cluster, dist = dist_mat_sim)
      sil_all_sim = append(sil_all_sim, mean(sil_sim_res[, 3]))
      
    }
    
    # Calculate real silhouette
    hclust_real =
      hcut(dist_mat, clu, hc_method = 'average', isdiss = TRUE)
    sil_real_res = silhouette(x = hclust_real$cluster, dist = dist_mat)
    sil_real_mean = mean(sil_real_res[, 3])
    sils = append(sils, sil_real_mean)
    
    # Get p-values of silhouette
    p_sil = sum(sil_all_sim > sil_real_mean) / n_sim
    sil_ps = append(sil_ps, p_sil)
    
    print(paste('nclu:', clu, 'sil:', sil_real_mean, 'p:', p_sil))
    
  }
  
  return(list(sils = sils, sil_ps = sil_ps))
  
}


#### Significance of clustering indexes vs. data permutation ####

perm_cluster_sig = function(X, nclu, n_perm) {
  sils = c()
  sil_ps = c()
  
  # Calculate distance matrix for real data
  dist_mat = as.dist(1 - WGCNA::cor(t(X)))
  
  # Loop through candidate number of clusters and store silhouette ps
  for (clu in nclu) {
    sil_all_sim = c()
    
    print('Running simulation')
    for (rep in 1:n_perm) {
      # Initialize matrix
      X_sim = matrix(nrow = nrow(X), ncol = ncol(X))
      
      # Generate matrix by shuffling rows of X for each column
      for (col in 1:ncol(X)) {
        X_sim[, col] = sample(X[, col], nrow(X), replace = FALSE)
      }
      
      # Cluster matrix
      dist_mat_sim = as.dist(1 - WGCNA::cor(t(X_sim)))
      hcres_sim = hcut(dist_mat_sim,
                       clu,
                       hc_method = 'average',
                       isdiss = TRUE)
      
      # Calculate silhouette in simulation
      sil_sim_res = silhouette(x = hcres_sim$cluster, dist = dist_mat_sim)
      sil_all_sim = append(sil_all_sim, mean(sil_sim_res[, 3]))
    }
    
    # Calculate real silhouette
    hclust_real =
      hcut(dist_mat, clu, hc_method = 'average', isdiss = TRUE)
    sil_real_res = silhouette(x = hclust_real$cluster, dist = dist_mat)
    sil_real_mean = mean(sil_real_res[, 3])
    sils = append(sils, sil_real_mean)
    
    # Get p-values of silhouette
    p_sil = sum(sil_all_sim > sil_real_mean) / n_perm
    sil_ps = append(sil_ps, p_sil)
    
    print(paste('nclu:', clu, 'sil:', sil_real_mean, 'p:', p_sil))
  }
  
  return(list(sils = sils, sil_ps = sil_ps))
}

#### Stability of clustering with leave one out validation (in line with Dinga et al.) ####

ari_loo = function(X, nclu) {
  nclu_test_ari = c()
  
  # Calculate distance matrix for real data
  dist_mat = as.dist(1 - WGCNA::cor(t(X)))
  
  for (clu in nclu) {
    ari_all = c()
    
    print('Running LOO')
    
    # Do clustering
    hclust_all =
      hcut(dist_mat, clu, hc_method = 'average', isdiss = TRUE)
    
    for (sam_idx in 1:nrow(X)) {
      # Extract one sample of participants
      sam = X[-sam_idx, ]
      
      # Do the clustering
      dist_mat_sam = as.dist(1 - WGCNA::cor(t(sam)))
      hcres_sam = hcut(dist_mat_sam,
                       clu,
                       hc_method = 'average',
                       isdiss = TRUE)
      clu_sam = hcres_sam$cluster
      
      # Get the original cluster assignments for the same participants
      clu_orig = hclust_all$cluster[-sam_idx]
      
      # Calculate overlap
      ari_all = append(ari_all, adjustedRandIndex(clu_sam, clu_orig))
    }
    
    # Save ARI
    nclu_test_ari = append(nclu_test_ari, mean(ari_all))
    print(paste('nclu:', clu, 'mean ARI:', mean(ari_all)))
  }
  
  return(nclu_test_ari)
}

#### Stability of clustering on a fraction of participants ####

ari_resampling = function(X, perc, n_rep, nclu_test) {
  n_rows = nrow(X)
  nclu_test_ari = c()
  
  # Calculate distance matrix for real data
  dist_mat = as.dist(1 - WGCNA::cor(t(X)))
  
  for (clu in nclu_test) {
    print('Running resampling')
    
    # Do clustering
    hclust_all =
      hcut(dist_mat, clu, hc_method = 'average', isdiss = TRUE)
    ari_all = c()
    
    for (rep in 1:n_rep) {
      # Extract a sample of participants
      rows_to_keep =
        sample(n_rows, round(perc * n_rows), replace = FALSE)
      
      # Subset the dataframe
      sam = X[rows_to_keep, ]
      
      # Do the clustering
      dist_mat_sam = as.dist(1 - WGCNA::cor(t(sam)))
      hcres_sam = hcut(dist_mat_sam,
                       clu,
                       hc_method = 'average',
                       isdiss = TRUE)
      clu_sam = hcres_sam$cluster
      
      # Get the original cluster assignments for the same participants
      clu_orig = hclust_all$cluster[rows_to_keep]
      
      # Calculate overlap
      ari_all = append(ari_all, mclust::adjustedRandIndex(clu_sam, clu_orig))
    }
    
    # Save ARI
    nclu_test_ari = append(nclu_test_ari, mean(ari_all))
    print(paste('nclu:', clu, 'mean ARI:', mean(ari_all)))
  }
  
  return(nclu_test_ari)
}


#### Compare participants in cluster to participants not in cluster using Wilcoxon Z ####

compare_clusters = function(data,
                            cluster_assignments,
                            var_names,
                            onesided = F) {
  # Add cluster assignments to the dataframe
  data$clu = cluster_assignments
  
  # Initialize the results dataframe
  results = as.data.frame(matrix(nrow = 0, ncol = 11))
  names(results) = c(
    'Cluster',
    'Symptom_composite',
    'greater',
    'N',
    'mdn',
    'mdn_other',
    'p',
    'z',
    'r',
    'CI_l',
    'CI_u'
  )
  
  for (clu in unique(data$clu)) {
    for (var in var_names) {
      temp = data[data$clu == clu, var]
      temp_other = data[data$clu != clu, var]
      
      # Check that there are enough people with data
      if (sum(is.na(temp)) < length(temp)) {
        res = wilcox.test(
          x = temp,
          mu = median(temp_other, na.rm = TRUE),
          conf.int = TRUE
        )
        z = wilcoxonZ(x = temp, mu = median(temp_other, na.rm = TRUE))
        mdn = median(temp, na.rm = TRUE)
        mdn_other = median(temp_other, na.rm = TRUE)
        dir = (mdn > mdn_other) * 1
        N = sum(!is.na(temp))
        r = z / sqrt(N)
        p = res$p.value
        
        if (onesided) {
          p = p / 2
        }
        
        results[nrow(results) + 1,] = c(clu,
                                        var,
                                        dir,
                                        N,
                                        mdn,
                                        mdn_other,
                                        p,
                                        z,
                                        r,
                                        res$conf.int[1],
                                        res$conf.int[2])
      } else {
        results[nrow(results) + 1,] = c(clu, var, NA, 0, NA, NA, NA, NA, NA, NA, NA)
      }
    }
  }
  
  return(results)
}

#### Compare participants in cluster to participants not in cluster using chi-square ####

compare_clusters_chi_square = function(data,
                                       cluster_assignments,
                                       var_names,
                                       onesided = F) {
  # Add cluster assignments to the dataframe
  data$clu = cluster_assignments
  
  # Initialize the results dataframe
  results = as.data.frame(matrix(nrow = 0, ncol = 7))
  names(results) = c('Cluster',
                     'Symptom_composite',
                     'N',
                     'frac',
                     'frac_other',
                     'p',
                     'chi')
  
  for (clu in unique(data$clu)) {
    for (var in var_names) {
      # Perform Chi-square test
      res = chisq.test(data$clu == clu, data[, var] > 0)
      p = res$p.value
      
      if (onesided) {
        p = p / 2
      }
      
      # Calculate fractions
      frac = sum(data[data$clu == clu, var] > 0, na.rm = TRUE) / sum(data$clu == clu, na.rm = TRUE)
      frac_other = sum(data[data$clu != clu, var] > 0, na.rm = TRUE) / sum(data$clu != clu, na.rm = TRUE)
      N = sum(data$clu == clu, na.rm = TRUE)
      
      # Append results
      results[nrow(results) + 1,] = c(clu, var, N, frac, frac_other, p, res$statistic)
    }
  }
  
  return(results)
}


#### Cluster on one split-half and assign participants in the second to a cluster ####

splithalf_and_cluster = function(X, nclu, img_vars_names) {
  # Split into two halves
  perc = 0.5
  num_in_sample1 = round(nrow(X) * perc)
  sample_indicator = c(rep(1, num_in_sample1), rep(2, nrow(X) - num_in_sample1))
  sample_indicator = sample(sample_indicator)
  X$split2 = sample_indicator
  data_spl1 = X[X$split2 == 1,]
  data_spl2 = X[X$split2 == 2,]
  
  # Cluster on the first subset of data
  dist_mat = as.dist(1 - WGCNA::cor(t(data_spl1[, img_vars_names])))
  hcres = hcut(dist_mat, nclu, hc_method = 'average', isdiss = TRUE)
  data_spl1$clu_spl2 = hcres$cluster
  
  # Calculate mean profile for each cluster
  clu_mean_mat = matrix(nrow = max(data_spl1$clu_spl2),
                        ncol = length(img_vars_names))
  for (clu in 1:max(data_spl1$clu_spl2)) {
    clu_mean_mat[clu,] = colMeans(data_spl1[data_spl1$clu_spl2 == clu, img_vars_names], na.rm = TRUE)
  }
  
  # Assign each subject in the second subset to a cluster
  for (rr in 1:nrow(data_spl2)) {
    subprof = as.numeric(data_spl2[rr, img_vars_names])
    cors = numeric(max(data_spl1$clu_spl2))
    for (clu in 1:max(data_spl1$clu_spl2)) {
      cors[clu] = cor(subprof, clu_mean_mat[clu,], use = "complete.obs")
    }
    data_spl2[rr, 'clu_spl2'] = which.max(cors)
  }
  
  return(list(data_spl1 = data_spl1, data_spl2 = data_spl2))
  
}

#### Cluster on all data except one study and assign participants in the study to a cluster ####

splitstudy_and_cluster = function(data, studyvec, nclu, img_vars_names) {
  for (study in unique(studyvec)) {
    # Create split variable
    splname = paste(study, '_study_spl2', sep = '')
    cluname = paste(study, '_cluspl', sep = '')
    data[studyvec != study, splname] = 0
    data[studyvec == study, splname] = 1
    
    # Cluster on the data not in the study
    X = data[studyvec != study, img_vars_names]
    dist_mat = as.dist(1 - WGCNA::cor(t(X)))
    hcres = hcut(dist_mat, nclu, hc_method = 'average', isdiss = TRUE)
    data[studyvec != study, cluname] = hcres$cluster
    
    # Calculate mean profile for each biotype
    clu_mean_mat = matrix(nrow = nclu, ncol = length(img_vars_names))
    for (clu in 1:nclu) {
      clu_mean_mat[clu,] = colMeans(data[data[, splname] == 0 &
                                           data[, cluname] == clu, img_vars_names])
    }
    
    # Assign each subject in the second subset to a biotype
    data_spl1 = data[data[, splname] == 0,]
    data_spl2 = data[data[, splname] == 1,]
    for (rr in 1:nrow(data_spl2)) {
      subprof = as.numeric(data_spl2[rr, img_vars_names])
      cors = numeric(nclu)
      for (clu in 1:max(data_spl1[, cluname])) {
        cors[clu] = cor(subprof, clu_mean_mat[clu,])
      }
      data[data[, splname] == 1,][rr, cluname] = which.max(cors)
    }
  }
  return(data)
}
