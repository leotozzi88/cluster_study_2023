library(data.table)
library(factoextra)
library(rcompanion)
library(vroom)
library(ggplot2)
library(dplyr)
library(tidyr)

#### Validation of clusters in split-half data ####

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')
set.seed(123123)

data=read.csv('appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_clin_std_clu.csv')
img_vars=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')

optclu=6

#### Calculate symptom composites ####

data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

# Rename variables
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score')
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol')
data=setnames(data, comps, comps_names)

#### Calculate behavior composites ####

# Rename variables
allbeh_wn=c('wn_emzcompk_norm', 'wn_emzerrk_norm', 'wn_g2avrtk_norm', 'wn_g2fpk_norm', 'wn_wmfnk_norm', 'wn_wmfpk_norm', 'wn_wmrtk_norm', 'wn_dgttrta_norm', 'wn_dgttrtf_norm', 'wn_dgttrth_norm', 'wn_dgttrts_norm', 'wn_dgttrtn_norm', 'wn_gettrta_norm', 'wn_gettrtf_norm', 'wn_gettrth_norm', 'wn_gettrts_norm')
comps_rename=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')
setnames(data, old = allbeh_wn, new = comps_rename)

# Flip sign of variables to interpret them in their original direction
data[, comps_rename]=-data[, comps_rename]

# Calculate composites
data[, 'Implicit_fear_priming_RT']=data[, 'Implicit_fear_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_anger_priming_RT']=data[, 'Implicit_anger_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_sad_priming_RT']=data[, 'Implicit_sad_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_happy_priming_RT']=data[, 'Implicit_happy_RT']-data[, 'Implicit_neutral_RT']
data[, 'Implicit_threat_priming_RT']=rowMeans(data[, c('Implicit_fear_priming_RT', 'Implicit_anger_priming_RT')], na.rm = TRUE)
data[, 'Explicit_threat_RT']=rowMeans(data[, c('Explicit_anger_RT', 'Explicit_fear_RT')], na.rm = TRUE)

#### Split half assignment ####

# Split into two halves
perc=0.5
num_in_sample1 <- round(nrow(data) * perc)
sample_indicator <- c(rep(1, num_in_sample1), rep(2, nrow(data) - num_in_sample1))
sample_indicator <- sample(sample_indicator)
data$split2 <- sample_indicator
data_spl1=data[data$split2==1, ]
data_spl2=data[data$split2==2, ]

# Cluster on the first subset of data
X=data_spl1[, img_vars]
dist_mat <- as.dist(1-cor(t(X)))
hcres=hcut(dist_mat, optclu, hc_method = 'average', isdiss=TRUE)
data_spl1[, 'clu_spl2']=hcres$cluster

# Calculate mean profile for each biotype
clu_mean_mat=matrix(nrow = max(data_spl1$clu_spl2), ncol = length(img_vars))
for (clu in 1:max(data_spl1$clu_spl2)){
  clu_mean_mat[clu, ]=colMeans(data_spl1[data_spl1$clu_spl2==clu, img_vars])
}

# Assign each subject in the second subset to a cluster
for (rr in 1:nrow(data_spl2)){
  subprof=as.numeric(data_spl2[rr, img_vars])
  cors=c()
  for (clu in 1:max(data_spl1$clu_spl2)){
    cors[clu]=cor(subprof, clu_mean_mat[clu, ])
  }
  data_spl2[rr, 'clu_spl2']=which(cors==max(cors))
}


# Add assignments to original data frame
data[data$split2==1, 'clu_spl2']=factor(data_spl1$clu_spl2)
data[data$split2==2, 'clu_spl2']=factor(data_spl2$clu_spl2)

# Assign names to clusters
data_spl1$clu_spl2=factor(data_spl1$clu_spl2)
data_spl2$clu_spl2=factor(data_spl2$clu_spl2)

#### Plot mean circuit profiles ####

# Calculate the mean for each feature, grouped by split2 and clu_spl2
data_means <- data %>%
  select(all_of(img_vars), split2, clu_spl2) %>%
  group_by(split2, clu_spl2) %>%
  summarise(across(all_of(img_vars), mean)) %>%
  pivot_longer(cols = all_of(img_vars), names_to = "feature", values_to = "mean")

# Plot the lines with panels for each clu_spl2 value
png(file=paste("appeal/analyses_competing_features/analysis_rsonly/circuit_profile_splhalf.png", sep=''),width=800, height=800)
ggplot(data_means, aes(x = feature, y = mean, group = split2, color = as.factor(split2))) +
  geom_line() +
  scale_color_manual(values = c("1" = "black", "2" = "red")) +
  facet_wrap(~ clu_spl2, ncol=1) +
  labs(title = "Mean of Features by Group",
       x = "Feature",
       y = "Mean") +
  theme_minimal()
dev.off()


#### Validation of symptom profiles in split-half data ####
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol')

# Test each cluster vs median of clinical participants not in the cluster
symps_results1=as.data.frame(matrix(nrow=0, ncol=9))
names(symps_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1', 'r_spl1')
for (clu in unique(data_spl1$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl1[data_spl1$clu_spl2==clu, comp ]
    temp_other=data_spl1[data_spl1$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
    res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
    z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
    mdn=median(temp, na.rm = T)
    mdn_other=median(temp_other, na.rm = T)
    dir=(mdn>mdn_other)*1
    N=sum(!is.na(temp))
    r=z/sqrt(N)
    symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value/2, z, r) #one sided confirmatory test
    }
    else{
      symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
    }
  }
}

# Test each cluster vs median of clinical participants not in the cluster
symps_results2=as.data.frame(matrix(nrow=0, ncol=9))
names(symps_results2)=c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'mdn_spl2', 'mdn_other_spl2','p_spl2', 'z_spl2', 'r_spl2')
for (clu in unique(data_spl2$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl2[data_spl2$clu_spl2==clu, comp ]
    temp_other=data_spl2[data_spl2$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
      mdn=median(temp, na.rm = T)
      mdn_other=median(temp_other, na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      r=z/sqrt(N)
      symps_results2[nrow(symps_results2)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value/2, z, r) #one sided confirmatory test
    }
    else{
      symps_results2[nrow(symps_results2)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
symps_results_merged=merge(symps_results1, symps_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(symps_results_merged, 'appeal/analyses_competing_features/analysis_rsonly/symps_results_sh_merged.csv', row.names = F)


#### Validation of behavior profiles in split-half data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
beh_results1=as.data.frame(matrix(nrow=0, ncol=9))
names(beh_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1', 'r_spl1')
for (clu in unique(data_spl1$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl1[data_spl1$clu_spl2==clu, comp ]
    temp_other=data_spl1[data_spl2$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
      mdn=median(temp, na.rm = T)
      mdn_other=median(temp_other, na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      r=z/sqrt(N)
      beh_results1[nrow(beh_results1)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value/2, z, r) #one sided confirmatory test
    }
    else{
      beh_results1[nrow(beh_results1)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
    }
  }
}

# Test each cluster vs median of clinical participants not in the cluster
beh_results2=as.data.frame(matrix(nrow=0, ncol=9))
names(beh_results2)=c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'mdn_spl2', 'mdn_other_spl2','p_spl2', 'z_spl2', 'r_spl2')
for (clu in unique(data_spl2$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl2[data_spl2$clu_spl2==clu, comp ]
    temp_other=data_spl2[data_spl2$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
      mdn=median(temp, na.rm = T)
      mdn_other=median(temp_other, na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      r=z/sqrt(N)
      beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, dir, N, mdn, mdn_other,  res$p.value/2, z, r) #one sided confirmatory test
    }
    else{
      beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
beh_results_merged=merge(beh_results1, beh_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(beh_results_merged, 'appeal/analyses_competing_features/analysis_rsonly/beh_results_sh_merged.csv', row.names = F)

# Export
write.csv(data, 'appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl.csv', row.names = F)


