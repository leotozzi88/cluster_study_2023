library(rcompanion)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

# Import data
data=read.csv('appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_clin_std_clu.csv')

# Scale severity between 0 and 1
data[!is.na(data$hdrs21_total_bl), 'treat_severity_bl_scaled']=data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl']/53 
data[!is.na(data$hdrs21_total_fu), 'treat_severity_fu_scaled']=data[!is.na(data$hdrs21_total_fu), 'hdrs21_total_fu']/53 
data[!is.na(data$scl20_bl), 'treat_severity_bl_scaled']=data[!is.na(data$scl20_bl), 'scl20_bl']/4
data[!is.na(data$scl20_fu), 'treat_severity_fu_scaled']=data[!is.na(data$scl20_fu), 'scl20_fu']/4

# Select only treatment data
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU')
data_mod=data[data$treatment_arm %in% treats,]
data_mod$clu=factor(data_mod$clu)

# Test for effects of cluster on treatment
treat_results=as.data.frame(matrix(nrow=0, ncol=9))
names(treat_results)=c('Cluster', 'Treatment', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r')
for (treat in treats){
  for (clu in unique(data_mod$clu)){
    temp=data_mod[data_mod$treatment_arm==treat & data_mod$clu == clu, 'treat_severity_fu_scaled']
    temp_other=data_mod[data_mod$treatment_arm==treat & data_mod$clu != clu, 'treat_severity_fu_scaled']
    if (sum(is.na(temp))<length(temp) & length(temp_other)>0){
      print(paste(treat, clu))
    res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
    z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
    mdn=median(temp, na.rm = T)
    mdn_other=median(temp_other, na.rm = T)
    dir=(mdn>mdn_other)*1
    N=sum(!is.na(temp))
    r=z/sqrt(N)
    treat_results[nrow(treat_results)+1, ]=c(clu, treat, dir, N, mdn, mdn_other, res$p.value, z, r)
    }
    }
}

# Export the results in one table
write.csv(treat_results, 'appeal/analyses_competing_features/analysis_rsonly/treat_results_all.csv', row.names = F)

# Export
write.csv(data, 'appeal/analyses_competing_features/analysis_rsonly/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl_treat.csv', row.names = FALSE)
