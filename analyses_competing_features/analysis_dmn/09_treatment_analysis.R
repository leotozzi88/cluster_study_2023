#### Libraries, imports and global variables ####

library(rcompanion)
setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
data=read.csv('analyses_competing_features/analysis_dmn/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl.csv')

#### Treatment response analyses ####

# Scale severity between 0 and 1
data[!is.na(data$hdrs21_total_bl), 'treat_severity_bl_scaled']=data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl']/53 
data[!is.na(data$hdrs21_total_fu), 'treat_severity_fu_scaled']=data[!is.na(data$hdrs21_total_fu), 'hdrs21_total_fu']/53 
data[!is.na(data$scl20_bl), 'treat_severity_bl_scaled']=data[!is.na(data$scl20_bl), 'scl20_bl']/4
data[!is.na(data$scl20_fu), 'treat_severity_fu_scaled']=data[!is.na(data$scl20_fu), 'scl20_fu']/4

# Define remission
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu<=7, 'remission']=1
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu>7, 'remission']=0
data[!is.na(data$scl20_fu) & data$scl20_fu<=0.5, 'remission']=1
data[!is.na(data$scl20_fu) & data$scl20_fu>0.5, 'remission']=0

# Define response 
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu<=7, 'remission']=1
data[!is.na(data$hdrs21_total_fu) & data$hdrs21_total_fu>7, 'remission']=0
data[!is.na(data$scl20_fu) & data$scl20_fu<=0.5, 'remission']=1
data[!is.na(data$scl20_fu) & data$scl20_fu>0.5, 'remission']=0

# Define treatment response as % symptom change
data[!is.na(data$hdrs21_total_bl), 'perc_treat_resp']=(data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_fu']-data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl'])/data[!is.na(data$hdrs21_total_bl), 'hdrs21_total_bl']
data[!is.na(data$scl20_bl), 'perc_treat_resp']=(data[!is.na(data$scl20_bl), 'scl20_fu']-data[!is.na(data$scl20_bl), 'scl20_bl'])/data[!is.na(data$scl20_bl), 'scl20_bl']
data$response=(data$perc_treat_resp < -0.50)*1

# Select only treatment data
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU')
data_mod=data[data$treatment_arm %in% treats,]
data_mod$clu=factor(data_mod$clu)

# Test for effects of cluster on treatment
treat_results=as.data.frame(matrix(nrow=0, ncol=11))
names(treat_results)=c('Cluster', 'Treatment', 'greater', 'N', 'mdn', 'mdn_other','p', 'z', 'r', 'CI_l', 'CI_u')
for (treat in treats){
  for (clu in unique(data_mod$clu)){
    temp=data_mod[data_mod$treatment_arm==treat & data_mod$clu == clu, 'treat_severity_fu_scaled']
    temp_other=data_mod[data_mod$treatment_arm==treat & data_mod$clu != clu, 'treat_severity_fu_scaled']
    if ((sum(is.na(temp))<length(temp))&(length(temp)>5) & (length(temp_other)>5)){
      print(paste(treat, clu))
    res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
    z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
    mdn=median(temp, na.rm = T)
    mdn_other=median(temp_other, na.rm = T)
    dir=(mdn>mdn_other)*1
    N=sum(!is.na(temp))
    r=z/sqrt(N)
    treat_results[nrow(treat_results)+1, ]=c(clu, treat, dir, N, mdn, mdn_other, res$p.value, z, r, res$conf.int[1], res$conf.int[2])
    }
    }
}

# Export the results in one table
write.csv(treat_results, 'analyses_competing_features/analysis_dmn/out/treat_results_all.csv', row.names = F)

# Number of response/remission
table(data$treatment_arm, data$response, data$clu)
table(data$treatment_arm, data$remission, data$clu)
for (treat in treats){
  temp=data[data$treatment_arm==treat, ]
  res=chisq.test(temp$response, temp$clu)
  print(res$p.value)
}
for (treat in treats){
  temp=data[data$treatment_arm==treat, ]
  res=chisq.test(temp$remission, temp$clu)
  print(res$p.value)
}


# Print percent of response
for (clu in unique(data$clu)){
  temp=data[data$clu == clu, ]
  print(clu)
  tab=round(prop.table(table(temp$treatment_arm, temp$response), margin = 1)*100)
  print(tab)
}

# Print percent of remission
for (clu in unique(data$clu)){
  temp=data[data$clu == clu, ]
  print(clu)
  tab=round(prop.table(table(temp$treatment_arm, temp$remission), margin = 1)*100)
  print(tab)
}


#### Save data ####

write.csv(data, 'analyses_competing_features/analysis_dmn/out/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl_treat.csv')

