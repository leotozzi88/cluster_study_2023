setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu.csv')

#### Calculate composites ####

# Calculate composites
data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

#### Symptoms analyses ####

comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score', 'Suicide', 'Sleep', 'qids_total')

# Test each cluster vs median of all clinical participants
symps_results=as.data.frame(matrix(nrow=0, ncol=9))
names(symps_results)=c('Symptom composite', 'Cluster', 'greater', 'N', 'p', 'sig', 'pFDR', 'sig FDR', 'pFDR_comp')
for (comp in comps){
  for (clu in unique(data$clu)){
  temp=data[data$clu==clu, comp ]
  res=wilcox.test(x=temp, mu = median(data[, comp], na.rm = TRUE))
  dir=(median(temp, na.rm = TRUE)>median(data[, comp], na.rm = TRUE))*1
  if(res$p.value<0.05){
    print(paste(comp, clu, res$p.value))
  }
  
  N=length(temp)
  
  symps_results[nrow(symps_results)+1, 1:8]=c(comp, clu, dir, N, res$p.value, (res$p.value<0.05)*1, NA, NA)
  
  }
  
  ps=symps_results[symps_results$`Symptom composite`==comp, 'p']
  symps_results[symps_results$`Symptom composite`==comp, 'pFDR_comp']=round(p.adjust(ps, method = 'fdr'), 3)
  
}
symps_results[, 'pFDR']=round(p.adjust(symps_results[, 'p'], method = 'fdr'), 3)
symps_results[, 'sig FDR']=(symps_results[, 'pFDR']<0.05)*1

# Export results
write.csv(symps_results, '/Users/ltozzi/Dropbox (PanLab)/cluster paper/tables/symps_results.csv', row.names = FALSE)

# Test the impact of other variables
summary(aov(age~clu, data = data))
chisq.test(data$clu, data$gender)
chisq.test(data$clu, data$mdd_current)
chisq.test(data$clu, data$gad_current)
chisq.test(data$clu, data$panic_current)
chisq.test(data$clu, data$social_phobia_current)
chisq.test(data$clu, data$ocd_current)
chisq.test(data$clu, data$ptsd_current)

chisq.test(data$clu, data$study)
chisq.test(data$clu, data$scanner)

# Export
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores.csv', row.names = FALSE)

