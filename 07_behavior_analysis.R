library(data.table)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores.csv')

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

#### Behavior analyses ####

comps=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_priming_RT','Implicit_sad_priming_RT','Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of all clinical participants
beh_results=as.data.frame(matrix(nrow=0, ncol=9))
names(beh_results)=c('Symptom composite', 'Cluster', 'greater', 'N', 'p', 'sig', 'pFDR', 'sig FDR', 'pFDR_comp')
for (comp in comps){
  for (clu in unique(data$clu)){
    temp=data[data$clu==clu, comp ]
    res=wilcox.test(x=temp, mu = median(data[, comp], na.rm = TRUE))
    
    dir=(median(temp, na.rm = TRUE)>median(data[, comp], na.rm = TRUE))*1
    if(res$p.value<0.05){
      print(paste(comp, clu, res$p.value))
    }
    
    N=length(temp)
    
    beh_results[nrow(beh_results)+1, 1:8]=c(comp, clu, dir, N, res$p.value, (res$p.value<0.05)*1, NA, NA)
    
  }
  
  ps=beh_results[beh_results$`Symptom composite`==comp, 'p']
  beh_results[beh_results$`Symptom composite`==comp, 'pFDR_comp']=round(p.adjust(ps, method = 'fdr'), 3)
  
}
beh_results[, 'pFDR']=round(p.adjust(beh_results[, 'p'], method = 'fdr'), 3)
beh_results[, 'sig FDR']=(beh_results[, 'pFDR']<0.05)*1

# Export results
write.csv(beh_results, '/Users/ltozzi/Dropbox (PanLab)/cluster paper/tables/beh_results.csv', row.names = FALSE)

# Export
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores_behscores.csv', row.names = FALSE)
