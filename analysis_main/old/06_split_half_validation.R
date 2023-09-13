library(data.table)
library(factoextra)
library(rcompanion)

#### Validation of clusters in split-half data ####

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')
set.seed(123123)

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu.csv')
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')

#### Calculate symptom composites ####

data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

# Rename variables
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score', 'Suicide', 'Sleep', 'qids_total')
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep', 'Total Severity')
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
X=data_spl1[, img_vars_names]
dist_mat <- as.dist(1-cor(t(X)))
optclu=6
hcres=hcut(dist_mat, optclu, hc_method = 'average', isdiss=TRUE)
data_spl1[, 'clu_spl2']=hcres$cluster

# Calculate mean profile for each biotype
clu_mean_mat=matrix(nrow = max(data_spl1$clu_spl2), ncol = length(img_vars_names))
for (clu in 1:max(data_spl1$clu_spl2)){
  clu_mean_mat[clu, ]=colMeans(data_spl1[data_spl1$clu_spl2==clu, img_vars_names])
}

# Assign each subject in the second subset to a cluster
for (rr in 1:nrow(data_spl2)){
  subprof=as.numeric(data_spl2[rr, img_vars_names])
  cors=c()
  for (clu in 1:max(data_spl1$clu_spl2)){
    cors[clu]=cor(subprof, clu_mean_mat[clu, ])
  }
  data_spl2[rr, 'clu_spl2']=which(cors==max(cors))
}

# Add assignments to original data frame
data[data$split2==1, 'clu_spl2']=factor(data_spl1$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))
data[data$split2==2, 'clu_spl2']=factor(data_spl2$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))

# Plot profiles for each half
scores_names_final_rename=c('DMN-C', 'SAL-C', 'ATT-C', 'NAs-A', 'NAs-C', 'NAt-A', 'NAt-C', 'NAnt-A', 'NAnt-C', 'POS-A', 'COG-A', 'COG-C')

data_spl1[, 'DMN-C']=rowMeans(data_spl1[, c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4')])
data_spl1[, 'SAL-C']=rowMeans(data_spl1[, c('S1S3', 'S2S4', 'S1S2')])
data_spl1[, 'ATT-C']=rowMeans(data_spl1[, c('A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')])
data_spl1[, 'NAs-A']=rowMeans(data_spl1[, c('NS4', 'NS5', 'NS3', 'NS2', 'NS1')])
data_spl1[, 'NAs-C']=rowMeans(data_spl1[, c('NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1')])
data_spl1[, 'NAt-A']=rowMeans(data_spl1[, c('NT2', 'NT3', 'NT1')])
data_spl1[, 'NAt-C']=rowMeans(data_spl1[, c('NT2NT1', 'NT3NT1')])
data_spl1[, 'NAnt-A']=rowMeans(data_spl1[, c('NTN3', 'NTN2', 'NTN1')])
data_spl1[, 'NAnt-C']=rowMeans(data_spl1[, c('NTN2NTN1', 'NTN3NTN1')])
data_spl1[, 'POS-A']=rowMeans(data_spl1[, c('P1', 'P2', 'P3')])
data_spl1[, 'COG-A']=rowMeans(data_spl1[, c('C1', 'C3', 'C2')])
data_spl1[, 'COG-C']=rowMeans(data_spl1[, c('C1C2', 'C3C2')])
data_spl2[, 'DMN-C']=rowMeans(data_spl2[, c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4')])
data_spl2[, 'SAL-C']=rowMeans(data_spl2[, c('S1S3', 'S2S4', 'S1S2')])
data_spl2[, 'ATT-C']=rowMeans(data_spl2[, c('A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')])
data_spl2[, 'NAs-A']=rowMeans(data_spl2[, c('NS4', 'NS5', 'NS3', 'NS2', 'NS1')])
data_spl2[, 'NAs-C']=rowMeans(data_spl2[, c('NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1')])
data_spl2[, 'NAt-A']=rowMeans(data_spl2[, c('NT2', 'NT3', 'NT1')])
data_spl2[, 'NAt-C']=rowMeans(data_spl2[, c('NT2NT1', 'NT3NT1')])
data_spl2[, 'NAnt-A']=rowMeans(data_spl2[, c('NTN3', 'NTN2', 'NTN1')])
data_spl2[, 'NAnt-C']=rowMeans(data_spl2[, c('NTN2NTN1', 'NTN3NTN1')])
data_spl2[, 'POS-A']=rowMeans(data_spl2[, c('P1', 'P2', 'P3')])
data_spl2[, 'COG-A']=rowMeans(data_spl2[, c('C1', 'C3', 'C2')])
data_spl2[, 'COG-C']=rowMeans(data_spl2[, c('C1C2', 'C3C2')])

for (clu in 1:optclu){
  mn1=colMeans(data_spl1[data_spl1$clu_spl2==clu, scores_names_final_rename])
  se1=sapply(data_spl1[data_spl1$clu_spl2==clu, scores_names_final_rename], sd, na.rm = TRUE)/sqrt(nrow(data_spl1[data_spl1$clu_spl2==clu, scores_names_final_rename]))
  mn2=colMeans(data_spl2[data_spl2$clu_spl2==clu, scores_names_final_rename])
  se2=sapply(data_spl1[data_spl1$clu_spl2==clu, scores_names_final_rename], sd, na.rm = TRUE)/sqrt(nrow(data_spl1[data_spl1$clu_spl2==clu, scores_names_final_rename]))
  
  temp=as.data.frame(matrix(nrow = length(scores_names_final_rename), ncol=0))
  temp$vals=mn1
  temp$se=se1
  temp$spl=1
  temp$vars=scores_names_final_rename
  temp2=as.data.frame(matrix(nrow = length(scores_names_final_rename), ncol=0))
  temp2$vals=mn2
  temp2$se=se2
  temp2$spl=2
  temp2$vars=scores_names_final_rename
  data_long=rbind(temp, temp2)
  data_long$spl=factor(data_long$spl)
  data_long$vars=factor(data_long$vars,levels =scores_names_final_rename,  labels = scores_names_final_rename)
  
  png(paste('plots/cluster_', clu, '_spl_profile.png', sep=''), width = 800, height = 600)
  print(ggplot(data=data_long, aes(x=vars, y=vals, group=spl, col=spl)) +
          geom_line(linewidth=1.5)+
          geom_point(size=2)+
          geom_errorbar(aes(ymin = vals - se, ymax = vals + se), width = 0.2)+
          geom_hline(yintercept = 0.5, linetype="dashed", color = "grey") +
          geom_hline(yintercept = -0.5, linetype="dashed", color = "grey") +
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          theme(legend.position = "top", legend.title=element_text(size=18), legend.text=element_text(size=18))+
          theme(panel.spacing = unit(2, "lines"))+
          theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18))+
          ylab("Personalized regional circuit score (mean)")+
          scale_color_manual(name="", labels=c('Split 1', 'Split 2'), values=c('black', 'red'))+
          ylim(c(-2, 2))
  )
  dev.off()
}

# Assign names to clusters
data_spl1$clu_spl2=factor(data_spl1$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))
data_spl2$clu_spl2=factor(data_spl2$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))


#### Validation of symptom profiles in split-half data ####
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Sleep')

# Test each cluster vs median of clinical participants not in the cluster
symps_results1=as.data.frame(matrix(nrow=0, ncol=8))
names(symps_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1')
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
    symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z)
    }
    else{
      symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA)
    }
  }
}

# Test each cluster vs median of clinical participants not in the cluster
symps_results2=as.data.frame(matrix(nrow=0, ncol=8))
names(symps_results2)=c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'mdn_spl2', 'mdn_other_spl2','p_spl2', 'z_spl2')
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
      symps_results2[nrow(symps_results2)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value/2, z) #one-sided test for second split
    }
    else{
      symps_results2[nrow(symps_results2)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
symps_results_merged=merge(symps_results1, symps_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(symps_results_merged, 'tables/symps_results_merged.csv', row.names = F)


#### Validation of behavior profiles in split-half data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
beh_results1=as.data.frame(matrix(nrow=0, ncol=8))
names(beh_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1')
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
      beh_results1[nrow(beh_results1)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value, z)
    }
    else{
      beh_results1[nrow(beh_results1)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA)
    }
  }
}

# Test each cluster vs median of clinical participants not in the cluster
beh_results2=as.data.frame(matrix(nrow=0, ncol=8))
names(beh_results2)=c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'mdn_spl2', 'mdn_other_spl2','p_spl2', 'z_spl2')
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
      beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, dir, N, mdn, mdn_other,  res$p.value/2, z) #one-sided test for second split
    }
    else{
      beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA)
    }
  }
}

# Export the results in one table
beh_results_merged=merge(beh_results1, beh_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(beh_results_merged, 'tables/beh_results_merged.csv', row.names = F)

# Export
write.csv(data, 'data/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl.csv', row.names = F)


