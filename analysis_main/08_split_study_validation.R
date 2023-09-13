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

#### Split study assignment ####

# Split into studies
for (study in unique(data$study_reduced)){
  
  # Create split variable
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  data[data$study_reduced != study,splname]=0
  data[data$study_reduced == study,splname]=1
  
  # Cluster on the data not in the study
  X=data[data$study_reduced != study, img_vars_names]
  dist_mat <- as.dist(1-cor(t(X)))
  optclu=6
  hcres=hcut(dist_mat, optclu, hc_method = 'average', isdiss=TRUE)
  data[data$study_reduced != study, cluname]=hcres$cluster
  
  # Calculate mean profile for each biotype
  clu_mean_mat=matrix(nrow = 6, ncol = length(img_vars_names))
  for (clu in 1:6){
    clu_mean_mat[clu, ]=colMeans(data[data[,splname]==0 & data[,cluname]==clu, img_vars_names])
  }
  
  # Assign each subject in the second subset to a biotype
  data_spl1=data[data[, splname]==0,]
  data_spl2=data[data[, splname]==1,]
  for (rr in 1:nrow(data_spl2)){
    subprof=as.numeric(data_spl2[rr, img_vars_names])
    cors=c()
    for (clu in 1:max(data_spl1[, cluname])){
      cors[clu]=cor(subprof, clu_mean_mat[clu, ])
    }
    data[data[, splname]==1,][rr,cluname]=which(cors==max(cors))
  }
  
  
}

########## PLOT CLUSTERS
library(plyr)

#### Reproduce original plot for each of the two splits #### 
# Function to summarize data in long format
data_summary <- function(data, varname, groupnames){
  summary_func <- function(x, col){
    c(
      mn = mean(x[[col]], na.rm=TRUE),
      mdn = median(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])),
      ci = qt(0.975,df=length(x[[col]])-1)*sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]]))
    )
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

for (study in unique(data$study_reduced)){
  
  # Split based on study
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  data_spl1=data[data[, splname]==0, ]
  data_spl2=data[data[, splname]==1, ]
  
  # Plot profiles for each half
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
  
# Prepare first split
scores_names_final_rename=c('DMN-C', 'SAL-C', 'ATT-C', 'NAs-A', 'NAs-C', 'NAt-A', 'NAt-C', 'NAnt-A', 'NAnt-C', 'POS-A', 'COG-A', 'COG-C')
data_long <- melt(data_spl1, id.vars = c("id", cluname), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", cluname))
df_plot[startsWith(as.character(df_plot$feature), 'DMN'),'color']='#83A4D1'
df_plot[startsWith(as.character(df_plot$feature), 'SAL'),'color']='#46B674'
df_plot[startsWith(as.character(df_plot$feature), 'ATT'),'color']='#EFD658'
df_plot[startsWith(as.character(df_plot$feature), 'NA'),'color']='#D77842'
df_plot[startsWith(as.character(df_plot$feature), 'POS'),'color']='#7E5DA6'
df_plot[startsWith(as.character(df_plot$feature), 'COG'),'color']='#B75A65'
df_plot[abs(df_plot$mn)<0.5, 'color']='grey'

df_plot$color=factor(df_plot$color, levels=c('#83A4D1', '#46B674', '#EFD658', '#D77842', '#7E5DA6', '#B75A65', 'grey'))
df_plot=na.omit(df_plot)

# Bar plot for first split
setnames(df_plot, cluname,'clu_spl2')
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars_", study,"_spl1.png", sep=''),width=700, height=1200)
print(ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu_spl2, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(-1.4,1.4), expand = c(0, 0)) +
  geom_errorbar(aes(x=feature, ymin=mn-se, ymax=mn+se), width=.1) + 
  labs(title="",x="", y = "")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1.2)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  scale_fill_manual(name='Regional circuit score', values = levels(df_plot$color), labels=c('Default', 'Salience', 'Attention', 'Negative', 'Positive', 'Cognitive', 'Within norm')) +
  theme(strip.text.x = element_blank()))
dev.off()

# Prepare second split
data_long <- melt(data_spl2, id.vars = c("id", cluname), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", cluname))
df_plot[startsWith(as.character(df_plot$feature), 'DMN'),'color']='#83A4D1'
df_plot[startsWith(as.character(df_plot$feature), 'SAL'),'color']='#46B674'
df_plot[startsWith(as.character(df_plot$feature), 'ATT'),'color']='#EFD658'
df_plot[startsWith(as.character(df_plot$feature), 'NA'),'color']='#D77842'
df_plot[startsWith(as.character(df_plot$feature), 'POS'),'color']='#7E5DA6'
df_plot[startsWith(as.character(df_plot$feature), 'COG'),'color']='#B75A65'
df_plot[abs(df_plot$mn)<0.5, 'color']='grey'
df_plot$color=factor(df_plot$color, levels=c('#83A4D1', '#46B674', '#EFD658', '#D77842', '#7E5DA6', '#B75A65', 'grey'))
df_plot=na.omit(df_plot)

# Bar plot for second split
setnames(df_plot, cluname,'clu_spl2')
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars_", study,"_spl2.png", sep=''),width=700, height=1200)
print(ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu_spl2, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(-1.4,1.4), expand = c(0, 0)) +
  geom_errorbar(aes(x=feature, ymin=mn-se, ymax=mn+se), width=.1) + 
  labs(title="",x="", y = "")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1.2)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  scale_fill_manual(name='Regional circuit score', values = levels(df_plot$color), labels=c('Default', 'Salience', 'Attention', 'Negative', 'Positive', 'Cognitive', 'Within norm')) +
  theme(strip.text.x = element_blank()))
dev.off()

}


# Assign names to clusters
data$RAD_cluspl=factor(data$RAD_cluspl, levels = c(1, 2, 3, 4, 5, 6), labels=c('Cognitive dyscontrol hypo','Inattention','Rest hyper-connectivity','Intact','Cognitive dyscontrol hyper', 'Context insensitivity'))
data$HCPDES_cluspl=factor(data$HCPDES_cluspl, levels = c(1, 2, 3, 4, 5, 6), labels=c('Cognitive dyscontrol hyper','Rest hyper-connectivity','Intact','Inattention','Context insensitivity', 'Cognitive dyscontrol hypo'))
data$ISPOTD_cluspl=factor(data$ISPOTD_cluspl, levels = c(1, 2, 3, 4, 5, 6), labels=c('Cognitive dyscontrol hypo','Inattention','Context insensitivity','Rest hyper-connectivity','Cognitive dyscontrol hyper', 'Intact'))
data$ENGAGE_cluspl=factor(data$ENGAGE_cluspl, levels = c(1, 2, 3, 4, 5, 6), labels=c('Intact','Rest hyper-connectivity','Context insensitivity','Cognitive dyscontrol hypo','Other', 'Other2'))

#### Validation of symptom profiles in split-study data ####
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Sleep')
 
for (study in unique(data$study_reduced)){
  
  # Split based on study
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  data_spl1=data[data[, splname]==0, ]
  data_spl2=data[data[, splname]==1, ]
  
  # Test each cluster vs median of clinical participants not in the cluster
  symps_results1=as.data.frame(matrix(nrow=0, ncol=9))
  names(symps_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1', 'r_spl1')
  for (clu in unique(data_spl1[, cluname])){
    for (comp in comps_names){
      temp=data_spl1[data_spl1[, cluname]==clu, comp ]
      temp_other=data_spl1[data_spl1[, cluname]!=clu, comp ]
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
  for (clu in unique(data_spl2[, cluname])){
    for (comp in comps_names){
      temp=data_spl2[data_spl2[, cluname]==clu, comp ]
      temp_other=data_spl2[data_spl2[, cluname]!=clu, comp ]
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
  write.csv(symps_results_merged, paste('tables/symps_results_merged_ho_', study, '.csv', sep=''), row.names = F)
  
}

#### Validation of behavior profiles in split-half data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')

for (study in unique(data$study_reduced)){
  
  # Split based on study
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  data_spl1=data[data[, splname]==0, ]
  data_spl2=data[data[, splname]==1, ]
  
  # Test each cluster vs median of clinical participants not in the cluster
  beh_results1=as.data.frame(matrix(nrow=0, ncol=9))
  names(beh_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'mdn_spl1', 'mdn_other_spl1','p_spl1', 'z_spl1', 'r_spl1')
  for (clu in unique(data_spl1[, cluname])){
    for (comp in comps_names){
      temp=data_spl1[data_spl1[, cluname]==clu, comp ]
      temp_other=data_spl1[data_spl1[, cluname]!=clu, comp ]
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
  for (clu in unique(data_spl2[, cluname])){
    for (comp in comps_names){
      temp=data_spl2[data_spl2[, cluname]==clu, comp ]
      temp_other=data_spl2[data_spl2[, cluname]!=clu, comp ]
      # Check that there are enough people with data
      if (sum(is.na(temp))<length(temp)){
        res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
        z=wilcoxonZ(x=temp, mu=median(temp_other, na.rm = T))
        mdn=median(temp, na.rm = T)
        mdn_other=median(temp_other, na.rm = T)
        dir=(mdn>mdn_other)*1
        N=sum(!is.na(temp))
        r=z/sqrt(N)
        beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, dir, N, mdn, mdn_other, res$p.value/2, z, r) #one sided confirmatory test
      }
      else{
        beh_results2[nrow(beh_results2)+1, ]=c(clu, comp, NA, 0, NA, NA, NA, NA, NA)
      }
    }
  }
  
  # Export the results in one table
  beh_results_merged=merge(beh_results1, beh_results2, by = c('Cluster', 'Symptom_composite'))
  write.csv(beh_results_merged, paste('tables/beh_results_merged_ho_', study, '.csv', sep=''), row.names = F)
  
}


