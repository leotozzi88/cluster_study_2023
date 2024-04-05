#### Libraries, imports and global variables ####

setwd('/Users/ltozzi/PanLab Dropbox/Leonardo Tozzi/cluster paper/manuscript/Revision_2')
source('analysis_main/custom_functions.R')
set.seed(123123)
data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_sympbeh_spl.csv')
library(plyr)

#### Validation of clusters in split-study data ####

img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')

#### Split study assignment ####
data=splitstudy_and_cluster(data, nclu = 6, img_vars_names = img_vars_names, studyvec = data$study_reduced)

#### Plot clusters in each dataset to name them #### 

# Reproduce original plot for each of the two split
# Function to summarize data in long format
data_summary = function(data, varname, groupnames){
  summary_func = function(x, col){
    c(
      mn = mean(x[[col]], na.rm=TRUE),
      mdn = median(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])),
      ci = qt(0.975,df=length(x[[col]])-1)*sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]]))
    )
  }
  data_sum=ddply(data, groupnames, .fun=summary_func,
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
data_long = melt(data_spl1, id.vars = c("id", cluname), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot = data_summary(data_long, varname="score", groupnames=c("feature", cluname))
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
png(file=paste("plots/circuit_profile_bars_", study,"_spl1.png", sep=''),width=700, height=1200)
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
data_long = melt(data_spl2, id.vars = c("id", cluname), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot = data_summary(data_long, varname="score", groupnames=c("feature", cluname))
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
png(file=paste("plots/circuit_profile_bars_", study,"_spl2.png", sep=''),width=700, height=1200)
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

#### Validation of symptom and behavior profiles in split-study data ####

for (study in unique(data$study_reduced)){
  
  # Split based on study
  splname=paste(study, '_study_spl2', sep='')
  cluname=paste(study, '_cluspl', sep='')
  spl1=data[data[, splname]==0, ]
  spl2=data[data[, splname]==1, ]
  
  # Compare symptoms
  comps_names=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol')
  spl1_symp_Z=compare_clusters(spl1, cluster_assignments = spl1[, cluname], var_names = comps_names)
  spl2_symp_Z=compare_clusters(spl2, cluster_assignments = spl2[, cluname], var_names = comps_names, onesided = T)
  
  # Export the results in one table
  symps_results_merged=merge(spl1_symp_Z, spl2_symp_Z, by = c('Cluster', 'Symptom_composite'))
  names(symps_results_merged) = gsub(".x", "_spl1", names(symps_results_merged))
  names(symps_results_merged) = gsub(".y", "_spl2", names(symps_results_merged))
  write.csv(symps_results_merged, paste('tables/symps_results_study_merged', study, '.csv'), row.names = F)

  # Compare behavior
  comps_names=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_RT','Implicit_sad_RT', 'Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
  spl1_beh_Z=compare_clusters(spl1, cluster_assignments = spl1[, cluname], var_names = comps_names)
  spl2_beh_Z=compare_clusters(spl2, cluster_assignments = spl2[, cluname], var_names = comps_names, onesided = T)
  
  # Export the results in one table
  beh_results_merged=merge(spl1_beh_Z, spl2_beh_Z, by = c('Cluster', 'Symptom_composite'))
  names(beh_results_merged) = gsub(".x", "_spl1", names(beh_results_merged))
  names(beh_results_merged) = gsub(".y", "_spl2", names(beh_results_merged))
  write.csv(beh_results_merged, paste('tables/beh_results_study_merged', study, '.csv'), row.names = F)
  
}


