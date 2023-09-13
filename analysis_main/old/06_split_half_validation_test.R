library(data.table)
library(factoextra)
library(plyr)

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper/')

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

#### Validation of clusters in split-half data ####

set.seed(123123)

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu.csv')
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')

#### Calculate symptom composites ####

data[, 'Sleep']=rowSums(data[, c('qids_01', 'qids_02', 'qids_03')], na.rm = FALSE)
data[, 'Suicide']=data[, c('qids_12')]

# Rename variables
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score', 'Suicide', 'Sleep')
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')
data=setnames(data, comps, comps_names)

# # Log transform
# for (var in comps_names){
#   data[, var]=log(data[, var] + 1)
# }

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
 
# Reproduce original plot for each of the two splits
scores_names_final_rename=c('DMN-C', 'SAL-C', 'ATT-C', 'NAs-A', 'NAs-C', 'NAt-A', 'NAt-C', 'NAnt-A', 'NAnt-C', 'POS-A', 'COG-A', 'COG-C')

data_long <- melt(data_spl1, id.vars = c("id", "clu_spl2"), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", "clu_spl2"))

df_plot[startsWith(as.character(df_plot$feature), 'DMN'),'color']='#83A4D1'
df_plot[startsWith(as.character(df_plot$feature), 'SAL'),'color']='#46B674'
df_plot[startsWith(as.character(df_plot$feature), 'ATT'),'color']='#EFD658'
df_plot[startsWith(as.character(df_plot$feature), 'NA'),'color']='#D77842'
df_plot[startsWith(as.character(df_plot$feature), 'POS'),'color']='#7E5DA6'
df_plot[startsWith(as.character(df_plot$feature), 'COG'),'color']='#B75A65'
df_plot[abs(df_plot$mn)<0.5, 'color']='grey'

df_plot$color=factor(df_plot$color, levels=c('#83A4D1', '#46B674', '#EFD658', '#D77842', '#7E5DA6', '#B75A65', 'grey'))
df_plot=na.omit(df_plot)

# Bar plot of full sample
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars_split1.png", sep=''),width=700, height=1200)
ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu_spl2, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(-1.3,1.3), expand = c(0, 0)) +
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
  theme(strip.text.x = element_blank())
dev.off()


data_long <- melt(data_spl2, id.vars = c("id", "clu_spl2"), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", "clu_spl2"))

df_plot[startsWith(as.character(df_plot$feature), 'DMN'),'color']='#83A4D1'
df_plot[startsWith(as.character(df_plot$feature), 'SAL'),'color']='#46B674'
df_plot[startsWith(as.character(df_plot$feature), 'ATT'),'color']='#EFD658'
df_plot[startsWith(as.character(df_plot$feature), 'NA'),'color']='#D77842'
df_plot[startsWith(as.character(df_plot$feature), 'POS'),'color']='#7E5DA6'
df_plot[startsWith(as.character(df_plot$feature), 'COG'),'color']='#B75A65'
df_plot[abs(df_plot$mn)<0.5, 'color']='grey'

df_plot$color=factor(df_plot$color, levels=c('#83A4D1', '#46B674', '#EFD658', '#D77842', '#7E5DA6', '#B75A65', 'grey'))
df_plot=na.omit(df_plot)

# Bar plot of full sample
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars_split2.png", sep=''),width=700, height=1200)
ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu_spl2, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(-1.1,1.1), expand = c(0, 0)) +
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
  theme(strip.text.x = element_blank())
dev.off()

# Assign names to clusters
data_spl1$clu_spl2=factor(data_spl1$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))
data_spl2$clu_spl2=factor(data_spl2$clu_spl2, levels = c(1, 2, 3, 4, 5, 6), labels=c('Rest hyper-connectivity','Cognitive dyscontrol hyper','Cognitive dyscontrol hypo','Inattention','Context insensitivity', 'Intact'))

#### Validation of symptom profiles in split-half data ####
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol')

comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')

# Modify the data frame to include p values from bootstrap and permutation tests
symps_results1 = as.data.frame(matrix(nrow=0, ncol=13))
names(symps_results1) = c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'N_spl1_other', 'mdn_spl1', 'mdn_other_spl1', 'stat', 'p_spl1', 'bootstrap_p_spl1', 'permutation_p_spl1', 'mann_u_p_spl1', 'kruskal_p_spl1')

data_spl1_scaled=data_spl1
data_spl2_scaled=data_spl2

for (comp in comps_names){
  vals1=data_spl1[, comp]
  vals2=data_spl2[, comp]
  # data_spl1_scaled[, comp]=scale(data_spl1[, comp], center = T, scale = T)
  # data_spl2_scaled[, comp]=scale(data_spl2[, comp], center = T, scale = T)
  # data_spl1_scaled[, comp]=(vals1-min(vals1, na.rm = T))/(max(vals1, na.rm = T)-min(vals1, na.rm = T))
  # data_spl2_scaled[, comp]=(vals2-min(vals2, na.rm = T))/(max(vals2, na.rm = T)-min(vals2, na.rm = T))
}

data_spl1_scaled=data_spl1_scaled[data_spl1_scaled$clu_spl2!='Cognitive dyscontrol hypo', ]
data_spl2_scaled=data_spl2_scaled[data_spl2_scaled$clu_spl2!='Cognitive dyscontrol hypo', ]

mydata_long <- melt(data_spl1_scaled[, c('id','clu_spl2', 'study_reduced','scanner', 'age', 'gender', comps_names)], id.vars = c("id", 'clu_spl2', 'age','scanner', 'gender','study_reduced'), variable.name = "symp", value.name = "Value")
mydata_long <- melt(data_spl2_scaled[, c('id','clu_spl2', 'study_reduced','scanner', 'age', 'gender', comps_names)], id.vars = c("id", 'clu_spl2', 'age','scanner', 'gender','study_reduced'), variable.name = "symp", value.name = "Value")

# Run model
library(lme4)
library(lmerTest)

repeated_measures_model=lmer(data=mydata_long, Value~symp*clu_spl2 + (1 | id) )
anova(repeated_measures_model)
check_model(repeated_measures_model)

# Compute the EMMs for the interaction
library(emmeans)
emmeans_interaction <- emmeans(repeated_measures_model, ~ clu_spl2|symp) # for exact SE use pbkrtest.limit = 7309

# Perform pairwise comparisons
#posthoc_tests_diags <- contrast(emmeans_interaction, adjust = 'none')
posthoc_tests_diags <- contrast(emmeans_interaction, method = 'pairwise', adjust = 'none')
posthoc_tests_df <- as.data.frame(posthoc_tests_diags)
print(posthoc_tests_df)

confint(posthoc_tests_diags)

posthoc_tests_df$p.value_os=posthoc_tests_df$p.value/2





########
library(BSDA)

data_spl1_scaled=data_spl1
data_spl2_scaled=data_spl2

for (comp in comps_names){
  # vals1=data_spl1[, comp]
  # vals2=data_spl2[, comp]
  data_spl1_scaled[, comp]=scale(data_spl1[, comp], center = T, scale = T)
  data_spl2_scaled[, comp]=scale(data_spl2[, comp], center = T, scale = T)
  # data_spl1_scaled[, comp]=(vals1-min(vals1, na.rm = T))/(max(vals1, na.rm = T)-min(vals1, na.rm = T))
  # data_spl2_scaled[, comp]=(vals2-min(vals2, na.rm = T))/(max(vals2, na.rm = T)-min(vals2, na.rm = T))
}

comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')
#comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')

current_df=data_spl1_scaled
res_df=as.data.frame(matrix(nrow=0, ncol=4))
for (comp in comps_names){
  for (clu in unique(current_df$clu_spl2)){
    cludata=current_df[current_df$clu_spl2==clu, comp]
    othdata=current_df[current_df$clu_spl2!=clu, comp]
    if (sum(is.na(cludata))<length(cludata)){
    res=wilcox.test(x=cludata, y=othdata)
    res_df[nrow(res_df)+1, ]=c(comp, clu, res$statistic, res$p.value)
  }
  }
}
res_df[res_df$V4<0.10, ]


##############

# Function to perform bootstrap on the difference in medians
bootstrap_diff_median <- function(data1, data2, n_bootstrap=1000, conf_level=0.95) {
  boot_results <- numeric(n_bootstrap)
  n1 <- length(data1)
  n2 <- length(data2)
  for (i in 1:n_bootstrap) {
    new_data1 <- sample(data1, n1, replace = TRUE)
    new_data2 <- sample(data2, n2, replace = TRUE)
    boot_results[i] <- median(new_data1) - median(new_data2)
  }
  c(
    "CI_low" = quantile(boot_results, (1-conf_level)/2),
    "CI_high" = quantile(boot_results, 1 - ((1-conf_level)/2))
  )
}

comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')

current_df=data_spl1_scaled
res_df=as.data.frame(matrix(nrow=0, ncol=6))
names(res_df) <- c("comp", "clu", "statistic", "p_value", "CI_low", "CI_high")

for (comp in comps_names){
  for (clu in unique(current_df$clu_spl2)){
    cludata=current_df[current_df$clu_spl2==clu, comp]
    othdata=current_df[current_df$clu_spl2!=clu, comp]
    cludata <- cludata[!is.na(cludata)] # Exclude NAs
    othdata <- othdata[!is.na(othdata)] # Exclude NAs
    if (length(cludata)>0 && length(othdata)>0) {
      res=wilcox.test(x=cludata, y=othdata)
      boot_res = bootstrap_diff_median(cludata, othdata)
      res_df[nrow(res_df)+1, ]=c(comp, clu, res$statistic, res$p.value, boot_res['CI_low.2.5%'], boot_res['CI_high.97.5%'])
    }
  }
}

res_df

# tewo sided Function to perform permutation test on the difference in medians
permute_diff_median <- function(data1, data2, n_permute=1000) {
  n1 <- length(data1)
  n2 <- length(data2)
  observed_diff <- median(data1) - median(data2)
  combined_data <- c(data1, data2)
  permute_results <- numeric(n_permute)
  for (i in 1:n_permute) {
    permute_data <- sample(combined_data, n1+n2) # permutation
    permute_diff <- median(permute_data[1:n1]) - median(permute_data[(n1+1):(n1+n2)])
    permute_results[i] <- permute_diff
  }
  p_value <- mean(abs(permute_results) >= abs(observed_diff))
  p_value
}

# one sided Function to perform one-sided permutation test on the difference in medians
permute_diff_median <- function(data1, data2, n_permute=1000) {
  n1 <- length(data1)
  n2 <- length(data2)
  observed_diff <- median(data1) - median(data2)
  combined_data <- c(data1, data2)
  permute_results <- numeric(n_permute)
  for (i in 1:n_permute) {
    permute_data <- sample(combined_data, n1+n2) # permutation
    permute_diff <- median(permute_data[1:n1]) - median(permute_data[(n1+1):(n1+n2)])
    permute_results[i] <- permute_diff
  }
  p_value <- mean(permute_results >= observed_diff)
  p_value
}



comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')

current_df=data_spl2_scaled
res_df=as.data.frame(matrix(nrow=0, ncol=7))
names(res_df) <- c("comp", "clu", "statistic", "p_value", "CI_low", "CI_high", "p_value_permutation")

for (comp in comps_names){
  for (clu in unique(current_df$clu_spl2)){
    cludata=current_df[current_df$clu_spl2==clu, comp]
    othdata=current_df[current_df$clu_spl2!=clu, comp]
    cludata <- cludata[!is.na(cludata)] # Exclude NAs
    othdata <- othdata[!is.na(othdata)] # Exclude NAs
    if (length(cludata)>0 && length(othdata)>0) {
      res=wilcox.test(x=cludata, y=othdata)
      boot_res = bootstrap_diff_median(cludata, othdata)
      permute_p_value = permute_diff_median(cludata, othdata)
      res_df[nrow(res_df)+1, ]=c(comp, clu, res$statistic, res$p.value, boot_res['CI_low.2.5%'], boot_res['CI_high.97.5%'], permute_p_value)
    }
  }
}

res_df

# Function to perform permutation test using Mahalanobis distance
permute_mahalanobis <- function(data1, data2, n_permute=1000) {
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  observed_diff <- mahalanobis(data1, colMeans(data2), cov(data2))
  combined_data <- rbind(data1, data2)
  permute_results <- numeric(n_permute)
  for (i in 1:n_permute) {
    permute_data <- combined_data[sample(nrow(combined_data)),] # permutation
    permute_diff <- mahalanobis(permute_data[1:n1,], colMeans(permute_data[(n1+1):(n1+n2),]), cov(permute_data[(n1+1):(n1+n2),]))
    permute_results[i] <- permute_diff
  }
  p_value <- mean(permute_results >= observed_diff)
  p_value
}

comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')

current_df=data_spl1_scaled
res_df=as.data.frame(matrix(nrow=0, ncol=3))
names(res_df) <- c("clu", "statistic", "p_value_permutation")

for (clu in unique(current_df$clu_spl2)){
  cludata=current_df[current_df$clu_spl2==clu, comps_names]
  othdata=current_df[current_df$clu_spl2!=clu, comps_names]
  if (nrow(cludata)>0 && nrow(othdata)>0) {
    res=wilcox.test(x=cludata, y=othdata)
    permute_p_value = permute_mahalanobis(cludata, othdata)
    res_df[nrow(res_df)+1, ]=c(clu, res$statistic, permute_p_value)
  }
}



##########

# Install required packages
install.packages(c("nnet", "broom"))

# Load required libraries
library(nnet)
library(broom)

# Define predictors
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias',
              'Threat dysregulation', 'Anhedonia','Anxious arousal', 
              'Cognitive dyscontrol', 'Suicide', 'Sleep')

# Remove NA values
data_spl1 = na.omit(data_spl1_scaled[, c(comps_names, "clu_spl2")])

# Fit multinomial logistic regression model
multinom_fit <- multinom(clu_spl2 ~ ., data = data_spl1)

# Summary of the model
summary(multinom_fit)

# Tidy the results of the multinomial logistic regression
tidied_results <- tidy(multinom_fit)

# Remove NA values from data_spl2
data_spl2 = na.omit(data_spl2[, c(comps_names, "clu_spl2")])

# Predicting the classes in data_spl2
predicted_classes <- predict(multinom_fit, newdata = data_spl2, "class")

# Evaluation of the model
confusion_matrix <- table(Predicted = predicted_classes, Actual = data_spl2$clu_spl2)
print(confusion_matrix)















###############








library(npmv)
current_df=data_spl1_scaled
comps_names=c('Ruminative worry', 'Ruminative brooding','Tension','Negative bias','Threat dysregulation', 'Anhedonia','Anxious arousal', 'Cognitive dyscontrol', 'Suicide', 'Sleep')
comps_names_new=c('Ruminative_worry', 'Ruminative_brooding','Tension','Negative_bias','Threat_dysregulation', 'Anhedonia','Anxious_arousal', 'Cognitive_dyscontrol', 'Suicide', 'Sleep')

data_spl1_scaled=data_spl1_scaled[data_spl1_scaled$clu_spl2!='Cognitive dyscontrol hypo', ]
data=setnames(data_spl1_scaled, comps_names, comps_names_new)
res=nonpartest(Ruminative_worry|Ruminative_brooding|Tension|Negative_bias|Threat_dysregulation|Anhedonia|Anxious_arousal|Cognitive_dyscontrol|Suicide|Sleep~clu_spl2, data=data_spl1_scaled, plots = F, permreps = 100, )





#######

data_spl1_scaled=data_spl1
data_spl2_scaled=data_spl2
data_scaled=data

for (comp in comps_names){
  vals=data[, comp]
  # vals1=data_spl1[, comp]
  # vals2=data_spl2[, comp]
  data_scaled[, comp]=scale(data_scaled[, comp], center = T, scale = T)
  # data_spl1_scaled[, comp]=scale(data_spl1[, comp], center = T, scale = T)
  # data_spl2_scaled[, comp]=scale(data_spl2[, comp], center = T, scale = T)
  # data_scaled[, comp]=(vals-min(vals, na.rm = T))/(max(vals, na.rm = T)-min(vals, na.rm = T))
  # data_spl1_scaled[, comp]=(vals1-min(vals1, na.rm = T))/(max(vals1, na.rm = T)-min(vals1, na.rm = T))
  # data_spl2_scaled[, comp]=(vals2-min(vals2, na.rm = T))/(max(vals2, na.rm = T)-min(vals2, na.rm = T))
}

# Run on all data, replicate in splits
mydata_long <- melt(data[, c('id','clu', 'study_reduced','age', 'gender', comps_names)], id.vars = c("id", 'clu', 'age', 'gender','study_reduced'), variable.name = "symp", value.name = "Value")

# Run model
library(lme4)
library(lmerTest)

repeated_measures_model=lmer(data=mydata_long, Value~symp*clu + (1 | id) )
anova(repeated_measures_model)
check_model(repeated_measures_model)

# Compute the EMMs for the interaction
library(emmeans)
emmeans_interaction <- emmeans(repeated_measures_model, ~ clu|symp) # for exact SE use pbkrtest.limit = 7309
posthoc_tests_diags <- contrast(emmeans_interaction, adjust = 'none')
















write.csv(symps_results1, 'tables/symps_split1.csv', row.names = F)


# Test each cluster vs median of clinical participants not in the cluster
symps_results2 = as.data.frame(matrix(nrow=0, ncol=13))
names(symps_results2) = c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'N_spl2_other', 'mdn_spl2', 'mdn_other_spl2', 'stat', 'p_spl2', 'bootstrap_p_spl2', 'permutation_p_spl2', 'mann_u_p_spl2', 'kruskal_p_spl2')
for (clu in unique(data_spl2$clu_spl2)){
  for (comp in comps_names){
    temp = data_spl2[data_spl2$clu_spl2 == clu, comp ]
    temp_other = data_spl2[, comp ]
    if (sum(is.na(temp)) < length(temp)){
      res = kruskal.test(list(temp, temp_other))
      mdn = median(temp, na.rm = T)
      mdn_other = median(data_spl2[data_spl2$clu_spl2 != clu, comp ], na.rm = T)
      dir = (mdn > mdn_other)*1
      N = sum(!is.na(temp))
      N_other = sum(!is.na(temp_other))
      
      # Add bootstrap test
      set.seed(123)
      temp_no_na <- temp[!is.na(temp)]
      temp_other_no_na <- temp_other[!is.na(temp_other)]
      diff_means = mean(temp_no_na) - mean(temp_other_no_na)
      bootstrap_diffs = replicate(1000, mean(sample(temp_no_na, replace = TRUE)) - mean(sample(temp_other_no_na, replace = TRUE)))
      bootstrap_p_value = mean(abs(bootstrap_diffs) >= abs(diff_means))
      
      # Add permutation test
      combined_no_na = c(temp_no_na, temp_other_no_na)
      diff_means = mean(temp_no_na) - mean(temp_other_no_na)
      permutation_diffs = replicate(1000, {
        permuted = sample(combined_no_na)
        mean(permuted[2:length(temp_no_na)]) - mean(permuted[(length(temp_no_na)+1):length(combined_no_na)])
      })
      permutation_p_value = mean(abs(permutation_diffs) >= abs(diff_means))
      
      # Add Mann-Whitney U test
      mann_u_test_result = wilcox.test(temp, temp_other)
      mann_u_p_value = mann_u_test_result$p.value
      
      symps_results2[nrow(symps_results2) + 1, ] = c(clu, comp, dir, N, N_other, mdn, mdn_other, res$statistic, res$p.value, bootstrap_p_value, permutation_p_value, mann_u_p_value, res$p.value)
    } else{
      symps_results2[nrow(symps_results2) + 1, ] = c(clu, comp, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    }
  }
}

write.csv(symps_results2, 'tables/symps_split2.csv', row.names = F)

symps_results_merged=merge(symps_results1, symps_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(symps_results_merged, 'tables/symps_results_merged.csv', row.names = F)

data_spl1_scaled=data_spl1
data_spl2_scaled=data_spl2

for (comp in comps_names){
  vals1=data_spl1[, comp]
    vals2=data_spl2[, comp]
  # data_spl1_scaled[, comp]=scale(data_spl1[, comp], center = T, scale = T)
  # data_spl2_scaled[, comp]=scale(data_spl2[, comp], center = T, scale = T)
    data_spl1_scaled[, comp]=(vals1-min(vals1, na.rm = T))/(max(vals1, na.rm = T)-min(vals1, na.rm = T))
    data_spl2_scaled[, comp]=(vals2-min(vals2, na.rm = T))/(max(vals2, na.rm = T)-min(vals2, na.rm = T))
}

mydata_long <- melt(data_spl1_scaled[, c('id','clu_spl2', 'study_reduced', comps_names)], id.vars = c("id", 'clu_spl2', 'study_reduced'), variable.name = "symp", value.name = "Value")

# Run model
library(lme4)
library(lmerTest)

repeated_measures_model=lmer(data=mydata_long, Value~symp*clu_spl2 + (1 | id) )
anova(repeated_measures_model)
check_model(repeated_measures_model)

# Compute the EMMs for the interaction
library(emmeans)
emmeans_interaction <- emmeans(repeated_measures_model, ~ symp | clu_spl2, pbkrtest.limit = 7309) # for exact SE use pbkrtest.limit = 7309

# Perform pairwise comparisons
posthoc_tests_diags <- contrast(emmeans_interaction, adjust = 'none')
posthoc_tests_df <- as.data.frame(posthoc_tests_diags)

mydata_long <- melt(data_spl2_scaled[, c('id','clu_spl2', 'study_reduced', comps_names)], id.vars = c("id", 'clu_spl2', 'study_reduced'), variable.name = "symp", value.name = "Value")
repeated_measures_model=lmer(data=mydata_long, Value~symp*clu_spl2 + (1 | id) )
anova(repeated_measures_model)
emmeans_interaction <- emmeans(repeated_measures_model, ~ symp | clu_spl2, pbkrtest.limit = 7309) # for exact SE use pbkrtest.limit = 7309
posthoc_tests_diags <- contrast(emmeans_interaction,adjust = 'none', method = "pairwise")

# Get emmeans for each symp at each clu_spl2
emmeans_interaction <- emmeans(repeated_measures_model, specs = ~ symp | clu_spl2)

# Get overall mean of each symp across all clu_spl2
overall_means <- emmeans(repeated_measures_model, specs = ~ symp)

# Compare each cluster's symp mean with the overall mean
contrast_result <- contrast(emmeans_interaction, method = list(overall_means), adjust = "none")


####

# Required libraries
library(lme4)
library(lmerTest)
library(emmeans)
library(reshape2)

# Melt your data
mydata_long <- melt(data_spl1_scaled[, c('id','clu_spl2', 'study_reduced', comps_names)], 
                    id.vars = c("id", 'clu_spl2', 'study_reduced'), 
                    variable.name = "symp", 
                    value.name = "Value")

# Fit a mixed-effects model
repeated_measures_model = lmer(data=mydata_long, Value ~ symp*clu_spl2+ study_reduced + (1 | id))

# Compute the emmeans for each symp at each clu_spl2
emmeans_interaction <- emmeans(repeated_measures_model, specs = ~ symp | clu_spl2)

# Convert to data frame
emmeans_df <- as.data.frame(summary(emmeans_interaction))

# Compute the mean of means for each symptom across all clusters
mean_of_means <- aggregate(emmeans_df$emmean, by=list(Symptom=emmeans_df$symp), FUN=mean)

# Merge the mean_of_means with the original emmeans data frame
emmeans_df <- merge(emmeans_df, mean_of_means, by.x="symp", by.y="Symptom")

# Compute the difference
emmeans_df$diff <- emmeans_df$emmean - emmeans_df$x

# Apply a Wilcoxon Signed-Rank Test to the diff column for each symptom, excluding NAs
wilcox_test_results <- by(emmeans_df, emmeans_df$symp, function(df) {
  # Only conduct the test if there are sufficient non-NA values
  if(sum(!is.na(df$diff)) > 1) {
    return(wilcox.test(df$diff, conf.int = TRUE, correct = FALSE))
  } else {
    return(NULL)
  }
})

# Remove NULL results
t_test_results <- t_test_results[!sapply(t_test_results, is.null)]

# Extract p-values from the t-test results
p_values <- sapply(t_test_results, function(x) x$p.value)

# Adjust p-values for multiple comparisons
p_adjusted <- p.adjust(p_values, method = "bonferroni")

# Print adjusted p-values
print(p_adjusted)








####













# Convert to data frame
posthoc_tests_df <- as.data.frame(posthoc_tests_diags)

# Divide p-values by 2
posthoc_tests_df$p.value_os <- posthoc_tests_df$p.value / 2

# View the updated data frame
print(posthoc_tests_df)


mydata_long <- melt(data_spl1[, c('id','clu_spl2', 'study_reduced', comps_names)], id.vars = c("id", 'clu_spl2', 'study_reduced'), variable.name = "symp", value.name = "Value")

# Fit the model
repeated_measures_model2 <- lmer(data = mydata_long, Value ~ symp * clu_spl2 + (1 | id))

# Compute the EMMs
emmeans_interaction2 <- emmeans(repeated_measures_model2, ~ clu_spl2 | symp)

# Perform the contrasts, with "Mean" as the control
posthoc_tests_diags2 <- contrast(emmeans_interaction2, method = list(c(0, 0, 0, 0, 0, 1)))

contrast(emmeans_interaction2, "eff", by = "clu_spl2")           # show the treatment effects





#### Validation of behavior profiles in split-half data ####
comps_names=c('Maze_completion_time','Maze_errors','Go-Nogo_mean_RT','Go-Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_anger_RT','Implicit_fear_RT','Implicit_happy_RT','Implicit_sad_RT','Implicit_neutral_RT', 'Explicit_anger_RT','Explicit_fear_RT','Explicit_happy_RT','Explicit_sad_RT')

# Test each cluster vs median of clinical participants not in the cluster
names(symps_results1)=c('Cluster', 'Symptom_composite', 'greater_spl1', 'N_spl1', 'N_spl1_other', 'mdn_spl1', 'mdn_other_spl1', 'stat','p_spl1')
for (clu in unique(data_spl1$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl1[data_spl1$clu_spl2==clu, comp ]
    temp_other=data_spl1[data_spl1$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      mdn=median(temp, na.rm = T)
      mdn_other=median(data_spl1[data_spl1$clu_spl2!=clu, comp ], na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      N_other=sum(!is.na(temp_other))
      symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, dir, N, N_other, mdn, mdn_other, res$statistic, res$p.value)
    }
    else{
      symps_results1[nrow(symps_results1)+1, ]=c(clu, comp, NA, NA, NA, NA, NA, NA, NA)
    }
  }
}
write.csv(symps_results1, 'tables/beh_split1.csv', row.names = F)

# Test each cluster vs median of clinical participants not in the cluster
symps_results2=as.data.frame(matrix(nrow=0, ncol=9))
names(symps_results2)=c('Cluster', 'Symptom_composite', 'greater_spl2', 'N_spl2', 'N_spl2_other', 'mdn_spl2', 'mdn_other_spl2', 'stat','p_spl2')
for (clu in unique(data_spl2$clu_spl2)){
  for (comp in comps_names){
    temp=data_spl2[data_spl2$clu_spl2==clu, comp ]
    temp_other=data_spl2[data_spl2$clu_spl2!=clu, comp ]
    # Check that there are enough people with data
    if (sum(is.na(temp))<length(temp)){
      res=wilcox.test(x=temp, mu=median(temp_other, na.rm = T), conf.int = T)
      mdn=median(temp, na.rm = T)
      mdn_other=median(data_spl2[data_spl2$clu_spl2!=clu, comp ], na.rm = T)
      dir=(mdn>mdn_other)*1
      N=sum(!is.na(temp))
      N_other=sum(!is.na(temp_other))
      symps_results2[nrow(symps_results2)+2, ]=c(clu, comp, dir, N, N_other, mdn, mdn_other, res$statistic, res$p.value)
    }
    else{
      symps_results2[nrow(symps_results2)+1, ]=c(clu, comp, NA, NA, NA, NA, NA, NA, NA)
    }
  }
}
write.csv(symps_results2, 'tables/beh_split2.csv', row.names = F)

symps_results_merged=merge(symps_results1, symps_results2, by = c('Cluster', 'Symptom_composite'))
write.csv(symps_results_merged, 'tables/beh_results_merged.csv', row.names = F)

