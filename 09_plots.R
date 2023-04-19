library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(factoextra)
library(plyr)
library(data.table)
library(ggradar)

library(emmeans)

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

setwd('/Users/ltozzi/Dropbox (PanLab)/cluster paper')

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_clinscores_behscores_treat.csv')
data$clu = factor(data$clu,levels = c('Rest hyper-connectivity', 'Inattention', 'Context insensitivity', 'Cognitive dyscontrol hyper',  'Cognitive dyscontrol hypo', 'Intact'))

#### Plot cluster profiles for two individuals ####
img_vars_names_final=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')
temp=data[1:2, ]
data_long <- melt(temp, id.vars = c("id", "clu"), measure.vars = img_vars_names_final, variable.name = "feature", value.name = "score")

# Basic line plot with points
ggplot(data=data_long, aes(x=feature, y=score, group=id, col=id)) +
  geom_line(linewidth=1.5)+
  geom_point(size=2)+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18))+
  ylab("Personalized regional circuit score")

##### Smaller distance matrix ####
X=data[1:100, img_vars_names_final]

# Calculate distance
dist_mat <- (1-cor(t(X)))
ggcorrplot(dist_mat, hc.order = TRUE, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = FALSE)+
  scale_fill_gradient2(limit = c(0,2), low = "blue", high =  "red", mid = "white", midpoint = 1) + labs(fill = "distance")

##### Dendrogram ####
X=data[, img_vars_names_final]
dist_mat <- as.dist(1-cor(t(X)))
hclust_res <- hclust(dist_mat, method = "average")
fviz_dend(hclust_res, k = 6, # Cut in 6 groups
          cex = 1, # label size
          k_colors = c("#2C3E50", "#E74C3C", "#3498DB", "#F1C40F","#27AE60", "#8E44AD"),
          show_labels = FALSE,horiz = TRUE)+  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18))


#### Plot cluster profiles ####
img_vars_names_final=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS1', 'NS2', 'NS3', 'NS4', 'NS5', 'NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1', 'NT1', 'NT2', 'NT3', 'NT2NT1', 'NT3NT1', 'NTN1', 'NTN2', 'NTN3', 'NTN2NTN1', 'NTN3NTN1', 'P1', 'P2', 'P3', 'C1', 'C2', 'C3', 'C1C2', 'C3C2')
data_long <- melt(data, id.vars = c("id", "clu"), measure.vars = img_vars_names_final, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", "clu"))

df_plot[startsWith(as.character(df_plot$feature), 'D'),'color']='#83A4D1'
df_plot[startsWith(as.character(df_plot$feature), 'S'),'color']='#46B674'
df_plot[startsWith(as.character(df_plot$feature), 'A'),'color']='#EFD658'
df_plot[startsWith(as.character(df_plot$feature), 'N'),'color']='#D77842'
df_plot[startsWith(as.character(df_plot$feature), 'P'),'color']='#7E5DA6'
df_plot[startsWith(as.character(df_plot$feature), 'C'),'color']='#B75A65'
df_plot$color=factor(df_plot$color, levels=c('#83A4D1', '#46B674', '#EFD658', '#D77842', '#7E5DA6', '#B75A65'))
df_plot=na.omit(df_plot)

# Bar plot of full sample
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars_allfeats.png", sep=''),width=2000, height=1200)
ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(-1.8,1.8), expand = c(0, 0)) +
  geom_errorbar(aes(x=feature, ymin=mn-se, ymax=mn+se), width=.1) + 
  labs(title="",x="", y = "")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1.2)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  scale_fill_manual(name='Regional circuit score', values = levels(df_plot$color), labels=c('Default', 'Salience', 'Attention', 'Negative', 'Positive', 'Cognitive')) +
  theme(strip.text.x = element_blank())
dev.off()


#### Plot summary of circuit scores ####
# Compute summary of circuit scores
data[, 'DMN-C']=rowMeans(data[, c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4')])
data[, 'SAL-C']=rowMeans(data[, c('S1S3', 'S2S4', 'S1S2')])
data[, 'ATT-C']=rowMeans(data[, c('A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7')])
data[, 'NAs-A']=rowMeans(data[, c('NS4', 'NS5', 'NS3', 'NS2', 'NS1')])
data[, 'NAs-C']=rowMeans(data[, c('NS2NS1', 'NS3NS1', 'NS4NS1', 'NS5NS1')])
data[, 'NAt-A']=rowMeans(data[, c('NT2', 'NT3', 'NT1')])
data[, 'NAt-C']=rowMeans(data[, c('NT2NT1', 'NT3NT1')])
data[, 'NAnt-A']=rowMeans(data[, c('NTN3', 'NTN2', 'NTN1')])
data[, 'NAnt-C']=rowMeans(data[, c('NTN2NTN1', 'NTN3NTN1')])
data[, 'POS-A']=rowMeans(data[, c('P1', 'P2', 'P3')])
data[, 'COG-A']=rowMeans(data[, c('C1', 'C3', 'C2')])
data[, 'COG-C']=rowMeans(data[, c('C1C2', 'C3C2')])

scores_names_final_rename=c('DMN-C', 'SAL-C', 'ATT-C', 'NAs-A', 'NAs-C', 'NAt-A', 'NAt-C', 'NAnt-A', 'NAnt-C', 'POS-A', 'COG-A', 'COG-C')

data_long <- melt(data, id.vars = c("id", "clu"), measure.vars = scores_names_final_rename, variable.name = "feature", value.name = "score")
df_plot <- data_summary(data_long, varname="score", groupnames=c("feature", "clu"))

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
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/circuit_profile_bars.png", sep=''),width=700, height=1200)
ggplot(df_plot)+ 
  geom_bar(aes(x = feature, y=mn, fill=color), stat="identity") + 
  facet_wrap(~clu, nrow = 3, scales='free') + 
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


#### Radar plots ####
data_melted=melt(data = data[, c('id', 'clu', scores_names_final_rename)], id.vars=c('id', 'clu'), measure.vars = scores_names_final_rename)
data_melted=na.omit(data_melted)
df_plot <- data_summary(data_melted, varname="value", groupnames=c("variable", "clu"))
temp=dcast(data = df_plot,formula = clu~variable,value.var = "mn")
clunames=temp$clu
temp$clu=as.numeric(factor(temp$clu))


for (clu in 1:max(temp$clu)){
  temp_plot=temp[clu, ]
  png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/", clu, '_circuit_radar.png', sep=''),width=800, height=600)
  print(ggradar(temp_plot, values.radar = c('','', ''), grid.min = -1, grid.mid = 0, grid.max = 1,  plot.title = paste(clunames[clu]),
                axis.labels = scores_names_final_rename, 
                gridline.min.linetype = 'solid',
                gridline.mid.linetype='dashed',
                gridline.max.linetype='solid',
                gridline.mid.colour = 'honeydew4',
                group.colours='honeydew4',
                background.circle.colour = "white",plot.extent.x.sf = 1.5))
  dev.off()
}

#### Symptom clusters ####

# Radar plots
comps=c('pswq_total', 'rrs_total','dass42_str_score','dass42_dep_score','dass42_anx_score', 'shaps_total','masq30_gen_score', 'bis_att_score', 'Suicide', 'Sleep')
comp_names=c('Ruminative worry', 'Ruminative brooding ', 'Tension', 'Negative bias','Threat dysregulation', 'Anhedonia', 'Anxious arousal', 'Cognitive dyscontrol','Suicide', 'Sleep')

# Scale all composites between 0 and 1
comps_scaled=c()
for (comp in comps){
  data[, paste(comp, '_scaled', sep = '')]=(data[, comp]-min(data[, comp], na.rm = TRUE))/(max(data[, comp], na.rm = TRUE)-min(data[, comp], na.rm = TRUE))
  comps_scaled=append(comps_scaled, paste(comp, '_scaled', sep = ''))
}

data_melted=melt(data = data[, c('id', 'clu', comps_scaled)], id.vars=c('id', 'clu'), measure.vars = comps_scaled)
data_melted=na.omit(data_melted)
df_plot <- data_summary(data_melted, varname="value", groupnames=c("variable", "clu"))
temp=dcast(data = df_plot,formula = clu~variable,value.var = "mdn")
clunames=temp$clu
temp$clu=as.numeric(factor(temp$clu))

for (clu in 1:max(temp$clu)){
  temp_plot=temp[clu, ]
  png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/", clu, '_symptom_radar.png', sep=''),width=800, height=600)
  print(ggradar(temp_plot, values.radar = c('','', ''), grid.min = 0, grid.mid = 0.5, grid.max = 1,  plot.title = paste(clunames[clu]),
                axis.labels = rep("", length(comp_names)),
                gridline.min.linetype = 'solid',
                gridline.mid.linetype='blank',
                gridline.max.linetype='solid',
                group.colours='honeydew4',
                background.circle.colour = "white",plot.extent.x.sf = 1.5))
  dev.off()
}

# Box plot by cluster
data_long <- melt(data, id.vars = c("id", "clu"), measure.vars = comps_scaled, variable.name = "feature", value.name = "value")

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/clu_sym_box.png", sep=''),width=800, height=1200)
ggplot(data_long)+ 
  geom_boxplot(aes(x = feature, y=value), fill='grey', outlier.shape = NA) + 
  facet_wrap(~clu, nrow = 3, scales='free') + 
  scale_y_continuous(limits=c(0,1), expand = c(0, 0)) +
  labs(title="",x="Circuit", y = "Symptom severity")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels=comp_names) +
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  scale_fill_manual(name='Circuit dysfunction', values = levels(df_plot$col), labels=c('Default', 'Salience', 'Attention', 'Negative', 'Positive', 'Cognitive', 'Within norm'))+
  theme(strip.text.x = element_blank())
dev.off()


#### Behavior clusters ####

comps=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_priming_RT','Implicit_sad_priming_RT','Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
comp_names=c('Maze completion time','Maze errors','Go-Nogo mean RT','Go-Nogo commission errors','Working memory omission errors','Working memory commission errors','Working memory RT','Implicit threat priming RT','Implicit happy priming RT','Implicit sad priming RT','Explicit threat RT','Explicit happy RT','Explicit sad RT')

# Scale all composites between 0 and 1
comps_scaled=c()
for (comp in comps){
  data[, paste(comp, '_scaled', sep = '')]=(data[, comp]-min(data[, comp], na.rm = TRUE))/(max(data[, comp], na.rm = TRUE)-min(data[, comp], na.rm = TRUE))
  comps_scaled=append(comps_scaled, paste(comp, '_scaled', sep = ''))
}

# Radar plots
data_melted=melt(data = data[, c('id', 'clu', comps_scaled)], id.vars=c('id', 'clu'), measure.vars = comps_scaled)
data_melted=na.omit(data_melted)
df_plot <- data_summary(data_melted, varname="value", groupnames=c("variable", "clu"))
temp=dcast(data = df_plot,formula = clu~variable,value.var = "mdn")
clunames=temp$clu
temp$clu=as.numeric(factor(temp$clu))

for (clu in 1:max(temp$clu)){
  temp_plot=temp[clu, ]
  png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/", clu, '_beh_radar.png', sep=''),width=800, height=600)
  print(ggradar(temp_plot, values.radar = c('','', ''), grid.min = 0, grid.mid = 0, grid.max =1,  plot.title = paste(clunames[clu]),
                axis.labels = rep("", length(comp_names)),
                gridline.min.linetype = 'solid',
                gridline.mid.linetype='blank',
                gridline.max.linetype='solid',
                group.colours='honeydew4',
                background.circle.colour = "white",plot.extent.x.sf = 1.5))
  dev.off()
}

# Box plot by cluster
data_long <- melt(data, id.vars = c("id", "clu"), measure.vars = comps, variable.name = "feature", value.name = "value")

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/clu_beh_box.png", sep=''),width=1200, height=1200)
ggplot(data_long)+ 
  geom_boxplot(aes(x = feature, y=value), fill='grey', outlier.shape = NA) + 
  facet_wrap(~clu, nrow =3, scales='free') + 
  scale_y_continuous(limits=c(-4,4), expand = c(0, 0)) +
  labs(title="",x="", y = "")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)) +
  xlab("") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_discrete(labels="") +
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  #theme(axis.text.x=element_blank(),axis.ticks.x=comp_names)+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  scale_fill_manual(name='Circuit dysfunction', values = levels(df_plot$col), labels=c('Default', 'Salience', 'Attention', 'Negative', 'Positive', 'Cognitive', 'Within norm')) + 
  theme(strip.text.x = element_blank())
dev.off()

#### Plot treatment results ####

# Test for effects of cluster and treatment
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'U-CARE')

data_treat=data[data$treatment_arm %in% treats,]
data_treat$treatment_arm=factor(data_treat$treatment_arm, levels = c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'U-CARE'), labels =  c('Escitalopram', 'Sertraline', 'Venlafaxine', 'I-CARE', 'U-CARE'), ordered = TRUE)

data_treat$clu=factor(data_treat$clu)
anova(lm(treat_severity_fu_scaled~treat_severity_bl_scaled+clu*treatment_arm, data=data_treat))
res=lm(treat_severity_fu_scaled~treat_severity_bl_scaled+clu*treatment_arm, data=data_treat)

# Contrasts
emmip(res, treatment_arm ~ clu, CIs = TRUE)
res_emmeans=emmeans(res, list(pairwise ~ treatment_arm | clu ), adjust='none', CI=TRUE)
emmeans_df=as.data.frame(res_emmeans)
confint(res_emmeans) # CIs
sig=sigma(res) # Cohen's d=emmean/sigma

temp=emmeans_df[1:30,]
temp$treatment_arm=factor(temp$treatment_arm, levels = c('Escitalopram', 'Sertraline', 'Venlafaxine', 'I-CARE', 'U-CARE'), labels =  c('Escitalopram', 'Sertraline', 'Venlafaxine', 'I-CARE', 'U-CARE'), ordered = TRUE)

# Plot of the longitudinal effects
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/treat_resp_bars.png", sep=''),width=1200, height=1200)
ggplot(temp)+ 
  geom_bar(aes(x = treatment_arm, y=emmean),stat="identity") + 
  facet_wrap(~clu, nrow = 3, scale='free') + 
  scale_y_continuous(limits=c(-0.1, 1), expand = c(0, 0)) +
  geom_errorbar(aes(x=treatment_arm, ymin=emmean-SE, ymax=emmean+SE), width=.1) + 
  labs(title="",x="", y = "Severity after treatment (marginal mean and SE)")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  theme(strip.text.x = element_blank())
dev.off()


# Radar plot for treatment effect
data$predicted_treatsev_scaled=(data$predicted_treatsev-min(data$predicted_treatsev, na.rm = TRUE))/(max(data$predicted_treatsev, na.rm = TRUE)-min(data$predicted_treatsev, na.rm = TRUE))

data_melted=melt(data = data[, c('id', 'clu', 'treatment_arm', 'predicted_treatsev_scaled')], id.vars=c('id', 'clu', 'treatment_arm'))
data_melted=na.omit(data_melted)
df_plot <- data_summary(data_melted, varname="value", groupnames=c("variable", "clu", "treatment_arm"))
df_plot$treatment_arm=factor(df_plot$treatment_arm, levels = c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU'), labels =  c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'U-CARE'), ordered = TRUE)

temp=dcast(data = df_plot,formula = clu~treatment_arm,value.var = "mdn")

# reorder columns
temp=temp[, c('clu', 'Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'U-CARE')]
clunames=temp$clu
temp$clu=as.numeric(factor(temp$clu))

for (clu in 1:max(temp$clu)){
  temp_plot=temp[temp$clu==clu, ]
  temp_plot=temp_plot[ , colSums(is.na(temp_plot))==0]
  
  png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/", clu, '_treatment_radar_nolabels.png', sep=''),width=800, height=600)
  print(ggradar(temp_plot, values.radar = c('','', ''), grid.min = 0, grid.mid = 0.5, grid.max = 1,  plot.title = paste(clunames[clu]),
                axis.labels = rep("", ncol(temp_plot)-1),
                gridline.min.linetype = 'solid',
                gridline.mid.linetype='blank',
                gridline.max.linetype='solid',
                group.colours='honeydew4',
                background.circle.colour = "white",plot.extent.x.sf = 1.5))
  dev.off()
}

#### Diagnosis pies ####
diags=c('mdd_current', 'gad_current', 'panic_current', 'social_phobia_current', 'ocd_current', 'ptsd_current')
diag_labels=c('Major depression', 'Generalized anxiety', 'Panic disorder', 'OCD', 'PTSD')

lab=1
par(mfrow=c(length(unique(data$clu)),length(diags)), mai=c(0,0,0,0))
for (clu in levels(data$clu)){
  for (diag in diags){
    
    diag_clucount=sum((data[, diag]==1) & (data['clu']==clu), na.rm = TRUE)
    nodiag_clucount=sum((data[, diag]==0) & (data['clu']==clu), na.rm = TRUE)
    
    pie(c(nodiag_clucount, diag_clucount), labels = "", main="", col = c("gray60", "red"), border = "white")
    lab=lab+1
  }
}




