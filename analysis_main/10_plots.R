library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(factoextra)
library(plyr)
library(data.table)
library(ggradar)
library(tidyverse)
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

data=read.csv('data/dataset_merged_qc_imputed_combat_clin_std_clu_dataspl_treat.csv')
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

# Bar plot of full profile of each cluster
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

# Bar plot of summary of circuit scores for each cluster
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


# Bar plot of summary of circuit scores for the two split halves
data_spl1=data[data$split2==1, ]
data_spl2=data[data$split2==2, ]

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

#### Reproduce original plot for each of the two splits #### 

# Prepare first split
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

# Bar plot for first split
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

# Prepare second split
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

# Bar plot for second split
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

#### Plots of symptoms ####

comps_names_old=c('Ruminative.worry','Ruminative.brooding','Tension','Negative.bias','Threat.dysregulation','Anhedonia','Anxious.arousal','Cognitive.dyscontrol')
comps_names=c('Ruminative worry','Ruminative brooding','Tension','Negative bias','Threat dysregulation','Anhedonia','Anxious arousal','Cognitive dyscontrol')
setnames(data,comps_names_old, comps_names)

# Function for rescaling 
rescale_min_max_vals <- function(x, minval, maxval) {
  (x - minval) / (maxval - minval)
}

# Rescale symptoms
data_copy=data
data_copy[, 'Ruminative worry']=rescale_min_max_vals(data_copy[, 'Ruminative worry'],16, 80) 
data_copy[, 'Ruminative brooding']=rescale_min_max_vals(data_copy[, 'Ruminative brooding'],22, 88) 
data_copy[, 'Tension']=rescale_min_max_vals(data_copy[, 'Tension'],0, 42) 
data_copy[, 'Negative bias']=rescale_min_max_vals(data_copy[, 'Negative bias'],0, 42) 
data_copy[, 'Threat dysregulation']=rescale_min_max_vals(data_copy[, 'Threat dysregulation'],0, 42) 
data_copy[, 'Anhedonia']=rescale_min_max_vals(data_copy[, 'Anhedonia'],0, 14) 
data_copy[, 'Anxious arousal']=rescale_min_max_vals(data_copy[, 'Anxious arousal'],10, 50) 
data_copy[, 'Cognitive dyscontrol']=rescale_min_max_vals(data_copy[, 'Cognitive dyscontrol'],8, 32) 
data_copy[, 'Sleep']=rescale_min_max_vals(data_copy[, 'Sleep'],0, 9) 

# Convert the data from wide to long format
long_data <- data_copy %>%
  pivot_longer(cols = all_of(comps_names), names_to = "Component", values_to = "Value")

# Compute the median for each Component in each cluster
medians <- long_data %>%
  group_by(Component, clu) %>%
  summarise(mdn_self = median(Value, na.rm = TRUE)) %>%
  ungroup()

# Compute the median for each Component for the other clusters
other_medians=as.data.frame(matrix(nrow = 0, ncol=3))
for (clu in unique(data_copy$clu)){
  for (comp in comps_names){
    vals=data_copy[data_copy$clu!=clu, comp]
    mdn=median(vals, na.rm=T)
    other_medians[nrow(other_medians)+1, ]=c(comp, clu, mdn)
  }
}
names(other_medians)=c('Component', 'clu', 'mdn_other')
other_medians$mdn_other=as.numeric(other_medians$mdn_other)
medians_all=merge(medians, other_medians,by = c('Component', 'clu') )

# Convert to factors
long_data$Component=factor(long_data$Component)
medians_all$Component=factor(medians_all$Component)

# Plot individual data points
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/sym_pointplot.png", sep=''),width=1000, height=1000)
ggplot(long_data, aes(x = Component, y = Value)) +
  geom_point(position = 'jitter', color = "grey", alpha=0.5) +
  facet_wrap(~clu, scales = "free_x") +  # add scales = "free_x" to allow each facet to have its own x scale + 
  geom_text(data=medians_all,aes(y=mdn_other),size=6,label="\u2014",family="Arial Unicode MS") +
  geom_point(data = medians_all, aes(y = mdn_self), color = "black", size = 4, shape = 23, fill = "red") +
  labs(title="", y = "Symptom severity")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) 
dev.off()



#### Plot boxplots of behavioral dysfunction ####
comps_names_old=c('Maze_completion_time','Maze_errors','Go.Nogo_mean_RT','Go.Nogo_commission_errors','Working_memory_omission_errors','Working_memory_commission_errors','Working_memory_RT','Implicit_threat_priming_RT','Implicit_happy_priming_RT','Implicit_sad_priming_RT','Explicit_threat_RT','Explicit_happy_RT','Explicit_sad_RT')
comps_names=c('Maze completion time','Maze errors','Go-Nogo mean RT','Go-Nogo commission errors','Working memory omission errors','Working memory commission errors','Working memory RT','Implicit threat priming RT','Implicit happy priming RT','Implicit sad priming RT','Explicit threat RT','Explicit happy RT','Explicit sad RT')

setnames(data_spl1,comps_names_old, comps_names, skip_absent = T)

# Convert the data from wide to long format
long_data <- data_copy %>%
  pivot_longer(cols = all_of(comps_names), names_to = "Component", values_to = "Value")

# Compute the median for each Component in each cluster
medians <- long_data %>%
  group_by(Component, clu) %>%
  summarise(mdn_self = median(Value, na.rm = TRUE)) %>%
  ungroup()

# Compute the median for each Component for the other clusters
other_medians=as.data.frame(matrix(nrow = 0, ncol=3))
for (clu in unique(data_copy$clu)){
  for (comp in comps_names){
    vals=data_copy[data_copy$clu!=clu, comp]
    mdn=median(vals, na.rm=T)
    other_medians[nrow(other_medians)+1, ]=c(comp, clu, mdn)
  }
}
names(other_medians)=c('Component', 'clu', 'mdn_other')
other_medians$mdn_other=as.numeric(other_medians$mdn_other)
medians_all=merge(medians, other_medians,by = c('Component', 'clu') )

# Convert to factors
long_data$Component=factor(long_data$Component)
medians_all$Component=factor(medians_all$Component)

# Plot individual data points
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/beh_pointplot.png", sep=''),width=1000, height=1000)
ggplot(long_data, aes(x = Component, y = Value)) +
  geom_point(position = 'jitter', color = "grey", alpha=0.5) +
  facet_wrap(~clu, scales = "free_x") +  # add scales = "free_x" to allow each facet to have its own x scale + 
  geom_text(data=medians_all,aes(y=mdn_other),size=6,label="\u2014",family="Arial Unicode MS") +
  geom_point(data = medians_all, aes(y = mdn_self), color = "black", size = 4, shape = 23, fill = "red") +
  labs(title="", y = "Performance")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) +
  ylim(c(-3, 5))
dev.off()


#### Plot treatment response ####

# Select only treatment data
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU')
data_mod=data[data$treatment_arm %in% treats,]
data_mod$clu=factor(data_mod$clu)
data_mod$treatment_arm=factor(data_mod$treatment_arm, levels = c('Escitalopram', 'Sertraline', 'Venlafaxine XR', 'I-CARE', 'TAU'), labels = c('Escitalopram', 'Sertraline', 'Venlafaxine', 'I-CARE', 'U-CARE'))
treats=c('Escitalopram', 'Sertraline', 'Venlafaxine', 'I-CARE', 'U-CARE')

# Convert the data from wide to long format
long_data <- data_mod %>%
  pivot_longer(cols = all_of('treat_severity_fu_scaled'), names_to = "Component", values_to = "Value")

# Compute the median for each Component in each cluster
medians <- long_data %>%
  group_by(Component, clu, treatment_arm) %>%
  summarise(
    mdn_self = median(Value, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Compute the median for each Component for the other clusters
other_medians=as.data.frame(matrix(nrow = 0, ncol=3))
for (clu in unique(data_mod$clu)){
  for (treat in treats){
    vals=data_mod[data_mod$clu!=clu & data_mod$treatment_arm==treat, 'treat_severity_fu_scaled']
    mdn=median(vals, na.rm=T)
    other_medians[nrow(other_medians)+1, ]=c(treat, clu, mdn)
  }
}

names(other_medians)=c('treatment_arm', 'clu', 'mdn_other')
other_medians$mdn_other=as.numeric(other_medians$mdn_other)
medians_all=merge(medians, other_medians,by = c('treatment_arm', 'clu') )

# Filter clusters with less than 6 people in each treatment from the median dataframe
medians_all=medians_all[medians_all$n>5, ]

# Now also remove these cluster-treatment combinations from the long_data dataframe
long_data <- long_data %>%
  inner_join(medians_all %>% select(clu, treatment_arm), by = c("clu", "treatment_arm"))

# Plot individual data points
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/treat_pointplot.png", sep=''),width=1000, height=1000)
ggplot(long_data, aes(x = clu, y = Value)) +
  geom_point(position = 'jitter', color = "grey", alpha=0.5) +
  facet_wrap(nrow = 1, ~treatment_arm, scales = "free_x") +  # add scales = "free_x" to allow each facet to have its own x scale + 
  geom_text(data=medians_all,aes(y=mdn_other),size=7,label="\u2014",family="Arial Unicode MS") +
  geom_point(data = medians_all, aes(y = mdn_self), color = "black", size = 4, shape = 23, fill = "red") +
  labs(title="", y = "Severity after treatment")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) 
dev.off()



#### Correlation matrix between circuit scores and symptoms ####
img_vars_names=c('D2D1', 'D1D3', 'D1D4', 'D2D4', 'D3D4', 'S1S3', 'S2S4', 'S1S2', 'A2A1', 'A3A1', 'A4A2', 'A5A3', 'A4A6', 'A5A7', 'NS4', 'NS5', 'NS3', 'NS2', 'NS1', 'NS2NS1b', 'NS1NS2a', 'NS3NS1b', 'NS1NS3a', 'NS4NS1b', 'NS1NS4a', 'NS5NS1b', 'NS1NS5a', 'NT2', 'NT3', 'NT1', 'NT2NT1a', 'NT1NT2b', 'NT3NT1b', 'NT1NT3a', 'NTN3', 'NTN2', 'NTN1', 'NTN2NTN1a', 'NTN1NTN2b', 'NTN3NTN1a', 'NTN1NTN3b', 'P1', 'P2', 'P3', 'C1', 'C3', 'C2', 'C1C2a', 'C2C1b', 'C3C2b', 'C2C3a')
comps_names=c('Ruminative worry','Ruminative brooding','Tension','Negative bias','Threat dysregulation','Anhedonia','Anxious arousal','Cognitive dyscontrol')

cmat=cor(data[, c(comps_names, img_vars_names)], use = 'pairwise.complete.obs', method = 'spearman')

# Only features of interest
cmat=cmat[1:length(comps_names),(length(comps_names)+1):ncol(cmat)]
  
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/symps_corrmat.png", sep=''),width=400, height=800)
ggcorrplot(cmat, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Calculate p-value matrix
pmat <- sapply(data[, c(img_vars_names, comps_names)], function(x) {
  sapply(data[, c(img_vars_names, comps_names)], function(y) {
    # Check if the overlap between x and y are all NA
    if (all(is.na(x) | is.na(y))) {
      return(NA)
    } else {
      return(cor.test(x, y, method = 'spearman', use = 'pairwise.complete.obs')$p.value)
    }
  })
})

# Only features of interest
pmat=pmat[1:length(comps_names),(length(comps_names)+1):ncol(pmat)]

# Correct p-values for multiple testing if necessary
pmat_fdr=pmat
pmat_fdr[,] <- p.adjust(pmat, method = "fdr")

# Plot corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/symps_corrmat_p.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Plot FDR corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat_fdr) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/symps_corrmat_pfdr.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


# Correlation matrix between circuit scores and cognition
comps_names=c('Maze completion time','Maze errors','Go-Nogo mean RT','Go-Nogo commission errors','Working memory omission errors','Working memory commission errors','Working memory RT','Implicit threat priming RT','Implicit happy priming RT','Implicit sad priming RT','Explicit threat RT','Explicit happy RT','Explicit sad RT')
cmat=cor(data[, c(comps_names, img_vars_names)], use = 'pairwise.complete.obs', method = 'spearman')

# Only features of interest
cmat=cmat[1:length(comps_names),(length(comps_names)+1):ncol(cmat)]

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/behs_corrmat.png", sep=''),width=400, height=800)
ggcorrplot(cmat, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Calculate p-value matrix
pmat <- sapply(data[, c(img_vars_names, comps_names)], function(x) {
  sapply(data[, c(img_vars_names, comps_names)], function(y) {
    # Check if the overlap between x and y are all NA
    if (all(is.na(x) | is.na(y))) {
      return(NA)
    } else {
      return(cor.test(x, y, method = 'spearman', use = 'pairwise.complete.obs')$p.value)
    }
  })
})

# Only features of interest
pmat=pmat[1:length(comps_names),(length(comps_names)+1):ncol(pmat)]

# Correct p-values for multiple testing if necessary
pmat_fdr=pmat
pmat_fdr[,] <- p.adjust(pmat, method = "fdr")

# Plot corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/behs_corrmat_p.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Plot FDR corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat_fdr) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/behs_corrmat_pfdr.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


# Correlation matrix between circuit scores and severity change
comps=c('treat_severity_bl_scaled', 'treat_severity_fu_scaled', 'response', 'remission')
comps_names=c('Treatment severity baseline', 'Treatment severity follow-up','Response','Remission')
data=setnames(data, comps, comps_names)

cmat=cor(data[, c(comps_names, img_vars_names)], use = 'pairwise.complete.obs', method = 'spearman')

# Only features of interest
cmat=cmat[1:length(comps_names),(length(comps_names)+1):ncol(cmat)]

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/treat_corrmat.png", sep=''),width=400, height=800)
ggcorrplot(cmat, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Calculate p-value matrix
pmat <- sapply(data[, c(img_vars_names, comps_names)], function(x) {
  sapply(data[, c(img_vars_names, comps_names)], function(y) {
    # Check if the overlap between x and y are all NA
    if (all(is.na(x) | is.na(y))) {
      return(NA)
    } else {
      return(cor.test(x, y, method = 'spearman', use = 'pairwise.complete.obs')$p.value)
    }
  })
})

# Only features of interest
pmat=pmat[1:length(comps_names),(length(comps_names)+1):ncol(pmat)]

# Correct p-values for multiple testing if necessary
pmat_fdr=pmat
pmat_fdr[,] <- p.adjust(pmat, method = "fdr")

# Plot corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/treat_corrmat_p.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Plot FDR corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat_fdr) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/treat_corrmat_pfdr.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


# Correlation matrix between rest features and task features
cmat=cor(data[, img_vars_names], use = 'pairwise.complete.obs', method = 'spearman')

# Only features of interest
cmat=cmat[1:14,15:ncol(cmat)]

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/rest_corrmat.png", sep=''),width=400, height=800)
ggcorrplot(cmat, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Calculate p-value matrix
pmat <- sapply(data[, c(img_vars_names)], function(x) {
  sapply(data[, c(img_vars_names)], function(y) {
    # Check if the overlap between x and y are all NA
    if (all(is.na(x) | is.na(y))) {
      return(NA)
    } else {
      return(cor.test(x, y, method = 'spearman', use = 'pairwise.complete.obs')$p.value)
    }
  })
})

# Only features of interest
pmat=pmat[1:14,15:ncol(pmat)]

# Correct p-values for multiple testing if necessary
pmat_fdr=pmat
pmat_fdr[,] <- p.adjust(pmat, method = "fdr")

# Plot corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/rest_corrmat_p.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# Plot FDR corrected matrix
cmat_adj <- cmat
cmat_adj[as.matrix(pmat_fdr) >= 0.05] <- 0
png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/rest_corrmat_pfdr.png", sep=''),width=400, height=800)
ggcorrplot(cmat_adj, hc.order = F, type = "full", ggtheme = ggplot2::theme_minimal, show.diag = T)+
  scale_fill_gradient2(limit = c(-1,1), low = "blue", high =  "red", mid = "white", midpoint = 0) + labs(fill = "Spearman rho")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


#### Plot barplots of suicidality and insomnia ####
clusters <- unique(data$clu)
data_combined <- data.frame()

for (cluster in clusters) {
  total_in_cluster <- sum(data$clu == cluster, na.rm = TRUE)
  total_not_in_cluster <- sum(data$clu != cluster, na.rm = TRUE)
  
  in_cluster_sleep <- 0
  in_cluster_suicide <- 0
  not_in_cluster_sleep <- 0
  not_in_cluster_suicide <- 0
  
  # Iterate through the rows of the data
  for (i in 1:nrow(data)) {
    if (is.na(data$Sleep[i]) || is.na(data$Suicide[i])) next # Skip if either value is NA
    
    if (data$clu[i] == cluster) {
      if (data$Sleep[i] > 0) in_cluster_sleep <- in_cluster_sleep + 1
      if (data$Suicide[i] > 0) in_cluster_suicide <- in_cluster_suicide + 1
    } else {
      if (data$Sleep[i] > 0) not_in_cluster_sleep <- not_in_cluster_sleep + 1
      if (data$Suicide[i] > 0) not_in_cluster_suicide <- not_in_cluster_suicide + 1
    }
  }
  
  # Convert to percentages
  in_cluster_sleep <- 100 * in_cluster_sleep / total_in_cluster
  in_cluster_suicide <- 100 * in_cluster_suicide / total_in_cluster
  not_in_cluster_sleep <- 100 * not_in_cluster_sleep / total_not_in_cluster
  not_in_cluster_suicide <- 100 * not_in_cluster_suicide / total_not_in_cluster
  
  # Add the percentages to the data_combined data frame
  data_combined <- rbind(
    data_combined,
    data.frame(clu = cluster, Metric = "Sleep", Count = in_cluster_sleep, Type = "In Cluster", Color = "lightcoral"),
    data.frame(clu = cluster, Metric = "Suicide", Count = in_cluster_suicide, Type = "In Cluster", Color = "lightcoral"),
    data.frame(clu = cluster, Metric = "Sleep", Count = not_in_cluster_sleep, Type = "Not In Cluster", Color = "lightgrey"),
    data.frame(clu = cluster, Metric = "Suicide", Count = not_in_cluster_suicide, Type = "Not In Cluster", Color = "lightgrey")
  )
}

data_combined$Type=factor(data_combined$Type, levels = c('In Cluster', 'Not In Cluster'))

png(file=paste("/Users/ltozzi/Dropbox (PanLab)/cluster paper/plots/sleepsuicid_bars.png", sep=''),width=800, height=800)
ggplot(data_combined, aes(x = Metric, y = Count, fill = Color)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ clu, ncol = 2) +
  scale_fill_identity() +
  labs(title="", y = "Participants reporting (%)")+
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "bottom", legend.title=element_text(size=18), legend.text=element_text(size=18))+
  theme(panel.spacing = unit(2, "lines"))+
  theme(axis.text = element_text(size = 18), axis.title =  element_text(size = 18), strip.text.x = element_text(size = 18)) 
dev.off()


