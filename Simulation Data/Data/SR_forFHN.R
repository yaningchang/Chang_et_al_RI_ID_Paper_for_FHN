library(languageR)
library(ggplot2)
library(gridExtra)
library(lme4)       # for lmer()
library(sjstats)
library(ggpubr)
library(rstatix)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(lm.beta)
library(corrplot)
library(Hmisc)
library(car)

rm(list=ls())      # clear all variables in memory

dol_data <- data.frame()

setwd('Your Path')

################################################################################
#Individual Level Analysis: The relationship between SR, focus
################################################################################
#Read DOL data
dol_data <- readRDS('dol_data.rds')
dol_data$focus <- factor(dol_data$focus, levels=c("OP", "OP-OS", "OS"))
dol_data[is.na(dol_data$focus), "focus"] <- 'OP-OS'
dol_data$focus <- factor(dol_data$focus, levels=c("OP", "OP-OS", "OS"))

fit_sr <- lm(sr ~ 1+focus, data = dol_data)
print(summary(fit_sr))
fit_sr.beta <- lm.beta(fit_sr)
coef(fit_sr.beta)

dol_data$focus <- relevel(dol_data$focus, ref='OP-OS')
fit_sr_1 <- lm(sr ~ 1+focus, data = dol_data)
print(summary(fit_sr_1))
fit_sr_1.beta <- lm.beta(fit_sr_1)
coef(fit_sr_1.beta)

dol_data %>%
  group_by(focus) %>%
  get_summary_stats(sr, type = "mean_sd")

mean(dol_data$sr)
sd(dol_data$sr)
min(dol_data$sr)
max(dol_data$sr)

#Read polarity
all_polarity <- read.table('all_polarity.txt', header=TRUE)
all_polarity_phon <- gather(subset(all_polarity, select=c(version, op_focus_phonology:ba_focus_phonology)), focus, phon_pol, op_focus_phonology:ba_focus_phonology, factor_key=TRUE)
levels(all_polarity_phon$focus)[levels(all_polarity_phon$focus)=="op_focus_phonology"] <- "OP"
levels(all_polarity_phon$focus)[levels(all_polarity_phon$focus)=="os_focus_phonology"] <- "OS"
levels(all_polarity_phon$focus)[levels(all_polarity_phon$focus)=="ba_focus_phonology"] <- "OP-OS"

all_polarity_sem <- gather(subset(all_polarity, select=c(version, op_focus_semantics:ba_focus_semantics)), focus, sem_pol, op_focus_semantics:ba_focus_semantics, factor_key=TRUE)
levels(all_polarity_sem$focus)[levels(all_polarity_sem$focus)=="op_focus_semantics"] <- "OP"
levels(all_polarity_sem$focus)[levels(all_polarity_sem$focus)=="os_focus_semantics"] <- "OS"
levels(all_polarity_sem$focus)[levels(all_polarity_sem$focus)=="ba_focus_semantics"] <- "OP-OS"

all_polarity_osh <- gather(subset(all_polarity, select=c(version, op_focus_osh:ba_focus_osh)), focus, osh_pol, op_focus_osh:ba_focus_osh, factor_key=TRUE)
levels(all_polarity_osh$focus)[levels(all_polarity_osh$focus)=="op_focus_osh"] <- "OP"
levels(all_polarity_osh$focus)[levels(all_polarity_osh$focus)=="os_focus_osh"] <- "OS"
levels(all_polarity_osh$focus)[levels(all_polarity_osh$focus)=="ba_focus_osh"] <- "OP-OS"

all_polarity_oph<- gather(subset(all_polarity, select=c(version, op_focus_oph:ba_focus_oph)), focus, oph_pol, op_focus_oph:ba_focus_oph, factor_key=TRUE)
levels(all_polarity_oph$focus)[levels(all_polarity_oph$focus)=="op_focus_oph"] <- "OP"
levels(all_polarity_oph$focus)[levels(all_polarity_oph$focus)=="os_focus_oph"] <- "OS"
levels(all_polarity_oph$focus)[levels(all_polarity_oph$focus)=="ba_focus_oph"] <- "OP-OS"

dol_data_act_polarity <- merge(dol_data_act, all_polarity_phon, by.x = c('version', 'focus'), by.y = c('version', 'focus'), sort=FALSE)
dol_data_act_polarity <- merge(dol_data_act_polarity, all_polarity_sem, by.x = c('version', 'focus'), by.y = c('version', 'focus'), sort=FALSE)
dol_data_act_polarity <- merge(dol_data_act_polarity, all_polarity_osh, by.x = c('version', 'focus'), by.y = c('version', 'focus'), sort=FALSE)
dol_data_act_polarity <- merge(dol_data_act_polarity, all_polarity_oph, by.x = c('version', 'focus'), by.y = c('version', 'focus'), sort=FALSE)

################################################################################
#Load reading aloud data for the SR analysis
################################################################################
data_all_op <- readRDS("data_reading_aloud.rds")

#Data merged psycholinguistic variables
data_variable <- read.table("lexical_variables_6229words.txt", head=TRUE)
data_naming <- merge(data_all_op, data_variable, by.x='item', by.y='item')
data_con <- read.csv("concreteness.csv")
data_img <- read.csv("cortese2004norms.csv", head=TRUE)
data_naming_con <- merge(data_naming, data_con, by.x='word_untagged', by.y='Word')
data_naming_con_img <- merge(data_naming_con, data_img, by.x='word_untagged', by.y='item')

#Only for mature reading (reading training 1M) and correct data
data_naming_mature <- data_naming_con_img[(data_naming_con_img$pretrain==2e+06) & (data_naming_con_img$time==1e+06), ]
data_naming_mature_correct <- data_naming_mature[data_naming_mature$correct=="correct", ]

avg_data_naming_mature <- aggregate(data_naming_mature, by=list(data_naming_mature$version, data_naming_mature$focus), FUN='mean')
avg_data_naming_mature_correct <- aggregate(data_naming_mature_correct, by=list(data_naming_mature_correct$version, data_naming_mature_correct$focus), FUN='mean')
avg_data_naming_mature$sse_correct <- avg_data_naming_mature_correct$sse
avg_data_naming_mature$error_correct <- avg_data_naming_mature_correct$error
avg_data_naming_mature$focus <- factor(avg_data_naming_mature$Group.2)
levels(avg_data_naming_mature$focus)[levels(avg_data_naming_mature$focus)=="BA"] <- "OP-OS"
dol_data_act_polarity_acc_sse <- merge(dol_data_act_polarity, avg_data_naming_mature, by.x=c('version', 'focus'), by.y=c('version', 'focus'))

#####correlation table for psycholinguistic variables
data_variable_img <- merge(data_variable, data_img, by.x='word_untagged', by.y='item')
x <- subset(data_variable_img, select=c("logFreq","orthN", "consistency", "rating"))
mcor <-cor(x,, use="complete.obs")
corrplot(mcor, method = "number",col="black", cl.pos="n")
rcorr(as.matrix(x))

# The SR effect: Regression-based Approach
dol_data$new_version_id <- seq(1,nrow(dol_data),1)
data_naming_mature_correct$focus <- factor(data_naming_mature_correct$focus, levels=c("OP", "OP-OS", "OS"))
data_naming_mature_correct[is.na(data_naming_mature_correct$focus), "focus"] <- 'OP-OS'
data_naming <- merge(dol_data, data_naming_mature_correct, by.x = c('version', 'focus'), by.y = c('version', 'focus'), sort=FALSE)
data_naming$op_error <- data_naming$error
data_naming$op_error_s <- scale(data_naming$error)
data_naming$log_op_error <- log10(data_naming$error)
data_naming$log_op_error_s <- scale(data_naming$log_op_error)
data_naming$logFreq_s <- scale(data_naming$logFreq)
data_naming$orthN_s <- scale(data_naming$orthN)
data_naming$consistency_s <- scale(data_naming$consistency)
data_naming$Conc.M_s <- scale(data_naming$Conc.M)
data_naming$rating_s <- scale(data_naming$rating)
data_naming$sr_s <- scale(data_naming$sr)
data_naming$log_sr <- log10(data_naming$sr)
data_naming$log_sr_s <- scale(data_naming$log_sr)
data_naming$focus <- factor(data_naming$focus, levels=c("OP", "OP-OS", "OS"))

data_naming_3SD <- data_naming[(abs(data_naming$op_error-mean(data_naming$op_error))<3*sd(data_naming$op_error)),] 
data_naming_pre <- data_naming_3SD

fit_naming_inter <- lmer(op_error_s ~ (1|item)+(1|new_version_id)+logFreq_s+orthN_s+sr_s*consistency_s*rating_s, data=data_naming_pre, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)), REML=FALSE)
print(summary(fit_naming_inter))
confint(fit_naming_inter)

# The SR effect: Factorial Approach (Woollams et al. 2016)
woollams_w <- read.table('Woolloams2016_Word.csv', sep=',', header=TRUE)
data_naming_sbuset <- merge(woollams_w, data_naming_pre, by.x='Word', by.y='word.x')
data_naming_sbuset$Consistency <- factor(data_naming_sbuset$Consistency, levels=c("Icon", "Con"))
data_naming_sbuset$Imageability <- factor(data_naming_sbuset$Imageability, levels=c("Low", "High"))

fit_woollams <- lmer(op_error_s ~ (1|new_version_id)+(1|item)+sr_s*consistency_s*rating_s, data=data_naming_sbuset, control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
print(summary(fit_woollams))

################################################################################
#Plots
################################################################################
set_theme(title.color = "black", 
          title.size = 3,
          axis.title.size = 1.5,
          axis.textsize = 2,
          axis.textcolor = "black",
          panel.gridcol="white",
          legend.size = 1.5,
          legend.item.backcol="white",
          legend.title.size = 1.5,
          legend.item.size = 0.8,
          legend.inside = TRUE,
          legend.pos = c(0.1,0.08),
          legend.just=c(0.1,0.08),
          legend.background = element_blank(),
          base = theme_bw())

############################################################
#Figure 2: SR distribution
############################################################
ggplot(dol_data, aes(x=sr)) + geom_histogram(color="black", fill="darkgray") +
  labs(x="Semantic Reliance", y = "Count")+
  theme(legend.position = c(0.2, 0.7), legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))

dol_data$focus <- relevel(dol_data$focus, ref='OP')
ggplot(dol_data, aes(x=focus, y=sr, fill=focus)) + 
  geom_violin(trim=FALSE)+
  labs(x="Reading Instruction", y = "Semantic Reliance")+
  geom_boxplot(width=0.1, fill='white')+
  scale_fill_brewer(palette="Dark2") +
  theme(legend.position = 'none', legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))


#########################################################################################################
#Figure 3: Phonological polarity and  Phonological SSE categorised by training focus and SR
#########################################################################################################
ggplot(dol_data_act_polarity_acc_sse, aes(x=sr, y=sse_correct), group=focus) +
  geom_point(aes(shape=focus, color=focus), size=2) +ylab("Phonological SSE")+xlab("Semantic Reliance")+
  #guides(color = guide_legend(override.aes = list(fill=NA, shape = NA)))+
  labs(shape = "Training Focus", color = "Training Focus")+
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c("black", "red", "blue")) +
  #guides(shape = guide_legend(title = "Users By guides"))+
  ylim(0,0.1)+
  theme(legend.position = c(0.2, 0.7), legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        legend.title = element_text(size=20), axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))

ggplot(dol_data_act_polarity_acc_sse, aes(x=sr, y=phon_pol), group=focus) +
  geom_point(aes(shape=focus, color=focus), size=2) +ylab("Phonological Polarity")+xlab("Semantic Reliance")+
  labs(shape = "Training Focus", color = "Training Focus")+
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c("black", "red", "blue")) +
  ylim(0.995,1)+
  theme(legend.position = c(0.2, 0.2), legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        legend.title = element_text(size=20), axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))

#########################################################################################################
#Figure 4: Interactions between SR and Consistency and between SR and Imageability
#########################################################################################################
p_naming_inter <- plot_model(fit_naming_inter, type = "pred", mdrt.values="meansd", term=c("sr_s", "consistency_s"),
                            title="",
                            axis.title=c("Semantic Reliance", "Phonological SSE"),
                            fill.color=NULL)
p_naming_inter +ylab("Phonological SSE") +
  guides(color = guide_legend(override.aes = list(fill=NA, shape = NA)))+ labs(colour = "Consistency")+
  scale_color_manual(values=c("black", "red", "blue"), labels = c('Low', 'Medium', 'High')) +
  theme(legend.position = c(0.2, 0.7), legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))

p_naming_inter_2 <- plot_model(fit_naming_inter, type = "pred", mdrt.values="meansd", term=c("sr_s", "rating_s"),
                               title="",
                               axis.title=c("Semantic Reliance", "Phonological SSE"),
                               fill.color=NULL)
p_naming_inter_2 +ylab("Phonological SSE") +
  guides(color = guide_legend(override.aes = list(fill=NA, shape = NA)))+ labs(colour = "Imageability")+
  scale_color_manual(values=c("black", "red", "blue"), labels = c('Low', 'Medium', 'High')) +
  theme(legend.position = c(0.2, 0.7), legend.text=element_text(size=20), legend.background = element_rect(fill="white"),
        axis.title.x=element_text(face="bold",size=24),axis.title.y=element_text(face="bold", size=24),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        plot.title = element_text(size=24, lineheight=.8, face="bold"),
        plot.margin=unit(c(2.1,2.1,2.1,2),"lines"),
        strip.text.x = element_text(size = 24))



