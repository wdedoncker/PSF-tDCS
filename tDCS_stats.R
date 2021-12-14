### Statistical Analysis of data from tDCS study
### Longitudinal sham-controlled intervention study in 30 patients
### Measures of fatigue (trait and state), neurophysiology (RMT and IO) and effort perception (implicit and explicit)

library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(datarium)
library(plotrix)
# for Windows
getwd()
setwd("C:/Users/zcbtwde/OneDrive - University College London/TIPS/Studies/tDCS/Data")
# for Mac
setwd("~/OneDrive - University College London/TIPS/Studies/tDCS/Data")
list.files()

# Import the .xlsx file into R. 
tdcs <- read_excel("tDCS_all_data.xlsx")
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")

tdcs.fss <- subset(tdcs.long, time == 1 | time == 3 | time == 4,
                   select = c(subject,intervention,time,fss))
tdcs.real <- subset(tdcs.fss, intervention == 'Real')
tdcs.sham <- subset(tdcs.fss, intervention == 'Sham')
tdcs.long <- tdcs.long %>%
  convert_as_factor(subject,intervention,hemisphere_aff,dom_side,stroke_type,stroke_location,time)
tdcs.fss <- tdcs.fss %>%
  convert_as_factor(subject,intervention,time)
tdcs.real <- tdcs.real %>%
  convert_as_factor(subject,intervention,time)
tdcs.sham <- tdcs.sham %>%
  convert_as_factor(subject,intervention,time)

# ----------------FSS analysis---------
# Visualise the data using boxplots
bxp <- ggboxplot(tdcs.fss, x = "time", y = "fss", color = "intervention", palette = "jco")
bxp

# Get Summary statsitics
tdcs.fss %>% 
  group_by(time,intervention) %>%
  get_summary_stats(fss, type = "mean_sd")

# Check for outliers in the data
tdcs.fss %>% 
  group_by(time,intervention) %>%
  identify_outliers(fss)

# Check for normality
tdcs.fss %>% 
  group_by(time,intervention) %>%
  shapiro_test(fss)

hist(tdcs.fss$fss)
hist(subset(tdcs.fss,time==3)$fss)

ggqqplot(tdcs, "fss", ggtheme = theme_bw()) +
  facet_grid(time ~ intervention)

# Check for homogeneity of variance - There is homogeneity of variance, as assessed by Levene's test (p>0.05) across all variables
tdcs %>%
  group_by(time) %>%
  levene_test(state ~ intervention)

# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = tdcs.fss, dv = fss, wid = subject,
  between = intervention, within = time
)
get_anova_table(res.aov)

# Effect of group at each time point
one.way <- tdcs.fss %>%
  group_by(time) %>%
  anova_test(dv = fss, wid = subject, between = intervention) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels
pwc <- tdcs.fss %>%
  group_by(time) %>%
  pairwise_t_test(fss ~ intervention, p.adjust.method = "bonferroni")
pwc

# Effect of time at each level 
one.way2 <- tdcs.fss %>%
  group_by(intervention) %>%
  anova_test(dv = fss, wid = subject, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- tdcs.fss %>%
  group_by(intervention) %>%
  pairwise_t_test(
    fss ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
pwc2

# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
pwc.filtered <- pwc %>% filter(time != "1")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Friedmann test for real and sham group
res.real <- tdcs.real %>% 
  friedman_test(fss ~ time |subject)
res.real
pwc.real <- tdcs.real %>%
  wilcox_test(fss ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc.real
tdcs.real %>%  wilcox_effsize(fss ~ time, paired = TRUE)

res.sham <- tdcs.sham %>% 
  friedman_test(fss ~ time |subject)
res.sham
pwc.sham <- tdcs.sham %>%
  wilcox_test(fss ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc.sham

# Change in fatigue between Sham and Real group
cfss <- tdcs.fss %>%
  spread(time, fss) 
cfss <- setNames(cfss, c("subject","intervention","t1","t2","t3"))
tdcs.cfss<- cfss %>%
  mutate(week = t2-t1)%>%
  mutate(month = t3-t1)%>%
  gather(time, fss, week:month)
pwc.wintervention <- subset(tdcs.cfss,time == 'week') %>%
  wilcox_test(fss ~ intervention, paired = FALSE, p.adjust.method = "bonferroni")
pwc.wintervention
subset(tdcs.cfss,time == 'week') %>%  wilcox_effsize(fss ~ intervention, paired = FALSE)

pwc.mintervention <- subset(tdcs.cfss,time == 'month') %>%
  wilcox_test(fss ~ intervention, paired = FALSE, p.adjust.method = "bonferroni")
pwc.mintervention
subset(tdcs.cfss,time == 'month') %>%  wilcox_effsize(fss ~ intervention, paired = FALSE)


# ----------------Secondary Outcome---------
# Remove missing values
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, slope_a) %>%
  spread(key = "time", value = "slope_a")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "slope_a", 
    `1`, `2`, `3`,`4`)

data.sham <- subset(data.long, intervention == 'Sham')
data.real <- subset(data.long, intervention == 'Real')
data.real <- data.real %>%
  convert_as_factor(subject,intervention,time)
data.sham <- data.sham %>%
  convert_as_factor(subject,intervention,time)

# Change in fatigue between Sham and Real group
cdata <- data.long %>%
  spread(time, slope_a) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(post = t2-t1)%>%
  mutate(week = t3-t1)%>%
  mutate(month = t4-t1)%>%
  gather(time, slope_a, post:month)

# Visualise the data using boxplots
bxp <- ggboxplot(data.long, x = "time", y = "slope_a", color = "intervention", palette = "jco")
bxp

# Get Summary statsitics
data.long %>% 
  group_by(time,intervention) %>%
  get_summary_stats(state, type = "mean_sd")

# Check for outliers in the data
data.long %>% 
  group_by(time,intervention) %>%
  identify_outliers(state)

# Check for normality
data.long %>% 
  group_by(time,intervention) %>%
  shapiro_test(state)

ggqqplot(data.long, "state", ggtheme = theme_bw()) +
  facet_grid(time ~ intervention)

# Check for homogeneity of variance - There is homogeneity of variance, as assessed by Levene's test (p>0.05) across all variables
data.long %>%
  group_by(time) %>%
  levene_test(state ~ intervention)

# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = data.long, dv = state, wid = subject,
  between = intervention, within = time
)
get_anova_table(res.aov)

# Effect of group at each time point
one.way <- data.long %>%
  group_by(time) %>%
  anova_test(dv = state, wid = subject, between = intervention) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# Pairwise comparisons between group levels
pwc <- data.long %>%
  group_by(time) %>%
  pairwise_t_test(state ~ intervention, p.adjust.method = "bonferroni")
pwc

# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
pwc.filtered <- pwc %>% filter(time != "1")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Non-significant 2 way interaction
data.long %>%
  pairwise_t_test(
    state ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
data.long %>%
  pairwise_t_test(
    state ~ intervention, 
    p.adjust.method = "bonferroni"
  )

# Friedmann test for real and sham group
res2.real <- data.real %>% 
  friedman_test(state ~ time |subject)
res2.real
pwc2.real <- data.real %>%
  wilcox_test(state ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc2.real
data.real %>%  wilcox_effsize(state ~ time, paired = TRUE)

res2.sham <- data.sham %>% 
  friedman_test(state ~ time |subject)
res2.sham
pwc2.sham <- data.sham %>%
  wilcox_test(state ~ time, paired = TRUE, p.adjust.method = "bonferroni")
pwc2.sham

pwc2.pintervention <- subset(cdata,time == 'post') %>%
  wilcox_test(state ~ intervention, paired = FALSE, p.adjust.method = "bonferroni")
pwc2.pintervention
pwc2.wintervention <- subset(cdata,time == 'week') %>%
  wilcox_test(state ~ intervention, paired = FALSE, p.adjust.method = "bonferroni")
pwc2.wintervention
subset(cdata,time == 'week') %>%  wilcox_effsize(state ~ intervention, paired = FALSE)

pwc2.mintervention <- subset(cdata,time == 'month') %>%
  wilcox_test(state ~ intervention, paired = FALSE, p.adjust.method = "bonferroni")
pwc2.mintervention

# ----------------FSS Figure---------
cfss <- tdcs.fss %>%
  spread(time, fss) 
cfss <- setNames(cfss, c("subject","intervention","t1","t2","t3"))
cfss.plot <- cfss %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  gather(time, fss, time1:time3)
gd <- cfss.plot %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),fss)
cfss.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "FSS-7",x = "Time",y = "ΔFSS")+
  scale_x_discrete(labels = c("Pre","Week","Month"))+
  scale_y_continuous(limits = c(-2.5,1),breaks = c(1,0,-1,-2))
cfss.plot <- cfss.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) +
  annotate("text", x = 2, y = 0.15, label = "*" , color="black", size=8 ,fontface="bold") + 
  annotate("text", x = 2, y = -0.9, label = "*" , color="royalblue3", size=8 ,fontface="bold") + 
  annotate("text", x = 2, y = -2, label = "*" , color="red2", size=8 ,fontface="bold") + 
  annotate("text", x = 3, y = -1.1, label = "*" , color="red2", size=8 ,fontface="bold")  
cfss.plot

ggsave(
  "fss.tiff",
  plot = cfss.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------State Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, state) %>%
  spread(key = "time", value = "state")
#data.wide[11,6] = NA # value is definitely an outlier in RMT_U
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "state", 
    `1`, `2`, `3`,`4`)

# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, state) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, state, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),state)
state.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "State Fatigue",x = "Time",y = "ΔState")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-2,2),breaks = c(2,1,0,-1,-2))
state.plot <- state.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) 
state.plot
ggsave(
  "state.tiff",
  plot = state.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------RMT-U Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, rmt_u) %>%
  spread(key = "time", value = "rmt_u")
data.wide[11,6] = NA # value is definitely an outlier in RMT_U
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "rmt_u", 
    `1`, `2`, `3`,`4`)

# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, rmt_u) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, rmt_u, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),rmt_u)
rmtu.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "RMT-U",x = "Time",y = "ΔRMT")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-3,3),breaks = c(3,2,1,0,-1,-2,-3))
rmtu.plot <- rmtu.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) 
rmtu.plot
ggsave(
  "rmtu.tiff",
  plot = rmtu.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------RMT-A Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, rmt_a) %>%
  spread(key = "time", value = "rmt_a")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "rmt_a", 
    `1`, `2`, `3`,`4`)

# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, rmt_a) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, rmt_a, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),rmt_a)
rmta.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "RMT-A",x = "Time",y = "ΔRMT")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-3,3),breaks = c(3,2,1,0,-1,-2,-3))
rmta.plot <- rmta.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) 
rmta.plot
ggsave(
  "rmta.tiff",
  plot = rmta.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------Slope-U Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, slope_u) %>%
  spread(key = "time", value = "slope_u")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "slope_u", 
    `1`, `2`, `3`,`4`)
data.long$slope_u <- data.long$slope_u*100
# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, slope_u) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, slope_u, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),slope_u)
slopeu.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "Slope-U",x = "Time",y = "ΔSlope")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-2,2),breaks = c(2,1,0,-1,-2))
slopeu.plot <- slopeu.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) 
slopeu.plot
ggsave(
  "slopeu.tiff",
  plot = slopeu.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------Slope-A Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, slope_a) %>%
  spread(key = "time", value = "slope_a")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "slope_a", 
    `1`, `2`, `3`,`4`)
data.long$slope_a <- data.long$slope_a*100
# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, slope_a) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, slope_a, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),slope_a)
slopea.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "Slope-A",x = "Time",y = "ΔSlope")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-2,2),breaks = c(2,1,0,-1,-2))
slopea.plot <- slopea.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) +
  annotate("text", x = 3, y = 1.5, label = "*" , color="black", size=8 ,fontface="bold") +
  annotate("text", x = 3, y = -1.5, label = "*" , color="red2", size=8 ,fontface="bold")
slopea.plot
ggsave(
  "slopea.tiff",
  plot = slopea.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------PE-Implicit Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, imp_pe_avg) %>%
  spread(key = "time", value = "imp_pe_avg")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "imp_pe_avg", 
    `1`, `2`, `3`,`4`)

# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, imp_pe_avg) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, imp_pe_avg, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),imp_pe_avg)
pei.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "PE-Implicit",x = "Time",y = "ΔPE")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-5,5),breaks = c(-5:5))
pei.plot <- pei.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) +
  annotate("text", x = 3, y = 4.75, label = "*" , color="black", size=8 ,fontface="bold") 
pei.plot
ggsave(
  "pei.tiff",
  plot = pei.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)
# ----------------PE-Explicit Figure---------
tdcs.long <- read_excel("tDCS_all_data_long.xlsx")
data.wide <- tdcs.long %>%
  select(subject, intervention, time, exp_pe_avg) %>%
  spread(key = "time", value = "exp_pe_avg")
data.wide <- data.wide %>%
  filter(complete.cases(data.wide))

data.long <- data.wide %>%
  gather(
    key = "time", value = "exp_pe_avg", 
    `1`, `2`, `3`,`4`)

# Change in state between Sham and Real group
cdata <- data.long %>%
  spread(time, exp_pe_avg) 
cdata <- setNames(cdata, c("subject","intervention","t1","t2","t3","t4"))
cdata <- cdata %>%
  mutate(time1 = t1-t1)%>%
  mutate(time2 = t2-t1)%>%
  mutate(time3 = t3-t1)%>%
  mutate(time4 = t4-t1)%>%
  gather(time, exp_pe_avg, time1:time4)

gd <- cdata %>% 
  group_by(time, intervention) %>% 
  summarise_each(funs(mean,std.error),exp_pe_avg)
pee.plot <- ggplot(gd, aes(x=time, y=mean, group=intervention, color=intervention))+
  geom_errorbar(aes(ymin=mean-std.error, ymax=mean+std.error), width=.1,position=position_dodge(0.05)) +
  geom_line() + geom_point()+      
  theme_classic()+ scale_color_manual(values=c("red2", "royalblue3", "#FC4E07"))+
  theme(axis.title.y = element_text(size=12),
        axis.text = element_text(size = 10))+
  theme(legend.position=c(0.13,0.23), legend.direction="vertical",legend.title = element_blank())+
  labs(title = "PE-Explicit",x = "Time",y = "ΔPE")+
  scale_x_discrete(labels = c("Pre","Post","Week","Month"))+
  scale_y_continuous(limits = c(-2,2),breaks = c(-2:2))
pee.plot <- pee.plot + geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.7) 
pee.plot

ggsave(
  "pee.tiff",
  plot = pee.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 300)

library(cowplot)
data.plot <- plot_grid(cfss.plot,state.plot,rmta.plot,rmtu.plot,
                       slopea.plot,slopeu.plot,pei.plot,pee.plot,
                 ncol = 2, nrow = 4,rel_widths = c(1, 1),
                 labels = c('A','B','C','D','E','F','G','H'))
data.plot
ggsave(
  "all_data.tiff",
  plot = data.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = 8.28,
  height = 10,
  dpi = 500)

# ----------------Linear Regression Model---------
# Change in fatigue between Sham and Real group
change <- tdcs %>%
  mutate(cslope_a = week_slope_a - pre_slope_a)%>%
  mutate(cfss = week_fss - pre_fss)%>%
  mutate(basefss = pre_fss)%>%
  mutate(cpe = week_state - pre_state)
change$cslope_a <- change$cslope_a*100 
change <- subset(subset(change,
                        select = c(subject:nfi,cslope_a:cpe)))
change.real <- subset(change, intervention == 'Real')
change.sham <- subset(change, intervention == 'Sham')
change <- change %>%
  convert_as_factor(subject,gender,intervention,perceived_real,hemisphere_aff,dom_side,stroke_type,stroke_location)
change.real <- change.real %>%
  convert_as_factor(subject,gender,intervention,perceived_real,hemisphere_aff,dom_side,stroke_type,stroke_location)
change.sham <- change.sham %>%
  convert_as_factor(subject,gender,intervention,perceived_real,hemisphere_aff,dom_side,stroke_type,stroke_location)

# Linear Regression model
m1 <- lm(cfss ~ 1, data=change.real)
summary(m1)
BIC(m1)
m2 <- lm(cfss ~ cslope_a, data=change.real)
BIC(m2)
summary(m2)
m3 <- lm(cfss ~ cslope_a + anxiety, data=change.real)
BIC(m3)
summary(m3)
m4 <- lm(cfss ~ cslope_a + cpe + anxiety, data=change.real)
BIC(m4)
summary(m4)

layout(matrix(c(1,2,3,4),2,2))
plot(m3)
layout(1)
hist(residuals(m3))
shapiro_test(residuals(m3))

library(ggResidpanel)
# Create the default panel of plots
resid_panel(m3)
library(olsrr)
ols_test_normality(m3)
library(moments)
skewness(residuals(m3))
kurtosis(residuals(m3))

# Create table with coefficients and CI of model for FSS model
betas <- data.frame(names(m3$coef),
                    (coef(m3)),
                    (confint(m3)),
                    coef(summary(m3))[,4])
write.csv(betas,'cfss_model.csv',row.names=F)
# ----------------Regression Plots---------
basefss.plot <- ggscatter(change, x = "basefss", y = "cfss",  
                      add = "reg.line",
                      conf.int = TRUE,
                      color = "intervention", 
                      palette = c("red2", "royalblue3"), size = 2,
                      xlab = "Baseline FSS-7", ylab = "ΔFSS-7 at Week",
                      font.label = c(14, "bold"),
                      ggtheme = theme_classic())+
  theme(legend.position=c(0.25,0.1), legend.direction="horizontal",legend.title = element_blank())+
  scale_x_continuous(breaks = c(1:7)) +
  scale_y_continuous(limits = c(-5,2),breaks = c(-5:1)) 
basefss.plot

anx_plot <- ggscatter(change.real, x = "anxiety", y = "cfss", 
                      add = "reg.line",
                      conf.int = TRUE, cor.coef = FALSE, cor.method = "spearman",
                      palette = "jco", size = 2,
                      add.params = list(color = "red2",fill = "lightgray"),
                      ylab = "ΔFSS-7 at Week", xlab = "Anxiety - HADS",
                      font.label = c(14, "bold"),
                      ggtheme = theme_classic())+
  rremove("legend") +
  scale_x_continuous(limits = c(0,13), breaks = c(0,3,6,9,12)) + 
  scale_y_continuous(limits = c(-5,2),breaks = c(-5:1)) 
anx_plot

change.plot <- plot_grid(basefss.plot,anx_plot,
                         ncol = 2, nrow = 1,
                         rel_widths = c(1, 1),
                         labels = c('A','B'))
change.plot

ggsave(
  "change.tiff",
  plot = change.plot,
  device = "tiff",
  path = "~/OneDrive - University College London/TIPS/Studies/tDCS/Data",
  scale = 1,
  width = NA,
  height = NA,
  dpi = 500)

basefss.anx <- ggscatter(change, x = "anxiety", y = "cfss",  
                          add = "reg.line",
                          conf.int = TRUE,
                          color = "intervention", 
                          palette = c("red2", "royalblue3"), size = 2,
                          xlab = "Anxiety - HADS", ylab = "ΔFSS-7 at Week",
                          font.label = c(14, "bold"),
                          ggtheme = theme_classic())+
  theme(legend.position=c(0.15,0.1), legend.direction="horizontal",legend.title = element_blank())+
  scale_y_continuous(limits = c(-5,2),breaks = c(-5:1)) 
basefss.anx
