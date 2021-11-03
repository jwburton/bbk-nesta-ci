## script for analysis of empirical data collected with the experiment

## install + load packages ----
pkgs <- c("tidyverse", "lme4", "lmerTest", "plotrix", "reshape2",
          "sjPlot", "cowplot", "e1071", "readxl", "afex")

for (p in pkgs){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
rm(p, pkgs)

## read in xlsx with event ids and outcomes ----
events <- read_excel("event-list.xlsx")

## read in csv with the data ----
DF <- read.csv("full_data.csv", 
               colClasses=c("NULL", rep("factor", 4), 
                            "integer", "factor","factor", "factor", 
                            NA, "numeric", "character"))

# divide predictions by 100 to put on 0-1 scale
DF$prediction <- DF$prediction/100

## inspect each event's distribution of initial predictions ----

# none are very skewed
p0 <- DF %>%
  filter(stage_num == 1) %>% # want to look at pre-influence estimates
  ggplot(aes(x=prediction))+
  geom_density(alpha=0.4, fill = "gray")+
  facet_wrap(event_id~., nrow = 2)+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x = "Prediction", y = "Density")

# png("fig1.png", res = 500, width = 14, height = 8, units = "cm")
p0
# dev.off()

## calculate var, ces/cae/csre, aies/aiae/aisre and contrasts ----

# split out results to get pre-influence (stage 1) and post-influence (stage 5) dfs
res_pre <- DF %>% filter(stage_num == 1)
res_post <- DF %>% filter(stage_num == 5)

# calculate collective responses, variance, and skewness for pre- and post-influence predictions
res_pre <- res_pre %>%
  group_by(batch, treatment, event_num, stage_num) %>%
  summarise(CR = mean(prediction, na.rm = T), 
            VAR = var(prediction, na.rm = T), 
            group_skew = skewness(prediction, na.rm = T))

res_post <- res_post %>%
  group_by(batch, treatment, event_num, stage_num) %>%
  summarise(CR = mean(prediction, na.rm = T), 
            VAR = var(prediction, na.rm = T))
res_post$group_skew <- res_pre$group_skew

# join event outcomes to collective response data
events$event_num <- as.factor(events$event_num) 
res_pre <- inner_join(res_pre, events, by = "event_num") %>% select(-data.question)
res_post <- inner_join(res_post, events, by = "event_num") %>% select(-data.question)

# calculate collective error squared
res_pre$CES <- with(res_pre, (CR-event_outcome)^2)
res_post$CES <- with(res_post, (CR-event_outcome)^2)

# calculate collective absolute error
res_pre$CAE <- with(res_pre, abs(CR-event_outcome))
res_post$CAE <- with(res_post, abs(CR-event_outcome))

# calculate collective square root error
res_pre$CSRE <- with(res_pre, sqrt(abs(CR-event_outcome)))
res_post$CSRE <- with(res_post, sqrt(abs(CR-event_outcome)))

# calculate subtractive contrasts
res_post$var_contrast <- (res_post$VAR)-(res_pre$VAR)
res_post$ces_contrast <- (res_post$CES)-(res_pre$CES)
res_post$cae_contrast <- (res_post$CAE)-(res_pre$CAE)
res_post$csre_contrast <- (res_post$CSRE)-(res_pre$CSRE)

# add group labels
res_pre$group <- c(rep(1:11, each = 10, times = 4)) %>% as.factor()
res_post$group <- c(rep(1:11, each = 10, times = 4)) %>% as.factor()

# add pre/post-influence labels
res_pre$influence <- "pre"
res_post$influence <- "post"

# calculate individuals error -- squared, absolute, square rooted
DF$ies <- with(DF, (prediction-event_outcome)^2)
DF$iae <- with(DF, abs(prediction-event_outcome))
DF$isre <- with(DF, sqrt(abs(prediction-event_outcome)))

# group and average
ind_error <- DF %>%
  group_by(batch, treatment, event_id, stage_num) %>%
  summarise(aies = mean(ies, na.rm=T),
            aiae = mean(iae, na.rm=T),
            aisre = mean(isre, na.rm=T))

# join 
res_post <- inner_join(res_post, ind_error, by = c("batch", "treatment", "event_id", "stage_num"))
res_pre <- inner_join(res_pre, ind_error, by = c("batch", "treatment", "event_id", "stage_num"))
rm(ind_error)

# calculate individual error contrasts
res_post$aies_contrast <- (res_post$aies)-(res_pre$aies)
res_post$aiae_contrast <- (res_post$aiae)-(res_pre$aiae)
res_post$aisre_contrast <- (res_post$aisre)-(res_pre$aisre)

## mixed effect models ----

# mixed effect model over all events - CES as DV
mod1 <- lmer(CES~treatment+(1|group), res_post)
summary(mod1)
anova(mod1)
plot_model(mod1, type = "pred")

# create a nicer viz of mod1 results
p1 <- afex_plot(mod1, x="treatment", 
                mapping = c("fill", "shape"),
                data_geom = ggpol::geom_boxjitter,
                data_arg = list(
                  width = 0.5, 
                  outlier.intersect = F),
                point_arg = list(size = 2),
                error_arg = list(size = 1, width = 0))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Network condition", y = "Average CES"
       #, subtitle = "Post-Communication CES"
       )+
  scale_x_discrete(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                              "scheduled" = "Scheduled","static" = "Static"))+
  scale_fill_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))
p1

# inspect data from p1
afex_plot(mod1, x="treatment", return = "data")

# mixed effect model over all events - CES contrast as DV
mod2 <- mixed(ces_contrast~treatment + (1|group), res_post)
summary(mod2)
anova(mod2)
#plot_model(mod2, type = "pred")

# check pairwise significance
pairs(emmeans::emmeans(mod2, "treatment"))

# create a nicer viz of the mod2 results
p2 <- afex_plot(mod2, x="treatment",
          mapping = c("fill", "shape"),
          data_geom = ggpol::geom_boxjitter,
          data_arg = list(
            width = 0.5, 
            outlier.intersect = TRUE),
          point_arg = list(size = 2),
          error_arg = list(size = 1, width = 0))+
  geom_hline(yintercept = 0, linetype=2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Network condition", 
       y = "Average change in CES"
       #, subtitle = "(Post-Comm. CES) - (Pre-Comm. CES)"
       )+
  scale_x_discrete(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                              "scheduled" = "Scheduled","static" = "Static"))+
  scale_fill_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))
p2

p3 <- plot_grid(p1, p2, labels = "AUTO")

# save hi res png
#png("mixed-mod.png", res = 500, width = 18, height = 8, units = "cm")
p3
#dev.off()

# inspect data from p2
afex_plot(mod2, x="treatment", return = "data")$data

## collective calibration ----

# function for rounding predictions into calibration bins
# e.g., 0.12 becomes 0.1; 0.99 becomes 0.9; etc.
roundDown <- function(x) {
  floor(x / 0.1) * 0.1
}

# determine bins for each collective prediction, pre- and post-influence
res_post$calib_bin <- lapply(res_post$CR, roundDown) %>% as.numeric()
res_pre$calib_bin <- lapply(res_pre$CR, roundDown) %>% as.numeric()

# calculate and plot callibration lines for pre-influence collective predictions
pre_calibration <- res_pre %>%
  group_by(treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n), n_total = sum(n)) %>%
  as.data.frame()  
pre_calibration$freq <- with(pre_calibration, ifelse(freq==1.00 & event_outcome==0, 0, freq))
temp <- pre_calibration %>% filter(freq==0)
pre_calibration <- pre_calibration %>% filter(event_outcome == 1)
pre_calibration <- rbind(pre_calibration, temp)

p4 <- pre_calibration %>%
  filter(n_total>1) %>%
  ggplot(aes(x=calib_bin, y=freq, color=treatment))+
  geom_abline(intercept = 0, slope = 1, linetype=2, color="darkgray")+
  geom_point(alpha=0.5)+ # , aes(size=n_total))+
  stat_smooth(method = "glm", ## fit logit lines
              method.args = list(family = "quasibinomial"),
              se=F, fullrange=TRUE)+
  scale_color_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"),
                     labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled","static" = "Static"))+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability"
       #,subtitle = "Not averaged (110 obs per treatment)"
       )+
  theme_bw()

# calculate and plot callibration lines for post-influence collective predictions
post_calibration <- res_post %>%
  group_by(treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n), n_total = sum(n)) %>%
  as.data.frame()
  #%>% filter(event_outcome==1)
post_calibration$freq <- with(post_calibration, ifelse(freq==1.00 & event_outcome==0, 0, freq))
temp <- post_calibration %>% filter(freq==0)
post_calibration <- post_calibration %>% filter(event_outcome == 1)
post_calibration <- rbind(post_calibration, temp)

p5 <- post_calibration %>%
  filter(n_total>1) %>%
  ggplot(aes(x=calib_bin, y=freq, color=treatment))+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  geom_point(alpha=0.5)+ # , aes(size=n_total))+
  stat_smooth(method = "glm", ## fit logit lines
              method.args = list(family = "quasibinomial"),
              formula = y ~ x,
              se=F, fullrange=TRUE)+
  scale_size_continuous(limits=c(2,14),breaks=c(2,6,10,14))+
  scale_color_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"),
                     labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled","static" = "Static"))+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability", color = "Network condition"
       #, subtitle = " "
       )+
  theme_bw()

# create panel
leg <- get_legend(p5+theme(legend.position = "bottom",
                           legend.title = element_text(size = 8), 
                           legend.text = element_text(size = 8)))
prow <- plot_grid(p4+theme(legend.position = "none"), 
                  p5+theme(legend.position = "none"),
                  ncol = 2,
                  labels = "AUTO")
p6 <- plot_grid(prow, leg, ncol = 1, rel_heights = c(1, .1))

# save hi res png
png("collective-calibration.png", res = 500, width = 18, height = 10, units = "cm")
p6
dev.off()

## individual-level calibration ----

# function for rounding predictions into calibration bins
# e.g., 0.12 becomes 0.1; 0.99 becomes 0.9; etc.
roundDown <- function(x) {
  floor(x / 0.1) * 0.1
}

# set new DFs to work with
calib_df_post <- DF %>% filter(stage_num == 5)
calib_df_pre <- DF %>% filter(stage_num == 1)

# determine bins for each collective prediction, pre- and post-influence
calib_df_post$calib_bin <- lapply(calib_df_post$prediction, roundDown) %>% as.numeric()
calib_df_pre$calib_bin <- lapply(calib_df_pre$prediction, roundDown) %>% as.numeric()

# calculate calibration for pre-communication predictions
pre_calibration <- calib_df_pre %>%
  group_by(playerId, treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n)) %>%
  as.data.frame()  
pre_calibration$freq <- with(pre_calibration, ifelse(freq==1.00 & event_outcome==0, 0, freq))
temp <- pre_calibration %>% filter(freq==0)
pre_calibration <- pre_calibration %>% filter(event_outcome == 1)
pre_calibration <- rbind(pre_calibration, temp)

# " " " for post-communication predictions
post_calibration <- calib_df_post %>%
  group_by(playerId, treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n)) %>%
  as.data.frame()  
post_calibration$freq <- with(post_calibration, ifelse(freq==1.00 & event_outcome==0, 0, freq))
temp <- post_calibration %>% filter(freq==0)
post_calibration <- post_calibration %>% filter(event_outcome == 1)
post_calibration <- rbind(post_calibration, temp)

## individual-level calibration lines
# ...uninterpretable due to sparse data 
# ...bc each individual only predicted 10 events (of which 4 occured)

# pre-communication
p7 <- pre_calibration %>%
  #filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq))+
  geom_abline(intercept = 0, slope = 1, linetype=2, color="darkgray")+
  geom_line(aes(color=playerId, group=playerId), method="lm",stat = "smooth", se=F, alpha = 0.1, show.legend = F)+
  geom_line(aes(group=treatment), method="lm",stat = "smooth", se=F, alpha = 1, size = 1, show.legend = F, color = "black")+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability"
       #,title = "Pre-Communication"
  )+
  facet_wrap(treatment~.)+
  theme_bw()

# post-communication
p8 <- post_calibration %>%
  #filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq))+
  geom_abline(intercept = 0, slope = 1, linetype=2, color="darkgray")+
  geom_line(aes(color=playerId, group=playerId), method="lm",stat = "smooth", se=F, alpha = 0.1, show.legend = F)+
  geom_line(aes(group=treatment), method="lm",stat = "smooth", se=F, alpha = 1, size = 1, show.legend = F, color = "black")+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability"
       #,title = "Post-Communication"
  )+
  facet_wrap(treatment~.)+
  theme_bw()

# create panel
p9 <- plot_grid(p7+theme(legend.position = "none"), 
                p8+theme(legend.position = "none"),
                ncol = 2,
                labels = "AUTO")

# save hi res png
png("individual-calibration-2.png", res = 500, width = 22, height = 12, units = "cm")
p9
dev.off()

## heat maps ... trying to interpret individual-level calibration
p10 <- pre_calibration %>%
  #filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq))+
  geom_hex(bins=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2, size=1, color="red")+
  geom_vline(xintercept = 0.5, linetype=2, size=1, color="red")+
  geom_hline(yintercept = 0.5, linetype=2, size=1, color="red")+
  scale_fill_continuous(type = "viridis") +
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability",
       fill = "Observations"
       #,title = "Pre-Communication"
  )+
  facet_wrap(treatment~.)+
  theme_bw()

p11 <- post_calibration %>%
  #filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq))+
  geom_hex(bins=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2, size=1, color="red")+
  geom_vline(xintercept = 0.5, linetype=2, size=1, color="red")+
  geom_hline(yintercept = 0.5, linetype=2, size=1, color="red")+
  scale_fill_continuous(type = "viridis") +
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective probability \n(collective prediction)", y="Objective probability",
       fill = "Observations"
       #,title = "Pre-Communication"
  )+
  facet_wrap(treatment~.)+
  theme_bw()

# create panel
leg <- get_legend(p10+theme(legend.position = "bottom",
                           legend.title = element_text(size = 8), 
                           legend.text = element_text(size = 8)))
prow <- plot_grid(p10+theme(legend.position = "none"), 
                  p11+theme(legend.position = "none"),
                  ncol = 2,
                  labels = "AUTO")
p12 <- plot_grid(prow, leg, ncol = 1, rel_heights = c(1, .1))
png("individual-calibration-heatmap.png", res = 500, width = 22, height = 12, units = "cm")
p12
dev.off()


## extremeness ----
ext_df <- DF %>% filter(stage_num == 1 | stage_num == 5)
ext_df$over_0.5 <- with(ext_df, ifelse(prediction > 0.5, 1, 0))
ext_df$extremity <- with(ext_df, prediction-0.5)

## change in individual-level extremity
ext_df <- ext_df %>%
  group_by(treatment, event_num, playerId, stage_num)%>%
  summarise(event_outcome, prediction, over_0.5, extremity)

ext_df_1 <- ext_df %>% filter(stage_num == 1)
ext_df_5 <- ext_df %>% filter(stage_num == 5)

ext_df_5$prediction_initial <- ext_df_1$prediction
ext_df_5$extremity_initial <- ext_df_1$extremity
ext_df_5$more_extreme <- with(ext_df_5, 
                              ifelse(extremity>0 & extremity_initial>=0 & extremity_initial<extremity, 1, 
                                     ifelse(extremity<0 & extremity_initial<=0 & extremity_initial>extremity, 1, 0)))

ext_df_5$extremity_change_binary <- with(ext_df_5, ifelse(extremity_change > 0, 1,
                                                          ifelse(extremity_change < 0, 0, NA)))

ext_df_5 %>%
  group_by(treatment) %>%
  summarise(n_obs = n(),
            more_extreme = sum(more_extreme, na.rm = T),
            prop_more_ext = (sum(more_extreme, na.rm = T)/n()))%>%
  as.data.frame()

## change in collective-level extremity
col_ext_df <- res_post 
col_ext_df$CR_initial <- res_pre$CR 

col_ext_df$more_extreme <- with(col_ext_df, 
                                ifelse(CR>0.5 & CR_initial>=0.5 & CR_initial<CR, 1,
                                       ifelse(CR<0.5 & CR_initial<=0.5 & CR_initial>CR, 1, 0)))
col_ext_df %>%
  group_by(treatment) %>%
  summarise(n_obs = n(),
            more_extreme = sum(more_extreme, na.rm = T),
            prop_more_ext = (sum(more_extreme, na.rm = T)/n()))%>%
  as.data.frame()

