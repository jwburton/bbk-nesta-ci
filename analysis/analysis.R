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
                            "numeric", "factor","factor", 
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

## calculate var, ces, aies and contrasts ----

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

# calculate subtractive var and ces contrast
res_post$ces_contrast <- (res_post$CES)-(res_pre$CES)
res_post$var_contrast <- (res_post$VAR)-(res_pre$VAR)

# add group labels
res_pre$group <- c(rep(1:11, each = 10, times = 4)) %>% as.factor()
res_post$group <- c(rep(1:11, each = 10, times = 4)) %>% as.factor()

# add pre/post-influence labels
res_pre$influence <- "pre"
res_post$influence <- "post"

# calculate individuals' error squared
DF$ies <- with(DF, (prediction-event_outcome)^2)

# group and average to get aies
aies <- DF %>%
  group_by(batch, treatment, event_id, stage_num) %>%
  summarise(aies = mean(ies, na.rm=T))

# join 
res_post <- inner_join(res_post, aies, by = c("batch", "treatment", "event_id", "stage_num"))
res_pre <- inner_join(res_pre, aies, by = c("batch", "treatment", "event_id", "stage_num"))
rm(aies)

# calculate aies contrasts
res_post$aies_contrast <- (res_post$aies)-(res_pre$aies)

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
                point_arg = list(size = 4),
                error_arg = list(size = 1.5, width = 0))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Network Treatment", y = "Average CES")+
  scale_x_discrete(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                              "scheduled" = "Scheduled","static" = "Static"))+
  scale_fill_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))
p1

# inspect data from p0
afex_plot(mod1, x="treatment", return = "data")

# mixed effect model over all events - CES contrast as DV
mod2 <- lmer(ces_contrast~treatment+(1|group), res_post)
summary(mod2)
anova(mod2)
plot_model(mod2, type = "pred")

# create a nicer viz of the mod2 results
p2 <- afex_plot(mod2, x="treatment",
          mapping = c("fill", "shape"),
          data_geom = ggpol::geom_boxjitter,
          data_arg = list(
            width = 0.5, 
            outlier.intersect = TRUE),
          point_arg = list(size = 4),
          error_arg = list(size = 1.5, width = 0))+
  geom_hline(yintercept = 0, linetype=2)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Network Treatment", y = "Average Change in CES")+
  scale_x_discrete(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                              "scheduled" = "Scheduled","static" = "Static"))+
  scale_fill_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))
p2

p3 <- plot_grid(p1, p2, labels = "AUTO")

# save hi res png
# png("fig2.png", res = 500, width = 18, height = 8, units = "cm")
p3
# dev.off()

# inspect data from p1
afex_plot(mod2, x="treatment", return = "data")

## collective calibration ----

# function for rounding predictions into calibration bins
# e.g., 0.12 becomes 0.1; 0.99 becomes 0.9; etc.
roundDown <- function(x) {
  ceiling(max(x)/.1)*.1-.1
  }

# determine bins for each collective prediction, pre- and post-influence
res_post$calib_bin <- lapply(res_post$CR, roundDown) %>% as.numeric()
res_pre$calib_bin <- lapply(res_pre$CR, roundDown) %>% as.numeric()

# calculate and plot callibration lines for pre-influence collective predictions
pre_calibration <- res_pre %>%
  group_by(treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n)) %>%
  as.data.frame() %>%
  filter(event_outcome == 1 & calib_bin != 0)
p4 <- pre_calibration %>%
  filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq, color=treatment))+
  geom_abline(intercept = 0, slope = 1, linetype=2, color="darkgray")+
  geom_point(alpha=0.5, aes(size=n))+
  geom_smooth(method="lm", se=F)+
  scale_color_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"),
                     labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled","static" = "Static"))+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective Probability", y="Objective Probability")+
  theme_bw()

# calculate and plot callibration lines for post-influence collective predictions
post_calibration <- res_post %>%
  group_by(treatment, calib_bin, event_outcome) %>%
  summarise(n = n()) %>%
  mutate(freq = n/ sum(n)) %>%
  as.data.frame() %>%
  filter(event_outcome == 1 & calib_bin != 0)
p5 <- post_calibration %>%
  filter(n>1) %>%
  ggplot(aes(x=calib_bin, y=freq, color=treatment))+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  geom_point(alpha=0.5, aes(size=n))+
  geom_smooth(method="lm", se=F)+
  scale_size_continuous(limits=c(2,14),breaks=c(2,6,10,14))+
  scale_color_manual(values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"),
                     labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled","static" = "Static"))+
  ylim(0,1)+
  xlim(0,1)+
  labs(x="Subjective Probability", y="Objective Probability", color = "Network Treatment")+
  theme_bw()

# create panel
leg <- get_legend(p5+theme(legend.position = "bottom",
                           legend.title = element_text(size = 8), 
                           legend.text = element_text(size = 8)))
pcol <- plot_grid(p4+theme(legend.position = "none"), 
                  p5+theme(legend.position = "none"),
                  ncol = 2,
                  labels = "AUTO")
p6 <- plot_grid(prow, leg, ncol = 1, rel_heights = c(1, .1))

# save hi res png
# png("fig3.png", res = 500, width = 18, height = 8, units = "cm")
p6
# dev.off()



