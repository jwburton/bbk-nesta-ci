## script for analysis of experimental data

## install + load packages ----
pkgs <- c("tidyverse", "lme4", "lmerTest", "plotrix", "reshape2",
          "sjPlot", "cowplot", "e1071", "readxl", "afex", "prediction")

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
#afex_plot(mod1, x="treatment", return = "data")

# mixed effect model over all events - CES contrast as DV
mod2 <- lmer(ces_contrast~treatment + (1|group), res_post)
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

# inspect data from p2
#afex_plot(mod2, x="treatment", return = "data")

p3 <- plot_grid(p1, p2, labels = "AUTO")

# save hi res png
png("mixed-mod.png", res = 500, width = 18, height = 8, units = "cm")
p3
dev.off()


## robustness checks/alternative models ----

# re-fit mod2 with events/stimuli as random factor
mod2.1 <- lmer(ces_contrast~treatment + (1|group) + (1|event_num), res_post)
summary(mod2.1)
anova(mod2.1)

# re-fit mod2 with other error functions
mod2.2 <- lmer(cae_contrast~treatment + (1|group), res_post) #absolute error
summary(mod2.2)
anova(mod2.2)

mod2.3 <- lmer(csre_contrast~treatment + (1|group), res_post) #square root error
summary(mod2.3)
anova(mod2.3)

# re-fit mod2 as anova
mod2_anova <- aov(ces_contrast~treatment, data=res_post)
summary(mod2_anova)

# re-fit mod2.1 as anova
mod2.1_anova <- aov(ces_contrast~treatment + event_num, data=res_post)
summary(mod2.1_anova)
