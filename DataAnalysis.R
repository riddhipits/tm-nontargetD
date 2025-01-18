# study 1 at ~ln 20; study 2 at ~ln 400; study 3 at  ~ln 800

rm(list = ls()) # clear environment

options(warn=-1) # remove warnings globally

# installs packages this script uses if not already installed; then loads the package
if (!require(plyr)) {install.packages("plyr")}; library(plyr)
if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
if (!require(MASS)) {install.packages("MASS")}; library(MASS)
if (!require(plotrix)) {install.packages("plotrix")}; library(plotrix)
if (!require(tidyr)) {install.packages("tidyr")}; library(tidyr)
if (!require(emmeans)) {install.packages("emmeans")}; library(emmeans)
if (!require(pwr)) {install.packages("pwr")}; library(pwr)
if (!require(clubSandwich)) {install.packages("clubSandwich")}; library(clubSandwich)

# finding .csv files in the wd # CHANGE THIS IF NEW USER
setwd('/Users/riddhi_rjp/Library/CloudStorage/OneDrive-Nexus365/Oxford/Research Projects/Ox_TM/DTrash/1_Data&Analysis/paper/')

# load source files for summarising data - taken from Dejan Draschkow
source("summarySE.R")
source("normDataWithin.R")
source("summarySEwithin.R")

################################################################################
############################## STUDY 1 (TM EXP3) ###############################
################################################################################

# read csv files
study3.dat <- read.csv("study3_data.csv")
study3.elimination.dat <- read.csv("study3_raw_task_4elimination.csv")
study3.demo.dat <- read.csv("study3_raw_demo.csv")

# ##### PRE-PROCESSING #####
with_noD <- 0

if (with_noD == 0) {0
  # recoding conditions
  study3.dat$Content[grepl("Empty", study3.dat$condition_1)] <- "Empty"
  study3.dat$Content[grepl("Single", study3.dat$condition_1)] <- "singleNT"
  study3.dat$Content[grepl("Multiple", study3.dat$condition_1)] <- "multipleNT"
  study3.dat$Contingency[grepl("Positive", study3.dat$condition_1)] <- "Positive"
  study3.dat$Contingency[grepl("Neutral", study3.dat$condition_1)] <- "Zero"
  study3.dat$Contingency[grepl("Negative", study3.dat$condition_1)] <- "Negative"
  study3.dat$Frequency[grepl("0", study3.dat$condition_2)] <- "0"
  study3.dat$Frequency[grepl("8", study3.dat$condition_2)] <- "8"
  study3.dat$Frequency[grepl("24", study3.dat$condition_2)] <- "24"
  study3.dat$Frequency[grepl("72", study3.dat$condition_2)] <- "72"
  
  # Z-SCORE CONTRASTS: (c(0,8,24,72)-mean(c(0,8,24,72))) / sd(c(0,8,24,72))
  study3.dat$contFrequency[study3.dat$Frequency == "0"] <- "-0.80622577" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "8"] <- "-0.55815631" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "24"] <- "-0.06201737" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "72"] <- "1.42639945" # uncomment to set proportional contrast values
  
  study3.dat$Content <- factor(study3.dat$Content, levels = c("Empty", 
                                                              "singleNT", 
                                                              "multipleNT"))
  
  study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                   levels = c("Negative", "Zero", "Positive"))
} else if (with_noD == 1) {
  # recoding conditions
  study3.dat$Content[grepl("Empty", study3.dat$condition_1)] <- "Empty"
  study3.dat$Content[grepl("Single", study3.dat$condition_1)] <- "singleNT"
  study3.dat$Content[grepl("Multiple", study3.dat$condition_1)] <- "multipleNT"
  study3.dat$Content[grepl("0", study3.dat$condition_2)] <- "noD"
  study3.dat$Contingency[grepl("Positive", study3.dat$condition_1)] <- "Positive"
  study3.dat$Contingency[grepl("Neutral", study3.dat$condition_1)] <- "Zero"
  study3.dat$Contingency[grepl("Negative", study3.dat$condition_1)] <- "Negative"
  study3.dat$Frequency[grepl("0", study3.dat$condition_2)] <- "0"
  study3.dat$Frequency[grepl("8", study3.dat$condition_2)] <- "8"
  study3.dat$Frequency[grepl("24", study3.dat$condition_2)] <- "24"
  study3.dat$Frequency[grepl("72", study3.dat$condition_2)] <- "72"
  
  # Z-SCORE CONTRASTS: (c(0,8,24,72)-mean(c(0,8,24,72))) / sd(c(0,8,24,72))
  study3.dat$contFrequency[study3.dat$Frequency == "0"] <- "-0.80622577" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "8"] <- "-0.55815631" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "24"] <- "-0.06201737" # uncomment to set proportional contrast values
  study3.dat$contFrequency[study3.dat$Frequency == "72"] <- "1.42639945" # uncomment to set proportional contrast values
  
  study3.dat$Content <- factor(study3.dat$Content, levels = c("noD", "Empty", 
                                                              "singleNT", 
                                                              "multipleNT"))
  
  study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                   levels = c("Negative", "Zero", "Positive"))
  
} else if (with_noD == 2) {
  # recoding conditions
  study3.dat$Content[grepl("Empty", study3.dat$condition_1)] <- "Empty"
  study3.dat$Content[grepl("Single", study3.dat$condition_1)] <- "singleNT"
  study3.dat$Content[grepl("Multiple", study3.dat$condition_1)] <- "multipleNT"
  study3.dat$Contingency[grepl("Positive", study3.dat$condition_1)] <- "Positive"
  study3.dat$Contingency[grepl("Neutral", study3.dat$condition_1)] <- "Zero"
  study3.dat$Contingency[grepl("Negative", study3.dat$condition_1)] <- "Negative"
  study3.dat$Frequency[grepl("0", study3.dat$condition_2)] <- 0
  study3.dat$Frequency[grepl("8", study3.dat$condition_2)] <- 8
  study3.dat$Frequency[grepl("24", study3.dat$condition_2)] <- 24
  study3.dat$Frequency[grepl("72", study3.dat$condition_2)] <- 72
  
  study3.dat <- study3.dat[!study3.dat$Frequency == 0,]
  
  # Z-SCORE CONTRASTS: (c(8,24,72)-mean(c(8,24,72))) / sd(c(8,24,72))
  study3.dat$contFrequency[study3.dat$Frequency == 8] <- -0.8006408
  study3.dat$contFrequency[study3.dat$Frequency == 24] <- -0.3202563 
  study3.dat$contFrequency[study3.dat$Frequency == 72] <- 1.1208971 
  
  study3.dat$Content <- factor(study3.dat$Content, levels = c("Empty", 
                                                              "singleNT", 
                                                              "multipleNT"))
  
  study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                   levels = c("Negative", "Zero", "Positive"))
}

study3.dat$PID <- as.factor(study3.dat$PID)
study3.dat$ratings <- as.numeric(study3.dat$ratings)
study3.dat$Frequency <- as.numeric(study3.dat$Frequency)
study3.dat$contFrequency <- as.numeric(study3.dat$contFrequency)
study3.dat$condOrder <- as.factor(study3.dat$condOrder)


study3.dat$ESMfirst <- NULL
for (i in 1:nrow(study3.dat)) {
  if (substring(study3.dat$condOrder[i], 1, 1) == "E") {
    study3.dat$ESMfirst[i] <- "Empty First"
  } else if (substring(study3.dat$condOrder[i], 1, 1) == "S") {
    study3.dat$ESMfirst[i] <- "Single First"
  } else {
    study3.dat$ESMfirst[i] <- "Multiple First"
  }
}
study3.dat$ESMfirst <- as.factor(study3.dat$ESMfirst)
study3.dat$contFrequency <- as.numeric(study3.dat$contFrequency)

study3.dat$PID <- as.factor(study3.dat$PID)
participants <- as.numeric(levels(unique(study3.dat$PID)))
nSubj <- length(participants)

# function to check whether responses were the same 90% of the time
check_percent_same <- function(responses) {
  unique_counts <- table(responses)
  max_count <- max(unique_counts)
  total_responses <- length(responses)
  return(max_count / total_responses >= 0.90)
}

# removing participants who gave 90% same ratings across all conditions
bad_pts <- c()

for (p in 1:length(participants)) {
  # extracting participants' responses
  temp_ratings <- study3.dat[study3.dat$PID == participants[p], "ratings"]
  
  # checking via function
  percent_same_check <- check_percent_same(temp_ratings)
  
  if (percent_same_check) {
    print(paste(participants[p], "bad"))
    bad_pts <- c(bad_pts, participants[p])
    study3.dat <- study3.dat[!study3.dat$PID == participants[p],]
  } else {
    # print(paste(participants[p], "good"))
  }
}

# recalculating number of participants
study3.dat$PID <- as.factor(study3.dat$PID)
study3.dat$PID <- droplevels(study3.dat$PID)
participants <- as.numeric(levels(unique(study3.dat$PID)))
nSubj <- length(participants)

# cleaning up 
study3.dat$X <- study3.dat$condition_1 <- study3.dat$Participant.Status <- study3.dat$condOrder <- study3.dat$Zone.Type <- study3.dat$condition_2 <- NULL

##### DEMOGRAPHICS #####

# defining the relevant columns
demo_cols <- c("Participant.Private.ID","Participant.Status","Question.Key", "Response")

# extracting the relevant columns and removing unwanted rows
study3.demo.dat <- study3.demo.dat[!study3.demo.dat$Question.Key == "BEGIN QUESTIONNAIRE" & !study3.demo.dat$Question.Key == "END QUESTIONNAIRE" & !study3.demo.dat$Question.Key == "conf_read_consent" & !study3.demo.dat$Question.Key == "resp-sex-quantised" & !is.na(study3.demo.dat$Participant.Private.ID) & study3.demo.dat$Participant.Status == "complete", demo_cols]

# removing participants who provided 90% the same responses
for (id_iterator in 1:length(bad_pts)) { 
  study3.demo.dat <- study3.demo.dat[study3.demo.dat$Participant.Private.ID != bad_pts[id_iterator], ]
}

# renaming columns and removing participant status column 
study3.demo.dat$Participant.Status <- NULL
study3.demo.dat <- study3.demo.dat %>% rename(PID = Participant.Private.ID, Question = Question.Key)

# adding columns and filling them out based on Response
study3.demo.dat$Sex <- ifelse(study3.demo.dat$Response == "Male", 1, 2)  
study3.demo.dat_Age <- study3.demo.dat$Response[study3.demo.dat$Question == "Age"]
study3.demo.dat <- study3.demo.dat[study3.demo.dat$Question == "Gender",]
study3.demo.dat$Age <- study3.demo.dat_Age 
study3.demo.dat$Age <- as.numeric(study3.demo.dat$Age)
study3.demo.dat$PID <- as.factor(study3.demo.dat$PID)

# creating empty dataframe for demographic information
study3.demo.sumdat <- data.frame(matrix(nrow = 5, ncol = 2))
study3.demo.sumdat <- study3.demo.sumdat %>% rename(Property = X1, Value = X2)
study3.demo.sumdat[,1] <- c("N", "num_M", "num_F", "mean_Age", "sd_Age") 

# filling in demographic information
study3.demo.sumdat$Value[study3.demo.sumdat$Property == "N"] <- length(unique(study3.demo.dat$PID))
sexDist <- table(study3.demo.dat$Sex)
study3.demo.sumdat$Value[study3.demo.sumdat$Property == "num_M"] <- sexDist[names(sexDist) == 1]
study3.demo.sumdat$Value[study3.demo.sumdat$Property == "num_F"] <- sexDist[names(sexDist) == 2]
study3.demo.sumdat$Value[study3.demo.sumdat$Property == "mean_Age"] <- mean(study3.demo.dat$Age)
study3.demo.sumdat$Value[study3.demo.sumdat$Property == "sd_Age"] <- sd(as.integer(study3.demo.dat$Age))

print(study3.demo.sumdat)

##### DATA ANALYSIS #####

control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
lme_full_study3 <- lmer(ratings ~ 1 + Content*contFrequency*Contingency*ESMfirst + (1 | PID),
                 verbose = FALSE, REML = FALSE, 
                 data = study3.dat, control = control)

summary(lme_full_study3)

lme_step <- step(lme_full_study3)
lme_step

study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                 levels = c("Negative", "Zero", "Positive"))

study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                 levels = c("Zero", "Negative", "Positive"))

lme_step_result_model <- lmer(ratings ~ Content + contFrequency + Contingency + ESMfirst + (1 | PID) + contFrequency:Contingency + Content:ESMfirst + contFrequency:ESMfirst + Contingency:ESMfirst + contFrequency:Contingency:ESMfirst,
                              data = study3.dat, REML = FALSE)

summary(lme_step_result_model)

descriptive_stats <- study3.dat %>%
  group_by(Contingency) %>%
  summarise(
    M = mean(ratings, na.rm = TRUE),
    SD = sd(ratings, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats)

### nested model comparison
no_interaction_model <- lmer(ratings ~ Content + contFrequency + Contingency + ESMfirst + (1 | PID),
                             data = study3.dat, REML = FALSE)

no_content_model <- lmer(ratings ~ contFrequency + Contingency + ESMfirst + (1 | PID) + Contingency:ESMfirst,
                         data = study3.dat, REML = FALSE)

no_contingency_model <- lmer(ratings ~ Content + contFrequency + ESMfirst + (1 | PID) + Content:ESMfirst,
                             data = study3.dat, REML = FALSE)

no_frequency_model <- lmer(ratings ~ Content + Contingency + ESMfirst + (1 | PID) + Content:ESMfirst,
                             data = study3.dat, REML = FALSE)

null_model <- lmer(ratings ~ 1 + (1 | PID), data = study3.dat, REML = FALSE)

anova(lme_full_study3, lme_step_result_model, no_interaction_model,
      no_content_model, no_contingency_model, no_frequency_model, null_model)


### model assumptions
qqnorm(resid(lme_step_result_model))
qqline(resid(lme_step_result_model))

hist(resid(lme_step_result_model), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

### homoscedasticity
plot_data <- data.frame(Fitted = fitted(lme_step_result_model), 
                        Residuals = resid(lme_step_result_model))
(study3_plot_homosce <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "loess", col = "blue", se = FALSE) +  
    labs(x = "Fitted values", y = "Residuals") + 
    theme_minimal())

### robust SE
study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                 levels = c("Negative", "Zero", "Positive"))

study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                 levels = c("Zero", "Negative", "Positive"))
lme_step_result_model <- lmer(ratings ~ Content + contFrequency + Contingency + ESMfirst + (1 | PID) + contFrequency:Contingency + Content:ESMfirst + contFrequency:ESMfirst + Contingency:ESMfirst + contFrequency:Contingency:ESMfirst,
                              data = study3.dat, REML = FALSE)

study3_robust_se <- coef_test(lme_step_result_model, vcov = "CR2", cluster = study3.dat$PID)
study3_robust_se$p_Satt <- round(study3_robust_se$p_Satt, 3)
View(study3_robust_se)

### pairwise comparisons
study3.dat$Frequency <- as.character(study3.dat$Frequency)
study3.dat$Frequency <- factor(study3.dat$Frequency, levels = c("0" , "8", "24", "72"))

study3.dat$Contingency <- factor(study3.dat$Contingency, 
                                 levels = c("Positive", "Zero", "Negative"))

lme_step_result_model_factorised <- lmer(ratings ~ Content + Frequency + Contingency + ESMfirst + (1 | PID) + Frequency:Contingency + Content:ESMfirst + Frequency:ESMfirst + Contingency:ESMfirst + Frequency:Contingency:ESMfirst,
                              data = study3.dat, REML = FALSE)

emms_Contingency <- emmeans(lme_step_result_model_factorised, pairwise ~ Contingency | ESMfirst)
summary(emms_Contingency)

emms_content <- emmeans(lme_step_result_model_factorised, pairwise ~ Content | ESMfirst)
summary(emms_content)

emms_freq <- emmeans(lme_step_result_model_factorised, pairwise ~ Frequency + Contingency | ESMfirst)
summary(emms_freq)

##### VISUALISATIONS #####

study3.dat$ESMfirst <- factor(study3.dat$ESMfirst, 
                              levels = c("Empty First", 
                                         "Single First",
                                         "Multiple First"))

study3.contingency.ESM <- study3.dat %>% 
  group_by(Contingency, ESMfirst) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study3_contingency_plot <- ggplot(study3.contingency.ESM, 
                                  aes(x = Contingency,
                                      y = avgRatings, 
                                      group = ESMfirst, 
                                      color = ESMfirst, 
                                      shape = ESMfirst)) +
  geom_line(aes(linetype = ESMfirst), size = 1) + 
  geom_point(aes(color = ESMfirst), size = 4)  +  
  labs(title = "Study 3", x = "Baseline Contingency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Block Order", 
                     values=c('#000000', '#646568', '#B6B7B9', "#B52003"),
                     labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  scale_linetype_manual(name = "Block Order", 
                        values=c('solid', 'longdash', 'dotted', "solid"), 
                        labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  scale_shape_manual(name = "Block Order", 
                     values=c(16, 15, 17, 8), 
                     labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study3_contingency_plot

study3.contingency.overall <- study3.dat %>% 
  group_by(Contingency) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study3_contingency_plot <- study3_contingency_plot +
  geom_point(data = study3.contingency.overall, 
             aes(x = Contingency, 
                 y = avgRatings,
                 color = "Average Rating",
                 shape = "Average Rating"),
             size = 5, 
             alpha = 0.8,
             inherit.aes = FALSE) + 
  geom_errorbar(data = study3.contingency.overall, 
                aes(x = Contingency, 
                    ymin = avgRatings - seRatings, 
                    ymax = avgRatings + seRatings,
                    color = "Average Rating"),
                width = 0.2, 
                alpha = 0.8,
                inherit.aes = FALSE) + 
  geom_line(data = study3.contingency.overall, 
            aes(x = Contingency, 
                y = avgRatings,
                group = 1,
                color = "Average Rating",
                linetype = "Average Rating"),
            size = 1, 
            alpha = 0.8,
            inherit.aes = FALSE)

study3_contingency_plot

ggsave("Study3_Fig1_Contingency.png", plot = study3_contingency_plot, width = 15, height = 10, units = "cm")

#  plot for contingency and frequency

study3.dat$ESMfirst <- factor(study3.dat$ESMfirst, 
                              levels = c("Empty First", 
                                         "Single First",
                                         "Multiple First"))

study3.contingency.freq.ESM <- study3.dat %>% 
  group_by(Frequency, Contingency, ESMfirst) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study3_contingencyfreq_plot <- ggplot(study3.contingency.freq.ESM, 
                                  aes(x = Frequency,
                                      y = avgRatings, 
                                      group = ESMfirst, 
                                      color = ESMfirst, 
                                      shape = ESMfirst)) +
  geom_line(aes(linetype = ESMfirst), size = 1) + 
  geom_point(aes(color = ESMfirst), size = 4)  +  
  labs(title = "Study 3", x = "D Trial Frequency", 
       y = "Mean Rating") + 
  scale_x_continuous(breaks = c(0, 16, 216)) +
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Block Order", 
                     values=c('#000000', '#646568', '#B6B7B9'),
                     labels=c("Empty First", "singleNT First", "multipleNT First")) +
  scale_linetype_manual(name = "Block Order", 
                        values=c('solid', 'longdash', 'dotted'), 
                        labels=c("Empty First", "singleNT First", "multipleNT First")) +
  scale_shape_manual(name = "Block Order", 
                     values=c(16, 15, 17), 
                     labels=c("Empty First", "singleNT First", "multipleNT First")) +
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  facet_grid(.~Contingency)

study3_contingencyfreq_plot

ggsave("Study3_Fig2_ContingencyFreq.png", plot = study3_contingencyfreq_plot, width = 15, height = 10, units = "cm")

# plot for content and block order
study3.content.ESM <- study3.dat %>% 
  group_by(Content, ESMfirst) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings),
    .groups = "drop"
  )

study3_content_plot <- ggplot(study3.content.ESM, 
                              aes(x = Content,
                                  y = avgRatings, 
                                  group = ESMfirst, 
                                  color = ESMfirst, 
                                  shape = ESMfirst)) +
  geom_line(aes(linetype = ESMfirst), size = 1) + 
  geom_point(size = 4)  +  
  labs(title = "Study 3", x = "D Trial Content", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Block Order", 
                     values=c('#000000', '#646568', '#B6B7B9', "#B52003"),
                     labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  scale_linetype_manual(name = "Block Order", 
                        values=c('solid', 'longdash', 'dotted', "solid"), 
                        labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  scale_shape_manual(name = "Block Order", 
                     values=c(16, 15, 17, 8), 
                     labels=c("Empty First", "singleNT First", "multipleNT First", "Average Rating")) +
  geom_errorbar(aes(ymin = avgRatings - seRatings, ymax = avgRatings + seRatings), 
                width = 0.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study3.content.overall <- study3.dat %>%
  group_by(Content) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings),
    seRatings = std.error(ratings),
    .groups = "drop"
  )


study3_content_plot <- study3_content_plot +
  geom_point(data = study3.content.overall, 
             aes(x = Content, 
                 y = avgRatings,
                 color = "Average Rating",
                 shape = "Average Rating"),
             size = 5, 
             alpha = 0.8,
             inherit.aes = FALSE) + 
  geom_errorbar(data = study3.content.overall, 
                aes(x = Content, 
                    ymin = avgRatings - seRatings, 
                    ymax = avgRatings + seRatings,
                    color = "Average Rating"),
                width = 0.2, 
                alpha = 0.8,
                inherit.aes = FALSE) + 
  geom_line(data = study3.content.overall, 
            aes(x = Content, 
                y = avgRatings,
                group = 1,
                color = "Average Rating",
                linetype = "Average Rating"),
            size = 1, 
            alpha = 0.8,
            inherit.aes = FALSE)

study3_content_plot

ggsave("Study3_Fig3_Content.png", plot = study3_content_plot, width = 15, height = 10, units = "cm")

# cleaning
study3_vars <- grep("study3", ls(), value = TRUE)
rm(list = study3_vars)
################################################################################
############################## STUDY 2 (TM EXP7) ###############################
################################################################################

# loading raw data (csv file)
study7.demo.dat <- read.csv("study7_raw_demo.csv")
study7.dat <- read.csv("study7_data.csv")
study7.elimination.dat <- read.csv("study7_raw_task_4elimination.csv")

##### PRE-PROCESSING #####

sensitivity_analysis <- 0

if (sensitivity_analysis == 0) {
  # recoding conditions
  study7.dat$Content[grepl("empty", study7.dat$condition_2)] <- "Empty"
  study7.dat$Content[grepl("single", study7.dat$condition_2)] <- "singleNT"
  study7.dat$Frequency[grepl("0", study7.dat$condition_2)] <- "0"
  study7.dat$Frequency[grepl("16", study7.dat$condition_2)] <- "16"
  study7.dat$Frequency[grepl("216", study7.dat$condition_2)] <- "216"
  study7.dat$Contingency[grepl("pos", study7.dat$condition_2)] <- "Positive"
  study7.dat$Contingency[grepl("neg", study7.dat$condition_2)] <- "Negative"
  study7.dat$Contingency[grepl("zero", study7.dat$condition_2)] <- "Zero"
  
  # Z-SCORE CONTRASTS: (c(0, 16, 216)-mean(c(0, 16, 216))) / sd(c(0, 16, 216))
  study7.dat$contFrequency[study7.dat$Frequency == "0"] <- "-0.6425434" 
  study7.dat$contFrequency[study7.dat$Frequency == "16"] <- "-0.5096034" 
  study7.dat$contFrequency[study7.dat$Frequency == "216"] <- "1.1521468" 
  
  study7.dat$Content <- factor(study7.dat$Content, levels = c("Empty", "singleNT"))
  
  study7.dat$Contingency <- factor(study7.dat$Contingency, 
                                   levels = c("Negative",
                                              "Zero",
                                              "Positive"))
  
} else if (sensitivity_analysis == 1) {
  # recoding conditions
  study7.dat$Content[grepl("empty", study7.dat$condition_2)] <- "Empty"
  study7.dat$Content[grepl("single", study7.dat$condition_2)] <- "singleNT"
  study7.dat$Content[grepl("0", study7.dat$condition_2)] <- "noD"
  study7.dat$Frequency[grepl("0", study7.dat$condition_2)] <- "0"
  study7.dat$Frequency[grepl("16", study7.dat$condition_2)] <- "16"
  study7.dat$Frequency[grepl("216", study7.dat$condition_2)] <- "216"
  study7.dat$Contingency[grepl("pos", study7.dat$condition_2)] <- "Positive"
  study7.dat$Contingency[grepl("neg", study7.dat$condition_2)] <- "Negative"
  study7.dat$Contingency[grepl("zero", study7.dat$condition_2)] <- "Zero"
  
  # Z-SCORE CONTRASTS: (c(0, 16, 216)-mean(c(0, 16, 216))) / sd(c(0, 16, 216))
  study7.dat$contFrequency[study7.dat$Frequency == "0"] <- "-0.6425434" 
  study7.dat$contFrequency[study7.dat$Frequency == "16"] <- "-0.5096034" 
  study7.dat$contFrequency[study7.dat$Frequency == "216"] <- "1.1521468" 
  
  study7.dat$Content <- factor(study7.dat$Content, levels = c("noD", "Empty", "singleNT"))
  
  study7.dat$Contingency <- factor(study7.dat$Contingency, 
                                   levels = c("Negative",
                                              "Zero",
                                              "Positive"))
  
} else if (sensitivity_analysis == 2) {
  # recoding conditions
  study7.dat$Content[grepl("empty", study7.dat$condition_2)] <- "Empty"
  study7.dat$Content[grepl("single", study7.dat$condition_2)] <- "singleNT"
  study7.dat$Content[grepl("0", study7.dat$condition_2)] <- "noD"
  study7.dat$Frequency[grepl("0", study7.dat$condition_2)] <- "0"
  study7.dat$Frequency[grepl("16", study7.dat$condition_2)] <- "16"
  study7.dat$Frequency[grepl("216", study7.dat$condition_2)] <- "216"
  study7.dat$Contingency[grepl("pos", study7.dat$condition_2)] <- "Positive"
  study7.dat$Contingency[grepl("neg", study7.dat$condition_2)] <- "Negative"
  study7.dat$Contingency[grepl("zero", study7.dat$condition_2)] <- "Zero"
  
  study7.dat <- study3.dat[!study7.dat$Frequency == 0,]
  
  # Z-SCORE CONTRASTS: (c(0, 16, 216)-mean(c(0, 16, 216))) / sd(c(0, 16, 216))
  study7.dat$contFrequency[study7.dat$Frequency == "16"] <- "-0.7071068" 
  study7.dat$contFrequency[study7.dat$Frequency == "216"] <- "0.7071068" 
  
  study7.dat$Content <- factor(study7.dat$Content, levels = c("Empty", "singleNT"))
  
  study7.dat$Contingency <- factor(study7.dat$Contingency, 
                                   levels = c("Negative",
                                              "Zero",
                                              "Positive"))
} 

study7.dat$PID <- as.factor(study7.dat$PID)
study7.dat$ratings <- as.numeric(study7.dat$ratings)
study7.dat$Frequency <- as.numeric(study7.dat$Frequency)
study7.dat$contFrequency <- as.numeric(study7.dat$contFrequency)
# study7.dat$Contingency <- as.numeric(study7.dat$Contingency)

# adding block orders
study7.dat$emptyfirst <- NA 

for (i in 1:nrow(study7.dat)) {
  if (study7.dat$counterbalance[i] == "CounterbalanceA"){
    study7.dat$emptyfirst[i] <- "Empty First"
  } else {
    study7.dat$emptyfirst[i] <- "singleNT First"
  }
}

study7.dat$emptyfirst <- factor(study7.dat$emptyfirst, 
                                levels = c("Empty First", "singleNT First"))

# get list of all participants
participants <- as.numeric(levels(unique(study7.dat$PID)))
nSubj <- length(participants) # number of participants

# eliminate participants based on criteria
study7.dat <- study7.dat[study7.dat$status == "complete", ]
study7.dat$status <- NULL

# removing participants who gave 90% same ratings across all conditions
bad_pts <- c()

for (p in 1:length(participants)) {
  # extracting participants' responses
  temp_ratings <- study7.dat[study7.dat$PID == participants[p], "ratings"]
  
  # checking via function
  percent_same_check <- check_percent_same(temp_ratings)
  
  if (percent_same_check) {
    print(paste(participants[p], "bad"))
    bad_pts <- c(bad_pts, participants[p])
    study7.dat <- study7.dat[!study7.dat$PID == participants[p],]
  } else {
    # print(paste(participants[p], "good"))
  }
}

study7.dat$PID <- droplevels(study7.dat$PID)
participants <- as.numeric(levels(unique(study7.dat$PID)))
nSubj <- length(participants)

##### DEMOGRAPHICS ######
# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Question.Key", "Response")

# extracting relevant columns and renaming them
study7.demo.dat <- study7.demo.dat[,cols]
colnames(study7.demo.dat)[1] <- "PID"
colnames(study7.demo.dat)[2] <- "status"
colnames(study7.demo.dat)[3] <- "question"
colnames(study7.demo.dat)[4] <- "response"

# eliminate participants who did not complete the whole task 
study7.demo.dat <- study7.demo.dat[study7.demo.dat$status == "complete", ]

# removing unwanted columns
study7.demo.dat$status <- NULL

# removing unwanted rows
study7.demo.dat <- study7.demo.dat[(study7.demo.dat$question == "Age" | study7.demo.dat$question == "Gender"), ]

# changing data structure
study7.demo.dat$PID <- as.factor(study7.demo.dat$PID)

# remove bad participants
bad_pts <- na.omit(bad_pts)
for (id_iterator in 1:length(bad_pts)) { 
  study7.demo.dat <- study7.demo.dat[study7.demo.dat$PID != bad_pts[id_iterator], ]
}

# get list of all participants
demo_pts <- unique(study7.demo.dat$PID)

# adding columns and filling them out based on Response
study7.demo.dat$sex[grepl("Male", study7.demo.dat$response)] <- 1
study7.demo.dat$sex[grepl("Female", study7.demo.dat$response)] <- 2
study7.demo.dat$sex[grepl("Prefer Not to Say", study7.demo.dat$response)] <- 3
study7.demo.dat$sex[grepl("If your gender is not specified in this list, please specify it here:", study7.demo.dat$response)] <- 4

# making an empty dataframe with the number of rows being the same as the number of participants
demo2.dat <- data.frame(matrix(nrow = length(demo_pts), ncol = 3))

# renaming columns in new dataframe
colnames(demo2.dat)[1] <- "pts"
colnames(demo2.dat)[2] <- "sex"
colnames(demo2.dat)[3] <- "age"

# fill in participant ID column
demo2.dat$pts <- demo_pts

# fill in age column
demo2.dat$age <- study7.demo.dat$response[study7.demo.dat$question == "Age"]
demo2.dat$age <- as.numeric(demo2.dat$age) # change data type of age column to numeric

demo2.dat <- demo2.dat[!is.na(demo2.dat$age),]
study7.demo.dat <- study7.demo.dat[!study7.demo.dat$PID == "9209739",] # older than 50
study7.demo.dat <- study7.demo.dat[!study7.demo.dat$PID == "9273859",] # older than 50
study7.dat <- study7.dat[!study7.dat$PID == "9209739",] # older than 50
study7.dat <- study7.dat[!study7.dat$PID == "9273859",] # older than 50

# fill in sex column
demo_sex <- study7.demo.dat$sex # make list from first dataframe
demo_sex <- study7.demo.dat$sex[seq(2, length(study7.demo.dat$sex), 2)]
demo2.dat$sex <- demo_sex # fill in sex column in second dataframe
demo2.dat <- demo2.dat %>% replace_na(list(sex = "3"))

# creating a summary dataframe
summary_demo.dat <- data.frame(matrix(nrow = 7, ncol = 2))
colnames(summary_demo.dat)[1] <- "Property"
colnames(summary_demo.dat)[2] <- "Value"
summary_demo.dat[,1] <- c("N", "num_M", "num_F","num_nosay", "num_other", "mean_Age", "sd_Age") 

# filling in demographic information
summary_demo.dat$Value[summary_demo.dat$Property == "N"] <- length(demo2.dat$pts)
sexDist <- table(demo2.dat$sex)
summary_demo.dat$Value[summary_demo.dat$Property == "num_M"] <- sexDist[names(sexDist) == 1]
summary_demo.dat$Value[summary_demo.dat$Property == "num_F"] <- sexDist[names(sexDist) == 2]
summary_demo.dat$Value[summary_demo.dat$Property == "mean_Age"] <- mean(demo2.dat$age)
summary_demo.dat$Value[summary_demo.dat$Property == "sd_Age"] <- sd(demo2.dat$age)

print(summary_demo.dat)

study7.dat$X <- NULL
study7.dat$counterbalance <- NULL
study7.dat$blockorderA1 <- NULL
study7.dat$blockorderA2 <- NULL
study7.dat$blockorderB1 <- NULL
study7.dat$blockorderB2 <- NULL
study7.dat$condition_1 <- NULL
study7.dat$condition_2 <- NULL
study7.dat$Zone.Type <- NULL

###### DATA ANALYSIS ######

lme_full_study7 <- lmer(ratings ~ 1 + Content*contFrequency*Contingency*emptyfirst + (1 | PID),
                        verbose = FALSE, REML = FALSE, data = study7.dat)
summary(lme_full_study7)

lme_step <- step(lme_full_study7)
lme_step

lme_step_result_model <- lmer(ratings ~ Content + contFrequency + Contingency + emptyfirst + (1 | PID) + Content:Contingency + Content:emptyfirst + Contingency:emptyfirst + Content:Contingency:emptyfirst,
                              data = study7.dat, REML = FALSE)
summary(lme_step_result_model)

descriptive_stats <- study7.dat %>%
  group_by(Contingency) %>%
  summarise(
    M = mean(ratings, na.rm = TRUE),
    SD = sd(ratings, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats)

### nested model comparison
no_interaction_model <- lmer(ratings ~ Content + contFrequency + Contingency + emptyfirst + (1 | PID),
                             data = study7.dat, REML = FALSE)

no_content_model <- lmer(ratings ~ contFrequency + Contingency + emptyfirst + (1 | PID) + Contingency:emptyfirst,
                         data = study7.dat, REML = FALSE)

no_contingency_model <- lmer(ratings ~ Content + contFrequency + emptyfirst + (1 | PID) + Content:emptyfirst,
                             data = study7.dat, REML = FALSE)

no_frequency_model <- lmer(ratings ~ Content + Contingency + emptyfirst + (1 | PID) + Content:emptyfirst,
                           data = study7.dat, REML = FALSE)

null_model <- lmer(ratings ~ 1 + (1 | PID), data = study7.dat, REML = FALSE)

anova(null_model, lme_full_study7, lme_step_result_model, no_interaction_model,
      no_content_model, no_contingency_model, no_frequency_model)


### model assumptions
qqnorm(resid(lme_step_result_model))
qqline(resid(lme_step_result_model))

hist(resid(lme_step_result_model), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

### homoscedasticity
plot_data <- data.frame(Fitted = fitted(lme_step_result_model), 
                        Residuals = resid(lme_step_result_model))
(study7_plot_homosce <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "loess", col = "blue", se = FALSE) +  
    labs(x = "Fitted values", y = "Residuals") + 
    theme_minimal())

### robust SE
study7.dat$Contingency <- factor(study7.dat$Contingency, 
                                 levels = c("Negative", "Zero", "Positive"))

study7.dat$Contingency <- factor(study7.dat$Contingency, 
                                 levels = c("Zero", "Negative", "Positive"))
lme_step_result_model <- lmer(ratings ~ Content + contFrequency + Contingency + emptyfirst + (1 | PID) + Content:Contingency + Content:emptyfirst + Contingency:emptyfirst + Content:Contingency:emptyfirst,
                              data = study7.dat, REML = FALSE)

study7_robust_se <- coef_test(lme_step_result_model, 
                              vcov = "CR2", cluster = study7.dat$PID)
study7_robust_se$p_Satt <- round(study7_robust_se$p_Satt, 3)
View(study7_robust_se)



study7.dat$Frequency <- factor(study7.dat$Frequency, levels = c("216", "16", "0"))

lme_step_result_model_factorised <- lmer(ratings ~ Content + Frequency + Contingency + emptyfirst + (1 | PID) + Content:Contingency + Content:emptyfirst + Contingency:emptyfirst + Content:Contingency:emptyfirst,
                              data = study7.dat, REML = FALSE)

emms_Contingency <- emmeans(lme_step_result_model_factorised, pairwise ~ Contingency | emptyfirst + Content)
summary(emms_Contingency)

emms_Frequency <- emmeans(lme_step_result_model_factorised, pairwise ~ Frequency)
summary(emms_Frequency)


##### VISUALISATIONS #####
study7.dat$Frequency <- as.character(study7.dat$Frequency)
study7.dat$Content <- as.character(study7.dat$Content)
# study7.dat$Content[study7.dat$Frequency == "0"] <- "noD"
study7.dat$Frequency <- factor(study7.dat$Frequency, levels = c("0", "16", "216"))
# study7.dat$Content <- factor(study7.dat$Content, levels = c("noD", "Empty", "singleNT"))
study7.dat$Content <- factor(study7.dat$Content, levels = c("Empty", "singleNT"))

study7.dat$Contingency <- factor(study7.dat$Contingency,
                                 levels = c("Positive",
                                            "Zero",
                                            "Negative"))

study7.contingency.content.block <- study7.dat %>% 
  group_by(Contingency, Content, emptyfirst) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study7_contingency_plot <- ggplot(study7.contingency.content.block, 
                                  aes(x = Contingency,
                                      y = avgRatings, 
                                      group = Content, 
                                      color = Content, 
                                      shape = Content)) +
  geom_line(aes(linetype = Content), size = 1) + 
  geom_point(aes(color = Content), size = 4)  +  
  labs(title = "Study 7", x = "Baseline Contingency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Content", 
                     values=c('#000000', '#646568', '#B6B7B9'),
                     labels=c("Empty", "singleNT", "multipleNT")) +
  scale_linetype_manual(name = "Content", 
                        values=c('solid', 'longdash', 'dotted'), 
                        labels=c("Empty", "singleNT", "multipleNT")) +
  scale_shape_manual(name = "Content", 
                     values=c(16, 15, 17), 
                     labels=c("Empty", "singleNT", "multipleNT")) +
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  facet_grid(.~emptyfirst)

study7_contingency_plot

ggsave("Study7_Fig1_Contingency.png", plot = study7_contingency_plot, width = 17, height = 10, units = "cm")

### frequency plot 
study7.freq <- study7.dat %>% 
  group_by(Frequency) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study7_freq_plot <- ggplot(study7.freq, 
                           aes(x = Frequency,
                               y = avgRatings)) +
  geom_point(size = 4) + 
  geom_line(size = 1) +
  labs(title = "Study 7", x = "D Trial Frequency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  scale_x_continuous(breaks = c(0, 16, 216)) +
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study7_freq_plot

ggsave("Study7_Fig2_Frequency.png", plot = study7_freq_plot, width = 17, height = 10, units = "cm")

### content plot
study7.content <- study7.dat %>% 
  group_by(Content) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study7_content_plot <- ggplot(study7.content,
                              aes(x = Content,
                                      y = avgRatings)) +
  geom_line(size = 1) + 
  geom_point(size = 4)  +  
  labs(title = "Study 7", x = "D Trial Content", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study7_content_plot

ggsave("Study7_Fig3_Content.png", plot = study7_content_plot, width = 17, height = 10, units = "cm")

# cleaning
study7_vars <- grep("study7", ls(), value = TRUE)
rm(list = study7_vars)

################################################################################
############################## STUDY 3 (TM EXP4) ###############################
################################################################################

study4.dat <- read.csv("study4_data.csv")
study4.elimination.dat <- read.csv("study4_raw_task_4elimination.csv")
study4.demo.dat <- rbind(read.csv("study4_raw_demo_1.csv"), read.csv("study4_raw_demo_2.csv"))

# df1 <- read.csv("study4_raw_demo_1.csv")
# df2 <- read.csv("study4_raw_demo_2.csv")
# colnames_df1 <- colnames(df1)
# colnames_df2 <- colnames(df2)
# not_in_df2 <- setdiff(colnames_df1, colnames_df2)
# not_in_df1 <- setdiff(colnames_df2, colnames_df1)
# cat("Columns in df1 but not in df2:\n")
# print(not_in_df2)
# cat("\nColumns in df2 but not in df1:\n")
# print(not_in_df1)

##### PRE-PROCESSING #####

sensitivity_analysis <- 0

if (sensitivity_analysis == 0) {
  # recoding conditions
  study4.dat$Content[grepl("Empty", study4.dat$condition_1)] <- "Empty"
  study4.dat$Content[grepl("Single", study4.dat$condition_1)] <- "singleNT"
  study4.dat$Content[grepl("noD", study4.dat$condition_2)] <- "noD"
  study4.dat$Contingency[grepl("Positive", study4.dat$condition_1)] <- "Positive"
  study4.dat$Contingency[grepl("Negative", study4.dat$condition_1)] <- "Negative"
  study4.dat$Contingency[grepl("positive", study4.dat$condition_2)] <- "Positive"
  study4.dat$Contingency[grepl("negative", study4.dat$condition_2)] <- "Negative"
  study4.dat$Frequency[grepl("16", study4.dat$condition_2)] <- "16"
  study4.dat$Frequency[grepl("72", study4.dat$condition_2)] <- "72"
  study4.dat$Frequency[is.na(study4.dat$Frequency)] <- "0"
  
  # Z-SCORE CONTRASTS: (c(0, 16, 72)-mean(c(0, 16, 72))) / sd(c(0, 16, 72))
  study4.dat$contFrequency[study4.dat$Frequency == "0"] <- "-0.7758802" 
  study4.dat$contFrequency[study4.dat$Frequency == "16"] <- "-0.3526728" 
  study4.dat$contFrequency[study4.dat$Frequency == "72"] <- "1.1285530" 
  
  study4.dat$Content <- factor(study4.dat$Content, levels = c("noD", "Empty", "singleNT"))
  study4.dat$Contingency <- factor(study4.dat$Contingency, levels = c("Negative", "Positive"))
  
} else {
  # recoding conditions
  study4.dat$Content[grepl("Empty", study4.dat$condition_1)] <- "Empty"
  study4.dat$Content[grepl("Single", study4.dat$condition_1)] <- "singleNT"
  study4.dat$Content[grepl("noD", study4.dat$condition_2)] <- "noD"
  study4.dat$Contingency[grepl("Positive", study4.dat$condition_1)] <- "Positive"
  study4.dat$Contingency[grepl("Negative", study4.dat$condition_1)] <- "Negative"
  study4.dat$Contingency[grepl("positive", study4.dat$condition_2)] <- "Positive"
  study4.dat$Contingency[grepl("negative", study4.dat$condition_2)] <- "Negative"
  study4.dat$Frequency[grepl("16", study4.dat$condition_2)] <- "16"
  study4.dat$Frequency[grepl("72", study4.dat$condition_2)] <- "72"
  study4.dat$Frequency[is.na(study4.dat$Frequency)] <- 0
  
  study4.dat <- study4.dat[!study4.dat$Frequency == 0,]
  
  # Z-SCORE CONTRASTS: (c(16, 72)-mean(c(16, 72))) / sd(c(16, 72))
  study4.dat$contFrequency[study4.dat$Frequency == "16"] <- "-0.7071068" 
  study4.dat$contFrequency[study4.dat$Frequency == "72"] <- "0.7071068" 
  
  study4.dat$Content <- factor(study4.dat$Content, levels = c("Empty", "singleNT"))
  study4.dat$Contingency <- factor(study4.dat$Contingency, levels = c("Negative", "Positive"))
  
}

study4.dat$Frequency <- as.numeric(study4.dat$Frequency)
study4.dat$contFrequency <- as.numeric(study4.dat$contFrequency)
study4.dat$PID <- as.factor(study4.dat$PID)
study4.dat$ratings <- as.numeric(study4.dat$ratings)
participants <- unique(study4.dat$PID)
# study4.dat$Contingency <- as.numeric(study4.dat$Contingency)


# removing participants who gave the same ratings across all conditions
bad_pts <- c()

for (p in 1:length(participants)) {
  # extracting participants' responses
  temp_ratings <- study4.dat[study4.dat$PID == participants[p], "ratings"]
  
  # checking via function
  percent_same_check <- check_percent_same(temp_ratings)
  
  if (percent_same_check) {
    print(paste(participants[p], "bad"))
    bad_pts <- c(bad_pts, participants[p])
    study4.dat <- study4.dat[!study4.dat$PID == participants[p],]
  } else {
    # print(paste(participants[p], "good"))
  }
}

study4.dat$PID <- droplevels(study4.dat$PID)
participants <- as.numeric(levels(unique(study4.dat$PID)))
nSubj <- length(participants)


###### DEMOGRAPHICS ######

# defining the relevant columns
demo_cols <- c("Participant.Private.ID","Participant.Status","Question.Key", "Response", "Experiment.ID")

# changing data structure
study4.demo.dat$Participant.Private.ID <- as.factor(study4.demo.dat$Participant.Private.ID)

# extracting the relevant columns and removing unwanted rows
study4.demo.dat <- study4.demo.dat[study4.demo.dat$Question.Key == "Age" | study4.demo.dat$Question.Key == "Gender", demo_cols]

# renaming columns and removing participant status column 
study4.demo.dat$Participant.Status <- NULL
study4.demo.dat <- study4.demo.dat %>% rename(PID = Participant.Private.ID, Question = Question.Key)

# adding columns and filling them out based on Response
study4.demo.dat$Gender[grepl("Male", study4.demo.dat$Response)] <- 1
study4.demo.dat$Gender[grepl("Female", study4.demo.dat$Response)] <- 2
study4.demo.dat$Gender[grepl("Prefer Not to Say", study4.demo.dat$Response)] <- 3
study4.demo.dat$Gender[grepl("If your gender is not specified in this list, please specify it here:", study4.demo.dat$Response)] <- 4

study4.demo.dat$PID <- as.factor(study4.demo.dat$PID)
study4.demo.dat$PID <- droplevels(study4.demo.dat$PID)
participants <- unique(study4.demo.dat$PID)

# making an empty dataframe with the number of rows being the same as the number of participants
study4.demo.dat_2 <- data.frame(matrix(nrow = length(participants), ncol = 4))

# renaming columns in new dataframe
study4.demo.dat_2 <- study4.demo.dat_2 %>% rename(PID = X1,
                                                  Gender = X2, 
                                                  Age = X3, 
                                                  samplegroup = X4)
# fill in participant ID column
study4.demo.dat_2$PID <- as.character(participants)

# fill in age column
study4.demo.dat_2$Age <- na.omit(study4.demo.dat$Response[study4.demo.dat$Question == "Age"])
study4.demo.dat_2$Age <- as.numeric(study4.demo.dat_2$Age) # change data type of age column to numeric

# fill in sex column
study4.demo.gender <- study4.demo.dat$Gender # make list from first dataframe
study4.demo.gender <- na.omit(study4.demo.gender) # remove NAs from that list
study4.demo.dat_2$Gender <- study4.demo.gender # fill in sex column in second dataframe

# fill in sample group 
study4.demo.samplegroup <- na.omit(study4.demo.dat$Experiment.ID[study4.demo.dat$Question == "Age"])
study4.demo.dat_2$samplegroup <- study4.demo.samplegroup

# diving dataframe by sample group 
study4.demo.mturk <- study4.demo.dat_2[study4.demo.dat_2$samplegroup == "82781",]
study4.demo.sona <- study4.demo.dat_2[study4.demo.dat_2$samplegroup == "74297",]

# creating a summary dataframe
study4.demo.sumdat <- data.frame(matrix(nrow = 7, ncol = 2))
study4.demo.sumdat <- study4.demo.sumdat %>% rename(Property = X1, Value = X2)
study4.demo.sumdat[,1] <- c("N", "num_M", "num_F","num_nosay", "num_other", "mean_Age", "sd_Age") 

# filling in demographic information
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "N"] <- length(participants)
sexDist <- table(study4.demo.dat_2$Gender)
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "num_M"] <- sexDist[names(sexDist) == 1]
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "num_F"] <- sexDist[names(sexDist) == 2]
# study4.demo.sumdat$Value[study4.demo.sumdat$Property == "num_nosay"] <- sexDist[names(sexDist) == 3]
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "num_other"] <- sexDist[names(sexDist) == 4]
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "mean_Age"] <- mean(study4.demo.dat_2$Age)
study4.demo.sumdat$Value[study4.demo.sumdat$Property == "sd_Age"] <- sd(study4.demo.dat_2$Age)

print(study4.demo.sumdat)

# creating a summary dataframe for mturk sample group
study4.demo.sumdat.mturk <- data.frame(matrix(nrow = 7, ncol = 2))
study4.demo.sumdat.mturk <- study4.demo.sumdat.mturk %>% rename(Property = X1, Value = X2)
study4.demo.sumdat.mturk[,1] <- c("N", "num_M", "num_F","num_nosay", "num_other", "mean_Age", "sd_Age") 

# filling in demographic information
study4.demo.sumdat.mturk$Value[study4.demo.sumdat.mturk$Property == "N"] <- length(unique(study4.demo.mturk$PID))
sexDist <- table(study4.demo.mturk$Gender)
study4.demo.sumdat.mturk$Value[study4.demo.sumdat.mturk$Property == "num_M"] <- sexDist[names(sexDist) == 1]
study4.demo.sumdat.mturk$Value[study4.demo.sumdat.mturk$Property == "num_F"] <- sexDist[names(sexDist) == 2]
study4.demo.sumdat.mturk$Value[study4.demo.sumdat.mturk$Property == "mean_Age"] <- mean(study4.demo.mturk$Age)
study4.demo.sumdat.mturk$Value[study4.demo.sumdat.mturk$Property == "sd_Age"] <- sd(study4.demo.mturk$Age)

print(study4.demo.sumdat.mturk)

# creating a summary dataframe for sona sample group
study4.demo.sumdat.sona <- data.frame(matrix(nrow = 7, ncol = 2))
study4.demo.sumdat.sona <- study4.demo.sumdat.sona %>% rename(Property = X1, Value = X2)
study4.demo.sumdat.sona[,1] <- c("N", "num_M", "num_F","num_nosay", "num_other", "mean_Age", "sd_Age") 

# filling in demographic information
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "N"] <- length(unique(study4.demo.sona$PID))
sexDist <- table(study4.demo.sona$Gender)
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "num_M"] <- sexDist[names(sexDist) == 1]
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "num_F"] <- sexDist[names(sexDist) == 2]
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "num_other"] <- sexDist[names(sexDist) == 4]
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "mean_Age"] <- mean(study4.demo.sona$Age)
study4.demo.sumdat.sona$Value[study4.demo.sumdat.sona$Property == "sd_Age"] <- sd(study4.demo.sona$Age)

print(study4.demo.sumdat.sona)

###### DATA ANALYSIS #####

# REMOVING ALL SONA SAMPLE DATA
study4.dat <- study4.dat[study4.dat$samplegroup == "82781",]

study4.dat$blockorder <- as.factor(study4.dat$blockorder)
study4.dat$condition_1 <- NULL
study4.dat$condition_2 <- NULL
study4.dat$Zone.Type <- NULL

lme_full_study4 <- lmer(ratings ~ 1 + Content*contFrequency*Contingency*blockorder + (1 | PID),
                 verbose = FALSE, REML = FALSE, data = study4.dat)
summary(lme_full_study4)

lme_step <- step(lme_full_study4)
lme_step

lme_step_result_model <- lmer(ratings ~ contFrequency + Contingency + (1 | PID),
                              data = study4.dat, REML = FALSE)
summary(lme_step_result_model)

descriptive_stats <- study4.dat %>%
  group_by(Contingency) %>%
  summarise(
    M = mean(ratings, na.rm = TRUE),
    SD = sd(ratings, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats)

### nested model comparison
with_content_model <- lmer(ratings ~ contFrequency + Contingency + Content + (1 | PID),
                         data = study4.dat, REML = FALSE)

no_contingency_model <- lmer(ratings ~ contFrequency + (1 | PID),
                             data = study4.dat, REML = FALSE)

no_frequency_model <- lmer(ratings ~ Contingency + (1 | PID),
                           data = study4.dat, REML = FALSE)

null_model <- lmer(ratings ~ 1 + (1 | PID), data = study4.dat, REML = FALSE)

anova(null_model, lme_full_study4, lme_step_result_model, with_content_model,
      no_contingency_model, no_frequency_model)


### model assumptions
qqnorm(resid(lme_step_result_model))
qqline(resid(lme_step_result_model))

hist(resid(lme_step_result_model), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

### homoscedasticity
plot_data <- data.frame(Fitted = fitted(lme_step_result_model), 
                        Residuals = resid(lme_step_result_model))
(study4_plot_homosce <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "loess", col = "blue", se = FALSE) +  
    labs(x = "Fitted values", y = "Residuals") + 
    theme_minimal())

### robust SE
study4_robust_se <- coef_test(lme_step_result_model, 
                              vcov = "CR2", cluster = study4.dat$PID)
study4_robust_se$p_Satt <- round(study4_robust_se$p_Satt, 3)
View(study4_robust_se)



study4.dat$Frequency <- factor(study4.dat$Frequency, levels = c("72", "16", "0"))

lme_step_result_model_factorised <- lmer(ratings ~ Frequency + Contingency + (1 | PID),
                                         data = study4.dat, REML = FALSE)

emms_Contingency <- emmeans(lme_step_result_model_factorised, pairwise ~ Contingency)
summary(emms_Contingency)

emms_Frequency <- emmeans(lme_step_result_model_factorised, pairwise ~ Frequency)
summary(emms_Frequency)


##### VISUALISATIONS #####
study4.dat$Frequency <- as.character(study4.dat$Frequency)
study4.dat$Content <- as.character(study4.dat$Content)
study4.dat$Frequency <- factor(study4.dat$Frequency, levels = c("0", "16", "72"))
study4.dat$Content <- factor(study4.dat$Content, levels = c("Empty", "singleNT"))

study4.dat$Contingency <- factor(study4.dat$Contingency,
                                 levels = c("Positive",
                                            "Negative"))
# contingency plot
study4.contingency <- study4.dat %>% 
  group_by(Contingency) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study4_contingency_plot <- ggplot(study4.contingency, 
                                  aes(x = Contingency,
                                      y = avgRatings)) +
  geom_point(size = 4)  +  
  labs(title = "Study 4", x = "Baseline Contingency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study4_contingency_plot

ggsave("Study4_Fig1_Contingency.png", plot = study4_contingency_plot, width = 14, height = 10, units = "cm")

# content plot
study4.content <- study4.dat %>% 
  group_by(Content) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study4_content_plot <- ggplot(study4.content, 
                           aes(x = Content,
                               y = avgRatings)) +
  geom_point(size = 4)  +  
  labs(title = "Study 4", x = "D Trial Content", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study4_content_plot

ggsave("Study4_Fig2_Content.png", plot = study4_content_plot, width = 14, height = 10, units = "cm")

# frequency plot
study4.freq <- study4.dat %>% 
  group_by(Frequency) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study4_freq_plot <- ggplot(study4.freq, 
                           aes(x = Frequency,
                               y = avgRatings)) +
  geom_line(size = 1) + 
  geom_point(size = 4)  +  
  labs(title = "Study 4", x = "D Trial Frequency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = c(0, 16, 72)) +
  geom_errorbar(aes(ymin=avgRatings-seRatings, ymax=avgRatings+seRatings), width=.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

study4_freq_plot

ggsave("Study4_Fig3_Frequency.png", plot = study4_freq_plot, width = 14, height = 10, units = "cm")



################################################################################
############################## STUDY 4 (TM EXP6) ###############################
################################################################################

# loading raw data (csv file)
study6.demo.dat <- read.csv("study6_raw_demo.csv")
study6.dat <- read.csv("study6_data.csv")
study6.elimination.dat <- read.csv("study6_raw_task_4elimination.csv")

##### PRE-PROCESSING #####

sensitivity_analysis <- 0

if (sensitivity_analysis == 0) {
  # recoding conditions
  study6.dat$Content[grepl("noD", study6.dat$condition_2)] <- "noD"
  study6.dat$Content[grepl("empty", study6.dat$condition_2)] <- "Empty"
  study6.dat$Content[grepl("single", study6.dat$condition_2)] <- "singleNT"
  study6.dat$Frequency[grepl("noD", study6.dat$condition_2)] <- "0"
  study6.dat$Frequency[grepl("16", study6.dat$condition_2)] <- "16"
  study6.dat$Frequency[grepl("216", study6.dat$condition_2)] <- "216"
  
  # Z-SCORE CONTRASTS: (c(0, 16, 216)-mean(c(0, 16, 216))) / sd(c(0, 16, 216))
  study6.dat$contFrequency[study6.dat$Frequency == "0"] <- "-0.6425434" 
  study6.dat$contFrequency[study6.dat$Frequency == "16"] <- "-0.5096034" 
  study6.dat$contFrequency[study6.dat$Frequency == "216"] <- "1.1521468" 
  
  study6.dat$Content <- factor(study6.dat$Content, levels = c("noD", "Empty", "singleNT"))
  
} else {
  # recoding conditions
  study6.dat$Content[grepl("noD", study6.dat$condition_2)] <- "noD"
  study6.dat$Content[grepl("empty", study6.dat$condition_2)] <- "Empty"
  study6.dat$Content[grepl("single", study6.dat$condition_2)] <- "singleNT"
  study6.dat$Frequency[grepl("noD", study6.dat$condition_2)] <- "0"
  study6.dat$Frequency[grepl("16", study6.dat$condition_2)] <- "16"
  study6.dat$Frequency[grepl("216", study6.dat$condition_2)] <- "216"
  
  study6.dat <- study6.dat[!study6.dat$Frequency == 0,]
  
  # Z-SCORE CONTRASTS: (c(16, 216)-mean(c(16, 216))) / sd(c(16, 216))
  study6.dat$contFrequency[study6.dat$Frequency == "16"] <- "-0.7071068" 
  study6.dat$contFrequency[study6.dat$Frequency == "216"] <- "0.7071068"
  
  study6.dat$Content <- factor(study6.dat$Content, levels = c("Empty", "singleNT"))
  
}

study6.dat$Frequency <- as.numeric(study6.dat$Frequency)
study6.dat$contFrequency <- as.numeric(study6.dat$contFrequency)
study6.dat$PID <- as.factor(study6.dat$PID)


# get list of all participants
participants <- as.numeric(levels(unique(study6.dat$PID)))
nSubj <- length(participants) # number of participants

# eliminate participants based on criteria
study6.dat <- study6.dat[study6.dat$status == "complete", ]
study6.dat$status <- NULL

# removing participants who gave the same ratings across all conditions
bad_pts <- c()

for (p in 1:length(participants)) {
  # extracting participants' responses
  temp_ratings <- study6.dat[study6.dat$PID == participants[p], "ratings"]
  
  # checking via function
  percent_same_check <- check_percent_same(temp_ratings)
  
  if (percent_same_check) {
    print(paste(participants[p], "bad"))
    bad_pts <- c(bad_pts, participants[p])
    study6.dat <- study6.dat[!study6.dat$PID == participants[p],]
  } else {
    # print(paste(participants[p], "good"))
  }
}

study6.dat$PID <- droplevels(study6.dat$PID)
participants <- as.numeric(levels(unique(study6.dat$PID)))
nSubj <- length(participants)

# defining block order in each stimulus pool
study6.dat$blockorder1 <- NA
study6.dat$blockorder2 <- NA

study6.dat$blockorder1[!is.na(study6.dat$blockorderA1)] <- study6.dat$blockorderA1[!is.na(study6.dat$blockorderA1)] 

study6.dat$blockorder1[!is.na(study6.dat$blockorderB1)] <- study6.dat$blockorderB1[!is.na(study6.dat$blockorderB1)] 

study6.dat$blockorder2[!is.na(study6.dat$blockorderA2)] <- study6.dat$blockorderA2[!is.na(study6.dat$blockorderA2)] 

study6.dat$blockorder2[!is.na(study6.dat$blockorderB2)] <- study6.dat$blockorderB2[!is.na(study6.dat$blockorderB2)] 

study6.dat$blockorder1 <- as.factor(study6.dat$blockorder1)
study6.dat$blockorder2 <- as.factor(study6.dat$blockorder2)

study6.dat$emptyfirstblock <- NA

for (i in 1:nrow(study6.dat)) {
  if (substring(study6.dat$blockorder1[i], 1, 1) == "B") {
    study6.dat$emptyfirstblock[i] <- "Empty First"
  } else {
    study6.dat$emptyfirstblock[i] <- "singleNT First"
  }
}
study6.dat$emptyfirstblock <- as.factor(study6.dat$emptyfirstblock)

###### DEMOGRAPHICS ######
# extracting all the relevant columns
cols <- c("Participant.Private.ID", "Participant.Status", "Question.Key", "Response")

# extracting relevant columns and renaming them
study6.demo.dat <- study6.demo.dat[,cols]
study6.demo.dat <- study6.demo.dat %>% rename(PID = Participant.Private.ID, 
                                status = Participant.Status,
                                response = Response, 
                                question = Question.Key
)

# eliminate participants who did not complete the whole task 
study6.demo.dat <- study6.demo.dat[study6.demo.dat$status == "complete", ]

# removing unwanted columns
study6.demo.dat$status <- NULL

# removing unwanted rows
study6.demo.dat <- study6.demo.dat[(study6.demo.dat$question == "Age" | study6.demo.dat$question == "Gender"), ]

# changing data structure
study6.demo.dat$PID <- as.factor(study6.demo.dat$PID)

for (id_iterator in 1:length(bad_pts)) { 
  study6.demo.dat <- study6.demo.dat[study6.demo.dat$PID != bad_pts[id_iterator], ]
}

# get list of all participants
demo_pts <- unique(study6.demo.dat$PID)

# adding columns and filling them out based on Response
study6.demo.dat$sex[grepl("Male", study6.demo.dat$response)] <- 1
study6.demo.dat$sex[grepl("Female", study6.demo.dat$response)] <- 2
study6.demo.dat$sex[grepl("Prefer Not to Say", study6.demo.dat$response)] <- 3
study6.demo.dat$sex[grepl("If your gender is not specified in this list, please specify it here:", study6.demo.dat$response)] <- 4

# making an empty dataframe with the number of rows being the same as the number of participants
demo2.dat <- data.frame(matrix(nrow = length(demo_pts), ncol = 3))

# renaming columns in new dataframe
demo2.dat <- demo2.dat %>% rename(pts = X1,
                                  sex = X2, 
                                  age = X3)
# fill in participant ID column
demo2.dat$pts <- demo_pts

# fill in age column
demo2.dat$age <- study6.demo.dat$response[study6.demo.dat$question == "Age"]
demo2.dat$age <- as.numeric(demo2.dat$age) # change data type of age column to numeric

# fill in sex column
demo_sex <- study6.demo.dat$sex # make list from first dataframe
demo_sex <- study6.demo.dat$sex[seq(2, length(study6.demo.dat$sex), 2)]
demo2.dat$sex <- demo_sex # fill in sex column in second dataframe
demo2.dat <- demo2.dat %>%
  mutate(sex = replace_na(sex, 3))

# creating a summary dataframe
summary_demo.dat <- data.frame(matrix(nrow = 7, ncol = 2))
summary_demo.dat <- summary_demo.dat %>% rename(Property = X1, Value = X2)
summary_demo.dat[,1] <- c("N", "num_M", "num_F","num_nosay", "num_other", "mean_Age", "sd_Age") 

# filling in demographic information
summary_demo.dat$Value[summary_demo.dat$Property == "N"] <- length(demo_pts)
sexDist <- table(demo2.dat$sex)
summary_demo.dat$Value[summary_demo.dat$Property == "num_M"] <- sexDist[names(sexDist) == 1]
summary_demo.dat$Value[summary_demo.dat$Property == "num_F"] <- sexDist[names(sexDist) == 2]
summary_demo.dat$Value[summary_demo.dat$Property == "num_nosay"] <- sexDist[names(sexDist) == 3]
#summary_demo.dat$Value[summary_demo.dat$Property == "num_other"] <- sexDist[names(sexDist) == 4]
summary_demo.dat$Value[summary_demo.dat$Property == "mean_Age"] <- mean(demo2.dat$age)
summary_demo.dat$Value[summary_demo.dat$Property == "sd_Age"] <- sd(demo2.dat$age)

print(summary_demo.dat)

##### DATA ANALYSIS #####
lme_full_study6 <- lmer(ratings ~ 1 + Content*contFrequency*emptyfirstblock + (1 | PID), 
                 control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
                 verbose = FALSE, REML = FALSE, data = study6.dat)
summary(lme_full_study6)

lme_step <- step(lme_full_study6)
lme_step

lme_step_result_model <- lmer(ratings ~ Content + contFrequency + (1 | PID) + Content:contFrequency,
                              data = study6.dat, REML = FALSE)
summary(lme_step_result_model)

### nested model comparison
no_interaction_model <- lmer(ratings ~ Content + contFrequency + (1 | PID),
                           data = study6.dat, REML = FALSE)

no_frequency_model <- lmer(ratings ~ Content + (1 | PID),
                             data = study6.dat, REML = FALSE)

no_content_model <- lmer(ratings ~ contFrequency + (1 | PID),
                           data = study6.dat, REML = FALSE)

null_model <- lmer(ratings ~ 1 + (1 | PID), data = study6.dat, REML = FALSE)

anova(null_model, lme_full_study6, lme_step_result_model, no_interaction_model,
      no_content_model, no_frequency_model)


### model assumptions
qqnorm(resid(lme_step_result_model))
qqline(resid(lme_step_result_model))

hist(resid(lme_step_result_model), breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

### homoscedasticity
plot_data <- data.frame(Fitted = fitted(lme_step_result_model), 
                        Residuals = resid(lme_step_result_model))
(study6_plot_homosce <- ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "loess", col = "blue", se = FALSE) +  
    labs(x = "Fitted values", y = "Residuals") + 
    theme_minimal())

### robust SE
study6_robust_se <- coef_test(lme_step_result_model, 
                              vcov = "CR2", cluster = study6.dat$PID)
study6_robust_se$p_Satt <- round(study6_robust_se$p_Satt, 3)
View(study6_robust_se)

descriptive_stats <- study6.dat %>%
  group_by(Content) %>%
  summarise(
    M = mean(ratings, na.rm = TRUE),
    SD = sd(ratings, na.rm = TRUE),
    n = n()
  )

print(descriptive_stats)

study6.dat$Content <- factor(study6.dat$Content,
                             levels = c("Empty", "singleNT", "noD"))
lme_step_result_model <- lmer(ratings ~ Content + contFrequency + (1 | PID) + Content:contFrequency,
                              data = study6.dat, REML = FALSE)
summary(lme_step_result_model)
study6_robust_se <- coef_test(lme_step_result_model, 
                              vcov = "CR2", cluster = study6.dat$PID)
study6_robust_se$p_Satt <- round(study6_robust_se$p_Satt, 3)
View(study6_robust_se)

#  pairwise
study6.dat$Frequency <- factor(study6.dat$Frequency, levels = c("216", "16", "0"))

lme_step_result_model_factorised <- lmer(ratings ~ Content + Frequency + (1 | PID) + Content:Frequency,
                                         data = study6.dat, REML = FALSE)

emms <- emmeans(lme_step_result_model_factorised, pairwise ~ Frequency | Content)
summary(emms)

emms2 <- emmeans(lme_step_result_model_factorised, pairwise ~ Content | Frequency)
summary(emms2)


##### VISUALISATIONS #####
study6.dat$Content <- as.character(study6.dat$Content)
study6.dat$Frequency <- as.character(study6.dat$Frequency)
# study6.dat$Frequency <- factor(study6.dat$Frequency, levels = c("0", "16", "216"))
study6.dat$Frequency <- as.numeric(study6.dat$Frequency)

# content plot
study6.sum <- study6.dat %>% 
  group_by(Frequency, Content) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study6.freq.sum <- study6.dat %>% 
  group_by(Frequency) %>%
  summarise(
    avgRatings = mean(ratings),
    sdRatings = sd(ratings), 
    seRatings = std.error(ratings)
  )

study6_plot <- ggplot(study6.sum, 
                      aes(x = Frequency,
                          y = avgRatings, 
                          group = Content, 
                          color = Content, 
                          shape = Content)) +
  geom_line(aes(linetype = Content), size = 1) + 
  geom_point(size = 4)  +  
  scale_x_continuous(breaks = c(0, 16, 216)) +
  labs(title = "Study 6", x = "D Trial Frequency", 
       y = "Mean Rating") + 
  geom_hline(yintercept = 0) + 
  scale_color_manual(name = "Content", 
                     values = c('#B6B7B9', '#000000', '#646568', '#B52003'),
                     labels = c("no D", "Empty", "singleNT", "Average Rating")) +
  scale_linetype_manual(name = "Content", 
                        values = c('dotted', 'solid', 'longdash', 'dotdash'), 
                        labels = c("no D", "Empty", "singleNT", "Average Rating")) +
  scale_shape_manual(name = "Content", 
                     values = c(17, 16, 15, 8), 
                     labels = c("no D", "Empty", "singleNT", "Average Rating")) +
  geom_errorbar(aes(ymin = avgRatings - seRatings, ymax = avgRatings + seRatings), 
                width = 0.25, alpha = 1.0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        plot.title = element_text(size = 16, family = "Arial", face = "bold"), 
        legend.text = element_text(size = 12, family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"), 
        strip.text.x = element_text(size = 12, family = "Arial"),
        legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Add "Average Rating" data distinctly
study6_plot <- study6_plot +
  geom_point(data = study6.freq.sum, 
             aes(x = Frequency, 
                 y = avgRatings,
                 color = "Average Rating",
                 shape = "Average Rating"),
             size = 5, 
             alpha = 0.8,
             inherit.aes = FALSE,
             show.legend = TRUE) + 
  geom_errorbar(data = study6.freq.sum, 
                aes(x = Frequency, 
                    ymin = avgRatings - seRatings, 
                    ymax = avgRatings + seRatings,
                    color = "Average Rating"),
                width = 0.2, 
                alpha = 0.8,
                inherit.aes = FALSE,
                show.legend = TRUE) + 
  geom_line(data = study6.freq.sum, 
            aes(x = Frequency, 
                y = avgRatings,
                group = "Average Rating",  # Use a distinct group for Average Rating
                color = "Average Rating",
                linetype = "Average Rating"),
            size = 1, 
            alpha = 0.8,
            inherit.aes = FALSE,
            show.legend = TRUE)

study6_plot

ggsave("Study6_Fig1.png", plot = study6_plot, width = 16, height = 10, units = "cm")
