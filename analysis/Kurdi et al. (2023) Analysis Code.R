# Kurdi et al. (2023). Testing the automaticity features of the Affect Misattribution Procedure:
# The roles of awareness and intentionality.
# Analysis script.

if (!require(beanplot)) {install.packages("beanplot"); require(beanplot)}
if (!require(emmeans)) {install.packages("emmeans"); require(emmeans)}
if (!require(lme4)) {install.packages("lme4"); require(lme4)}
if (!require(lmerTest)) {install.packages("lmerTest"); require(lmerTest)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)}
if (!require(mgcv)) {install.packages("mgcv"); require(mgcv)}
if (!require(reshape2)) {install.packages("reshape2"); require(reshape2)}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

citation <- "Kurdi et al. (2023)"

orderedTable <- function(x) {
     table(x, useNA = "always")[order(table(x, useNA = "always"), decreasing = TRUE)]
}

log2prob <- function(x) exp(x)/(1+exp(x))

engCount <- c("United States", "Canada",
              "United Kingdom", "Australia",
              "South Africa", "New Zealand", "Ireland")

# Exp. 1 ------------------------------------------------------------------

exp1All <- read.csv(paste(citation, "Exp. 1.csv"))
exp1AllLong <- read.csv(paste(citation, "Exp. 1 Long-format AMP.csv"))

# Participant exclusions

nrow(exp1All)
table(exp1All$all_same, useNA = "always")
table(exp1All$complete_amp, useNA = "always")
table(exp1All$use_data, useNA = "always")

exp1 <- exp1All[exp1All$all_same == 0 & exp1All$complete_amp == 1, ]
exp1long <- exp1AllLong[exp1AllLong$session_id %in% exp1$session_id, ]

# Demographics

orderedTable(exp1$citizenship)
prop.table(orderedTable(exp1$citizenship))
prop.table(table(exp1$citizenship %in% c("United States", "Canada",
                                         "United Kingdom", "Australia",
                                         "South Africa", "New Zealand", "Ireland")))

orderedTable(exp1$gender)
prop.table(orderedTable(exp1$gender))

mean(exp1$age, na.rm = TRUE)
sd(exp1$age, na.rm = TRUE)

# Descriptive statistics
mean(exp1$ampDiff)
median(exp1$ampDiff)
sd(exp1$ampDiff)
table(exp1$ampDiff > 0)
prop.table(table(exp1$ampDiff > 0))
hist(exp1$ampDiff, col = "white", main = "Distribution of AMP scores", xlab = "AMP score")

mean(exp1$shareAware)
median(exp1$shareAware)
sd(exp1$shareAware)
hist(exp1$shareAware, col = "white", main = "Distribution of awareness responses", xlab = "Awareness response")

tapply(exp1long$trial_response, list(exp1long$block_name, exp1long$aware_response), mean)

plot(exp1$shareAware, exp1$ampDiff)
abline(lm(exp1$ampDiff ~ exp1$shareAware), col = "red")
cor(exp1$shareAware, exp1$ampDiff)

# Trial-level analysis

exp1long$aware_response_fact <- factor(exp1long$aware_response)

# (M1) Intercept only; random intercepts for participants
exp1Model1 <- glmer(trial_response ~ 1 + (1 | session_id), family = "binomial", exp1long)

# (M2) Intercept only; random intercepts for participants and target stimuli
exp1Model2 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target), family = "binomial",
                   exp1long)
anova(exp1Model1, exp1Model2)

# (M3) Intercept only; random intercepts for participants, target stimuli, and prime stimuli
exp1Model3 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target) + (1 | prime),
                   family = "binomial", exp1long)
anova(exp1Model2, exp1Model3)

# (M4) Main effect for prime type; random intercepts for participants, target stimuli, and prime stimuli
exp1Model4 <- glmer(trial_response ~ block_name + (1 | session_id) + (1 | target) + (1 | prime),
                   family = "binomial", exp1long)
anova(exp1Model3, exp1Model4)

# (M5) Main effect for prime type and awareness; random intercepts for participants, target stimuli, and prime stimuli
exp1Model5 <- glmer(trial_response ~ block_name + aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                   family = "binomial", exp1long)
anova(exp1Model4, exp1Model5)

# (M6) Prime Type × Awareness interaction; random intercepts for participants, target stimuli, and prime stimuli
exp1Model6 <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                   family = "binomial", exp1long)
anova(exp1Model5, exp1Model6)
summary(exp1Model6)

emmeansFitExp1 <- emmeans(exp1Model6, ~ block_name | aware_response_fact)
pairs(emmeansFitExp1, adjust = "none")

margMeansExp1 <- data.frame(emmeansFitExp1)
margMeansExp1 <- margMeansExp1[c(2, 1, 4, 3), ]
margMeansExp1$emmean <- log2prob(margMeansExp1$emmean)
margMeansExp1$asymp.LCL <- log2prob(margMeansExp1$asymp.LCL)
margMeansExp1$asymp.UCL <- log2prob(margMeansExp1$asymp.UCL)

# Refitting model 6 to participants who reported their data should be used
exp1longSubset <- exp1long[exp1long$use_data == 1, ]
exp1Model6subset <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp1longSubset)

emmeansFitExp1subset <- emmeans(exp1Model6subset, ~ block_name | aware_response_fact)
pairs(emmeansFitExp1subset, adjust = "none")

# Refitting model 6 to all participants
exp1AllLong$aware_response_fact <- factor(exp1AllLong$aware_response)
exp1Model6all <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                          family = "binomial", exp1AllLong)

emmeansFitExp1all <- emmeans(exp1Model6all, ~ block_name | aware_response_fact)
pairs(emmeansFitExp1all, adjust = "none")

# Refitting model 6 to participants from English-speaking countries
exp1Model6eng <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                       family = "binomial", exp1long[exp1long$session_id %in% exp1$session_id[exp1$citizenship %in% engCount], ])

emmeansFitExp1eng <- emmeans(exp1Model6eng, ~ block_name | aware_response_fact)
pairs(emmeansFitExp1eng, adjust = "none")

exp1sum <- with(exp1long, tapply(trial_response, list(session_id, block_name, aware_response), mean))

pdf(paste0(citation, " Exp. 1.pdf"), width = 12)
beanplot(exp1sum[, , 1][, "Positive"],
         exp1sum[, , 1][, "Negative"],
         exp1sum[, , 2][, "Positive"],
         exp1sum[, , 2][, "Negative"],
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 0, cutmax = 1,
         col = list(rainbow(4, alpha = 0.30)[2], rainbow(4, alpha = 0.30)[3]),
         border = "black",
         main = "Implicit evaluations by awareness\nExp. 1",
         axes = FALSE, bw = 0.2, ylab = "P(pleasant)",
         ylim = c(0, 1.2), xlab = "Prime type")
abline(h = 0.5, lty = 3)
axis(1, at = 1:2, labels = c("Positive", "Negative"))
axis(1, at = 3:4, labels = c("Positive", "Negative"))
axis(2, at = seq(0, 1, by = 0.2))
abline(v = 2.5)
text(1.5, 1.1, "Unaware trials")
text(3.5, 1.1, "Aware trials")
dev.off()

# Exploratory analyses

exp1$prime_present <- as.numeric(paste(exp1$prime_present))
hist(exp1$prime_present, col = "white")
mean(exp1$prime_present, na.rm = TRUE)
median(exp1$prime_present, na.rm = TRUE)

exp1$target_present <- as.numeric(paste(exp1$target_present))
hist(exp1$target_present, col = "white")
mean(exp1$target_present, na.rm = TRUE)
median(exp1$target_present, na.rm = TRUE)

exp1$influence <- as.numeric(paste(exp1$influence))
hist(exp1$influence, col = "white")

plot(exp1$influence, exp1$shareAware)
cor.test(exp1$influence, exp1$shareAware)

summary(lm(scale(exp1$ampDiff, center = FALSE) ~ scale(exp1$influence, center = FALSE) + scale(exp1$shareAware, center = FALSE)))

exp1$influence_unintentional <- as.numeric(paste(exp1$influence_unintentional))
hist(exp1$influence_unintentional, col = "white")

exp1$influence_intentional <- as.numeric(paste(exp1$influence_intentional))
hist(exp1$influence_intentional, col = "white")

cor.test(exp1$influence_unintentional, exp1$influence_intentional)

summary(lm(scale(exp1$ampDiff, center = FALSE) ~ scale(exp1$influence_unintentional, center = FALSE)
           + scale(exp1$influence_intentional, center = FALSE)))

# Exp. 2 ------------------------------------------------------------------

exp2All <- read.csv(paste(citation, "Exp. 2.csv"))
exp2AllLong <- read.csv(paste(citation, "Exp. 2 Long-format AMP.csv"))

# Participant exclusions

nrow(exp2All)
table(exp2All$all_same, useNA = "always")
table(exp2All$complete_amp, useNA = "always")
table(exp2All$use_data, useNA = "always")

exp2 <- exp2All[exp2All$all_same == 0 & exp2All$complete_amp == 1, ]
exp2long <- exp2AllLong[exp2AllLong$session_id %in% exp2$session_id, ]

# Demographics

orderedTable(exp2$citizenship)
prop.table(orderedTable(exp2$citizenship))
prop.table(table(exp2$citizenship %in% c("United States", "Canada",
                                         "United Kingdom", "Australia",
                                         "South Africa", "New Zealand", "Ireland")))

orderedTable(exp2$gender)
prop.table(orderedTable(exp2$gender))

mean(exp2$age, na.rm = TRUE)
sd(exp2$age, na.rm = TRUE)

# Descriptive statistics
table(exp2$cond)

mean(exp2$ampDiff)
median(exp2$ampDiff)
sd(exp2$ampDiff)
table(exp2$ampDiff > 0)
prop.table(table(exp2$ampDiff > 0))
hist(exp2$ampDiff, col = "white", main = "Distribution of AMP scores", xlab = "AMP score")

mean(exp2$shareAware)
median(exp2$shareAware)
sd(exp2$shareAware)
hist(exp2$shareAware, col = "white", main = "Distribution of awareness responses", xlab = "Awareness response")

tapply(exp2long$trial_response, exp2long$cond, mean)
tapply(exp2long$trial_response, list(exp2long$block_name, exp2long$aware_response, exp2long$cond), mean)

plot(exp2$shareAware, exp2$ampDiff)
abline(lm(exp2$ampDiff ~ exp2$shareAware), col = "red")
cor(exp2$shareAware, exp2$ampDiff)

cor(exp2[exp2$cond == "Standard", ]$shareAware, exp2[exp2$cond == "Standard", ]$ampDiff)
cor(exp2[exp2$cond == "Reversed", ]$shareAware, exp2[exp2$cond == "Reversed", ]$ampDiff)

# Trial-level analysis

exp2long$aware_response_fact <- factor(exp2long$aware_response)

# (M1) Intercept only; random intercepts for participants
exp2Model1 <- glmer(trial_response ~ 1 + (1 | session_id), family = "binomial", exp2long)

# (M2) Intercept only; random intercepts for participants and target stimuli
exp2Model2 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target), family = "binomial",
                    exp2long)
anova(exp2Model1, exp2Model2)

# (M3) Intercept only; random intercepts for participants, target stimuli, and prime stimuli
exp2Model3 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model2, exp2Model3)

# (M4) Main effect for prime type; random intercepts for participants, target stimuli, and prime stimuli
exp2Model4 <- glmer(trial_response ~ block_name + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model3, exp2Model4)

# (M5) Main effect for prime type and awareness; random intercepts for participants, target stimuli, and prime stimuli
exp2Model5 <- glmer(trial_response ~ block_name + aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model4, exp2Model5)

# (M6) Prime Type × Awareness interaction; random intercepts for participants, target stimuli, and prime stimuli
exp2Model6 <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model5, exp2Model6)

# (M7) Prime Type × Awareness interaction, main effect for condition; random intercepts for participants, target stimuli, and prime stimuli
exp2Model7 <- glmer(trial_response ~ block_name * aware_response_fact + cond + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model6, exp2Model7)

# (M8) Prime Type × Awareness × Condition interaction; random intercepts for participants, target stimuli, and prime stimuli
exp2Model8 <- glmer(trial_response ~ block_name * aware_response_fact * cond + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp2long)
anova(exp2Model7, exp2Model8)
allFit(exp2Model8)

emmeansFitExp2 <- emmeans(exp2Model8, ~ block_name | aware_response_fact * cond)
pairs(emmeansFitExp2, adjust = "none")

margMeansExp2 <- data.frame(emmeansFitExp2)
margMeansExp2 <- margMeansExp2[c(6, 5, 8, 7, 2, 1, 4, 3), ]
margMeansExp2$emmean <- log2prob(margMeansExp2$emmean)
margMeansExp2$asymp.LCL <- log2prob(margMeansExp2$asymp.LCL)
margMeansExp2$asymp.UCL <- log2prob(margMeansExp2$asymp.UCL)

# Refitting model 8 to participants who reported their data should be used
exp2longSubset <- exp2long[exp2long$use_data == 1, ]
exp2Model8subset <- glmer(trial_response ~ block_name * aware_response_fact * cond + (1 | session_id) + (1 | target) + (1 | prime),
                          family = "binomial", exp2longSubset)

emmeansFitexp2subset <- emmeans(exp2Model8subset, ~ block_name | aware_response_fact * cond)
pairs(emmeansFitexp2subset, adjust = "none")

# Refitting model 8 to all participants
exp2AllLong$aware_response_fact <- factor(exp2AllLong$aware_response)
exp2Model8all <- glmer(trial_response ~ block_name * aware_response_fact * cond + (1 | session_id) + (1 | target) + (1 | prime),
                       family = "binomial", exp2AllLong)

emmeansFitexp2all <- emmeans(exp2Model8all, ~ block_name | aware_response_fact * cond)
pairs(emmeansFitexp2all, adjust = "none")

# Refitting model 8 to participants from English-speaking countries
exp2Model8eng <- glmer(trial_response ~ block_name * aware_response_fact * cond + (1 | session_id) + (1 | target) + (1 | prime),
                          family = "binomial", exp2long[exp2long$session_id %in% exp2$session_id[exp2$citizenship %in% engCount], ])

emmeansFitexp2eng <- emmeans(exp2Model8eng, ~ block_name | aware_response_fact * cond)
pairs(emmeansFitexp2eng, adjust = "none")

exp2sum <- with(exp2long, tapply(trial_response, list(session_id, block_name, aware_response, cond), mean))

pdf(paste0(citation, " Exp. 2.pdf"), width = 14)
beanplot(exp2sum[, , 1, 2][, "Positive"],
         exp2sum[, , 1, 2][, "Negative"],
         exp2sum[, , 2, 2][, "Positive"],
         exp2sum[, , 2, 2][, "Negative"],
         exp2sum[, , 1, 1][, "Positive"],
         exp2sum[, , 1, 1][, "Negative"],
         exp2sum[, , 2, 1][, "Positive"],
         exp2sum[, , 2, 1][, "Negative"],
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 0, cutmax = 1,
         col = list(rainbow(4, alpha = 0.30)[2], rainbow(4, alpha = 0.30)[3]),
         border = "black",
         main = "Implicit evaluations by awareness\nExp. 2",
         axes = FALSE, bw = 0.2, ylab = "P(pleasant)",
         ylim = c(0, 1.2), xlab = "Prime type")
abline(h = 0.5, lty = 3)
axis(1, at = 1:2, labels = c("Positive", "Negative"))
axis(1, at = 3:4, labels = c("Positive", "Negative"))
axis(1, at = 5:6, labels = c("Positive", "Negative"))
axis(1, at = 7:8, labels = c("Positive", "Negative"))
axis(2, at = seq(0, 1, by = 0.2))
abline(v = 2.5)
abline(v = 4.5)
abline(v = 6.5)
text(1.5, 1.1, "Standard condition\nUnaware trials")
text(3.5, 1.1, "Standard condition\nAware trials")
text(5.5, 1.1, "Reversed condition\nUnaware trials")
text(7.5, 1.1, "Reversed condition\nAware trials")
dev.off()

# Exploratory analyses

exp2$prime_present <- as.numeric(paste(exp2$prime_present))
hist(exp2$prime_present, col = "white")
mean(exp2$prime_present, na.rm = TRUE)
median(exp2$prime_present, na.rm = TRUE)

exp2$target_present <- as.numeric(paste(exp2$target_present))
hist(exp2$target_present, col = "white")
mean(exp2$target_present, na.rm = TRUE)
median(exp2$target_present, na.rm = TRUE)

exp2$influence <- as.numeric(paste(exp2$influence))
hist(exp2$influence, col = "white")

plot(exp2$influence, exp2$shareAware)
cor.test(exp2$influence, exp2$shareAware)

summary(lm(scale(exp2$ampDiff, center = FALSE) ~ scale(exp2$influence, center = FALSE) + scale(exp2$shareAware, center = FALSE)))

exp2$influence_unintentional <- as.numeric(paste(exp2$influence_unintentional))
hist(exp2$influence_unintentional, col = "white")

exp2$influence_intentional <- as.numeric(paste(exp2$influence_intentional))
hist(exp2$influence_intentional, col = "white")

cor.test(exp2$influence_unintentional, exp2$influence_intentional)

summary(lm(scale(exp2$ampDiff, center = FALSE) ~ scale(exp2$influence_unintentional, center = FALSE)
           + scale(exp2$influence_intentional, center = FALSE)))

exp2$skip_advance <- as.numeric(paste(exp2$skip_advance))
hist(exp2$skip_advance, col = "white")
mean(exp2$skip_advance, na.rm = TRUE)
sd(exp2$skip_advance, na.rm = TRUE)

t.test(exp2$skip_advance)
cohensD(exp2$skip_advance)

cor.test(exp2$shareAware[exp2$cond == "Standard"],
         exp2$skip_advance[exp2$cond == "Standard"])

cor.test(exp2$shareAware[exp2$cond == "Reversed"],
         exp2$skip_advance[exp2$cond == "Reversed"])

# Exp. 3 ------------------------------------------------------------------

exp3All <- read.csv(paste(citation, "Exp. 3.csv"))
exp3AllLong <- read.csv(paste(citation, "Exp. 3 Long-format AMP.csv"))

# Participant exclusions

nrow(exp3All)
table(exp3All$all_same, useNA = "always")
table(exp3All$complete_amp, useNA = "always")

exp3 <- exp3All[exp3All$all_same == 0 & exp3All$complete_amp == 1, ]
exp3long <- exp3AllLong[exp3AllLong$session_id %in% exp3$session_id, ]

# Demographics

orderedTable(exp3$citizenship)
prop.table(orderedTable(exp3$citizenship))
prop.table(table(exp3$citizenship %in% c("United States", "Canada",
                                         "United Kingdom", "Australia",
                                         "South Africa", "New Zealand", "Ireland")))

orderedTable(exp3$gender)
prop.table(orderedTable(exp3$gender))

mean(exp3$age, na.rm = TRUE)
sd(exp3$age, na.rm = TRUE)

# Descriptive statistics
mean(exp3$ampDiff)
median(exp3$ampDiff)
sd(exp3$ampDiff)
table(exp3$ampDiff > 0)
prop.table(table(exp3$ampDiff > 0))
hist(exp3$ampDiff, col = "white", main = "Distribution of AMP scores", xlab = "AMP score")

mean(exp3$shareAware)
median(exp3$shareAware)
sd(exp3$shareAware)
hist(exp3$shareAware, col = "white", main = "Distribution of awareness responses", xlab = "Awareness response")

tapply(exp3long$trial_response, list(exp3long$block_name, exp3long$aware_response), mean)

plot(exp3$shareAware, exp3$ampDiff)
abline(lm(exp3$ampDiff ~ exp3$shareAware), col = "red")
cor(exp3$shareAware, exp3$ampDiff)

# Trial-level analysis

exp3long$aware_response_fact <- factor(exp3long$aware_response)

# (M1) Intercept only; random intercepts for participants
exp3Model1 <- glmer(trial_response ~ 1 + (1 | session_id), family = "binomial", exp3long)

# (M2) Intercept only; random intercepts for participants and target stimuli
exp3Model2 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target), family = "binomial",
                    exp3long)
anova(exp3Model1, exp3Model2)

# (M3) Intercept only; random intercepts for participants, target stimuli, and prime stimuli
exp3Model3 <- glmer(trial_response ~ 1 + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp3long)
anova(exp3Model2, exp3Model3)

# (M4) Main effect for prime type; random intercepts for participants, target stimuli, and prime stimuli
exp3Model4 <- glmer(trial_response ~ block_name + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp3long)
anova(exp3Model3, exp3Model4)

# (M5) Main effect for prime type and awareness; random intercepts for participants, target stimuli, and prime stimuli
exp3Model5 <- glmer(trial_response ~ block_name + aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp3long)
anova(exp3Model4, exp3Model5)

# (M6) Prime Type × Awareness interaction; random intercepts for participants, target stimuli, and prime stimuli
exp3Model6 <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                    family = "binomial", exp3long)
anova(exp3Model5, exp3Model6)

emmeansFitExp3 <- emmeans(exp3Model6, ~ block_name | aware_response_fact)
pairs(emmeansFitExp3, adjust = "none")

margMeansExp3 <- data.frame(emmeansFitExp3)
margMeansExp3 <- margMeansExp3[c(2, 1, 4, 3), ]
margMeansExp3$emmean <- log2prob(margMeansExp3$emmean)
margMeansExp3$asymp.LCL <- log2prob(margMeansExp3$asymp.LCL)
margMeansExp3$asymp.UCL <- log2prob(margMeansExp3$asymp.UCL)

# Refitting model 6 to all participants
exp3AllLong$aware_response_fact <- factor(exp3AllLong$aware_response)
exp3Model6all <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                       family = "binomial", exp3AllLong)

emmeansFitExp3all <- emmeans(exp3Model6all, ~ block_name | aware_response_fact)
pairs(emmeansFitExp3all, adjust = "none")

# Refitting model 6 to participants from English-speaking countries
exp3Model6eng <- glmer(trial_response ~ block_name * aware_response_fact + (1 | session_id) + (1 | target) + (1 | prime),
                       family = "binomial", exp3long[exp3long$session_id %in% exp3$session_id[exp3$citizenship %in% engCount], ])

emmeansFitExp3eng <- emmeans(exp3Model6eng, ~ block_name | aware_response_fact)
pairs(emmeansFitExp3eng, adjust = "none")


exp3sum <- with(exp3long, tapply(trial_response, list(session_id, block_name, aware_response), mean))

pdf(paste0(citation, " Exp. 3.pdf"), width = 12)
beanplot(exp3sum[, , 1][, "Positive"],
         exp3sum[, , 1][, "Negative"],
         exp3sum[, , 2][, "Positive"],
         exp3sum[, , 2][, "Negative"],
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 0, cutmax = 1,
         col = list(rainbow(4, alpha = 0.30)[2], rainbow(4, alpha = 0.30)[3]),
         border = "black",
         main = "Implicit evaluations by awareness\nExp. 3",
         axes = FALSE, bw = 0.2, ylab = "P(pleasant)",
         ylim = c(0, 1.2), xlab = "Prime type")
abline(h = 0.5, lty = 3)
axis(1, at = 1:2, labels = c("Positive", "Negative"))
axis(1, at = 3:4, labels = c("Positive", "Negative"))
axis(2, at = seq(0, 1, by = 0.2))
abline(v = 2.5)
text(1.5, 1.1, "Unaware trials")
text(3.5, 1.1, "Aware trials")
dev.off()

# Exploratory analyses

exp3$prime_present <- as.numeric(paste(exp3$prime_present))
hist(exp3$prime_present, col = "white")
mean(exp3$prime_present, na.rm = TRUE)
median(exp3$prime_present, na.rm = TRUE)

exp3$target_present <- as.numeric(paste(exp3$target_present))
hist(exp3$target_present, col = "white")
mean(exp3$target_present, na.rm = TRUE)
median(exp3$target_present, na.rm = TRUE)

exp3$influence <- as.numeric(paste(exp3$influence))
hist(exp3$influence, col = "white")

plot(exp3$influence, exp3$shareAware)
cor.test(exp3$influence, exp3$shareAware)

summary(lm(scale(exp3$ampDiff, center = FALSE) ~ scale(exp3$influence, center = FALSE) + scale(exp3$shareAware, center = FALSE)))

exp3$influence_unintentional <- as.numeric(paste(exp3$influence_unintentional))
hist(exp3$influence_unintentional, col = "white")

exp3$influence_intentional <- as.numeric(paste(exp3$influence_intentional))
hist(exp3$influence_intentional, col = "white")

cor.test(exp3$influence_unintentional, exp3$influence_intentional)

summary(lm(scale(exp3$ampDiff, center = FALSE) ~ scale(exp3$influence_unintentional, center = FALSE)
           + scale(exp3$influence_intentional, center = FALSE)))

# Exp. 4 ------------------------------------------------------------------

exp4All <- read.csv(paste(citation, "Exp. 4.csv"))
exp4AllLong <- read.csv(paste(citation, "Exp. 4 Long-format AMP.csv"))

# Participant exclusions

nrow(exp4All)
table(exp4All$all_same, useNA = "always")
table(exp4All$complete_amp, useNA = "always")

exp4 <- exp4All[exp4All$all_same == 0 & exp4All$complete_amp == 1, ]
exp4long <- exp4AllLong[exp4AllLong$session_id %in% exp4$session_id, ]

# Demographics

orderedTable(exp4$citizenship)
prop.table(orderedTable(exp4$citizenship))
prop.table(table(exp4$citizenship %in% c("United States", "Canada",
                                         "United Kingdom", "Australia",
                                         "South Africa", "New Zealand", "Ireland")))

orderedTable(exp4$gender)
prop.table(orderedTable(exp4$gender))

mean(exp4$age, na.rm = TRUE)
sd(exp4$age, na.rm = TRUE)

# Descriptive statistics
mean(exp4$ampDiff)
median(exp4$ampDiff)
sd(exp4$ampDiff)
table(exp4$ampDiff > 0)
prop.table(table(exp4$ampDiff > 0))
hist(exp4$ampDiff, col = "white", main = "Distribution of AMP scores", xlab = "AMP score")

mean(exp4$shareAware)
median(exp4$shareAware)
sd(exp4$shareAware)
hist(exp4$shareAware, col = "white", main = "Distribution of awareness responses", xlab = "Awareness response")

tapply(exp4long$trial_response, list(exp4long$block_name, exp4long$aware_response), mean)

plot(exp4$shareAware, exp4$ampDiff)
abline(lm(exp4$ampDiff ~ exp4$shareAware), col = "red")
cor(exp4$shareAware, exp4$ampDiff)

# Trial-level analysis

exp4long$aware_response_fact <- factor(exp4long$aware_response)

exp4GamFit <- gamm(trial_response ~ s(valence, by = aware_response_fact),
                random = list(session_id =~ 1), family = binomial, data = exp4long)
summary(exp4GamFit$gam)
summary(exp4GamFit$lme)

exp4GlmerFit <- glmer(trial_response ~ valence * aware_response_fact + (1 | session_id),
                   family = "binomial", data = exp4long)

anova(exp4GamFit$gam, exp4GlmerFit)

plot.data <- {
     pdf(NULL)
     res <- plot(exp4GamFit$gam)
     invisible(dev.off())
     res
}


exp4desc <- with(exp4long, tapply(trial_response, list(valence, aware_response), mean))
plot.data[[1]]$fit <- summary(exp4GamFit$gam)[[1]] + plot.data[[1]]$fit # Adding intercept

pdf(paste0(citation, " Exp. 4.pdf"), width = 10)
plot(plot.data[[1]]$x, log2prob(plot.data[[1]]$fit), type = "l", col = rainbow(2)[1], xlim = c(1, 7), ylim = c(0, 1),
     axes = FALSE, xlab = "Prime valence", ylab = "P(pleasant)",
     main = "Implicit evaluations by prime valence and awareness\nExp. 4", lwd = 1.5)
points(as.numeric(paste(rownames(exp4desc))), exp4desc[, 1], col = rainbow(2)[1], pch = 21, bg = rainbow(2, alpha = 0.3)[1])
points(as.numeric(paste(rownames(exp4desc))), exp4desc[, 2], col = rainbow(2)[2], pch = 21, bg = rainbow(2, alpha = 0.3)[2])
lines(plot.data[[2]]$x, log2prob(plot.data[[2]]$fit), col = rainbow(2)[2], lwd = 1.5)
lines(plot.data[[1]]$x, log2prob(plot.data[[1]]$fit + plot.data[[1]]$se), col = rainbow(2)[1], lty = 3)
lines(plot.data[[1]]$x, log2prob(plot.data[[1]]$fit - plot.data[[1]]$se), col = rainbow(2)[1], lty = 3)
lines(plot.data[[2]]$x, log2prob(plot.data[[2]]$fit + plot.data[[2]]$se), col = rainbow(2)[2], lty = 3)
lines(plot.data[[2]]$x, log2prob(plot.data[[2]]$fit - plot.data[[2]]$se), col = rainbow(2)[2], lty = 3)
abline(h = 0.5, lty = 3)
axis(1)
axis(2)
legend("topleft", legend = c("Aware", "Unaware"), col = c(rainbow(2)[2], rainbow(2)[1]), lty = 1, bty = "n")
dev.off()


# Exploratory analyses

exp4$prime_present <- as.numeric(paste(exp4$prime_present))
hist(exp4$prime_present, col = "white")
mean(exp4$prime_present, na.rm = TRUE)
median(exp4$prime_present, na.rm = TRUE)

exp4$target_present <- as.numeric(paste(exp4$target_present))
hist(exp4$target_present, col = "white")
mean(exp4$target_present, na.rm = TRUE)
median(exp4$target_present, na.rm = TRUE)

exp4$influence <- as.numeric(paste(exp4$influence))
hist(exp4$influence, col = "white")

plot(exp4$influence, exp4$shareAware)
cor.test(exp4$influence, exp4$shareAware)

summary(lm(scale(exp4$ampDiff, center = FALSE) ~ scale(exp4$influence, center = FALSE) + scale(exp4$shareAware, center = FALSE)))

exp4$influence_unintentional <- as.numeric(paste(exp4$influence_unintentional))
hist(exp4$influence_unintentional, col = "white")

exp4$influence_intentional <- as.numeric(paste(exp4$influence_intentional))
hist(exp4$influence_intentional, col = "white")

cor.test(exp4$influence_unintentional, exp4$influence_intentional)

summary(lm(scale(exp4$ampDiff, center = FALSE) ~ scale(exp4$influence_unintentional, center = FALSE)
           + scale(exp4$influence_intentional, center = FALSE)))

# Exp. 5 ------------------------------------------------------------------

exp5All <- read.csv(paste(citation, "Exp. 5.csv"))
exp5AllLong <- read.csv(paste(citation, "Exp. 5 Long-format AMP.csv"))

# Participant exclusions

nrow(exp5All)
table(exp5All$all_sameSelf, useNA = "always")
table(exp5All$complete_ampSelf, useNA = "always")
table(exp5All$all_sameThird, useNA = "always")
table(exp5All$complete_ampThird, useNA = "always")


exp5 <- exp5All[exp5All$all_sameSelf == 0 & exp5All$complete_ampSelf == 1 & exp5All$all_sameThird == 0 & exp5All$complete_ampThird == 1, ]
exp5long <- exp5AllLong[exp5AllLong$session_id %in% exp5$session_id, ]

# Demographics

orderedTable(exp5$citizenship)
prop.table(orderedTable(exp5$citizenship))
prop.table(table(exp5$citizenship %in% c("United States", "Canada",
                                         "United Kingdom", "Australia",
                                         "South Africa", "New Zealand", "Ireland")))


orderedTable(exp5$gender)
prop.table(orderedTable(exp5$gender))

mean(exp5$age, na.rm = TRUE)
sd(exp5$age, na.rm = TRUE)

# Descriptive statistics
mean(exp5$ampDiffSelf, na.rm = TRUE)
median(exp5$ampDiffSelf, na.rm = TRUE)
sd(exp5$ampDiffSelf, na.rm = TRUE)
table(exp5$ampDiffSelf > 0)
prop.table(table(exp5$ampDiffSelf > 0))
hist(exp5$ampDiffSelf, col = "white", main = "Distribution of AMP scores", 
     xlab = "First-person AMP score")

mean(exp5$shareAwareSelf, na.rm = TRUE)
median(exp5$shareAwareSelf, na.rm = TRUE)
sd(exp5$shareAwareSelf, na.rm = TRUE)
hist(exp5$shareAwareSelf, col = "white", main = "Distribution of awareness responses",
     xlab = "First-person awareness response")

mean(exp5$ampDiffThird, na.rm = TRUE)
median(exp5$ampDiffThird, na.rm = TRUE)
sd(exp5$ampDiffThird, na.rm = TRUE)
table(exp5$ampDiffThird > 0)
prop.table(table(exp5$ampDiffThird > 0))
hist(exp5$ampDiffThird, col = "white", main = "Distribution of AMP scores", 
     xlab = "Third-party AMP score")

mean(exp5$shareAwareThird, na.rm = TRUE)
median(exp5$shareAwareThird, na.rm = TRUE)
sd(exp5$shareAwareThird, na.rm = TRUE)
hist(exp5$shareAwareThird, col = "white", main = "Distribution of awareness responses",
     xlab = "Third-party awareness response")


tapply(exp5long$aware_response[exp5long$amp_target == "First person"],
       list(exp5long$block_name[exp5long$amp_target == "First person"],
            exp5long$trial_response[exp5long$amp_target == "First person"]), mean)
tapply(exp5long$aware_response[exp5long$amp_target == "Third party"],
       list(exp5long$block_name[exp5long$amp_target == "Third party"],
            exp5long$trial_response[exp5long$amp_target == "Third party"]), mean)

plot(exp5$shareAwareSelf, exp5$ampDiffSelf)
abline(lm(exp5$ampDiffSelf ~ exp5$shareAwareSelf), col = "red")
cor(exp5$shareAwareSelf, exp5$ampDiffSelf, use = "pairwise.complete.obs")

plot(exp5$shareAwareThird, exp5$ampDiffThird)
abline(lm(exp5$ampDiffThird ~ exp5$shareAwareThird), col = "red")
cor(exp5$shareAwareThird, exp5$ampDiffThird, use = "pairwise.complete.obs")

# Trial-level analysis

exp5long$aware_response_fact <- factor(exp5long$aware_response)
exp5long$trial_response_fact <- factor(exp5long$trial_response)

# (M1) Intercept only; random intercepts for participants
exp5Model1 <- glmer(aware_response_fact ~ 1 + (1 | session_id), family = "binomial", exp5long)

# (M2) Intercept only; random intercepts for participants and past participants
exp5Model2 <- glmer(aware_response_fact ~ 1 + (1 | session_id) + (1 | target_session_id), family = "binomial",
                    exp5long)
anova(exp5Model1, exp5Model2)

# (M3) Intercept only; random intercepts for participants and target stimuli
exp5Model3 <- glmer(aware_response_fact ~ 1 + (1 | session_id) + (1 | target),
                    family = "binomial", exp5long)
anova(exp5Model1, exp5Model3)

# (M4) Intercept only; random intercepts for participants and prime stimuli
exp5Model4 <- glmer(aware_response_fact ~ 1 + (1 | session_id) + (1 | prime),
                    family = "binomial", exp5long)
anova(exp5Model1, exp5Model4)

# (M5) Main effect for response; random intercepts for participants and prime stimuli
exp5Model5 <- glmer(aware_response_fact ~ trial_response_fact + (1 | session_id) + (1 | prime),
                    family = "binomial", exp5long)
anova(exp5Model4, exp5Model5)

# (M6) Main effect for response and prime type; random intercepts for participants and prime stimuli
exp5Model6 <- glmer(aware_response_fact ~ trial_response_fact + block_name + (1 | session_id) + (1 | prime),
                    family = "binomial", exp5long)
anova(exp5Model5, exp5Model6)

# (M7) Response × Prime Type interaction; random intercepts for participants and prime stimuli
exp5Model7 <- glmer(aware_response_fact ~ trial_response_fact * block_name + (1 | session_id) + (1 | prime),
                    family = "binomial", exp5long)
anova(exp5Model6, exp5Model7)

# (M8) Response × Prime Type interaction, main effect of AMP type; random intercepts for participants and prime stimuli
exp5Model8 <- glmer(aware_response_fact ~ trial_response_fact * block_name + amp_target +
                         (1 | session_id) + (1 | prime), family = "binomial", exp5long)
anova(exp5Model7, exp5Model8)

# (M9) Response × Prime Type × AMP Type interaction; random intercepts for participants and prime stimuli
exp5Model9 <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                         (1 | session_id) + (1 | prime), family = "binomial", exp5long)
anova(exp5Model8, exp5Model9)

emmeansFitExp5 <- emmeans(exp5Model9, ~ trial_response_fact | block_name * amp_target)
pairs(emmeansFitExp5, adjust = "none")

margMeansExp5 <- data.frame(emmeansFitExp5)
margMeansExp5 <- margMeansExp5[c(4, 2, 3, 1, 8, 6, 7, 5), ]
margMeansExp5$emmean <- log2prob(margMeansExp5$emmean)
margMeansExp5$asymp.LCL <- log2prob(margMeansExp5$asymp.LCL)
margMeansExp5$asymp.UCL <- log2prob(margMeansExp5$asymp.UCL)

# Refitting model 9 to all participants
exp5AllLong$aware_response_fact <- factor(exp5AllLong$aware_response)
exp5AllLong$trial_response_fact <- factor(exp5AllLong$trial_response)

exp5Model9all <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                         (1 | session_id) + (1 | prime), family = "binomial", exp5AllLong)
emmeansFitExp5all <- emmeans(exp5Model9all, ~ block_name | trial_response_fact * amp_target)
pairs(emmeansFitExp5all, adjust = "none")

# Refitting model 9 to participants from English-speaking countries
exp5Model9eng <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                         (1 | session_id) + (1 | prime),
                       family = "binomial", exp5long[exp5long$session_id %in% exp5$session_id[exp5$citizenship %in% engCount], ])
emmeansFitExp5eng <- emmeans(exp5Model9eng, ~ block_name | trial_response_fact * amp_target)
pairs(emmeansFitExp5eng, adjust = "none")


exp5sum <- with(exp5long, tapply(aware_response, list(session_id, block_name, trial_response, amp_target), mean))

pdf(paste0(citation, " Exp. 5.pdf"), width = 12)
beanplot(exp5sum[, , 2, 1][, "Positive"],
         exp5sum[, , 1, 1][, "Positive"],
         exp5sum[, , 2, 1][, "Negative"],
         exp5sum[, , 1, 1][, "Negative"],
         exp5sum[, , 2, 2][, "Positive"],
         exp5sum[, , 1, 2][, "Positive"],
         exp5sum[, , 2, 2][, "Negative"],
         exp5sum[, , 1, 2][, "Negative"],
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 0, cutmax = 1,
         col = list(rainbow(4, alpha = 0.30)[1], rainbow(4, alpha = 0.30)[4]),
         border = "black",
         main = "Awareness response by prime, response, and AMP type\nExp. 5",
         axes = FALSE, bw = 0.2, ylab = "P(aware)",
         ylim = c(0, 1.2), xlab = "Response")
abline(h = 0.5, lty = 3)
axis(1, at = 1:2, labels = c("Pleasant", "Unpleasant"))
axis(1, at = 3:4, labels = c("Pleasant", "Unpleasant"))
axis(1, at = 5:6, labels = c("Pleasant", "Unpleasant"))
axis(1, at = 7:8, labels = c("Pleasant", "Unpleasant"))
axis(2, at = seq(0, 1, by = 0.2))
abline(v = 4.5)
text(2.5, 1.2, "First-person AMP")
text(1.5, 1.1, "Positive prime")
text(3.5, 1.1, "Negative prime")
text(6.5, 1.2, "Third-party AMP")
text(5.5, 1.1, "Positive prime")
text(7.5, 1.1, "Negative prime")
dev.off()

# First-person and third-party awareness judgments
cor.test(exp5$shareAwareSelf, exp5$shareAwareThird)
cor.test(exp5$ampDiffSelf, exp5$ampDiffThird)

# Exploratory analyses

exp5$influence_own <- as.numeric(paste(exp5$influence_own))
hist(exp5$influence_own, col = "white")
mean(exp5$influence_own, na.rm = TRUE)
median(exp5$influence_own, na.rm = TRUE)

# Exp. 6 ------------------------------------------------------------------

exp6All <- read.csv(paste(citation, "Exp. 6.csv"))
exp6AllLong <- read.csv(paste(citation, "Exp. 6 Long-format AMP.csv"))

# Participant exclusions

nrow(exp6All)
table(exp6All$all_same, useNA = "always")
table(exp6All$complete_amp, useNA = "always")

exp6 <- exp6All[exp6All$all_same == 0 & exp6All$complete_amp == 1, ]
exp6long <- exp6AllLong[exp6AllLong$session_id %in% exp6$session_id, ]

# Demographics

orderedTable(exp6$gender)
prop.table(orderedTable(exp6$gender))

mean(exp6$age, na.rm = TRUE)
sd(exp6$age, na.rm = TRUE)

# Descriptive statistics
mean(exp6$ampDiff)
median(exp6$ampDiff)
sd(exp6$ampDiff)
table(exp6$ampDiff > 0)
prop.table(table(exp6$ampDiff > 0))
hist(exp6$ampDiff, col = "white", main = "Distribution of AMP scores", xlab = "AMP score")

mean(exp6$shareAware)
median(exp6$shareAware)
sd(exp6$shareAware)
hist(exp6$shareAware, col = "white", main = "Distribution of awareness responses", xlab = "Awareness response")

tapply(exp6long$aware_response, list(exp6long$block_name, exp6long$trial_response), mean)

plot(exp6$shareAware, exp6$ampDiff)
abline(lm(exp6$ampDiff ~ exp6$shareAware), col = "red")
cor(exp6$shareAware, exp6$ampDiff)

# Trial-level analysis

exp6long$aware_response_fact <- factor(exp6long$aware_response)
exp6long$trial_response_fact <- factor(exp6long$trial_response)

# (M1) Intercept only; random intercepts for participants
exp6Model1 <- glmer(aware_response_fact ~ 1 + (1 | session_id), family = "binomial", exp6long)

# (M2) Intercept only; random intercepts for participants and target stimuli
exp6Model2 <- glmer(aware_response_fact ~ 1 + (1 | session_id) + (1 | target), family = "binomial",
                    exp6long)
anova(exp6Model1, exp6Model2)

# (M3) Intercept only; random intercepts for participants and prime stimuli
exp6Model3 <- glmer(aware_response_fact ~ 1 + (1 | session_id) + (1 | prime),
                    family = "binomial", exp6long)
anova(exp6Model1, exp6Model3)

# (M4) Main effect for prime type; random intercepts for participants
exp6Model4 <- glmer(aware_response_fact ~ block_name + (1 | session_id),
                    family = "binomial", exp6long)
anova(exp6Model1, exp6Model4)

# (M5) Main effect for response; random intercepts for participants
exp6Model5 <- glmer(aware_response_fact ~ trial_response_fact + (1 | session_id),
                    family = "binomial", exp6long)
anova(exp6Model1, exp6Model5)

# (M6) Prime Type × Response interaction; random intercepts for participants
exp6Model6 <- glmer(aware_response_fact ~ block_name * trial_response_fact + (1 | session_id),
                    family = "binomial", exp6long)
anova(exp6Model1, exp6Model6)

emmeansFitExp6 <- emmeans(exp6Model6, ~ trial_response_fact | block_name)
pairs(emmeansFitExp6, adjust = "none")

margMeansExp6 <- data.frame(emmeansFitExp6)
margMeansExp6 <- margMeansExp6[c(4, 2, 3, 1), ]
margMeansExp6$emmean <- log2prob(margMeansExp6$emmean)
margMeansExp6$asymp.LCL <- log2prob(margMeansExp6$asymp.LCL)
margMeansExp6$asymp.UCL <- log2prob(margMeansExp6$asymp.UCL)

# Refitting model 6 to all participants
exp6AllLong$aware_response_fact <- factor(exp6AllLong$aware_response)
exp6AllLong$trial_response_fact <- factor(exp6AllLong$trial_response)

exp6Model6all <- glmer(aware_response_fact ~ block_name * trial_response_fact + (1 | session_id),
                       family = "binomial", exp6AllLong)

emmeansFitExp6all <- emmeans(exp6Model6all, ~ block_name | trial_response_fact)
pairs(emmeansFitExp6all, adjust = "none")

exp6sum <- with(exp6long, tapply(aware_response, list(session_id, block_name, trial_response), mean))

pdf(paste0(citation, " Exp. 6.pdf"), width = 12)
beanplot(exp6sum[, , 2][, "Positive"],
         exp6sum[, , 1][, "Positive"],
         exp6sum[, , 2][, "Negative"],
         exp6sum[, , 1][, "Negative"],
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 0, cutmax = 1,
         col = list(rainbow(4, alpha = 0.30)[1], rainbow(4, alpha = 0.30)[4]),
         border = "black",
         main = "Awareness by prime and response\nExp. 6",
         axes = FALSE, bw = 0.2, ylab = "P(aware)",
         ylim = c(0, 1.2), xlab = "Response")
abline(h = 0.5, lty = 3)
axis(1, at = 1:2, labels = c("Pleasant", "Unpleasant"))
axis(1, at = 3:4, labels = c("Pleasant", "Unpleasant"))
axis(2, at = seq(0, 1, by = 0.2))
abline(v = 2.5)
text(1.5, 1.1, "Positive primes")
text(3.5, 1.1, "Negative primes")
dev.off()

# Exploratory analyses

exp6$prime_present <- as.numeric(paste(exp6$prime_present))
hist(exp6$prime_present, col = "white")
mean(exp6$prime_present, na.rm = TRUE)
median(exp6$prime_present, na.rm = TRUE)

exp6$target_present <- as.numeric(paste(exp6$target_present))
hist(exp6$target_present, col = "white")
mean(exp6$target_present, na.rm = TRUE)
median(exp6$target_present, na.rm = TRUE)

exp6$influence <- as.numeric(paste(exp6$influence))
hist(exp6$influence, col = "white")

plot(exp6$influence, exp6$shareAware)
cor.test(exp6$influence, exp6$shareAware)

exp6$influence_unintentional <- as.numeric(paste(exp6$influence_unintentional))
hist(exp6$influence_unintentional, col = "white")

exp6$influence_intentional <- as.numeric(paste(exp6$influence_intentional))
hist(exp6$influence_intentional, col = "white")

cor.test(exp6$influence_unintentional, exp6$influence_intentional)


# Exp. 7 ------------------------------------------------------------------

exp7AllLong <- read.csv(paste(citation, "Exp. 7 Long-format AMP.csv"))

# Participant exclusions

nrow(exp7AllLong[match(unique(exp7AllLong$session_id), exp7AllLong$session_id), ])
table(exp7AllLong$attCheckPass[match(unique(exp7AllLong$session_id), exp7AllLong$session_id)], useNA = "always")
table(exp7AllLong$all_same[match(unique(exp7AllLong$session_id), exp7AllLong$session_id)], useNA = "always")
table(exp7AllLong$chinese[match(unique(exp7AllLong$session_id), exp7AllLong$session_id)], useNA = "always")

exp7long <- exp7AllLong[exp7AllLong$attCheckPass == 1 & exp7AllLong$all_same == 0 & exp7AllLong$chinese == 0, ]
nrow(exp7long[match(unique(exp7long$session_id), exp7long$session_id), ])

# Demographics
orderedTable(exp7long[match(unique(exp7long$session_id), exp7long$session_id), ]$gender)
prop.table(orderedTable(exp7long[match(unique(exp7long$session_id), exp7long$session_id), ]$gender))

mean(exp7long[match(unique(exp7long$session_id), exp7long$session_id), ]$age, na.rm = TRUE)
sd(exp7long[match(unique(exp7long$session_id), exp7long$session_id), ]$age, na.rm = TRUE)

# Descriptive statistics
round(tapply(exp7long$trial_response, exp7long$block_name, mean), 3)
round(tapply(exp7long$trial_response, exp7long$block_name, sd), 3)

round(tapply(exp7long$aware_response, list(exp7long$block_name, exp7long$aware_type), mean), 2)
round(tapply(exp7long$aware_response, list(exp7long$block_name, exp7long$aware_type), sd), 2)

# Trial-level analysis

exp7long$aware_response_fact <- factor(exp7long$aware_response)

# (M1) Intercept only; random intercepts for participants
exp7Model1 <- lmer(aware_response ~ 1 + (1 | session_id), exp7long)

# (M2) Intercept only; random intercepts for participants and prime stimuli
exp7Model2 <- lmer(aware_response ~ 1 + (1 | session_id) + (1 | prime), exp7long)
anova(exp7Model1, exp7Model2)

# (M3) Main effect for prime type; random intercepts for participants and prime stimuli
exp7Model3 <- lmer(aware_response ~ block_name + (1 | session_id) + (1 | prime), exp7long)
anova(exp7Model2, exp7Model3)

# (M4) Main effect for prime type and response type; random intercepts for participants and prime stimuli
exp7Model4 <- lmer(aware_response ~ block_name + aware_type + (1 | session_id) + (1 | prime),
                    exp7long)
anova(exp7Model3, exp7Model4)

# (M5) Prime Type × Response Type interaction; random intercepts for participants and prime stimuli
exp7Model5 <- lmer(aware_response ~ block_name * aware_type + (1 | session_id) + (1 | prime),
                    exp7long)
anova(exp7Model4, exp7Model5)

emmeansFitExp7 <- emmeans(exp7Model5, ~ block_name | aware_type)
pairs(emmeansFitExp7, adjust = "none")

margMeansExp7 <- data.frame(emmeansFitExp7)
margMeansExp7 <- margMeansExp7[c(3:1, 6:4, 9:7), ]

# Refitting model 5 to all participants
exp7Model5all <- lmer(aware_response ~ block_name * aware_type + (1 | session_id) + (1 | prime),
                   exp7AllLong)
emmeansFitExp7all <- emmeans(exp7Model5all, ~ block_name | aware_type)
pairs(emmeansFitExp7all, adjust = "none")

exp7sum <- with(exp7long, tapply(aware_response, list(session_id, block_name, aware_type), mean))

pdf(paste0(citation, " Exp. 7.pdf"), width = 12)
beanplot(exp7sum[, , 1][, "Neu"],
         exp7sum[, , 1][, "NegLo"],
         exp7sum[, , 1][, "NegHi"],
         exp7sum[, , 3][, "Neu"],
         exp7sum[, , 3][, "NegLo"],
         exp7sum[, , 3][, "NegHi"],
         exp7sum[, , 2][, "Neu"],
         exp7sum[, , 2][, "NegLo"],
         exp7sum[, , 2][, "NegHi"],
         log = "",
         what = c(0, 1, 1, 0), beanlinewd = 1,
         cutmin = 1, cutmax = 4,
         col = list(rainbow(6, alpha = 0.30)[1], rainbow(6, alpha = 0.30)[3], rainbow(6, alpha = 0.30)[5]),
         border = "black",
         main = "Awareness by prime type and rating type\nExp. 7",
         axes = FALSE, bw = 0.2, ylab = "Awareness",
         ylim = c(1, 4.2), xlab = "Prime type")
abline(h = 2.5, lty = 3)
axis(1, at = 1:3, labels = c("Neutral", "Mild", "Extreme"))
axis(1, at = 4:6, labels = c("Neutral", "Mild", "Extreme"))
axis(1, at = 7:9, labels = c("Neutral", "Mild", "Extreme"))
axis(2, at = seq(1, 4, by = 1))
abline(v = 3.5)
abline(v = 6.5)
text(2, 4.1, "Prime influence")
text(5, 4.1, "Unpleasant target influence")
text(8, 4.1, "Pleasant target influence")
dev.off()

# Rerunning analyses using ANOVA approach
exp7prime <- data.frame(with(exp7long[exp7long$aware_type == "prime", ], tapply(aware_response, list(session_id, block_name), mean)))
exp7target_pleasant <- data.frame(with(exp7long[exp7long$aware_type == "target pleasant", ], tapply(aware_response, list(session_id, block_name), mean)))
exp7target_unpleasant <- data.frame(with(exp7long[exp7long$aware_type == "target unpleasant", ], tapply(aware_response, list(session_id, block_name), mean)))

exp7prime$aware_type <- "prime"
exp7target_pleasant$aware_type <- "target_pleasant"
exp7target_unpleasant$aware_type <- "target_unpleasant"

exp7prime$session_id <- paste(rownames(exp7prime))
exp7target_pleasant$session_id <- paste(rownames(exp7target_pleasant))
exp7target_unpleasant$session_id <- paste(rownames(exp7target_unpleasant))

exp7summary <- rbind(exp7prime, exp7target_pleasant, exp7target_unpleasant)
rownames(exp7summary) <- 1:nrow(exp7summary)

exp7summary <- reshape2::melt(exp7summary, id.vars = c("session_id", "aware_type"))

anova_test(data = exp7summary, dv = value, wid = session_id, within = variable, between = aware_type)
get_anova_table(anova_test(data = exp7summary, dv = value, wid = session_id, within = variable, between = aware_type))

exp7summaryPrime <- exp7summary[exp7summary$aware_type == "prime", ]
exp7summaryPrime$aware_type <- factor(exp7summaryPrime$aware_type)
anova_test(data = exp7summaryPrime, dv = value, wid = session_id, within = variable)
get_anova_table(anova_test(data = exp7summaryPrime, dv = value, wid = session_id, within = variable))

exp7summaryTarget <- exp7summary[grepl("target", exp7summary$aware_type), ]
exp7summaryTarget$aware_type <- factor(exp7summaryTarget$aware_type)
anova_test(data = exp7summaryTarget, dv = value, wid = session_id, within = c(variable, aware_type))
get_anova_table(anova_test(data = exp7summaryTarget, dv = value, wid = session_id, within = c(variable, aware_type)))


# Secondary analyses: Hughes et al. (2022), Exp. 5 ------------------------------------------------------

HCH5 <- read.csv("Hughes et al. (2022) Exp 5.csv")
head(HCH5)

cor.test(HCH5$influence_rate_obama_trump, HCH5$influence_rate_positive_negative)

cor(HCH5[, c("influence_rate_obama_trump", "influence_rate_positive_negative", "IA_AMP_effect_obama_trump",
             "IA_AMP_effect_positive_negative")], use = "pairwise.complete.obs")

summary(lm(IA_AMP_effect_positive_negative ~ influence_rate_obama_trump + influence_rate_positive_negative, data = HCH5))
summary(lm(IA_AMP_effect_obama_trump ~ influence_rate_obama_trump + influence_rate_positive_negative, data = HCH5))

cor.test(HCH5$IA_AMP_effect_positive_negative, HCH5$influence_rate_positive_negative)
cor.test(HCH5$IA_AMP_effect_obama_trump, HCH5$influence_rate_obama_trump)

cocor(~ IA_AMP_effect_positive_negative + influence_rate_positive_negative | IA_AMP_effect_obama_trump + influence_rate_obama_trump, HCH5)


mean(HCH5$IA_AMP_effect_obama_trump, na.rm = TRUE)
mean(HCH5$IA_AMP_effect_obama_trump_influenced, na.rm = TRUE)
mean(HCH5$IA_AMP_effect_obama_trump_uninfluenced, na.rm = TRUE)

# Secondary analyses: Morris & Kurdi (2022), Exp. 3 ------------------------------------------------------

MK3 <- read.csv("Morris & Kurdi (2022) Exp. 3.csv")

head(MK3)

MK3 <- MK3[, colnames(MK3) %in% c("session_id", "comp", "ampScore", "expDiff")]

length(unique(MK3$comp))

MK3wide <- dcast(MK3, session_id ~ comp, value.var = "ampScore")
MK3wide

cor(MK3wide[, -1], use = "pairwise.complete.obs")
corImp <- cor(MK3wide[, -1], use = "pairwise.complete.obs")[lower.tri(cor(MK3wide[, -1], use = "pairwise.complete.obs"))]
hist(corImp, col = "white")
mean(corImp)
cohensD(corImp)
1/ttestBF(corImp)
t.test(corImp)

MK3wideExp <- dcast(MK3, session_id ~ comp, value.var = "expDiff")
MK3wideExp

cor(MK3wideExp[, -1], use = "pairwise.complete.obs")
corExp <- cor(MK3wideExp[, -1], use = "pairwise.complete.obs")[lower.tri(cor(MK3wideExp[, -1], use = "pairwise.complete.obs"))]
hist(corExp, col = "white")
mean(corExp)
cohensD(corExp)

t.test(corImp, corExp, paired = TRUE)
1/ttestBF(corImp, corExp, type = "paired")
cohensD(corImp, corExp, method = "paired")

MK3$ampScoreAbs <- abs(MK3$ampScore)
MK3$expDiffAbs <- abs(MK3$expDiff)

impAbsMean <- tapply(MK3$ampScoreAbs, MK3$session_id, mean)
expAbsMean <- tapply(MK3$expDiffAbs, MK3$session_id, mean)

cor.test(impAbsMean, expAbsMean)

impAbsMean12 <- tapply(MK3$ampScoreAbs, MK3$session_id, function(x) mean(x[1:2]))
expAbsMean35 <- tapply(MK3$expDiffAbs, MK3$session_id, function(x) mean(x[3:5]))

cor.test(impAbsMean12, expAbsMean35)

mySample <- combn(5, 2)
myCors <- vector("list", ncol(mySample))

for (i in 1:length(myCors)) {
  impAbsMeanSub <- tapply(MK3$ampScoreAbs, MK3$session_id, function(x) mean(x[mySample[, i]]))
  expAbsMeanSub <- tapply(MK3$expDiffAbs, MK3$session_id, function(x) mean(x[c(1:5)[!(c(1:5) %in% mySample[, i])]]))
  
  myCors[[i]] <- cor.test(impAbsMeanSub, expAbsMeanSub)
}

mySample <- combn(5, 2)
myCors2 <- vector("list", ncol(mySample))

for (i in 1:length(myCors)) {
  impAbsMeanSub <- tapply(MK3$ampScoreAbs, MK3$session_id, function(x) mean(x[c(1:5)[!(c(1:5) %in% mySample[, i])]]))
  expAbsMeanSub <- tapply(MK3$expDiffAbs, MK3$session_id, function(x) mean(x[mySample[, i]]))
  
  myCors2[[i]] <- cor.test(impAbsMeanSub, expAbsMeanSub)
}

meanCors <- c(unlist(lapply(myCors, function(x) x$estimate)), unlist(lapply(myCors2, function(x) x$estimate)))
mean(meanCors)


# Exploratory analysis: Refitting mixed-effects models with maximal random effects structure --------

# Exp. 1
exp1MaxModel <- glmer(trial_response ~ block_name * aware_response_fact +
                        (block_name * aware_response_fact | session_id) +
                        (1 | target) + (1 | prime),
                      family = "binomial", exp1long,
                      control = glmerControl(calc.derivs = FALSE))
summary(exp1MaxModel)
plot(allEffects(exp1MaxModel))

emmeansFitExp1
emmeansFitMaxExp1 <- emmeans(exp1MaxModel, ~ block_name | aware_response_fact)

pairs(emmeansFitExp1, adjust = "none")
pairs(emmeansFitMaxExp1, adjust = "none")

# Exp. 2
exp2MaxModel <- glmer(trial_response ~ block_name * aware_response_fact * cond +
                        (block_name * aware_response_fact * cond | session_id) +
                        (1 | target) + (1 | prime),
                    family = "binomial", exp2long,
                    control = glmerControl(calc.derivs = FALSE))

exp2MaxModel2 <- glmer(trial_response ~ block_name * aware_response_fact * cond +
                        (block_name * aware_response_fact * cond || session_id) +
                        (1 | target) + (1 | prime),
                      family = "binomial", exp2long,
                      control = glmerControl(calc.derivs = FALSE))

summary(exp2MaxModel2)
summary(rePCA(exp2MaxModel2))

exp2MaxModel3 <- glmer(trial_response ~ block_name * aware_response_fact * cond +
                         (block_name * aware_response_fact + block_name * cond + aware_response_fact * cond || session_id) +
                         (1 | target) + (1 | prime),
                       family = "binomial", exp2long,
                       control = glmerControl(calc.derivs = FALSE))

summary(exp2MaxModel3)
summary(rePCA(exp2MaxModel3))

exp2MaxModel3b <- glmer(trial_response ~ block_name * aware_response_fact * cond +
                         (block_name * aware_response_fact + block_name * cond + aware_response_fact * cond || session_id) +
                         (1 | target) + (1 | prime),
                       family = "binomial", exp2long,
                       control = glmerControl(calc.derivs = FALSE,
                        optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))


summary(exp2MaxModel3b)
plot(allEffects(exp2MaxModel3b))

emmeansFitExp2
emmeansFitMaxExp2 <- emmeans(exp2MaxModel3b, ~ block_name | aware_response_fact * cond)

pairs(emmeansFitExp2, adjust = "none")
pairs(emmeansFitMaxExp2, adjust = "none")

# Exp. 3
exp3MaxModel <- glmer(trial_response ~ block_name * aware_response_fact +
                        (block_name * aware_response_fact | session_id) +
                        (1 | target) + (1 | prime),
                    family = "binomial", exp3long,
                    control = glmerControl(calc.derivs = FALSE))
summary(exp3MaxModel)
plot(allEffects(exp3MaxModel))

emmeansFitExp3
emmeansFitMaxExp3 <- emmeans(exp3MaxModel, ~ block_name | aware_response_fact)

pairs(emmeansFitExp3, adjust = "none")
pairs(emmeansFitMaxExp3, adjust = "none")

# Exp. 4
# N/A, the model is additive and not linear

# Exp. 5
exp5MaxModel <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                      (trial_response_fact * block_name * amp_target | session_id)
                      + (1 | prime), family = "binomial", exp5long,
                      control = glmerControl(calc.derivs = FALSE))

exp5MaxModel2 <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                        (trial_response_fact * block_name * amp_target || session_id)
                      + (1 | prime), family = "binomial", exp5long,
                      control = glmerControl(calc.derivs = FALSE))

summary(exp5MaxModel2)
summary(rePCA(exp5MaxModel2))

exp5MaxModel3 <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                         (trial_response_fact * block_name +
                            trial_response_fact * amp_target + 
                            amp_target * block_name || session_id)
                       + (1 | prime), family = "binomial", exp5long,
                       control = glmerControl(calc.derivs = FALSE))

summary(exp5MaxModel3)
summary(rePCA(exp5MaxModel3))

exp5MaxModel4 <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                         (trial_response_fact * block_name +
                            trial_response_fact * amp_target || session_id)
                       + (1 | prime), family = "binomial", exp5long,
                       control = glmerControl(calc.derivs = FALSE))

exp5MaxModel4b <- glmer(aware_response_fact ~ trial_response_fact * block_name * amp_target +
                          (trial_response_fact * block_name +
                             trial_response_fact * amp_target || session_id)
                        + (1 | prime), family = "binomial", exp5long,
                        control = glmerControl(calc.derivs = FALSE,
                        optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

summary(exp5MaxModel4b)
plot(allEffects(exp5MaxModel4b))

emmeansFitExp5
emmeansFitMaxExp5 <- emmeans(exp5MaxModel4b, ~ trial_response_fact | block_name * amp_target)

pairs(emmeansFitExp5, adjust = "none")
pairs(emmeansFitMaxExp5, adjust = "none") # One minor difference: Positive first person AMP the awareness effect is not significant

# Exp. 6
exp6MaxModel <- glmer(aware_response_fact ~ block_name * trial_response_fact
                      + (block_name * trial_response_fact | session_id),
                      family = "binomial", exp6long,
                      control = glmerControl(calc.derivs = FALSE))

exp6MaxModel1b <- glmer(aware_response_fact ~ block_name * trial_response_fact
                      + (block_name * trial_response_fact | session_id),
                      family = "binomial", exp6long,
                      control = glmerControl(calc.derivs = FALSE,
                      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

summary(exp6MaxModel1b)
plot(allEffects(exp6MaxModel1b))

emmeansFitExp6
emmeansFitMaxExp6 <- emmeans(exp6MaxModel1b, ~ trial_response_fact | block_name)

pairs(emmeansFitExp6, adjust = "none")
pairs(emmeansFitMaxExp6, adjust = "none") # Deviation: Awareness effect is not significant in the positive domain

# Exp. 7
exp7MaxModel <- lmer(aware_response ~ block_name * aware_type +
                     (block_name * aware_type | session_id) + (1 | prime),
                     exp7long)

exp7MaxModel1b <- lmer(aware_response ~ block_name * aware_type +
                      (block_name * aware_type | session_id) + (1 | prime),
                       exp7long, control = lmerControl(calc.derivs = FALSE,
                      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

exp7MaxModel2 <- lmer(aware_response ~ block_name * aware_type +
                         (block_name * aware_type || session_id) + (1 | prime),
                       exp7long, control = lmerControl(calc.derivs = FALSE,
                                                       optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(exp7MaxModel2)
summary(rePCA(exp7MaxModel2))

exp7MaxModel3 <- lmer(aware_response ~ block_name * aware_type +
                        (block_name + aware_type | session_id) + (1 | prime),
                      exp7long, control = lmerControl(calc.derivs = FALSE,
                                                      optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(exp7MaxModel3)


emmeansFitExp7
emmeansFitMaxExp7 <- emmeans(exp7MaxModel3, ~ block_name | aware_type)

pairs(emmeansFitExp7, adjust = "none")
pairs(emmeansFitMaxExp7, adjust = "none")