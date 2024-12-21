### library

library(dplyr)
library(readxl)
library(mgcv)
library(glmmTMB)
## data import
data=read.csv("PvDBP_DARC.csv")

### transform
data$log_parasite_density <- log10(data$Pv_copies + 0.1)
data$log_PvDBP_copy_num=log10(data$PvDBP_copy_num + 0.1)
data$Age_Category <- cut(data$Age, breaks = c(0, 5.1, 14.1, Inf), labels = c("≤5","6 to 14","≥15"), right = FALSE)
data$Age_Category=factor(data$Age_Category,levels = c("≤5","6 to 14","≥15"))
# Replace categories in Duffy_blood_group
data <- data %>%
  mutate(Duffy_blood_group = recode(Duffy_blood_group,
                                    "D+ Heterozygote" = "Heterozygote(+)",
                                    "D+ Homozygote" = "Homozygote(+)",
                                    "Duffy negative" = "Duffy negative"))
data$v=factor(data$Duffy_blood_group,levels = c("Duffy negative","Heterozygote(+)","D+ Homozygote"))

data <- data %>%
  mutate(N_PvDBP_copy_num_cat = recode(N_PvDBP_copy_num_cat,
                                     "Single copy" = "Single",
                                     "Multiple copies" = "2 to 3",
                                     "More than 3 copies" = "More than 3"))



data$N_PvDBP_copy_num_cat <- factor(data$N_PvDBP_copy_num_cat, levels = c("Single", "2 to 3", "More than 3"))



## GAM model

mod0 <- bam(log_parasite_density ~ factor(Age_Category) + factor(N_PvDBP_copy_num_cat) + factor(Duffy_blood_group) ,family = Gamma(link = "log"), data = data)

####
# Obtain coefficient estimates
coef <- coef(mod0)

# Calculate density ratio as the exponentiated difference of coefficients
density_ratio <- exp(coef)

# Calculate standard errors
se <- sqrt(diag(vcov(mod0)))

# Calculate 95% confidence intervals
ci_lower <- exp(coef - 1.96 * se)
ci_upper <- exp(coef + 1.96 * se)

# Extract p-values
p_values <- summary(mod0)$p.table[, "Pr(>|t|)"]

# Create a dataframe for density ratio, 95% CI, and p-value
results <- data.frame(Density_Ratio = density_ratio, CI_95_lower = ci_lower, CI_95_upper = ci_upper, p_value = p_values)


