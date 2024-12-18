#  libraryies
library(ggplot2)
library(readxl)
library(dplyr)
library(ggprism)
library(patchwork)
library(magrittr)
library(rstatix)
library(dunn.test)
library(FSA)
library(DescTools)
library(ggpubr)
library(dplyr)
library(ggstatsplot)
library(carData)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(grid)

### import the data
data=read.csv("PvDBP-DARC Dataset - 18 March 2024.csv")
names(data)

data$log_parasite_density <- log10(data$Pv_copies + 0.01)
data$log_parasite_density <- as.numeric(data$log_parasite_density)

# normality tests

shapiro.test(data$log_parasite_density)
shapiro.test(data$Age)
data$Age_Category <- cut(data$Age, breaks = c(0, 5.1, 14.1, Inf), labels = c("≤5","6 to 14","≥15"), right = FALSE)
table(data$Age_Category)
### make the order of the age catagory 
data$Age_Category=factor(data$Age_Category,levels = c("≤5","6 to 14","≥15"))
##### re code PvDBP

data$N_PvDBP_copy_num_cat <- data$N_PvDBP_copy_num_cat %>%
  dplyr::recode("Single copy" = "Single", 
                "Multiple copies" = "2 to 3", 
                "More than 3 copies" = "More than 3")
data$N_PvDBP_copy_num_cat <- factor(data$N_PvDBP_copy_num_cat, levels = c("Single", "2 to 3", "More than 3"))


#####  site vs copy number   
kruskal_result <- kruskal.test(N_PvDBP_copy_num_cat ~ Site, data = data)
df_p=data %>% dunn_test(N_PvDBP_copy_num_cat ~ Site)
# Create a copy of the p-value column with three digits
df_p$p.adj <- sprintf("%.3f", df_p$p.adj)

##### Age group  and parasite density
## kruskal and dunn test
kruskal_result <- kruskal.test(log_parasite_density ~ Age_Category, data = data)
df_p=data %>% dunn_test(log_parasite_density ~ Age_Category)
# Create a copy of the p-value column with three digits
df_p$p.adj <- sprintf("%.3f", df_p$p.adj)
## plot 
p <- ggplot(data, aes(x = factor(Age_Category), y = log_parasite_density)) + 
geom_violin(trim = FALSE) + 
  geom_jitter(aes(group = Age_Category,color = Age_Category), width = 0.4, height = 0.1, alpha = 0.5,color="black")+ 
  geom_boxplot(width = 0.2) +
  theme_prism() +
  coord_cartesian(ylim = c(0,10)) + 
  xlab("Age in year") +
  ylab("Pv18S copies/μL(log10)") + 
  theme(legend.position = "none")+
  scale_color_grey(start = 0.1, end = 0.1, guide = FALSE)+
  scale_y_continuous(breaks = seq(0, 10, 2), labels = function(x) format(x, nsmall = 0))

# Add p-values to the plot with three digits
P1 <- p + add_pvalue(df_p, label = "p = {p.adj}",
                     y.position = c(8,9,8),
                     bracket.shorten = c(0.025, 0, 0.025))+ annotate("text", x = 0, y = max(data$log_parasite_density) + 1,
                                                                     label = "", hjust = -2.5, vjust = -10, color = "red")


#### for duffy geno type
### kruskal and dunn test
kruskal_result <- kruskal.test(log_parasite_density ~ Duffy_blood_group, data = data)
df_p=data %>% dunn_test(log_parasite_density ~ Duffy_blood_group)
# Create a copy of the p-value column with three digits
df_p$p.adj <- sprintf("%.3f", df_p$p.adj)

### plot

p <- ggplot(data, aes(x = factor(Duffy_blood_group), y = log_parasite_density)) + 
  geom_violin(trim = FALSE) + 
  geom_jitter(aes(group = Duffy_blood_group,color = Duffy_blood_group), width = 0.4, height = 0.1, alpha = 0.5,color="black")+ 
  geom_boxplot(width = 0.2) +
  theme_prism() +
  coord_cartesian(ylim = c(0,10)) + 
  xlab("Duffy genotype") +
  ylab("Pv18S copies/μL(log10)") + 
  theme(legend.position = "none")+
  scale_color_grey(start = 0.1, end = 0.1, guide = FALSE)+
  scale_y_continuous(breaks = seq(0, 10, 2), labels = function(x) format(x, nsmall = 0))

# Add p-values to the plot with three digits
P2 <- p + add_pvalue(df_p, label = "p = {p.adj}",
                     y.position = c(8,9,8),
                     bracket.shorten = c(0.025, 0, 0.025))+ annotate("text", x = 0, y = max(data$log_parasite_density) + 1,
                                                                     label = "", hjust = -2.5, vjust = -10, color = "red") 
 


###plot with PvDBP_copy_num_cat
data$N_PvDBP_copy_num_cat <- factor(data$N_PvDBP_copy_num_cat, levels = c("Single", "2 to 3", "More than 3"))
kruskal_result <- kruskal.test(log_parasite_density ~ N_PvDBP_copy_num_cat, data = data)
df_p=data %>% dunn_test(log_parasite_density ~ N_PvDBP_copy_num_cat)
# Create a copy of the p-value column with three digits
df_p$p <- sprintf("%.3f", df_p$p)

## plot
p <- ggplot(data, aes(x = factor(N_PvDBP_copy_num_cat), y = log_parasite_density)) + 
  geom_violin(trim = FALSE) + 
  geom_jitter(aes(group = N_PvDBP_copy_num_cat,color = N_PvDBP_copy_num_cat), width = 0.2, height = 0.1, alpha = 0.5,color="black")+ 
  geom_boxplot(width = 0.2) +
  theme_prism() +
  coord_cartesian(ylim = c(0,10)) + 
  xlab("PvDBP gene copy number") +
  ylab("Pv18S copies/μL(log10)") +
  theme(legend.position = "none")+
  scale_color_grey(start = 0.1, end = 0.1, guide = FALSE)+
  scale_y_continuous(breaks = seq(0, 10, 2), labels = function(x) format(x, nsmall = 0))
 

p3=p + add_pvalue(df_p, label = ("p ={p}"),
                    y.position = c(8,9,8),
                    bracket.shorten = c(0.025, 0, 0.025))+annotate("text", x = 0, y = max(data$log_parasite_density) + 1,
                                                                   label = "", hjust = -2.5, vjust = -10, color = "red") 
##### for clinical status
wilcox.test(log_parasite_density~Clin_stat,data=data)
kruskal_result <- kruskal.test(log_parasite_density ~ Clin_stat, data = data)
df_p=data %>% dunn_test(log_parasite_density ~ Clin_stat)

# Create a copy of the p-value column with three digits

df_p$p.adj <- sprintf("%.3f", df_p$p.adj)

# plot

p <- ggplot(data, aes(x = factor(Clin_stat), y = log_parasite_density)) + 
  geom_violin(trim = FALSE) + 
  geom_jitter(aes(group = Clin_stat,color = Clin_stat), width = 0.4, height = 0.1, alpha = 0.5,color="black")+ 
  #geom_jitter(aes(group = Clin_stat, color = Clin_stat), width = 0.4, height = 0.1, alpha = 0.5, fill = "transparent") +
  geom_boxplot(width = 0.2) +
  theme_prism() +
  coord_cartesian(ylim = c(0,10)) + 
  xlab("Clinical Status") +
  ylab("Pv18S copies/μL(log10)") + 
  theme(legend.position = "none")+
 scale_color_grey(start = 0.1, end = 0.1, guide = FALSE)+
  scale_y_continuous(breaks = seq(0, 10, 2), labels = function(x) format(x, nsmall = 0))
  

# Add p-values to the plot with three digits

P4 <- p + add_pvalue(df_p, label = "p = {p.adj}",
                     y.position = 9,
                     bracket.shorten = 0.025) +
  annotate("text", x = 0, y = max(data$log_parasite_density) + 1,
           label = "", hjust = -2.5, vjust = -10, color = "red")

### Specify the labels
label_A <- "(A)"
label_B <- "(B)"
label_C = "(C)"
label_D = "(D)"
# Specify the layout for the combined plot
layout <- rbind(c(1, 2), c(3,4))
# Create the combined plot
combined_plot <- grid.arrange(P4,P1, P2,p3, layout_matrix = layout)

# Add labels to the combined plot
combined_plot_with_labels <- grid.draw(combined_plot)
grid.text(label_A, x = 0.01, y = 0.97, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label_B, x = 0.5, y = 0.97, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label_C, x = 0.01, y = 0.5, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text(label_D, x = 0.5, y = 0.5, gp = gpar(fontsize = 12, fontface = "bold"))

# Display the combined plot with labels
print(combined_plot_with_labels)



