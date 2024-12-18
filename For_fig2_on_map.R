
#  libraryies
library(ggplot2)
library(readxl)
library(dplyr)
library(ggprism)
library(rstatix)
library(ggpubr)
library(dplyr)
library(ggstatsplot)
library(carData)
library(ggpubr)
library(rstatix)
library(gridExtra)
library(grid)

#### bar chart for Fig 2
stack=read_excel("CNV and duffy.xlsx")

# Convert PvDBP_copy to a factor with the desired order
stack$PvDBP_copy <- factor(stack$PvDBP_copy, levels = c("Single", "2 to 3",">3"))
# Stacked
adama= stack %>%
  filter(site=="Adama")
Arb= stack %>%
  filter(site=="Arba Minch")
batu= stack %>%
  filter(site=="Batu")
dilla= stack %>%
  filter(site=="Dilla")
gondar= stack %>%
  filter(site=="Gondar Zuria")
plot_adama=ggplot(adama, aes(fill = Duffy_blood, y = percent_1, x = PvDBP_copy)) + 
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#FED976", "#FB6A4A", "#993404")) +
  #facet_wrap(~site)+
  ylim(0,100)+
  ggtitle("Adama") +
  theme_classic() +
  xlab("") +
  ylab("Prportion ,%") +
  guides(fill = FALSE)
plot_arb=ggplot(Arb, aes(fill = Duffy_blood, y = percent_1, x = PvDBP_copy)) + 
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#FED976", "#FB6A4A", "#993404")) +
  #facet_wrap(~site)+
  ylim(0,100)+
  ggtitle("Arba Minch Zuriya") +
  theme_classic() +
  xlab("") +
  ylab("Prportion ,%") +
  guides(fill = FALSE)
####
plot_batu=ggplot(batu, aes(fill = Duffy_blood, y = percent_1, x = PvDBP_copy)) + 
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#FED976", "#FB6A4A", "#993404")) +
  #facet_wrap(~site)+
  ylim(0,100)+
  ggtitle("Batu") +
  theme_classic() +
  xlab("") +
  ylab("Prportion ,%") +
  guides(fill = FALSE)
###
plot_dilla=ggplot(dilla, aes(fill = Duffy_blood, y = percent_1, x = PvDBP_copy)) + 
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#FED976", "#FB6A4A", "#993404")) +
  #facet_wrap(~site)+
  ylim(0,100)+
  ggtitle("Dilla Town") +
  theme_classic() +
  xlab("") +
  ylab("Prportion ,%") +
  guides(fill = FALSE)
####
plot_gondar=ggplot(gondar, aes(fill = Duffy_blood, y = percent_1, x = PvDBP_copy)) + 
  #geom_bar(position = "stack", stat = "identity") +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#FED976", "#FB6A4A", "#993404")) +
  #facet_wrap(~site)+
  ylim(0,100)+
  ggtitle("Gondar") +
  theme_classic() +
  xlab("") +
  ylab("Prportion ,%") +
  guides(fill = FALSE)
# Save the ggplot object
ggsave("D:\\New_eshe_file\\Eshtu_revision\\Revised\\plot\\adama.tiff", plot = last_plot(), width =1.7, height = 1.5, dpi = 300, device = "tiff")
