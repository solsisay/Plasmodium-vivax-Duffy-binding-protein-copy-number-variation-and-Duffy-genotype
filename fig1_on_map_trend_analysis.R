
####load packages and functions ####
library(ggplot2)
library(ggpubr)
library(prismatic)
library(ggprism)
library(reshape2)
library(cowplot)
library(gridExtra)
library(gridExtra)
library(factoextra)
library(NbClust)
library(cluster)
library(dplyr)
library(tidyr)
library(themp)
library(reshape2)
library(scales)

# Assuming you have your time series data in a CSV file

trend=read.csv("5-Years Malar Trend.csv")


## filtered the data( 2018:2022) 
filtered_data <- subset(trend, Year %in% 2018:2022)

### over data: from 2018:2022 and with 
cases_by= aggregate(Confirmed_cases~Year+Species+District,data=filtered_data,FUN=sum)
##### proportion labels for P. vivax  for each site 

total_by_y=cases_by %>%
  group_by(Year,Species,District) %>%
  summarise(total_cases = sum(Confirmed_cases))

###
adama <- total_by_y %>%
  filter(District =="Adama")%>%
  group_by(Year) %>%
  mutate(proportion = total_cases/sum(total_cases))
Arb <- total_by_y %>%
  filter(District =="Arba Minch Zuriya")%>%
  group_by(Year) %>%
  mutate(proportion = total_cases/sum(total_cases))
Batu= total_by_y %>%
  filter(District =="Batu")%>%
  group_by(Year) %>%
  mutate(proportion = total_cases/sum(total_cases))
Dilla= total_by_y %>%
  filter(District =="Dilla Town")%>%
  group_by(Year) %>%
  mutate(proportion = total_cases/sum(total_cases))
Gondar <- total_by_y %>%
  filter(District =="Gondar Zuria")%>%
  group_by(Year) %>%
  mutate(proportion = total_cases/sum(total_cases))
### merge data
merged_data <- bind_rows(adama, Arb, Batu, Dilla)

#### Create the initial bar plot with facets
### for 4 sites( Adama, Batu, Dilla , Arbaminch)
ggp_4site <- ggplot(data = merged_data, aes(x = Year)) +
  geom_bar(aes(y = total_cases, fill = Species), position = "dodge", stat = "identity",color="black",size=1) +
  #xlab("Year") + ylab("") +
  xlab(expression(italic("Year"))) +
  ylab(substitute(paste(italic('Confirmed cases')))) +
  ggtitle("")+
  coord_cartesian(ylim = c(0,5000)) +
  theme(axis.text = element_text(size = 12)) +
        #strip.text = element_blank()) +
  scale_fill_manual(values = c("#808080","darkgray"))+
  guides(fill = guide_legend(title = NULL, override.aes = list(color = "black", size = 1)))+
    facet_wrap( ~ District)
#####
# Plot the data with proportion labels for P. vivax by district
####
ggp2 <- ggp_4site +
  geom_line(data = merged_data[merged_data$Species == "P. vivax", ], aes(Year, proportion * max(merged_data$total_cases), col = "Proportion of P. vivax"), position = "dodge",
            lwd = 1) +
  geom_point(data = merged_data[merged_data$Species == "P. vivax", ], aes(Year, proportion * max(merged_data$total_cases)), col = "black",  position = "dodge",
             col = "black", shape = 16, size = 2) 

ggp2 +
    theme(axis.text = element_text(size = 16))+
  guides(fill = guide_legend(title = NULL, override.aes = list(linetype = 0)))

# Add secondary axis and update labels
ggp4 <- ggp2 +
  scale_color_manual(values = c("Proportion of P. vivax" = "black")) +
  labs(color = "", linetype = "", show.legend = TRUE) +
  #theme(axis.text = element_text(size = 16))+
  theme(axis.text = element_text(size = 12))+
        #strip.text = element_blank()) +
  scale_y_continuous(sec.axis = sec_axis(~. / max( merged_data$total_cases), name = "")) +
  theme_classic() +
  theme(legend.position = "bottom")+#, strip.text = element_blank()) +
  facet_wrap(~ District)

ggp4

##### new for Gondar
ggp_go <- ggplot(data = Gondar, aes(x = Year)) +
  geom_bar(aes(y = total_cases, fill = Species), position = "dodge", stat = "identity",color="black",size=1) +
  xlab("Year") + ylab("Confirmed Cases") +
  ggtitle("Gondar")+
  theme(axis.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#808080","darkgray"))+
  guides(fill = guide_legend(title = NULL, override.aes = list(color="black",size=1)))
  

# Plot the data with proportion labels for P. vivax by district
####
ggp2 <- ggp_go +
  geom_line(data = Gondar[Gondar$Species == "P. vivax", ], aes(Year, proportion * max(Gondar$total_cases), col = "Proportion of P. vivax"), position = "dodge",
            lwd = 1) +
  geom_point(data = Gondar[Gondar$Species == "P. vivax", ], aes(Year, proportion * max(Gondar$total_cases)), col = "black",  position = "dodge",
             col = "black", shape = 16, size = 2) 


ggp2 +
  theme(axis.text = element_text(size = 16))+
  guides(fill = guide_legend(title = NULL, override.aes = list(linetype = 0)))


# Add secondary axis and update labels
ggpG <- ggp2 +
  scale_color_manual(values = c("Proportion of P. vivax" = "black")) +
  labs(color = "", linetype = "", show.legend = TRUE) +
  theme(axis.text = element_text(size = 16))+
  scale_y_continuous(sec.axis = sec_axis(~. / max( Gondar$total_cases), name = "")) +
  labs(shape = "Species", show.legend = TRUE) +
  theme_classic()+
  theme(legend.position = "bottom") 

ggpG
#### if necessary we can combine
ggarrange(ggp4,NULL,
          ggpG)











