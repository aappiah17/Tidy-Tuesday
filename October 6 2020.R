# Load libraries
library(tidyverse)
library(tidytuesdayR)
library(ggrepel)
library(ggimage)

options(scipen = 999)
# Set theme
AAtheme_leg_1<-theme(plot.title=element_text(face="bold", size=15),
                     plot.subtitle=element_text(size=11, color="gray50"),
                     axis.text.y = element_text(face="bold",size=10),
                     axis.title.y = element_text(face="bold",size=10),
                     axis.text.x = element_text(face="bold",size=10),
                     axis.title.x = element_text(face="bold",size=10),
                     axis.ticks.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     panel.grid.major.y = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     legend.text=element_text(size=10),
                     legend.title = element_blank(),
                     legend.background =element_blank(),
                     legend.position="top",
                     legend.direction = "horizontal",
                     plot.caption = element_text(size = 9, face = "italic"))

# Load schools logo csv file 
schools_logo <- read_csv("schools.csv")
# Load the Tuesday September 1 data
tuesdata <- tidytuesdayR::tt_load('2020-10-06')
tournament <- tuesdata$tournament

# Summarize data and select schools with at least 20 seasons of data
Top_school_wins <- tournament %>%
  group_by(school)%>% filter(!is.na(tourney_w))%>%
  summarise(mean_tourney_win=mean(tourney_w), mean_reg_win=mean(reg_w), number_seasons=n())%>%
  arrange(-mean_reg_win)%>%
 filter(number_seasons >=20)%>%
  left_join(schools_logo, by="school")

# Make chart
Top_school_wins %>%
  ggplot(aes(x = mean_reg_win, y = mean_tourney_win)) +
  geom_hline(yintercept = mean(Top_school_wins$mean_tourney_win), color = "firebrick1", linetype = "dashed", size=1) +
  geom_vline(xintercept =  mean(Top_school_wins$mean_reg_win), color = "firebrick1", linetype = "dashed", size=1) +
  geom_image(aes(image = logo), size = 0.05) +
geom_text_repel(aes(label=school)) +
 annotate("text", x = 28, y = 3.6, label = "More regular season Ws, \nMore tourney Ws", color="red", fontface="bold")+
 annotate("text", x = 22.5, y = 0.4, label = "Fewer regular season Ws, \nFewer tourney Ws", color="red", fontface="bold")+
 annotate("text", x = 22.5, y = 3.8, label = "Fewer regular season Ws, \nMore tourney Ws", color="red", fontface="bold")+
 annotate("text", x = 28, y = 1, label = "More regular season Ws, \nFewer tourney Ws", color="red", fontface="bold")+
  labs(x = "Number of regular season wins per season",
       y = "Number of tournament wins per season",
       title = "Tournament versus Regular Season Wins in NCAA Women's Basketball",
       subtitle = "Includes only teams that have played in at least 20 seasons",
       caption = "Chart by @CallmeAlfredo | Data from FiveThirtyEight via #TidyTuesday") +
  AAtheme_leg_1 
ggsave("Charts/schools_wins.png", width = 8.5, height = 6.5, dpi=800)
