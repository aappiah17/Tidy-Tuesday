# Load libraries
library(tidyverse)
library(tidytuesdayR)
devtools::install_github("rensa/ggflags")
library(ggflags)
library(gganimate)

options(scipen = 999)
# Set theme
AAtheme_anim<-theme(plot.title=element_text(face="bold", size=30, hjust = 0.5),
                    plot.subtitle=element_text(size=22, color="gray50", hjust = 0.5),
                    axis.text.y = element_text(face="bold",size=20),
                    axis.title.y = element_text(face="bold",size=20),
                    axis.title.x = element_text(face="bold",size=20),
                    axis.text.x = element_text(face="bold",size=20,hjust=0.5),
                    #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                    axis.ticks.y=element_blank(),
                    axis.ticks.x=element_blank(),
                    panel.grid.major.y = element_blank(), 
                    panel.grid.major.x = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    legend.text=element_text(size=10),
                    legend.key = element_blank(),
                    legend.title = element_blank(),
                    legend.position="none",
                    legend.direction = "horizontal",
                    plot.caption = element_text(size = 20, face = "italic"))

# Load the Tuesday September 1 data
tuesdata <- tidytuesdayR::tt_load('2020-09-01')

key_crop_yields <- tuesdata$key_crop_yields
cereal_fert <- tuesdata$cereal_crop_yield_vs_fertilizer_application %>%
  rename(cereal_yield=`Cereal yield (tonnes per hectare)`, nitrogen_use=`Nitrogen fertilizer use (kilograms per hectare)`)%>%
  filter(!is.na(Code))

# Get top 10 countries by cereal yield and 2 alpha code for ggflags later
cereal_fert_10 <- cereal_fert %>% group_by (Year) %>%
  arrange(Year, cereal_yield) %>% 
  mutate(rank=rank(-cereal_yield))%>%
  filter(rank <=10) %>% ungroup() %>%
  mutate(code_icons=case_when(Entity=="Austria"~"at",
                              Entity=="Bahamas"~"bs",
                              Entity=="Belgium"~"be",
                              Entity=="Denmark"~"dk",
                              Entity=="Dominican Republic"~"do",
                              Entity=="Egypt"~"eg",
                              Entity=="France"~"fr",
                              Entity=="French Guiana"~"gf",
                              Entity=="Germany"~"de",
                              Entity=="Hungary"~"hu",
                              Entity=="Ireland"~"ie",
                              Entity=="Japan"~"jp",
                              Entity=="Kuwait"~"kw",
                              Entity=="Mauritius"~"mu",
                              Entity=="Netherlands"~"nl",
                              Entity=="New Zealand"~"nz",
                              Entity=="North Korea"~"kp",
                              Entity=="Oman"~"om",
                              Entity=="Puerto Rico"~"pr",
                              Entity=="Qatar"~"qa",
                              Entity=="Reunion"~"re",
                              Entity=="Saint Vincent and the Grenadines"~"vc",
                              Entity=="South Korea"~"kr",
                              Entity=="Suriname"~"kr",
                              Entity=="Sweden"~"se",
                              Entity=="Switzerland"~"ch",
                              Entity=="Taiwan"~"tw",
                              Entity=="United Arab Emirates"~"ae",
                              Entity=="United Kingdom"~"gb",
                              Entity=="United States"~"us"))

# Add population data 
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production %>% 
  select(Entity, Code, Year, population=`Total population (Gapminder)`) %>%
  mutate(Year=as.numeric(Year))
plot_data <-left_join(cereal_fert_10, land_use, by=c("Entity", "Code", "Year"))

# Make the plot and animate it
p<-plot_data %>% 
  ggplot(aes(x=population,y=cereal_yield))+
  geom_flag(aes(country=code_icons),size=25)+
  scale_x_continuous(labels = comma)+
  geom_text(x= 250000000, y = 15,
            aes(label = as.character(Year)),
            size = 30, col = "grey18")+
  AAtheme_anim
anim = p + transition_time(Year)+
  labs(title = 'Comparing the Top 10 Countries in Cereal Yield with their Population over Time',  
       x="Population", y="Cereal Yield (tonnes per hectare)",
       caption = "Chart by @CallmeAlfredo | Data from Our World in Data via #TidyTuesday")
animate(anim, 100, fps = 25, end_pause = 20, width = 1400, height = 1000) 
anim_save("Charts/top10_countries_Sep1.gif")
