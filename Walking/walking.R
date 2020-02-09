#Tidytusday project-week 45
#Bike and walk commutes
#Libraries
library(tidyverse)
library(scales)
library(ggmap)

#Read in Data
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

df<- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/table_3.csv")
# cleaning state names
commute_mode$state<-recode(commute_mode$state,
         "Ca"="California",
         "Massachusett" = "Massachusetts")

by_age<- df %>%
  slice(1:6)

by_sex<- df %>%
  slice(9:10)%>%
  rename(sex=age)

by_race <- df %>%
  slice(13:18) %>%
  rename(race=age)
  
by_child <- df %>%
  slice(21:24) %>%
  rename(child_number=age)

by_income<- df%>%
  slice(27:36) %>%
  rename(income=age)

by_edu<- df %>%
  slice(39:max(row_number())) %>%
  rename(edu=age)

#Walking to Work by Region and City Size

commute_mode %>%
  filter(mode == "Walk")%>%
  drop_na()%>%
  group_by(state_region, city_size) %>%
  summarise(pct= mean(percent, na.rm = TRUE),
            )%>%
  ggplot(aes(state_region, pct, fill=city_size))+
  geom_col(position = "dodge")+
  theme_bw()

#map first attempt
us<- map_data("state")
diff<- commute_mode %>%
  filter(!is.na(state)) %>%
  filter(state!="Alaska") %>%
  group_by(state, mode)%>%
  summarise(pct= mean(percent)) %>%
  pivot_wider(names_from = "mode", values_from = "pct") %>%
  mutate(diff=Walk-Bike)%>%
  mutate(col=case_when(between(diff,0,1.8)~ 1,
                   between(diff,1.8,4.9)~2,
                   TRUE~3)) 
#set factor colors
diff$col <- factor(diff$col, levels = c(1,2,3), labels = c("1"="Low", "2"="Medium", "3"="High"))
glimpse(diff)

#the map
ggplot()+
  geom_map(data = us, map = us, aes(long,lat,map_id=region), fill="white")+
  geom_map(data = diff, map = us, aes(fill=diff, map_id=tolower(state)), color="black", size=0.1)+
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "green",
                       midpoint = 7.5,breaks=c(0,8,16),
                       labels=c("Bikers","It depends","Walkers"),
                       limits=c(0,16))+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#1D2024"),
    plot.background = element_rect(fill = "#1D2024"),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(colour = "lightyellow"),
    legend.background = element_rect(fill = "#1D2024"),
    legend.key.height = unit(2.5,units = "line"),
    legend.justification = c(0,1),
    legend.position= c(0.85,0.4),
    plot.title = element_text(hjust = 0.02, color="gold", size = 18),
    plot.caption = element_text(hjust = 0.99, face = "italic", color = "gold")
      )+
  labs(
    fill="",
    title = "Individual preferences by states",
    subtitle = "",
    caption = "Design:Bahman | Data:US Census")

##second attempt for map with cuatomized discrete colors

#data frame for states' abb
states <- data.frame(state.center, state.abb)
#preparing the data frame for map names
states <- states %>%
  filter(state.abb !="AK")%>%
  filter(state.abb !="HI")

#pallette for factors
palette <- c("Low"= "#ffdc34", "Medium"="#4dd599", "High"="#00918e")

#second map and lots of theme customization 
ggplot()+
  geom_map(data = us, map = us, aes(long,lat,map_id=region), fill="white")+
  geom_map(data = diff, map = us, aes(fill=col, map_id=tolower(state)), color="#110133", size=0.1)+
  scale_fill_manual(values = palette)+
  geom_text(data=states, aes(x, y, label=state.abb), size=3)+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#1D2024"),
    plot.background = element_rect(fill = "#1D2024"),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(colour = "lightyellow"),
    legend.background = element_rect(fill = "#1D2024"),
    legend.key.height = unit(1.5,units = "line"),
    legend.key.width = unit(1.5, "line"),
    legend.justification = c(0,1),
    legend.position= c(0.8,0.3),
    legend.direction = "horizontal",
    plot.title = element_text(color="lightyellow", size = 16, face = "bold"),
    plot.subtitle = element_text(color="lightyellow", size = 12),
    plot.caption = element_text(hjust = 0.99, face = "italic", color = "gold")
  )+
  labs(
    fill="",
    title = "Walking to Work by State",
    subtitle = "Data based on sample. Colors show the difference of percentages 
    between individuals who prefer Walking (Walking% - Biking%)"
  )


