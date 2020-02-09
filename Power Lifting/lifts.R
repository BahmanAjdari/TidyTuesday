######TidyTuesdays 41
#International Powerlifting
######

#Libraries
library(tidyverse)
library(scales)
library(naniar)
library(glmnet)
library(widyr)
library(broom)

#Data into R
lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

#EDA
lifts %>%
  count(event, sort = TRUE)

lifts %>%
  count(name, sort=TRUE)

lifts%>%
  count(meet_name, sort = TRUE)
#Data Processing

lifts_processed <- lifts%>%
  separate(date, into = c("year", "month", "day"), sep = "-", remove = FALSE,convert = TRUE)%>%
  filter(event!="SB")%>%
  drop_na()%>%
  mutate(total= best3squat_kg+best3bench_kg+best3deadlift_kg)


#What are the best records?

lifts_processed%>%
  group_by(year, event, sex)%>%
  summarise(best_sq=max(best3squat_kg, na.rm = TRUE),
            best_bench=max(best3bench_kg, na.rm = TRUE),
            best_deadlift=max(best3deadlift_kg, na.rm = TRUE))%>%
  pivot_longer(cols = -c("year", "event", "sex"), names_to = "best", values_to = "value")%>%
  ggplot(aes(year,value, color=best))+
  geom_line()+
  facet_wrap(~sex)

#How much does age matter in the weight you lift?
lifts_processed %>% 
  ggplot(aes(age, total,color=bodyweight_kg))+
  geom_point(size=1)+
  facet_wrap(~sex)+
  scale_color_gradient2(guide = "colourbar",low = "orange", high = "red", mid = "navy", midpoint = 120)+
  theme(
    panel.background = element_rect(fill = "#1D2024"),
    plot.background = element_rect(fill = "#1D2024"),
    panel.grid.major.x =  element_blank(),
    panel.grid.minor.x = element_blank(),
    text = element_text(colour = "lightyellow"),
    axis.text = element_text(colour = "gold", face = "bold"),
    legend.background = element_rect(fill = "#1D2024"),
    legend.position = "right",
    legend.text = element_text(color = "lightyellow")
    )+
    labs(
      title = "How much does age matter in the weight you lift?",
      subtitle = "Shown in Females and Males",
      y = "Total Weights (Kg)",
      x = "Age",
      caption = "Design: Bahman, Data:#Tidytuesday"
    )

# linear Model

mens<- lifts_processed %>%
  filter(sex=="M")%>%
  drop_na()
womens<- lifts_processed%>%
  filter(sex=="F",
         age>26) %>%
  drop_na()
model<- lm(formula = total~age+bodyweight_kg+equipment, data = mens)
summary(model)  


model_w<- lm(formula = total~age+bodyweight_kg, data = womens)
summary(model_w) 

fit<- aov(total~equipment, data = mens)
summary(fit)
fit1<- aov(total~equipment*sex, data = lifts_processed)
summary(fit1)
plot(fit1)
levels(my_data$sex)
my_data <- lifts_processed
my_data$sex <- ordered(my_data$sex,
                         levels = c("male", "female"))
my_data %>%
  as_factor(sex)
lifts_processed %>%
  add_count(name, sort = TRUE)

dist<- dist(lifts_processed)
fit_mds<- cmdscale()