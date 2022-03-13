library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
setwd("~/Documents/GitHub/final-project-slemisch")
setwd("~/Documents/test")

gl <- read.csv("GLHYD_data_english.csv")
gl <- slice(gl, -(1:11))
my_names <- gl[1,]
colnames(gl) <- my_names
gl <- slice(gl, -(1))

mi <- gl[, c(1, 2, 4)]
#mi <- slice(mi, -(1))

mi <- mi %>% 
  mutate(`Michigan-Huron` = as.numeric(`Michigan-Huron`))

mi <- mi %>%
  pivot_wider(names_from = month, values_from = "Michigan-Huron") %>% 
  filter(year >= 1985) %>% 
  mutate(year = as.numeric(year))

mi$annual_mean <- rowMeans(mi[,-1])

# geom area source: https://ggplot2.tidyverse.org/reference/geom_ribbon.html
ggplot(mi, aes(x=year, y=annual_mean)) +
  geom_line(color="#72bcd4", size=0.5) +
  geom_point(size=0.5, color="#72bcd4")+
  theme_ipsum()+
  labs(title = "Lakes Michigan-Huron Elevation", subtitle = "annual average feet above IGLD 1985")+
  ylab("feet")+ 
  geom_ribbon(aes(ymin=575, ymax=annual_mean), fill="#72bcd4", alpha = 0.5)+
  geom_segment(aes(x = 1985, xend = 2021, y = 576.8, yend = 576.8), lty = 2, size = 0.2)+
  geom_segment(aes(x = 1985, xend = 2021, y = 580.8, yend = 580.8), lty = 2, size = 0.2)+
  annotate("text", x = 2021, y = 580.1, label = "OHWM", size = 2.8)+
  annotate("text", x = 2021, y = 576.1, label = "LWD", size = 2.8)

mi_2020 <- mi %>% 
  filter(year == 2020) %>% 
  pivot_longer(!year & !annual_mean, names_to = "month", values_to = "mean") %>% 
  mutate(LWD = 576.8) %>% 
  mutate(OHWM = 580.8)

write_csv(mi_2020, file = "mi_2020.csv")
write_csv(mi, file = "mi.csv")

ggplot(mi_2020, aes(x=fct_inorder(month), y=mean, group = 1)) +
  geom_line(color="#72bcd4", size=0.5) +
  geom_line(aes(x=fct_inorder(month), y=LWD), lty = 2, size = 0.2) +
  geom_text(aes(11.5, LWD, label = "LWD", hjust = 0, vjust = 2), size = 2.8)+
  geom_text(aes(11.5, OHWM, label = "OHWM", hjust = 0, vjust = 2), size = 2.8)+
  geom_line(aes(x=fct_inorder(month), y=OHWM), lty = 2, size = 0.2) +
  geom_point(size=0.5, color="#72bcd4")+
  theme_ipsum()+
  labs(title = "2020 Lakes Michigan-Huron Elevation")+
  ylab("feet")+
  xlab("year")+
  scale_y_continuous(limits = c(575, 583))+
  geom_ribbon(aes(ymin=575, ymax=mean), fill="#72bcd4", alpha = 0.5)

rain <- read_csv("miHuronPrecipLakemmAnn.csv")

rain <- rain %>% 
  filter(Year >= 2000)

ggplot(data = rain, aes(x = Year, y = Total))+
  geom_line(color = "#4B68B8")+
  ylab("millimeters")+
  xlab("year")+
  theme_ipsum()+
  geom_hline(yintercept = 797.78, lty = 2, size = 0.2)+
  annotate("text", x = 2014.5, y = 770, label = "Ann. Avg", size = 2.8)+
  labs(title = "Lakes Michigan-Huron Avg Annual Rainfall")

ice <- read.csv("ice.csv")

ice <- ice %>% 
  select(1, 2)

ice_names <- ice[1,]
colnames(ice) <- ice_names
ice <- slice(ice, -(1))
ice <- ice %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Perc = as.numeric(Perc)) %>% 
  filter(Year > 2000)

write_csv(ice, "ice_yes.csv")
write_csv(rain, "rain_yes.csv")

ggplot(data = ice, aes(x = Year, y = Perc))+
  geom_line(color = "#869cda")+
  ylab("Winter Percent Ice Cover")+
  xlab("year")+
  theme_ipsum()+
  geom_hline(yintercept = 40.2, lty = 2, size = 0.2)+
  annotate("text", x = 2021.5, y = 43, label = "LT Avg", size = 2.8)+
  labs(title = "Lake Michigan Avg Annual Ice Cover")
  

  











