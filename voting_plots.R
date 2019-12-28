# voting plot
library(tidyverse)
library(ggunchained)

bt = tibble(period = c(14, 15, 16, 17, 18, 19),
            year = c(1998, 2002, 2005, 2009, 2013, 2017),
            major = c(0.81, 0.82, 0.73, 0.62, 0.80, 0.56),
            turnout = c(0.822, 0.791, 0.777, 0.708, 0.715, 0.762),
            crazy = c(0, 0, 0, 0, 0.047, 0.126),
            nation = rep("Bundestag (DEU)", 6))

an = tibble(period = c(11, 12, 13, 14, 15),
            year = c(1997, 2002, 2007, 2012, 2017),
            major = c(0.68, 0.86, 0.86, 0.82, 0.31),
            turnout = c(0.6791, 0.6442, 0.6042, 0.5722, 0.487),
            crazy = c(0.1494, 0.1143, 0.0429, 0.1360, 0.1320),
            #strog_gp = c(, 0.9459)
            nation = rep("Assemblée nationale (FRA)", 5))


sit = bind_rows(bt, an)

sit %>%
  ggplot(aes(year, turnout, color = nation)) +
  geom_point() +
  geom_path() +
  theme_janco_point() +
  scale_x_continuous(breaks = seq(1997, 2017, 1)) +
  scale_y_continuous(limits = c(0, 1) ,breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=16)) +
  labs(title = "Voter Turnout",
       subtitle = "Assemblée nationale & Bundestag (1998 - 2017)",
       y = "voter turnout \n(proportion)",
       color = NULL)

ggsave("turnout.png")


sit %>%
  ggplot(aes(year, major, color = nation)) +
  geom_point() +
  geom_path() +
  theme_janco_point() +
  scale_x_continuous(breaks = seq(1997, 2017, 1)) +
  scale_y_continuous(limits = c(0, 1) ,breaks = seq(0, 1, 0.2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=15)) +
  labs(title = "Seats won by either of the 2 major parties*",
       subtitle = "Assemblée nationale & Bundestag (1998 - 2017)",
       y = "seats won \n(proportion)",
       caption = "*DE: CDU/CSU and SPD \nFR: PS and UMP or RPR",
       color = NULL)

ggsave("major.png")


sit %>%
  ggplot(aes(year, crazy, color = nation)) +
  geom_point() +
  geom_path() +
  theme_janco_point() +
  scale_x_continuous(breaks = seq(1997, 2017, 1)) +
  scale_y_continuous(limits = c(0, 0.5) ,breaks = seq(0, 0.5, 0.1)) +
  #scale_y_continuous(limits = c(0, 0.5) ,breaks = seq(0, 0.5, 0.05)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size=16)) +
  labs(title = "Popular vote to right-wing populists*",
       subtitle = "Assemblée nationale & Bundestag (1998 - 2017)",
       y = "popular vote \n(proportion)",
       caption = "*DE: AfD\nFR: FN ",
       color = NULL) 

ggsave("crazy.png")


# barplot
bt %>%
  ggplot(aes(period, major, fill = turnout)) +
  geom_col() +
  theme_janco_bar() +
  scale_x_continuous(breaks = seq(14, 19, 1)) +
  scale_fill_continuous() +
  labs(title = "Bundestag elections 1998 - 2017",
       subtitle = "Voter turnout and ",
       x = "legislative period",
       y = "seats of two major parties \n(proportion)",
       fill = "voter turnout (%)")
