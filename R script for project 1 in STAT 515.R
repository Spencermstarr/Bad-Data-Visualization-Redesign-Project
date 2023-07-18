# STAT515 Redesign Project R script

# set working directory 
getwd()
setwd('/Users/spenc/OneDrive/Documents/George Mason University/Fall 2020/STAT 515/Project 1')


library(tidyverse)
library(ggplot2)
library(lubridate)


# Load the data set from the csv file with monthly LFPR data 
# for both men and women from January of 1965 to 2020 into RStudio. 
# B stands for both.
B <- read.csv("LFPR for Men & Women in the US from 1965 to 2020.csv")
B$DATE <- mdy(B$DATE)
str(B)
ggplot(data = B, aes(x = Year)) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = LFPR.for.Men, color = "blue"), 
              size = 0.7, span = 0.1) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = LFPR.for.Women, color = "red"), 
              size = 0.7, span = 0.1) +
  scale_color_identity(name = "Legend", breaks = c("blue", "red"), 
                       labels = c("Men", "Women"), guide = "legend") +
  scale_x_continuous(breaks = seq(1970, 2010, 5), 
                     minor_breaks = seq(1970, 2010, 5)) +
  scale_y_continuous(breaks = seq(40, 80, 5), 
                     minor_breaks = seq(40, 80, 5)) +
  coord_cartesian(xlim = c(1970, 2012), ylim = c(40, 80)) +
  labs(x = 'Note: the vertical grey bars represent recessions', y = 'Rate (%)',
       title = 'US Labor Force Participation Rate of Men and Women',
       caption=expression(paste('Sources: U.S. Census Bureau/BLS Current Population Survey, St. Louis Fed'))) +
  theme_light()+ 
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.caption = element_text(size = 8))


#calculate a 12 month moving averages
B$MA12mLFPRm <- TTR::SMA(B$LFPR.for.Men, n = 12)
B$MA12mLFPRw <- TTR::SMA(B$LFPR.for.Women, n = 12)
str(B)
tail(B)

ggplot(data = B, aes(x = Year)) +
  geom_line(aes(y = MA12mLFPRm, color = "blue")) +
  geom_line(aes(y = MA12mLFPRw, color = "red")) +
  scale_color_identity(name = "Legend", breaks = c("blue", "red"), 
                       labels = c("Men", "Women"), guide = "legend") +
  scale_x_continuous(breaks = seq(1970, 2010, 5), 
                     minor_breaks = seq(1970, 2010, 5)) +
  scale_y_continuous(breaks = seq(40, 80, 5), 
                     minor_breaks = seq(40, 80, 5)) +
  coord_cartesian(xlim = c(1970, 2012), ylim = c(40, 80)) +
  labs(x = "", y = "Rate (%)",
       title = "US Labor Force Participation Rate of Men and Women",
       caption=expression(paste("Sources: U.S. Census Bureau/BLS Current Population Survey, St. Louis Fed"))) +
  theme_light() + 
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 9, face = "bold"),
        axis.title = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.caption = element_text(size = 7))


# final version of my redesign of "Chart 2", a combination of
# the previous two, a smoothed plot of the 12 month moving averages 
ggplot(data = B, aes(x = Year)) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = MA12mLFPRm, color = "blue"), 
              size = 0.7, span = 0.1) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = MA12mLFPRw, color = "red"), 
              size = 0.7, span = 0.1) +
  scale_color_identity(name = "Legend", breaks = c("blue", "red"), 
                       labels = c("Men", "Women"), guide = "legend") +
  scale_x_continuous(breaks = seq(1970, 2020, 5), 
                     minor_breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(breaks = seq(40, 80, 5), 
                     minor_breaks = seq(40, 80, 5)) +
  coord_cartesian(xlim = c(1970, 2018), ylim = c(40, 80)) +
  labs(x = "", y = "Rate (%)",
       title = "US Labor Force Participation Rate of Men and Women vs Time",
       caption=expression(paste("Sources: U.S. Census Bureau/BLS Current Population Survey, St. Louis Fed"))) +
  theme_light()+ 
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        plot.caption = element_text(size = 8))


# everything the same as the previous version except no legend
pl <- ggplot(data = B, aes(x = Year)) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = MA12mLFPRm, color = "red"), 
              size = 0.7, span = 0.1, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(y = MA12mLFPRw, color = "blue"), 
              size = 0.7, span = 0.1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1970, 2010, 5), 
                     minor_breaks = seq(1970, 2010, 5)) +
  scale_y_continuous(breaks = seq(40, 80, 5), 
                     minor_breaks = seq(40, 80, 5)) +
  coord_cartesian(xlim = c(1970, 2012), ylim = c(40, 80)) +
  labs(x = "", y = "Rate (%)",
       title = "US Labor Force Participation Rate of Men and Women vs Time",
       caption=expression(paste("Sources: U.S. Census Bureau/BLS Current Population Survey, St. Louis Fed"))) +
  theme_light()+ 
  theme(panel.border = element_blank(), 
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9, face = "bold"),
        axis.text = element_text(size = 8),
        plot.caption = element_text(size = 8))


pl



# Final version of my Redesign of "Chart 2", a smoothed version
# of the 12 month moving averages of the LFPR of men & women.
# Also, add a recession bars for all of the recessions over this time frame 
# because the blog post the bad graph I am redesigning came from is about
# The Great Recession's impact on labor force participation rates.
g <- ggplot(data = B) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1969-12-01"))),
                              xmax = decimal_date(as.Date(c("1970-11-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1973-11-01"))),
                              xmax = decimal_date(as.Date(c("1975-03-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1980-01-01"))),
                              xmax = decimal_date(as.Date(c("1980-07-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1981-07-01"))),
                              xmax = decimal_date(as.Date(c("1982-11-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1990-07-01"))),
                              xmax = decimal_date(as.Date(c("1991-03-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("2001-03-01"))),
                              xmax = decimal_date(as.Date(c("2001-11-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("2007-12-01"))),
                              xmax = decimal_date(as.Date(c("2009-06-01"))),
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(x = Year, y = MA12mLFPRm, color = "red"), 
              size = 0.7, span = 0.1, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, 
              aes(x = Year, y = MA12mLFPRw, color = "blue"), 
              size = 0.7, span = 0.1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1970, 2010, 5), 
                     minor_breaks = seq(1970, 2010, 5)) +
  scale_y_continuous(breaks = seq(40, 80, 5), 
                     minor_breaks = seq(40, 80, 5)) +
  coord_cartesian(xlim = c(1970, 2012), ylim = c(40, 80)) +
  labs(x = "Note: the vertical grey bars represent recessions", y = "Rate (%)",
       title = "US Labor Force Participation Rate of Men and Women vs Time",
       caption=expression(paste("Sources: U.S. Census Bureau/BLS Current Population Survey, St. Louis Fed"))) +
  theme_light()+ 
  theme(panel.border = element_blank(), 
        axis.line = element_line(color = "grey"),
        panel.grid.major.x = element_blank(), 
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 9),
        axis.text = element_text(size = 8),
        plot.caption = element_text(size = 8))

g

