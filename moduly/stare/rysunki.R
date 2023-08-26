

hrbrthemes::import_roboto_condensed()

library(ggplot2)
library(hrbrthemes)

# create data
xValue <- 1:10
yValue <- cumsum(rnorm(10))
data <- data.frame(xValue,yValue)

# Plot
ggplot(data, aes(x=xValue, y=yValue)) +
  geom_line( color="#69b3a2", size=1, alpha=0.7, linetype=1) +
  theme_ipsum() +
  ggtitle("Evolution of something")


ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum()

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum_rc()

df <- data.frame(x=c(20, 25, 30), y=c(4, 4, 4), txt=c("One", "Two", "Three"))

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="This is some txt", y="This is more text",
       title="Thisy is a titlle",
       subtitle="This is a subtitley",
       caption="This is a captien") +
  theme_ipsum_rc(grid="XY") -> gg

gg_check(gg)
## Possible misspelled words in [title]: (Thisy, titlle)
## Possible misspelled words in [subtitle]: (subtitley)
## Possible misspelled words in [caption]: (captien)
library(ggplot2)
library(patchwork)

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

(p1 + p2) /(p1+p2)

library(tidyr)
library(dplyr)
df <- economics %>%
  select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -date)
head(df, 3)
## # A tibble: 3 x 3
## date variable value
## <date> <chr> <dbl>
## 1 1967-07-01 psavert 12.5
## 2 1967-08-01 psavert 12.5
## 3 1967-09-01 psavert 11.7
# Multiple line plot
ggplot(df, aes(x = date, y = value)) +
  geom_line(aes(color = variable), size = 1.2) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_ipsum()

