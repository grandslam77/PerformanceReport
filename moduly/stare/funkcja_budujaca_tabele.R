# funkcja


TableBuilder<-function(TabelaDanych,numeryKolumnGrupujacych,NazwaKolumnyMapujacej,czyMonthCzyYTD,NazwaTabeliWyjsciowej){
  
  return
  a=3
}



dfx <- data.frame(
  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
  sex = sample(c("M", "F"), size = 29, replace = TRUE),
  age = runif(n = 29, min = 18, max = 54)
)

# Note the use of the '.' function to allow
# group and sex to be used without quoting
ddply(dfx, .(group, sex), summarize,
      mean = round(mean(age), 2),
      sd = round(sd(age), 2),
      ojej = mean-sd)

dev.off()

dev.print( device = jpeg,              # what are we printing to?
           +            filename = "thisfile.png",  # name of the image file
           +            width = 480,                # how many pixels wide should it be
           +            height = 300                # how many pixels high should it be
           )

png(here::here(),"my_plot.png")

library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

# Make the histogram
data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum()

# Close device
dev.off()




library(ggplot2)
library(ggthemes)
library(extrafont)
p1 <- ggplot() + theme_bw() +
  geom_line(aes(y = export, x = year, colour = product), size=1.5, data = charts.data,
            stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  scale_x_continuous(breaks=seq(2006,2014,1)) +
  labs(x="Year", y="USD million") +
  ggtitle("Composition of Exports to China ($)") +
  scale_colour_manual(values=colour)

p1 <- ggplot() +
  geom_line(aes(y = export, x = year, colour = product), size=1.5, data = charts.data, 
            stat="identity") + 
  scale_x_continuous(breaks=seq(2006,2014,1)) + 
  labs(x="Year", y="USD million") + 
  ggtitle("Composition of Exports to China ($)") +
  theme_economist() + scale_colour_economist() +
  theme(axis.line.x = element_line(size=.5, colour = "black"),
        legend.position="bottom", 
        legend.direction="horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(family="OfficinaSanITC-Book"),
        text=element_text(family="OfficinaSanITC-Book")) 
p1

library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

# Make the histogram
data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Night price distribution of Airbnb appartements") +
  theme_ipsum()


count(mpg, class) %>% 
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(class, pct)) +
  geom_col() +
  scale_y_percent() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 column chart example with percents",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum(grid="Y")



