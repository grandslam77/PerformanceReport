library(tidyverse)
library(pilot)
library(dplyr)
# Read in and prepare the data ------------------------------------------------

# Load the data from the csv as a dataframe
bazaQuant<-bazaPerformance[str_sub(bazaPerformance$NazwaWskaznika,1,5)=="Quant" & str_sub(bazaPerformance$NazwaWskaznika,1,7)!="Quant_Y",]

bazaQuant <- bazaQuant[bazaQuant$NumerMiesiaca<=MiesiacAnalizy,]
bazaQuant <- bazaQuant[,c(2,4,5)]

# Create the plot -------------------------------------------------------------

# Use ggplot to create a plot with data and mappings
plot <- ggplot(
  data = bazaQuant,
  mapping = aes(
    x = DataAnalizy,
    y = Wartosc,
    color = NazwaWskaznika)) +
  # Add a line geometry to draw lines
  geom_line(linewidth = 0.5) +
  # Set labels for the axes, legend, and caption, but don't set titles here
  labs(
    color = NULL,
    x = NULL,
    y = "thousands MT",
    caption = "") +
  # Configure the the x and y axes: we set the y axis breaks and limits
  scale_x_date(
    expand = c(0,3)) +
  scale_y_continuous(
    breaks = seq(100, 10000, 10000),
    limits = c(80000, 100000),
    expand = c(0, 0)) +
  # Add the pilot theme, setting a bottom axis and horizontal gridlines
  theme_pilot(
    axes = "b",
    grid = "h") +
  # Use scale_color_manual and pilot_color to set colors for each lines
  scale_color_manual(values = c(
    "Quant" = pilot_color("navy"),
    "QuantPRV" = pilot_color("blue"))) +
  # Here we use a theme customisation to overlay the legend on the plot area:
  # We could have used legend_position = "top-right" in theme_pilot
  # to put the legend at the top-right above the plot area
  theme(
    legend.position = c(1.03, 0.99),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.text = element_text(margin = margin(r = 10)))

# After creating the plot, add a title and subtitle with add_pilot_titles
plot <- add_pilot_titles(
  plot,
  title = "Total CEE results",
  subtitle = "Total CEE Results")

plot

# Save the plot in different formats ------------------------------------------

# Save a high resolution export of the plot as a png


# Save an editable verson of the plot as an svg
ggsave(
  filename = "line-chart.svg",
  plot = plot,
  width = 7.7,
  height = 5.8,
  dpi = 400)


EtykietaIlosci="QUANTITY IN MT"

Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")


bazaQuant$NazwaWskaznika[bazaQuant$NazwaWskaznika=="Quant"]<-as.character(year(dataAnalizy))
bazaQuant$NazwaWskaznika[bazaQuant$NazwaWskaznika=="QuantPRV"]<-as.character(year(dataAnalizy)-1)

z <- ggplot(data=bazaQuant,
       aes(x=as.POSIXct(DataAnalizy), y=Wartosc, colour=NazwaWskaznika)) +
  geom_line(linewidth=0.20)+
  geom_point(size=0.001)+
  scale_y_continuous(limits = c(min(bazaQuant$Wartosc), max(bazaQuant$Wartosc)+500), expand = c(0.15, 0)) +
  
  scale_x_datetime(limits = c(as.POSIXct(ymd(min(bazaQuant$DataAnalizy)) %m-% period("1 week")),as.POSIXct(ymd(max(bazaQuant$DataAnalizy)) %m+% period("1 week"))),date_labels = "%b",expand = c(0.05, 0.05),date_breaks = "1 month")+
  labs(title="TOTAL CEE QUANTITY",
       x = "PERIOD", 
       y = EtykietaIlosci,
       colour="")+

  theme_minimal(base_size = 1.5)+## output not shown, it's equivalent to the below graph (with a tiny difference in the legend title)
  geom_text(aes(label=Wartosc),hjust=0.9, vjust=-1.2,size=0.4,color="black")+
  theme(axis.title.x = element_text(margin = margin(t = 1.2)))+
  theme(axis.title.y = element_text(margin = margin(r = 1.2)))+
  theme(legend.key.size = unit(0.04, "cm"))+
  theme(legend.position = c(0.94, 1.01), plot.title = element_text(face="bold"), axis.title = element_text(face="bold"))

z

#legend.position = c(0.8, 0.2), legend.justification = "center"


ggsave(
  filename = "line-chart.png",
  plot = z ,
  width = 0.45,
  height = 0.45,
  dpi = 620)


#theme(legend.position = "bottom",


library(ggplot2)

# Tworzenie przykładowych danych
data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))

# Tworzenie wykresu punktowego z etykietami danych
ggplot(data, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = y), nudge_y = 0.2)





