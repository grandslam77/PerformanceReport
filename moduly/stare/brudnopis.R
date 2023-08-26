

library(ggplot2)
library(ggpubr)
state1 <- data.frame(state=c(rep("ALABAMA",3), rep("CALIFORNIA",3)), 
                     value=c(61,94,27,10,30,77), 
                     type=rep(c("state","local","fed"),2),
                     cumSum=c(rep(182,3), rep(117,3)))
state2 <- data.frame(state=c(rep("ALABAMA",3), rep("CALIFORNIA",3)), 
                     value=c(10,30,7,61,94,27), 
                     type=rep(c("state","local","fed"),2),
                     cumSum=c(rep(117,3), rep(182,3)))
fill <- c("#40b8d0", "#b2d183", "#F9756D")

p1 <- ggplot(data = state1) +
  geom_bar(aes(x = reorder(state, value), y = value, fill = type), stat="identity") +
  theme_bw() + 
  scale_fill_manual(values=fill) + 
  labs(x="", y="Total budget in 1M$") +
  theme(legend.position="none", 
        legend.direction="horizontal", 
        legend.title = element_blank(),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  coord_flip() 

p2 <- ggplot(data = state2) +
  geom_bar(aes(x = reorder(state, value), y = value, fill = type), stat="identity") +
  theme_bw() + 
  scale_fill_manual(values=fill) + labs(x="", y="Total budget in 1M$") +
  theme(legend.position="none", 
        legend.direction="horizontal", 
        legend.title = element_blank(),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  scale_x_discrete(position = "top") + 
  scale_y_reverse() +
  coord_flip()

p3 <- ggarrange(p1, p2, common.legend = TRUE, legend = "bottom")




vs_dt <- iris[1:10, ]
vs_dt[1:4] <- lapply(vs_dt[1:4], function(x) {
  cell_spec(x, bold = T,
            color = spec_color(x, end = 0.9),
            font_size = spec_font_size(x))
})
vs_dt[5] <- cell_spec(vs_dt[[5]], color = "white", bold = T,
                      background = spec_color(1:10, end = 0.9, option = "A", direction = -1))

z <- kbl(vs_dt, booktabs = TRUE, escape = FALSE, align = "c") %>%
  kable_classic("striped", full_width = FALSE)

data("mtcars")
mtcars

z<-kbl(dt, booktabs = TRUE, align = "c") %>%
  kable_styling(latex_options = "striped", full_width = FALSE) %>%
  row_spec(0, angle = 45)

library(kableExtra)
kbl(dt)
dt<-mtcars

kbl(mtcars[1:8, 1:4], booktabs = T, linesep = "") %>%
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6))


z<-kbl(mtcars[1:8, 1:4], booktabs = TRUE, linesep = "") %>%
  kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6))


kbl(cbind(dt, dt, dt), booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down"))
require(rvest)


theme_538 <- function(..., base_size = 12) {
  
  theme(
    # plotting components
    
    ## drop minor gridlines
    panel.grid.minor = element_blank(),
    # change grid lines to gray
    panel.grid.major =  element_line(color = "#d0d0d0"),
    # fill the plot and panel spaces with grey and remove border
    panel.background = element_rect(fill = "#f0f0f0", color = NA),
    plot.background = element_rect(fill = "#f0f0f0", color = NA),
    panel.border = element_blank(),
    # remove strip background
    strip.background = element_blank(),
    # adjust the margins of plots and remove axis ticks
    plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
    axis.ticks = element_blank(),
    # change text family, size, and adjust position of titles
    text = element_text(family = "Chivo", size = base_size),
    axis.text = element_text(face = "bold", color = "grey", size = base_size),
    axis.title = element_text(face = "bold", size = rel(1.33)),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
    plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
    plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
    strip.text = element_text(size = rel(1.33), face = "bold"),
    ...
  )
}





### web scrapinng by tom mock 








raw_url <- "https://www.pro-football-reference.com/years/2020/opp.htm"

raw_html <- read_html(raw_url)

raw_table <- raw_html %>% 
  html_table(fill = TRUE) %>% 
  .[[2]] %>% 
  janitor::clean_names() %>% 
  tibble()

pressure_df <- raw_table %>% 
  select(tm, blitz_pct = bltz_percent, press_pct = prss_percent) %>% 
  mutate(across(c(blitz_pct, press_pct), parse_number))

pass_def_raw <- raw_html %>% 
  html_node("#all_passing") %>% 
  html_nodes(xpath = "comment()") %>% 
  html_text() %>% 
  read_html() %>% 
  html_node("table") %>% 
  html_table() %>% 
  janitor::clean_names() %>% 
  tibble()

pass_def_df <- pass_def_raw %>% 
  select(tm, pass_att = att, int, pass_def = pd, sack = sk, ypa = y_a, anypa = any_a)

combo_pass <- left_join(
  pressure_df, pass_def_df,
  by = "tm"
)

combo_pass 

combo_pass %>% 
  ggplot(aes(x = blitz_pct, y = press_pct)) +
  geom_point() +
  labs(
    x = "Blitz Rate", y = "Pressure Rate",
    title = "The Colts are pressuring QBs without much of a blitz",
    subtitle = "Blitz rate vs. pressure rate for each NFL defense, through Week 15 of the 2020 season"
  ) + 
  theme_538()

colt_df <- combo_pass %>% 
  mutate(
    color = if_else(tm == "Indianapolis Colts", "#359fda", "#91c390"),
    fill = colorspace::lighten(color, amount = 0.3)
  ) %>% 
  rowwise() %>% 
  mutate(
    att_def = sum(int, pass_def, sack),
    cov_rate = att_def/pass_att*100
  ) %>% 
  ungroup() %>% 
  arrange(desc(cov_rate))

colt_df %>% 
  ggplot(aes(x = blitz_pct, y = press_pct, fill = fill, color = color)) +
  geom_point(size = 5, pch = 21, alpha = 0.8) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(
    x = "Blitz Rate", y = "Pressure Rate",
    title = "The Colts are pressuring QBs without much of a blitz",
    subtitle = "Blitz rate vs. pressure rate for each NFL defense,\nthrough Week 15 of the 2020 season",
    caption = toupper("Plot: @thomas_mock | Data: PFR | Inspiration: FiveThirtyEight")
  ) +
  scale_x_continuous(limits = c(10, 45), breaks = seq(10, 45, by = 5)) +
  scale_y_continuous(limits = c(10, 35), breaks = seq(10, 35, by = 5)) +
  theme_538()


label_df_press <- tibble(
  label = c("Colts", "Everyone else"),
  color = c("#359fda", "#91c390"),
  fill = colorspace::lighten(color, amount = 0.3),
  x = c(16, 30),
  y = c(25, 29)
)

colt_df %>% 
  ggplot(aes(x = blitz_pct, y = press_pct, fill = fill, color = color)) +
  geom_point(size = 5, pch = 21, alpha = 0.8) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(
    x = "Blitz Rate", y = "Pressure Rate",
    title = "The Colts are pressuring QBs without much of a blitz",
    subtitle = "Blitz rate vs. pressure rate for each NFL defense,\nthrough Week 15 of the 2020 season",
    caption = "Source: Pro-Football-Reference.com"
  ) +
  scale_x_continuous(limits = c(10, 45), breaks = seq(10, 45, by = 5)) +
  scale_y_continuous(limits = c(10, 35), breaks = seq(10, 35, by = 5)) +
  geom_label(
    data = label_df_press,
    aes(x = x, y = y, color = color, label = label),
    fill = "#f0f0f0",
    size = 6,
    fontface = "bold",
    hjust = 0.8,
    label.size = NA # remove the border
  ) +
  theme_538()

label_df_cov <- tibble(
  label = c("Colts", "Everyone else"),
  color = c("#359fda", "#91c390"),
  fill = colorspace::lighten(color, amount = 0.3),
  x = c(16, 33),
  y = c(25, 28)
)

colt_df %>%
  ggplot(aes(x = blitz_pct, y = cov_rate, color = color, fill = fill)) +
  geom_point(size = 5, pch = 21) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(
    x = "Blitz Rate",
    y = "Pass Affected Rate",
    title = "The Colts affect passes at an elite rate while blitzing the least",
    subtitle = "Blitz rate vs. Pass affected rate for each NFL defense,\nthrough Week 15 of the 2020 season",
    caption = "Plot: @thomas_mock | Source: PFR"
  ) +
  scale_x_continuous(limits = c(10, 45), breaks = seq(10, 45, by = 5)) +
  scale_y_continuous(limits = c(10, 35), breaks = seq(10, 35, by = 5)) +
  coord_cartesian(clip = "off") +
  annotate("text", x = 10, y = 10, label = "Pass affected rate = (ints + sacks + passes defended)/pass attempts", vjust = 10, hjust = 0.2, color = "darkgrey") +
  theme_538() +
  geom_label(
    data = label_df_cov,
    aes(x = x, y = y, color = color, label = label),
    fill = "#f0f0f0",
    size = 6,
    fontface = "bold",
    hjust = 0.8,
    label.size = NA
  )


library(tidyverse)
library(gghighlight)
library(lubridate)

invisible(Sys.setlocale("LC_TIME", "C"))

data <- read_csv(here::here("data", "spending-jpmorgan.csv")) %>% 
  mutate(category = fct_inorder(category))

label_df <-
  tibble(
    date = ymd("2020-03-13"),
    change = -60,
    label = "National emergency\ndeclared March 13",
    category = factor("Groceries", levels = levels(data$category))
  )

z<- ggplot(data, aes(date, change, color = category)) +
  geom_point() +
  geom_line(size = .8, alpha = .5) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "grey80", size = .5)
  ) +
  geom_vline(xintercept = ymd("2020-03-13"), color = "grey25", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey30", size = .8) +
  geom_area(alpha = .2) +
  geom_line(size = 1.2) +
  geom_point(size = 1.8) +
  geom_text(
    data = label_df,
    aes(label = label),
    color = "grey25",
    family = "Cabinet Grotesk",
    size = 4.1,
    lineheight = .95,
    hjust = 1.1
  ) +
  facet_wrap(~ category, ncol = 4, scales = "free_x") +
  coord_cartesian(clip = "off") +
  scale_x_date(
    expand = c(.003, .003),
    breaks = seq(ymd("2020-01-04"), ymd("2020-04-11"), length.out = 8),
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = seq(-80, 60, by = 20),
    labels = glue::glue("{seq(-80, 60, by = 20)}%")
  ) +
  rcartocolor::scale_color_carto_d(
    palette = "Prism", guide = "none"
  ) +
  labs(
    x = "End of week", y = NULL,
    title = "Year-over-year percent change in spending by essential category",
    caption = "Source: JPMorgan Chase Institute | Makeover: Cédric Scherer"
  ) +
  theme_minimal(
    base_family = "Cabinet Grotesk", base_size = 14
  ) +
  theme(
    plot.title = element_text(size = 22, margin = margin(b = 20)),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey25", size = 10, margin = margin(t = 0)),
    plot.caption.position = "plot",
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12, margin = margin(l = 10, r = 7)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.line.x = element_line(),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length.x = unit(.5, "lines"),
    strip.text = element_text(size = 15, face = "bold", margin = margin(b = 0)),
    panel.grid.major.y = element_line(color = "grey90", size = .4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(2.5, "lines"),
    panel.spacing.y = unit(1.5, "lines"),
    plot.margin = margin(20, 35, 20, 20)
  )


ggsave(here::here("szkielet", "wykres1.svg"), width = 15, height = 10)


ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum_rc()

require(pilot)
remotes::install_github("olihawkins/pilot")
library(ggplot2)
library(pilot)

plot <- ggplot(
  data = mpg,
  mapping = aes(
    x = displ, 
    y = hwy, 
    color = class)) + 
  geom_point() +
  labs(
    title = "Cars with smaller engines are more efficient",
    subtitle = "Engine size by fuel efficiency and class",
    x = "Engine size in litres",
    y = "Miles per gallon",
    color = "Class",
    caption = "Reproduced from Chapter 3 of R for Data Science") + 
  theme_pilot() +
  scale_color_pilot()


plot


library(tidyverse)
library(pilot)

# Read in and prepare the data ------------------------------------------------

# Load the data from the csv as a dataframe
df <- read_csv("bar-chart-labels.csv")

# Turn the region column into a factor and order it by the population in each
# region: this sorts the bars in the chart from largest to smallest
df$region <- factor(df$region)
df$region <- fct_reorder(df$region, df$population, max)

# Create the plot -------------------------------------------------------------

# Use ggplot to create a plot with data
plot <- ggplot(data = df) +
  # Add a column geometry for the bars
  geom_col(
    mapping = aes(
      x = population,
      y = region),
    fill = pilot_color("navy")) +
  # Add a text geometry for the labels: geom_text_pilot uses the theme fonts
  geom_text_pilot(
    mapping = aes(
      x = population,
      y = region,
      label = format(population, digits = 2)),
    hjust = "center",
    nudge_x = -0.4) +
  # Set labels for the axes, but don't set titles here
  labs(
    x = "Millions of people",
    y = NULL) +
  # Configure the the x and y axes, removing the expansion for the x axis
  scale_x_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2),
    expand = c(0,0)) +
  scale_y_discrete(
    expand = expansion(add = c(0.6, 0.6))) +
  # Add the pilot theme, setting a bottom axis with no gridlines
  theme_pilot(
    axes = "b",
    grid = "")

# After creating the plot, add a title and subtitle with add_pilot_titles
plot <- add_pilot_titles(
  plot,
  title = "Countries and regions vary in population",
  subtitle = "Population of countries and regions in mid-2020, United Kingdom")

# Save the plot in different formats ------------------------------------------

# Save a high resolution export of the plot as a png
ggsave(
  filename = "bar-chart-labels.png",
  plot = plot,
  width = 7.7,
  height = 6.2,
  dpi = 400)

# Save an editable verson of the plot as an svg
ggsave(
  filename = "bar-chart-labels.svg",
  plot = plot,
  width = 7.7,
  height = 6.2,
  dpi = 400)











1library(tidyverse)
library(ggtext)
library(colorspace)

data <- 
  read_csv(here::here("data", "information-speech.csv")) %>% 
  group_by(language) %>% 
  mutate(
    avg_sr = mean(speech_rate),
    avg_ir = mean(info_rate)
  ) %>% 
  ungroup() %>% 
  mutate(
    language = fct_reorder(language, avg_sr),
    language_long = fct_reorder(language_long, avg_sr)
  )

systemfonts::register_variant(
  name = "Cabinet Grotesk ExtraBold",
  family = "Cabinet Grotesk",
  weight = "ultrabold"
)

systemfonts::register_variant(
  name = "Cabinet Grotesk Medium",
  family = "Cabinet Grotesk",
  weight = "medium"
)

data_long <-
  data %>% 
  dplyr::select(starts_with("lang"), speech_rate, info_rate) %>%
  ## normalize
  mutate(
    speech_rate = (speech_rate - min(speech_rate)) / (max(speech_rate) - min(speech_rate)),
    info_rate = (info_rate - min(info_rate)) / (max(info_rate) - min(info_rate))
  ) %>% 
  group_by(language) %>% 
  mutate(
    avg_sr = median(speech_rate),
    avg_ir = median(info_rate)
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c(speech_rate, info_rate), 
    names_to = "metric", 
    values_to = "rate"
  ) %>% 
  mutate(metric = factor(metric, levels = c("speech_rate", "info_rate")))

data_labs <-
  data.frame(
    language_long = factor("Japanese", levels = levels(data_long$language_long)),
    label = c("Speak less quickly", "Convey less information", "Speak more quickly", "Convey more information"),
    metric = factor(c("speech_rate", "info_rate", "speech_rate", "info_rate"), levels = levels(data_long$metric)),
    rate = c(.01, .01, .99, .99),
    vjust = c(-6.5, -4.7, -6.5, -4.7),
    hjust = c(0, 0, 1, 1)
  )

ggplot(data_long, aes(x = rate, y = language_long)) +
  ## rain dots
  geom_point(
    aes(color = metric, color = after_scale(desaturate(lighten(color, .2), .4))),
    position = position_nudge(y = -.06), shape = 1, size = .8, alpha = .35
  ) +
  ## distribution
  ggdist::stat_halfeye(
    aes(color = metric, fill = after_scale(color)), 
    slab_alpha = .35, .width = 0, trim = TRUE, shape = 21, point_colour = "grey25", stroke = 1.6, scale = .86
  ) +
  ## median line
  geom_linerange(
    aes(xmin = avg_sr, xmax = avg_ir), 
    size = .7, color = "grey25", stat = "unique"
  ) +
  ## median points
  ggdist::stat_halfeye(
    aes(color = metric), .width = c(0), slab_fill = NA
  ) +
  ## language labels
  geom_text(
    aes(label = language_long, x = .01), 
    position = position_nudge(y = .4), stat = "unique", hjust = 0,
    family = "Cabinet Grotesk ExtraBold", color = "grey25", size = 5.5
  ) +
  geom_text(
    data = data_labs, aes(label = label, color = metric, vjust = vjust, hjust = hjust),
    family = "Cabinet Grotesk Medium", size = 5
  ) +
  coord_cartesian(xlim = c(0, 1), clip = "off") +
  scale_x_continuous(
    expand = c(0, 0), breaks = 0:5 / 5, guide = "none"
  ) +
  scale_y_discrete(
    expand = c(.01, .01)
  ) +
  scale_color_manual(
    values = c("#7d28a8", "#28a87d"), guide = "none"
  ) +
  labs(
    x = "Normalized rates of <b style='color:#7d28a8;'>speech (syllables per second)</b> and <b style='color:#28a87d;'>information (bits per second)</b>", 
    y = NULL,
    title = "Communicating fast doesn't necessarily mean communicating more",
    subtitle = "Variation in speech and information rates across languages, shown as normalized rates for direct comparison.\nWhile there are stark cross-linguistic differences in speech rates, information rates are more similar.",
    caption = "Source: Coupé *et al.* 2019 *Science Advances* **5**(9). DOI: 10.1126/sciadv.aaw2594<br>Graphic: Cédric Scherer  •  Dots show the median rates for each language."
  ) +
  theme_minimal(base_size = 16, base_family = "Cabinet Grotesk") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = .6, color = "grey82"),
    axis.line.x = element_line(color = "black", size = .6),
    axis.ticks.x = element_line(color = "black", size = .6),
    axis.ticks.length.x = unit(.5, "lines"),
    axis.title.x = element_markdown(margin = margin(t = 10)),
    axis.title.x.top = element_markdown(),
    axis.text.x = element_text(family = "Tabular", size = 14, color = "grey25"),
    axis.text.y = element_blank(),
    legend.position = "top",
    plot.margin = margin(30, 35, 20, 35),
    plot.title = element_text(family = "Cabinet Grotesk ExtraBold", size = 25),
    plot.subtitle = element_text(margin = margin(b = 45)),
    plot.caption = element_markdown(hjust = 0, margin = margin(t = 18), color = "grey25", lineheight = 1.15, size = 11)
  )








library(tidyverse)

data <- read_csv(here::here("data", "carbon-footprint-travel.csv"))

data %>% 
  mutate(
    type = case_when(
      str_detect(entity, "car|Motorcycle") ~ "Private motorized transport",
      str_detect(entity, "flight") ~ "Public air transport",
      str_detect(entity, "Ferry") ~ "Public water transport",
      TRUE ~ "Public land transport"
    )
  ) %>% 
  ggplot(
    aes(x = emissions, 
        y = forcats::fct_reorder(entity, -emissions), 
        fill = type)
  ) +
  geom_col(orientation = "y", width = .8) +
  geom_text(
    aes(label = paste0(emissions, "g")),
    nudge_x = 5,
    hjust = 0,
    size = 5,
    family = "Lato",
    color = "grey40"
  ) +
  scale_x_continuous(
    breaks = seq(0, 250, by = 50), 
    labels = function(x) glue::glue("{x} g"),
    expand = c(0, 0),
    limits = c(0, 285)
  ) +
  scale_fill_manual(
    values = c("#dfb468", "#8fb9bf", "#28a87d"), name = NULL, guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Carbon footprint of travel per kilometer, 2018", 
    subtitle = "The carbon footprint of travel is measured in grams of carbon dioxide equivalents per passenger kilometer.\nThis includes carbon dioxide, but also other greenhouse gases, and increased warming from aviation emissions at altitude.", 
    caption = "Source: UK Department for Business, Energy & Industrial Grenhouse gas reporting: conversion factors 2019.\nNote: Data is based on official conversion factors used in UK reporting. These factors may vary slightly depending on the country.\nOriginal visualization by Hannah Ritchie, OurWorldInData.org | Makeover by Cédric Scherer"
  ) +
  theme_minimal(base_size = 18, base_family = "Cabinet Grotesk") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "grey30"),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    legend.position = c(.75, .8),
    legend.text = element_text(size = 20),
    legend.key.height = unit(2.6, "lines"),
    plot.title = element_text(family = "Cabinet Grotesk", size = 40, color = "grey30", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 17, color = "grey30", margin = margin(b = 20)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 14, hjust = 0, color = "grey60", margin = margin(t = 20), lineheight = 1.2),
    plot.caption.position = "plot",
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave("emissions.png", width = 15, height = 10)


Total sales (3rd party + intercompany)
```{r BBSMa, echo=FALSE}

#big$cyl = cell_spec(big$cyl, color = ifelse(big$cyl > 5, "red", "blue"))
#big$vs = cell_spec(cs_dt$vs, color = ifelse(cs_dt$vs > 0, "red", "blue"))

kbl(BBSM, booktabs = T,escape = FALSE,digits = 0) %>%
  kable_styling(latex_options = c("striped", "scale_down","hold_position"))#%>% 
#row_spec(4, bold = TRUE, italic = TRUE ,color = "black", background = "red") 



