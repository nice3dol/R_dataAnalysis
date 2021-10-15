### Line Graphs
library(gcookbook)

## Making a Basic Line Graph

BOD

ggplot(BOD, aes(Time, demand)) +
  geom_line()  # demand is numeric

bod <- BOD
bod$Time <- factor(bod$Time)

ggplot(bod, aes(Time, demand, group = 1)) +
  geom_line()  # graph with a factor on the x-axis

ggplot(BOD, aes(Time, demand)) +
  geom_line() +
  ylim(0, max(BOD$demand))
ggplot(BOD, aes(Time, demand)) +
  geom_line() +
  expand_limits(y = 0)  # the same result as above 


## Adding points to a Line Graph

ggplot(BOD, aes(Time, demand)) +
  geom_line() +
  geom_point()

ggplot(worldpop, aes(Year, Population)) +
  geom_line() +
  geom_point() 

ggplot(worldpop, aes(Year, Population)) +
  geom_line() +
  geom_point() +
  scale_y_log10()  # log y-axis

  
## Making a Line Graph with Multiple Lines

tg
ggplot(tg, aes(dose, length, colour = supp)) +
  geom_line()  # Map supp to colour
ggplot(tg, aes(dose, length, linetype = supp)) +
  geom_line()  # Map supp to linetype

ggplot(tg, aes(factor(dose), length, colour = supp, group = supp)) +
  geom_line()

ggplot(tg, aes(dose, length)) + 
  geom_line()  # incorrect grouping

ggplot(tg,aes(dose, length, shape = supp)) +
  geom_line() +
  geom_point(size = 4)  # make the points a little larger

ggplot(tg, aes(dose, length, fill = supp)) + 
  geom_line()  +
  geom_point(size = 4, shape = 21)  # also use a point with a color fill

ggplot(tg, aes(dose, length, shape = supp)) +
  geom_line(position = position_dodge(0.2)) +
  geom_point(position = position_dodge(0.2), size = 4)


## Changing the Appearance of Lines

ggplot(BOD, aes(Time, demand)) + 
  geom_line(linetype = "dashed", size = 1, colour = "red")

ggplot(tg, aes(dose, length, colour = supp)) +
  geom_line() +
  scale_colour_brewer(palette = "Set1")

ggplot(tg, aes(dose, length, group = supp)) +
  geom_line(colour = "darkgreen", size = 1.5)  # both lines have the same properties

ggplot(tg, aes(dose, length, colour = supp)) +
  geom_line(linetype = "dashed") +
  geom_point(shape = 22, size = 3, fill = "black")  # automatically be used for grouping


## Changing the Appearance of Points

ggplot(BOD, aes(Time, demand)) +
  geom_line() +
  geom_point(size = 4, shape = 22, colour = "darkred", fill = "pink")

ggplot(BOD, aes(Time, demand)) +
  geom_line() +
  geom_point(size = 4, shape = 21, fill = "white")

pd <- position_dodge(0.2)

ggplot(tg, aes(dose, length, fill = supp)) +
  geom_line(position = pd) +
  geom_point(shape = 21, size = 3, position = pd) +
  scale_fill_manual(values = c("black", "white"))


## Making a Graph with a Shaded Area

# Data
head(sunspot.year)
str(sunspot.year)
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)

# ggplot
ggplot(sunspotyear, aes(Year, Sunspots)) +
  geom_area()

ggplot(sunspotyear, aes(Year, Sunspots)) +
  geom_area(colour = "black", fill = "blue", alpha = 0.2)  # alpha makes 80%transparent

ggplot(sunspotyear, aes(Year, Sunspots)) +
  geom_area(fill = "blue", alpha = 0.2) +
  geom_line()


## Making a Stacked Area Graph
library(dplyr)

uspopage
ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) +
  geom_area()

ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) +
  geom_area(colour = "black", size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues")

ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup,
                     order = dplyr::desc(AgeGroup))) +
  geom_area(colour = NA, alpha = .4) +
  scale_fill_brewer(palette = "Blues") +
  geom_line(position = "stack", size = .2)


## Making a Proportional Stacked Area Graph

ggplot(uspopage, aes(Year, Thousands, fill = AgeGroup)) +
  geom_area(position = "fill", colour = "black", 
            size = .2, alpha = .4) +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::percent)


## Adding a Confidence Region

climate
climate.mod <- climate %>% filter(Source == "Berkeley") %>% 
  select(Year, Anomaly10y, Unc10y)

climate.mod

ggplot(climate.mod, aes(Year, Anomaly10y)) + 
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y,
                  ymax = Anomaly10y + Unc10y), alpha = .2) +
  geom_line()

ggplot(climate.mod, aes(Year, Anomaly10y)) +
  geom_line(aes(y = Anomaly10y - Unc10y), colour = "grey50",
            linetype = "dotted") +
  geom_line(aes(y = Anomaly10y + Unc10y), colour = "grey50",
            linetype = "dotted") +
  geom_line()



### Scatter Plots


## Making a Basic Scatter Plot

# Data
heightweight
heightweight %>% select(ageYear, heightIn)

# basic
ggplot(heightweight, aes(ageYear, heightIn)) +
  geom_point()

# Discussion
ggplot(heightweight, aes(ageYear, heightIn)) +
  geom_point(shape = 21)

ggplot(heightweight, aes(ageYear, heightIn)) +
  geom_point(size = 1.5)


## Grouping points Together using Shapes or Colors

# Data
hw1 <- heightweight %>% select(sex, ageYear, heightIn)

# Discussion
ggplot(hw1, aes(ageYear, heightIn, shape = sex, colour = sex)) +
  geom_point()

ggplot(hw1, aes(ageYear, heightIn, shape = sex, colour = sex)) +
  geom_point() +
  scale_shape_manual(values = c(1, 2)) +   # shape number
  scale_colour_brewer(palette = "Set1")


## Using Different Point Shapes

ggplot(heightweight, aes(ageYear, heightIn, shape = sex)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 4))

hw <- heightweight %>% mutate(weightgroup = ifelse(
  weightLb < 100, "< 100", ">= 100"))
hw

ggplot(hw, aes(ageYear, heightIn, shape = sex, fill = weightLb)) + 
  geom_point(size = 2.5) + 
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(NA, "black"),
                    guide = guide_legend(override.aes = 
                                           list(shape = 21)))


## Mapping a Continuous Variable to Color or Size
