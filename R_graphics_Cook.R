### Quickly Exploring Data


## Scatter Plot

# base
mtcars

plot(mtcars$wt, mtcars$mpg)

# ggplot2
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

ggplot(NULL, aes(x = mtcars$wt, y = mtcars$mpg)) +
  geom_point()


## Line Graph

# base
pressure

plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure / 2,
     col = "red")
points(pressure$temperature, pressure$pressure / 2,
      col = "red")

# ggplot2
ggplot(pressure, aes(temperature, pressure)) +
  geom_line() + geom_point()


## Bar Graph

# base
BOD

barplot(BOD$demand, names.arg = BOD$Time)

table(mtcars$cyl)
barplot(table(mtcars$cyl))  # barplot of counts

# ggplot
ggplot(BOD, aes(Time, demand)) +
  geom_col()  # no entry for 6

ggplot(BOD, aes(factor(Time), demand)) + 
  geom_col()

ggplot(mtcars, aes(factor(cyl))) +
  geom_bar()  # y position is calculated by counting 


## Histogram

# base
hist(mtcars$mpg)

hist(mtcars$mpg, breaks = 10)  # Specify number of bins with breaks

# ggplot
ggplot(mtcars, aes(mtcars$mpg)) +
  geom_histogram()

ggplot(mtcars, aes(mtcars$mpg)) +
  geom_histogram(binwidth = 4)  # with wider bins
#! defaulting to 30 bins


## Box Plot

# base
ToothGrowth

plot(ToothGrowth$supp, ToothGrowth$len)

boxplot(len ~ supp, data = ToothGrowth)  # Formula syntax
boxplot(len ~ supp + dose, data = ToothGrowth)  # interaction of two variables on x-axis

# ggplot
ggplot(ToothGrowth, aes(supp, len)) +
  geom_boxplot()

ggplot(ToothGrowth, aes(interaction(supp, dose), y = len)) +
  geom_boxplot()
#! Box plots from basegraphics are ever so slightly different from ggplot2. 
#! Because they use slightly different methods for calculating quatiles


## Plotting a Function Curve

# base
curve(x ^ 3 - 5 * x, from = -4, to = 4)  # pass it an expression with the variable x

myfun <- function(xvar){
  1 / (1 + exp(-xvar + 10))
}
curve(myfun(x), from = 0, to = 20)  # plot to function
curve(1 - myfun(x), add = T, col = 'red')  # add a line

# ggplot
x <- c(0, 20)
ggplot(data.frame(x), aes(x)) + 
  stat_function(fun =myfun, geom = "line")  # defaulting to geom="line"



### Bar Graphs


## Basic Bar Graph

# Data
install.packages("gcookbook")  # for the pg_mean data set
library(gcookbook)
pg_mean

# ggplot
ggplot(pg_mean, aes(group, weight)) + 
  geom_col()  # col() use for show average or value

ggplot(pg_mean, aes(group, weight)) + 
  geom_col(fill = "lightblue", colour = "red")


## Grouping Bars Together

# Data
cabbage_exp

# ggplot
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_col(position = "dodge")

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_col(position = "dodge", colour = "black") + 
  scale_fill_brewer(palette = "Pastel1")

ce <- cabbage_exp[-6, ]

ggplot(ce, aes(Date, Weight, fill = Cultivar)) + 
  geom_col(position = "dodge", colour = "black") + 
  scale_fill_brewer(palette = "Pastel1")


## A Bar Graph of Counts

# Data
diamonds

# ggplot
ggplot(diamonds, aes(cut)) +
  geom_bar()


## Using Colors in a Bar Graph

# Data
library(dplyr)

uspopchange
upc <- uspopchange %>% arrange(desc(Change)) %>% slice(1:10)
upc

# ggplot
ggplot(upc, aes(Abb, Change, fill = Region)) +
  geom_col()

ggplot(upc, aes(reorder(Abb, Change), Change, fill = Region)) + 
  geom_col(colour = "black") +
  scale_fill_manual(values = c("#669933", "#FFCC66")) + 
  xlab("State")
#! reorder() to reorder the levels of the factor Abb based on the values of Change.  


## Coloring Negative and positive Bars Differently

# Data
str(climate)

climate.sub <- climate %>%
  filter(Source == "Berkeley" & Year >= 1900) %>%  
  mutate(pos = Anomaly10y >= 0)
climate.sub

# ggplot
ggplot(climate.sub, aes(Year, Anomaly10y, fill = pos)) + 
  geom_col(postion = "identity")  # Different colors for positive and negative values

ggplot(climate.sub, aes(Year, Anomaly10y, fill = pos)) +
  geom_col(position = "identity", colour = "black", size = 0.25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = F)  # guide = F -> No legend


## Adjusting Bar Width and Spacing

ggplot(pg_mean, aes(group, weight)) +
  geom_col()  # stardard width bars -> 0.9
ggplot(pg_mean, aes(group, weight)) +
  geom_col(width = 0.5)  # narrow width bars
ggplot(pg_mean, aes(group, weight)) +
  geom_col(width = 1)  # maximum width of 1

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_col(width = 0.5, position = "dodge")  # with narrow bars
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_col(width = 0.5, position = position_dodge(0.7))  # with space


## Making a Stacked Bar Graph

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col()

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col() +
  guides(fill = guide_legend(reverse = T))  # reverse only the legend

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col(position = position_stack(reverse = T)) +
  guides(fill = guide_legend(reverse = T))  # match the order of the bar

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col(colour = "black") +
  scale_fill_brewer(palette = "Pastel1")


## Making a proportional Stacked Bar Graph

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col()  # Stacked bar graph to Value

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col(position = "fill")  # Proportional stacked bar graph
       
ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) + 
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent)  # using the percent
#! You could instead do "library(scales)" and then just use "percent"

cabbage_exp
ce <- cabbage_exp %>% group_by(Date) %>%
  mutate(percent_weight = Weight / sum(Weight) * 100)
ce

ggplot(ce, aes(Date, percent_weight, fill = Cultivar)) + 
  geom_col()  # same result with "labels = percent"


## Adding Labels to a Bar Graph

ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")  # Below the top

ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = -0.2)  # Above the top

ggplot(mtcars, aes(factor(cyl))) + 
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "white")

ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) +
  geom_col() +
  geom_text(aes(label = Weight), vjust = -0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)  # Adjust y limits to be a little higher

ggplot(cabbage_exp, aes(interaction(Date, Cultivar), Weight)) +
  geom_col() +
  geom_text(aes(y = Weight + 0.1, label = Weight))  # Map y positions slightly above bar top - y range of plot will auto-adjust

ggplot(cabbage_exp, aes(Date, Weight, fill = Cultivar)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Weight), colour = "white", size = 3,
            vjust = 1.5, position = position_dodge(.9))

# Label on Stacked Bars
ce <- cabbage_exp %>% arrange(Date, rev(Cultivar)) %>% 
  group_by(Date) %>% mutate(label_y = cumsum(Weight))
ce

ggplot(ce, aes(Date, Weight, fill = Cultivar)) +
  geom_col() +
  geom_text(aes(y = label_y, label = Weight), vjust = 1.5,
            colour = "white")  # aes(y = label_y) -> location of TEXT

ce2 <- cabbage_exp %>% arrange(Date, rev(Cultivar)) %>% 
  group_by(Date) %>% mutate(label_y = cumsum(Weight) - 0.5 * Weight)
ce2  # Calculate y position, placing it in the middle

ggplot(ce2, aes(Date, Weight, fill = Cultivar)) + 
  geom_col() +
  geom_text(aes(y = label_y, label = Weight), colour = "white")

# For a more polished graph
ggplot(ce2, aes(Date, Weight, fill = Cultivar)) +
  geom_col(colour = "black") +
  geom_text(aes(y = label_y, 
                label = paste(format(Weight, nsmall = 2), "kg")),
            size = 4) +
  scale_fill_brewer(palette = "Pastel2")
  

## Cleveland Dot Plot

# Data
tophit <- tophitters2001[1:25, ]
head(tophit)

ggplot(tophit, aes(avg, name)) +
  geom_point()

# Discussion
tophit2 <- tophit[, c("name", "lg", "avg")]  #use only three of Cols

ggplot(tophit2, aes(avg, reorder(name, avg))) +
  geom_point(size = 3) +  # use a Larger dot
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60",
                                           linetype = "dashed"))

ggplot(tophit2, aes(reorder(name, avg), avg)) +
  geom_point(size = 3) +  # use a Larger dot
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60",
                                          linetype = "dashed"),
        axis.text.x = element_text(angle = 60, hjust = 1))
tophit2

nameorder <- tophit2$name[order(tophit2$lg, tophit2$avg)]
tophit2$name <- factor(tophit2$name, levels = nameorder)
tophit2$name

ggplot(tophit2, aes(avg, name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") + 
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),  # No horizontal grid line
        legend.position = c(1, 0.55),  # Put legend inside plot area
        legend.justification = c(1, 0.5))

ggplot(tophit2, aes(avg, name)) +
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") + 
  geom_point(size = 3, aes(colour = lg)) +
  scale_colour_brewer(palette = "Set1", limits = c("NL", "AL")) + 
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space = "free_y")
