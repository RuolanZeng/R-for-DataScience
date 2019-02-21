library(ggplot2)
?geom_point

# package is ggplot2 but function is ggplot()
# ggplot() is used to construct the initial plot object,and is almost always followed by + to add component to the plot.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
# displ and hwy are attributes of data(mpg)

# color points
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# smooth_line: geom_smooth
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

# separate plot for each factor
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)

# Q1:Plot city highway (hwy on y-axis) against year (year on x-axis) for just two manufacturers - audi and honda. 
# The points for each manufacturer should be differently colored.

install.packages("tidyverse")
library(tidyverse) # if don't use the library, there will be an error: object 'manufacturer' not found
ggplot(data = filter(mpg, manufacturer %in% c("audi","honda"))) +
  geom_point(mapping = aes(x = year, y = hwy, color = manufacturer))

# Q2:Plot city highway (hwy on y-axis) against year (displ on x-axis), with different color for each drv variable. 
# Also fit the best possible model between y and x

ggplot(data = mpg,mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth() +
  geom_point()

# Q3:Plot city city mileage (cty on y-axis) against highway mileage (hwy on x-axis), with different plots for each manufacturer. 
# Hint: Use facet_grid

ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = cty)) + facet_wrap(~ manufacturer, nrow = 2)

install.packages("dplyr")
library(dplyr)
require(nycflights13) # flights data

# grouping data
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

# Multiple Operationscan be piped together
delays <- flights %>% group_by(dest) %>% summarise(
  count = n(),
  dist = mean(distance, na.rm = TRUE), 
  delay = mean(arr_delay, na.rm = TRUE)
) %>%
  filter(count > 20, dest != "HNL")
delays

# Q1: In the flights dataset from nycflights13 package, find the average delay time grouped by month for flights that have not been cancelled. 
# Do summers have more delays than winters?

View(flights)
?flights

by_month <- group_by(flights, month)
summarise(by_month, delay = mean(dep_delay, na.rm = TRUE))


flights %>% 
  group_by(month) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE)) 

# Q2: Repeat the previous question, but generate bar plot as the output, with x axis being month and y being the average delay

ggplot(flights %>% 
         group_by(month) %>% 
         summarise(delay = mean(dep_delay, na.rm = TRUE)))+
  geom_bar(mapping = aes(x = month, y = delay),stat="identity")


# Q3: How many days are in the dataset with Temp > 80
View(airquality)
count(filter(airquality, Temp>80))
