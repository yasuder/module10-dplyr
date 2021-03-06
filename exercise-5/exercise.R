# Exercise 5: DPLYR Grouped Operations

# Install the `nycflights13` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
#install.packages("nycflights13")  # should be done already
library(nycflights13)
library(dplyr)
View(flights)
# What was the average department delay in each month?
# Save this as a data frame `dep.delay.by.month`
# Hint: you'll have to perform a grouping operation before summarizing your data
dep.delay.by.month <- group_by(flights, month) %>% 
  summarise(
    delay = mean(dep_delay, na.rm = TRUE)
  )
View(dep.delay.by.month)
# Which month had the greatest average departure delay?
max.dep.delay <- filter(dep.delay.by.month, delay == max(delay))

# If your above data frame contains the columns "month", and "delay", you can create
# a scatterplot by passing that data frame to the 'plot()' function
plot.data <- plot(dep.delay.by.month)

# To which destinations were the average arrival delays the highest?
# Hint: you'll have to perform a grouping operation before summarizing your data
dest.delay <- flights %>% 
  group_by(dest) %>% 
  summarise(
    delay.time = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  arrange(desc(delay.time))
print(dest.delay)
# You can look up these airports in the `airports` data frame!


### Bonus ###
# Which city was flown to with the highest average speed?
fastest.city <- flights %>% 
  group_by(dest) %>% 
  summarise(
    avg.speed = mean(distance / air_time, na.rm = TRUE)
  ) %>% 
  arrange(-avg.speed)
print(fastest.city)
