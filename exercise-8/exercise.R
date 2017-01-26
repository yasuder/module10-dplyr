#Use "dplyr"
#Install.packages("dplyr")
## getwd()
## setwd("../module10-dplyr/exercise-8/")

#Load in SwissData from data set from data folder and view it to understand what is in it.
swiss <- read.csv("data/SwissData.csv", stringsAsFactors = FALSE)

#Add a column (using dpylr) that is the absolute difference between Education and Examination and call it
# Educated.Score
swiss <- mutate(swiss, Education.Score = abs(Education - Examination))

#Which area(s) had the largest difference
max.diff <- filter(swiss, Education.Score == max(Education.Score))
print(max.diff)

#Find which region has the highest percent of men in agriculture and retunr only the
#percent and region name.  Use pipe operators to accomplish this.
max.agriculture <- filter(swiss, Agriculture == max(Agriculture)) %>% 
  select(Region, Agriculture) 
print(max.agriculture)

#Find the average of all infant.mortality rates and create a column (Mortality.Difference)
# showing the difference between a regions mortality rate and the mean. Arrange the dataframe in
# Descending order based on this new column. Use pipe operators.
swiss <- mutate(swiss, Mortality.Difference = Infant.Mortality - mean(Infant.Mortality)) %>% 
  arrange(desc(Mortality.Difference))

# Create a new data frame that only is that of regions that have a Infant mortality rate less than the
# mean.  Have this data frame only have the regions name, education and mortality rate.
low.mortality <- select(swiss, Region, Education, Infant.Mortality) %>% 
  filter(Infant.Mortality < mean(Infant.Mortality))

#Filter one of the columns based on a question that you may have (which regions have a higher
#education rate, etc.) and write that to a csv file
# Q: Which regions have a higher education rate?
low.mortality <- filter(low.mortality, Education > 50)
# write.csv(low.mortality, file = "data/low_mortality.csv")

# Create a function that can take in two different region names and compare them based on a statistic
# Of your choice (education, Examination, ect.)  print out a statment describing which one is greater
# and return a data frame that holds the selected region and the compared variable.  If your feeling adventurous
# also have your function write to a csv file.
CompareExamination <- function(region1, region2) {
  region1.exam <- filter(swiss, Region == region1) %>% 
    select(Examination)
  print(region1.exam)
  region2.exam <- filter(swiss, Region == region2) %>% 
    select(Examination)
  max.region <- select(swiss, Examination == max(region1.exam, region2.exam))
  is.data.frame(max.region)
  print(paste("Of these two regions,", max.region, "has a higher examination rate."))
}
CompareExamination("Glane", "Broye")
