# Import the airquality dataset and check its structure using str ( ) .
data(airquality)

str(airquality)

# Find the number of missing values in airquality.
data(airquality)
    # way1
summary(is.na(airquality))
    # way2
colSums(is.na(airquality))

# Use summary() on the mtcars dataset and interpret the output.
data(mtcars)

summary(mtcars)

# Find the mean horsepower (hp) in the mtcars dataset.
data(mtcars)

mean_hp <- mean(mtcars$hp)

print(mean_hp)


# Group the iris dataset by species and calculate the average Sepal.Width.
data(iris)

aggregate(Sepal.Width ~ Species, data = iris, FUN = mean)

library(dplyr)

iris %>%
  group_by(Species) %>%
  summarise(avg_sepal_width = mean(Sepal.Width, na.rm = TRUE))
