# Display the first 15 rows of the mtcars dataset.
data(mtcars)
head(data,15)

# Find the maximum and minimum value of Sepal. Length from iris.
data(iris)

names(iris)

min_val <- min(iris$Sepal.Length, na.rm = TRUE)
max_val <- max(iris$Sepal.Length, na.rm = TRUE)

cat("Min Sepal.Length:", min_val, "\n")
cat("Max Sepal.Length:", max_val, "\n")


# Calculate the mean of the variable mpg in mtcars.
data(mtcars)

mean_of_cars <- mean(mtcars$mpg)

print(mean_of_cars)


# Display the structure of the airquality dataset.
data(airquality)

str(airquality)

# Check whether the number 100 is greater than 50 in R.
x <- 100
y <- 50
x>y
