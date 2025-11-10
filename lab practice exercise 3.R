#Part A: Descriptive Statistics & Visual Summaries
# 1.Calculate mean, median, mode, variance, standard deviation, and range of the mpg column.
data(mtcars)
cat("Summary statistics for mpg:\n")
summary(mtcars$mpg)
cat("\nMean:", mean(mtcars$mpg))
cat("\nMedian:", median(mtcars$mpg))
cat("\nmode:", var(mtcars$mpg))
cat("\nStandard Deviation:", sd(mtcars$mpg))
cat("\nVariance:", var(mtcars$mpg))
cat("\nrange:", var(mtcars$mpg))


#2.Create a frequency table of the number of cylinders (cyl)
freq_table <- table(mtcars$cyl)
freq_table

#3.Generate a histogram of mpg and overlay a density curve.
hist(mtcars$mpg,
     breaks = 10,                 
     col = "lightblue", 
     border = "black",          
     main = "Histogram of MPG with Density Curve",
     xlab = "Miles per Gallon",
     freq = FALSE)

#4.Create a boxplot of mpg by cyl and interpret the spread.
boxplot(mpg ~ cyl, 
        data = mtcars,
        main = "Boxplot of MPG by Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles per Gallon (MPG)",
        col = c("lightblue", "lightgreen", "lightpink"))

#5.Use summary() to produce a descriptive statistics report of the dataset.
summary(mtcars)

#Part B: Probability & Distributions
#1.# Create a sequence of x values around the data range
data(iris)
curve(dlnorm(x, meanlog=0, sdlog=1), from=0, to=25)

#2.Perform a Shapiro–Wilk test to check if Sepal.Length follows a normal distribution# Load the built-in iris dataset
data(iris)
shapiro.test(iris$Sepal.Length)

#3.Simulate 1000 samples from a binomial distribution (n = 10, p = 0.5) and plot its histogram.
rbinom(1000, size = 10, prob = 1 / 2)
hist(rbinom(1000, size = 10, prob = 1 / 2))

#4.Compare the sample mean and variance with theoretical values for the binomial distribution.
n <- 10
p <- 0.5       
N <- 1000   

set.seed(123)
samples <- rbinom(N, size = n, prob = p)

sample_mean <- mean(samples)
sample_var  <- var(samples)

theoretical_mean <- n * p
theoretical_var  <- n * p * (1 - p)

cat("Sample Mean:       ", sample_mean, "\n")
cat("Theoretical Mean:  ", theoretical_mean, "\n\n")
cat("Sample Variance:   ", sample_var, "\n")
cat("Theoretical Var:   ", theoretical_var, "\n")

#Part C: Estimation & Confidence Intervals

#1. Construct a 95% confidence interval for the mean of mpg.
t.test(mtcars$mpg, conf.level=0.95)

#2. Use bootstrapping (boot package) to estimate CI for hp (horsepower).
library(boot)
boot_mean <- function(data, i) mean(data[i])
boot_res <- boot(mtcars$hp, boot_mean, R=1000)
boot.ci(boot_res, type="bca")

#3. Compare confidence intervals of mpg for automatic vs manual cars (am variable).
t.test(mpg ~ am, data=mtcars, conf.level=0.95)

#Part D: Hypothesis Testing
#1. Perform a one-sample t-test: Is the mean Sepal.Length significantly different from 5.5?
t.test(iris$Sepal.Length, mu=5.5)

#2. Perform a two-sample t-test: Is there a significant difference in mpg between automatic and manual cars?
t.test(mpg ~ am, data=mtcars)

#3. Conduct a chi-square test of independence: Are Survived and Sex independent in the Titanic dataset?
table_t <- table(titanic$Survived, titanic$Sex)
chisq.test(table_t)

#4. Perform a one-way ANOVA: Compare Sepal.Length means across the 3 iris species.
aov_res <- aov(Sepal.Length ~ Species, data=iris)
summary(aov_res)

#5. Apply a post-hoc Tukey HSD test after ANOVA and interpret.
TukeyHSD(aov_res)


#Part E: Correlation & Association
library(arules)
data("Groceries")

#1. Compute the Pearson correlation between mpg and hp.
cor(mtcars$mpg, mtcars$hp)

#2. Plot a scatterplot with regression line of mpg ~ hp.
plot(mtcars$hp, mtcars$mpg, main="MPG vs HP", xlab="HP", ylab="MPG", col="blue")
abline(lm(mpg ~ hp, data=mtcars), col="red")

#3. Create a correlation matrix for all numeric columns in mtcars.
cor(mtcars)

#4. Compute and interpret the Spearman rank correlation between Sepal.Length and Petal.Length.
cor(iris$Sepal.Length, iris$Petal.Length, method="spearman")

#5. Perform association rule mining (using arules package) on a small market basket dataset.
rules <- apriori(Groceries, parameter=list(supp=0.01, conf=0.5))
inspect(head(rules))

#Part F: Mini Data Science Applications
#1. Build a logistic regression model predicting Survived using Age, Sex, and Pclass.
#   Interpret coefficients and odds ratios.
titanic$Sex <- as.factor(titanic$Sex)
model_log <- glm(Survived ~ Age + Sex + Pclass, data=titanic, family="binomial")
summary(model_log)
exp(coef(model_log))  # odds ratios

#2. Fit a linear regression model predicting Petal.Length from Sepal.Length.
#   Plot fitted line + residuals.
model_lin <- lm(Petal.Length ~ Sepal.Length, data=iris)
summary(model_lin)
plot(iris$Sepal.Length, iris$Petal.Length, main="Linear Regression", xlab="Sepal.Length", ylab="Petal.Length")
abline(model_lin, col="red")
plot(resid(model_lin), main="Residuals", col="purple")

#3. Cluster cars using k-means clustering (k = 3) on mpg, hp, wt.
#   Visualize clusters.
data_cluster <- mtcars[, c("mpg", "hp", "wt")]
km <- kmeans(scale(data_cluster), centers=3)
plot(data_cluster$mpg, data_cluster$hp, col=km$cluster, pch=19, main="K-means Clusters")



