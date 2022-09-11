library(dplyr)
library(gsubfn)

#################################
## Part 1: fertility rate data ##
#################################

# 1. Download the file fertilityrate.csv from blackboard and create a data
# object in R. The dataset provides the fertility rate (defined as the average
# number of children per woman) for 229 countries from 1960 to 2014.
data <- read.csv("lec2/fertilityrate.csv", sep = ",", dec = ".", stringsAsFactors = FALSE, row.names = "Country.or.Area", nrows = 229)
data <- data %>% select(-contains(".Footnotes"))


# 2. Within one R plot generate four boxplots with the fertility rates for
# 1984, 1994, and 2004, 2014. Make the plot ‘nice’, i.e. add some title and
# name the boxplots according to the year they are based on.
bb <- data[, c("X1984", "X1994", "X2004", "X2014")]

boxplot(bb,
    main = "Global Fertility Rates",
    xlab = "Year",
    ylab = "Fertility Rate",
    names = c("1984", "1994", "2004", "2014")
)


# 3. Choose one country and produce a plot showing the evolution of the
# fertility rate for that country during the period 1960 to 2014.
chad <- data["Chad", ]
chad <- as.data.frame(t(as.matrix(chad)))
plot(chad, type = "l", main = "Fertility rate in Chad (1960-2014)", xlab = "year", ylab = "fertility")

# 4. Compute summaries (mean, median, quartiles) for the years 1980 and 2000
# (use the R command summary).
y80 <- summary(data$X1980)
y00 <- summary(data$X2000)

# 5. Compute the number of missing values (=NA’s) for the years 2001 and 2007.
y01 <- sum(is.na(data$X2001))
y07 <- sum(is.na(data$X2007))

# 6. Write your own function which, given a vector as input, returns the number
# of missing values.
count_nans <- function(vec) sum(is.na(vec))

# 7. Compute the mean and the median for every year (not by hand) and plot them
colnames(data) <- lapply(colnames(data), function(x) substring(x, 2))
par(las = 2, cex = .8)
plot(colMeans(data, na.rm = TRUE), type = "l", main = "Average Global Fertility Rate", xlab = "Year", ylab = "Fertility Rate")
axis(1, at = 0:54, colnames(data))


#################################
## Part 2: student survey data ##
#################################

# 8. Read Survey2017.csv into R, file available in blackboard. The dataset
# consists ofthe answers given, in 2017, to 16 questions by 111 students of a
# large university.
survey <- read.csv("lec2/Survey2017.csv")

# 9. Compute the percentage of male and female participants. Can you say if
# the gender mix in the sample is representative for the student body of the
# university?
male_pct <- nrow(survey[survey$Gender == "Male", ]) / nrow(survey)
female_pct <- nrow(survey[survey$Gender == "Female", ]) / nrow(survey)

# 10. The survey question corresponding to the variable
# ComparisonOfFinancialSituation was “In comparison to other students how do
# you identify your financial situation?”. Make yourself familiar with the
# function table in R. Use this to summarise the data in
# ComparisonOfFinancialSituation. Interpret the outcome.
fin_sit <- table(survey$ComparisonOfFinancialSituation)


# 11. Make yourself familiar with the gsub function in R. Use this R function to
# remove the Euro-symbols and the expressions “euros” and “euro” in the vector
# WeeklySpending.c
weekly_spending <- gsub("euro|euros|\u20AC", "", survey$WeeklySpending)


# 12. Produce and compare the histograms of the variable Happiness for students
# who have financial stability and students who do not. For this consider only
# rows whose entry for the variable Happiness is numeric.
trust_fund_kid <- as.numeric(survey[survey$FinancialStability == "Yes", "Happiness"])
poor_kid <- as.numeric(survey[survey$FinancialStability == "No", "Happiness"])
par(mfrow = c(2, 1))
hist(trust_fund_kid, main = "Happiness of Trust Fund Kids", xlab = "Happiness (/10)")
hist(poor_kid, main = "Happiness of Poor Kids", xlab = "Happiness (/10)")
