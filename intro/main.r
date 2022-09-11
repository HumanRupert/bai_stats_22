# Create a vector v in R consisting of all odd number up to 101;
# that is, v = (1; 3; : : : ; 501).
v <- seq(1, 101, 2)

# Use v to make a new vector w that contains all values of v between 4 and 46:
w <- v[v >= 4 & v <= 46]

# How many entries does w have?
w_l <- length(w)

# Create a new vector u which contains the squares of the elements in w.
u <- v^2

# Download the file "crime.csv" (from blackboard) and read it into R.
crime <- read.csv("intro/crime.csv")

# What is the average number of murders?
avg_murders <- mean(crime$MURDER)

# Find the lowest an highest life expectancy in the dataset.
min_life <- min(crime[["LIFE.EXPECT."]])
