# ==============================================================================
# Comprehensive R Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major R language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment
## Another style of comment

# ==============================================================================
# Variables and Assignment
# ==============================================================================

# Assignment operators
x <- 42           # Preferred assignment
y = 3.14          # Also valid
z <<- 100         # Global assignment
42 -> w           # Right assignment (less common)

# Variable names
my_variable <- 1
myVariable <- 2
.hidden_var <- 3  # Hidden from ls()
..two_dots <- 4

# ==============================================================================
# Basic Data Types
# ==============================================================================

# Numeric
integer_val <- 42L           # Integer
double_val <- 3.14159        # Double
scientific <- 6.022e23       # Scientific notation
hex_val <- 0xDEADBEEF       # Hexadecimal
complex_val <- 3 + 4i        # Complex number
inf_val <- Inf               # Infinity
neg_inf <- -Inf              # Negative infinity
nan_val <- NaN               # Not a number

# Character/String
single_quoted <- 'Hello, R!'
double_quoted <- "Hello, R!"
with_escapes <- "Line 1\nLine 2\tTabbed"
with_unicode <- "\u0048\u0065\u006c\u006c\u006f"
raw_string <- r"(No need to escape \ or ")"
raw_string2 <- R"---(Complex "string" with \ and ')---"

# Logical
true_val <- TRUE
false_val <- FALSE
t_short <- T      # Shorthand (not recommended)
f_short <- F      # Shorthand (not recommended)
na_logical <- NA  # Missing logical

# Special values
null_val <- NULL
na_val <- NA           # Missing value
na_int <- NA_integer_
na_real <- NA_real_
na_complex <- NA_complex_
na_char <- NA_character_

# ==============================================================================
# Vectors
# ==============================================================================

# Creating vectors
numeric_vec <- c(1, 2, 3, 4, 5)
char_vec <- c("a", "b", "c")
logical_vec <- c(TRUE, FALSE, TRUE)

# Sequence generation
seq_vec <- 1:10
seq_by <- seq(1, 10, by = 2)
seq_length <- seq(1, 10, length.out = 5)
rep_vec <- rep(1:3, times = 3)
rep_each <- rep(1:3, each = 3)

# Vector operations
vec_sum <- numeric_vec + 10
vec_mult <- numeric_vec * 2
vec_div <- numeric_vec / 2
vec_power <- numeric_vec ^ 2
vec_sqrt <- sqrt(numeric_vec)

# Vector indexing
first_element <- numeric_vec[1]
slice <- numeric_vec[2:4]
negative_index <- numeric_vec[-1]  # All except first
logical_index <- numeric_vec[numeric_vec > 3]
named_index <- c(a = 1, b = 2, c = 3)["a"]

# Vector functions
len <- length(numeric_vec)
total <- sum(numeric_vec)
avg <- mean(numeric_vec)
med <- median(numeric_vec)
variance <- var(numeric_vec)
std_dev <- sd(numeric_vec)
minimum <- min(numeric_vec)
maximum <- max(numeric_vec)
sorted <- sort(numeric_vec)
reversed <- rev(numeric_vec)
unique_vals <- unique(c(1, 1, 2, 2, 3))
cumsum_vec <- cumsum(numeric_vec)
diff_vec <- diff(numeric_vec)

# ==============================================================================
# Matrices
# ==============================================================================

# Creating matrices
mat <- matrix(1:12, nrow = 3, ncol = 4)
mat_byrow <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)

# Matrix from vectors
rbind_mat <- rbind(c(1, 2, 3), c(4, 5, 6))
cbind_mat <- cbind(c(1, 2), c(3, 4), c(5, 6))

# Matrix operations
mat_transpose <- t(mat)
mat_mult <- mat %*% t(mat)  # Matrix multiplication
element_mult <- mat * 2
mat_inverse <- solve(matrix(c(1, 2, 3, 4), nrow = 2))

# Matrix indexing
mat[1, 2]          # Single element
mat[1, ]           # First row
mat[, 2]           # Second column
mat[1:2, 2:3]      # Submatrix

# Matrix functions
nrow(mat)
ncol(mat)
dim(mat)
rowSums(mat)
colSums(mat)
rowMeans(mat)
colMeans(mat)
det_val <- det(matrix(c(1, 2, 3, 4), nrow = 2))
eigen_result <- eigen(matrix(c(1, 2, 3, 4), nrow = 2))

# ==============================================================================
# Arrays
# ==============================================================================

# Creating arrays (multi-dimensional)
arr <- array(1:24, dim = c(2, 3, 4))
arr[1, 2, 3]  # Access element

# Array operations
dim(arr)
dimnames(arr) <- list(
  c("row1", "row2"),
  c("col1", "col2", "col3"),
  c("mat1", "mat2", "mat3", "mat4")
)

# ==============================================================================
# Lists
# ==============================================================================

# Creating lists
my_list <- list(
  name = "Alice",
  age = 30,
  scores = c(85, 90, 92),
  active = TRUE
)

# Empty list
empty_list <- list()

# Nested list
nested_list <- list(
  level1 = list(
    level2 = list(
      value = 42
    )
  )
)

# List access
my_list$name           # By name
my_list[["age"]]       # By name with [[]]
my_list["scores"]      # Returns list
my_list[[3]]           # By index
nested_list$level1$level2$value

# List functions
names(my_list)
length(my_list)
unlist(my_list)
lapply(my_list, class)
sapply(1:5, function(x) x^2)

# ==============================================================================
# Data Frames
# ==============================================================================

# Creating data frames
df <- data.frame(
  id = 1:5,
  name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
  age = c(25, 30, 35, 28, 32),
  salary = c(50000, 60000, 70000, 55000, 65000),
  active = c(TRUE, TRUE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Alternative creation
df2 <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  group = sample(c("A", "B", "C"), 100, replace = TRUE)
)

# Data frame access
df$name             # Column by name
df[["age"]]         # Column by name
df[, "salary"]      # Column by name
df[1, ]             # First row
df[, 2]             # Second column
df[1:3, c("name", "age")]  # Subset

# Data frame functions
nrow(df)
ncol(df)
dim(df)
names(df)
colnames(df)
rownames(df)
str(df)
summary(df)
head(df, 3)
tail(df, 3)

# Data manipulation
df$bonus <- df$salary * 0.1  # Add column
df[df$age > 30, ]            # Filter rows
df[order(df$age), ]          # Sort
subset(df, age > 28, select = c(name, salary))  # Subset

# Merging data frames
df_a <- data.frame(id = 1:3, value_a = c("a", "b", "c"))
df_b <- data.frame(id = 2:4, value_b = c("x", "y", "z"))
merged <- merge(df_a, df_b, by = "id", all = TRUE)

# ==============================================================================
# Factors
# ==============================================================================

# Creating factors
gender <- factor(c("male", "female", "male", "female"))
ordered_factor <- factor(
  c("low", "medium", "high", "low"),
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

# Factor operations
levels(gender)
nlevels(gender)
as.numeric(gender)
as.character(gender)
droplevels(gender[1:2])

# ==============================================================================
# Control Flow
# ==============================================================================

# If-else
x <- 10
if (x > 0) {
  print("Positive")
} else if (x < 0) {
  print("Negative")
} else {
  print("Zero")
}

# Single line if
if (x > 0) print("Positive") else print("Non-positive")

# Vectorized if
result <- ifelse(x > 0, "Positive", "Non-positive")

# Switch
day <- "Monday"
switch(day,
  "Monday" = "Start of week",
  "Friday" = "End of week",
  "Other day"
)

# For loop
for (i in 1:5) {
  print(i)
}

for (item in c("a", "b", "c")) {
  print(item)
}

# While loop
count <- 0
while (count < 5) {
  print(count)
  count <- count + 1
}

# Repeat loop
count <- 0
repeat {
  print(count)
  count <- count + 1
  if (count >= 5) break
}

# Loop control
for (i in 1:10) {
  if (i == 3) next  # Skip iteration
  if (i == 8) break  # Exit loop
  print(i)
}

# ==============================================================================
# Functions
# ==============================================================================

# Basic function
add <- function(a, b) {
  return(a + b)
}

# Implicit return
multiply <- function(a, b) {
  a * b  # Last expression is returned
}

# Default arguments
greet <- function(name = "World") {
  paste("Hello,", name)
}

# Variable arguments
sum_all <- function(...) {
  args <- list(...)
  total <- 0
  for (x in args) {
    total <- total + x
  }
  total
}

# Named arguments
create_person <- function(name, age, city = "Unknown") {
  list(name = name, age = age, city = city)
}
person <- create_person(age = 30, name = "Alice")

# Anonymous functions
squared <- sapply(1:5, function(x) x^2)

# Lambda-style (R 4.1+)
# squared <- sapply(1:5, \(x) x^2)

# Function with environment
counter <- function() {
  count <- 0
  function() {
    count <<- count + 1
    count
  }
}
increment <- counter()

# Recursive function
factorial <- function(n) {
  if (n <= 1) return(1)
  n * factorial(n - 1)
}

# Infix function
`%between%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}
5 %between% c(1, 10)

# Replacement function
`set_first<-` <- function(x, value) {
  x[1] <- value
  x
}
vec <- 1:5
set_first(vec) <- 100

# ==============================================================================
# Apply Family
# ==============================================================================

mat <- matrix(1:12, nrow = 3)

# apply: apply function over array margins
row_sums <- apply(mat, 1, sum)      # 1 = rows
col_means <- apply(mat, 2, mean)    # 2 = columns

# lapply: apply function to list, return list
lapply(1:3, function(x) x^2)

# sapply: simplified lapply (returns vector if possible)
sapply(1:3, function(x) x^2)

# vapply: type-safe sapply
vapply(1:3, function(x) x^2, numeric(1))

# mapply: multivariate sapply
mapply(function(x, y) x + y, 1:3, 4:6)

# tapply: apply function to groups
tapply(df$salary, df$active, mean)

# by: apply function to data frame groups
by(df, df$active, function(x) mean(x$salary))

# aggregate: aggregate data by groups
aggregate(salary ~ active, data = df, FUN = mean)

# ==============================================================================
# String Manipulation
# ==============================================================================

# Basic string functions
str <- "Hello, World!"
nchar(str)
toupper(str)
tolower(str)
substr(str, 1, 5)
substring(str, 8)
strsplit(str, ", ")
paste("Hello", "World", sep = " ")
paste0("Hello", "World")
paste(letters[1:5], collapse = "-")
trimws("  hello  ")

# String formatting
sprintf("Value: %d, Name: %s, Float: %.2f", 42, "test", 3.14159)
format(3.14159, nsmall = 2)
formatC(12345.6789, format = "f", digits = 2)

# Pattern matching
grep("o", c("hello", "world", "foo"))
grepl("o", c("hello", "world", "foo"))
sub("o", "0", "hello")
gsub("l", "L", "hello")
regmatches(str, regexpr("\\w+", str))

# Regular expressions
pattern <- "^[A-Za-z]+$"
grepl(pattern, "Hello")
grepl("\\d{3}-\\d{4}", "555-1234")

# stringr package functions (if loaded)
# str_detect(str, "World")
# str_replace(str, "World", "R")
# str_extract(str, "\\w+")

# ==============================================================================
# Date and Time
# ==============================================================================

# Current date/time
today <- Sys.Date()
now <- Sys.time()

# Creating dates
date1 <- as.Date("2024-01-15")
date2 <- as.Date("15/01/2024", format = "%d/%m/%Y")

# Date components
format(today, "%Y")  # Year
format(today, "%m")  # Month
format(today, "%d")  # Day
format(today, "%A")  # Weekday name
weekdays(today)
months(today)
quarters(today)

# Date arithmetic
tomorrow <- today + 1
next_week <- today + 7
date_diff <- as.Date("2024-12-31") - today

# Sequences of dates
date_seq <- seq(as.Date("2024-01-01"), as.Date("2024-01-31"), by = "day")
month_seq <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)

# POSIXct and POSIXlt
datetime <- as.POSIXct("2024-01-15 12:30:45")
datetime_lt <- as.POSIXlt(datetime)
datetime_lt$year + 1900  # Year
datetime_lt$mon + 1      # Month (0-11)
datetime_lt$mday         # Day of month
datetime_lt$hour
datetime_lt$min
datetime_lt$sec

# Time zones
Sys.timezone()
as.POSIXct("2024-01-15 12:00:00", tz = "UTC")
as.POSIXct("2024-01-15 12:00:00", tz = "America/New_York")

# ==============================================================================
# File I/O
# ==============================================================================

# Reading files
# read.csv("data.csv")
# read.csv("data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# read.table("data.txt", header = TRUE, sep = "\t")
# readLines("file.txt")
# scan("numbers.txt", what = numeric())
# readRDS("data.rds")
# load("data.RData")

# Writing files
# write.csv(df, "output.csv", row.names = FALSE)
# write.table(df, "output.txt", sep = "\t", row.names = FALSE)
# writeLines(c("line1", "line2"), "output.txt")
# saveRDS(df, "data.rds")
# save(df, df2, file = "data.RData")

# File operations
# file.exists("file.txt")
# file.create("newfile.txt")
# file.remove("file.txt")
# file.copy("source.txt", "dest.txt")
# file.rename("old.txt", "new.txt")
# dir.create("new_directory")
# list.files()
# list.dirs()
# getwd()
# setwd("path/to/directory")

# ==============================================================================
# Error Handling
# ==============================================================================

# Try-catch
result <- tryCatch({
  log(-1)
}, warning = function(w) {
  message("Warning: ", conditionMessage(w))
  NaN
}, error = function(e) {
  message("Error: ", conditionMessage(e))
  NA
}, finally = {
  message("Cleanup code")
})

# Simple try
result <- try(log("not a number"), silent = TRUE)
if (inherits(result, "try-error")) {
  print("An error occurred")
}

# Stop execution with error
check_positive <- function(x) {
  if (x <= 0) {
    stop("Value must be positive")
  }
  sqrt(x)
}

# Warning
warn_negative <- function(x) {
  if (x < 0) {
    warning("Value is negative, taking absolute value")
    x <- abs(x)
  }
  sqrt(x)
}

# Message (informational)
verbose_function <- function(x, verbose = TRUE) {
  if (verbose) message("Processing...")
  x * 2
}

# Custom conditions
condition <- simpleCondition("Custom condition", "myCondition")
# signalCondition(condition)

# ==============================================================================
# Object-Oriented Programming - S3
# ==============================================================================

# Create S3 class
create_person <- function(name, age) {
  obj <- list(name = name, age = age)
  class(obj) <- "person"
  obj
}

# S3 generic function
greet <- function(x, ...) {
  UseMethod("greet")
}

# S3 method for person class
greet.person <- function(x, ...) {
  paste("Hello, my name is", x$name)
}

# Default method
greet.default <- function(x, ...) {
  paste("Hello,", x)
}

# Print method
print.person <- function(x, ...) {
  cat("Person:", x$name, ", Age:", x$age, "\n")
}

# Summary method
summary.person <- function(object, ...) {
  cat("Name:", object$name, "\n")
  cat("Age:", object$age, "\n")
}

# Create instance
alice <- create_person("Alice", 30)
greet(alice)
print(alice)

# ==============================================================================
# Object-Oriented Programming - S4
# ==============================================================================

# Define S4 class
setClass(
  "Person",
  slots = c(
    name = "character",
    age = "numeric",
    email = "character"
  ),
  prototype = list(
    name = NA_character_,
    age = NA_real_,
    email = NA_character_
  )
)

# Validity method
setValidity("Person", function(object) {
  if (object@age < 0) {
    return("Age must be non-negative")
  }
  TRUE
})

# Constructor
Person <- function(name, age, email = NA_character_) {
  new("Person", name = name, age = age, email = email)
}

# Create instance
bob <- Person("Bob", 25, "bob@example.com")

# Access slots
bob@name
bob@age
slot(bob, "email")

# Generic function
setGeneric("introduce", function(x) standardGeneric("introduce"))

# Method
setMethod("introduce", "Person", function(x) {
  paste("I am", x@name, "and I am", x@age, "years old")
})

# Show method (like print)
setMethod("show", "Person", function(object) {
  cat("Person:", object@name, "\n")
  cat("Age:", object@age, "\n")
})

# ==============================================================================
# Object-Oriented Programming - R6
# ==============================================================================

# library(R6)
#
# PersonR6 <- R6Class("PersonR6",
#   public = list(
#     name = NULL,
#     age = NULL,
#
#     initialize = function(name, age) {
#       self$name <- name
#       self$age <- age
#     },
#
#     greet = function() {
#       paste("Hello, I'm", self$name)
#     },
#
#     have_birthday = function() {
#       self$age <- self$age + 1
#       invisible(self)
#     }
#   ),
#
#   private = list(
#     secret = "hidden"
#   ),
#
#   active = list(
#     birth_year = function() {
#       as.integer(format(Sys.Date(), "%Y")) - self$age
#     }
#   )
# )
#
# charlie <- PersonR6$new("Charlie", 35)
# charlie$greet()
# charlie$have_birthday()
# charlie$birth_year

# ==============================================================================
# Environments
# ==============================================================================

# Create environment
env <- new.env()
env$x <- 10
env$y <- 20

# Access environment
get("x", envir = env)
exists("x", envir = env)
ls(env)
rm("x", envir = env)

# Environment hierarchy
globalenv()
baseenv()
emptyenv()
environment()
parent.env(env)

# Search path
search()
searchpaths()

# Lexical scoping
make_counter <- function() {
  n <- 0
  list(
    increment = function() n <<- n + 1,
    get = function() n
  )
}
counter <- make_counter()

# ==============================================================================
# Formulas
# ==============================================================================

# Create formula
f <- y ~ x
f2 <- y ~ x + z
f3 <- y ~ x * z           # With interaction
f4 <- y ~ x:z             # Interaction only
f5 <- y ~ x + I(x^2)      # Polynomial
f6 <- y ~ .               # All other variables
f7 <- y ~ x - 1           # No intercept
f8 <- y ~ log(x) + sqrt(z)  # Transformations

# Formula components
f[[1]]  # ~
f[[2]]  # Response (y)
f[[3]]  # Predictors

# Update formula
update(f, . ~ . + z)

# ==============================================================================
# Statistical Functions
# ==============================================================================

# Descriptive statistics
x <- rnorm(100, mean = 50, sd = 10)
mean(x)
median(x)
sd(x)
var(x)
min(x)
max(x)
range(x)
quantile(x, probs = c(0.25, 0.5, 0.75))
IQR(x)
cor(x, rnorm(100))
cov(x, rnorm(100))

# Distributions
dnorm(0)              # Density
pnorm(0)              # CDF
qnorm(0.975)          # Quantile
rnorm(10)             # Random samples

# Other distributions
rbinom(10, size = 20, prob = 0.5)
rpois(10, lambda = 5)
runif(10, min = 0, max = 1)
rexp(10, rate = 1)
rgamma(10, shape = 2, rate = 1)
rbeta(10, shape1 = 2, shape2 = 5)
rt(10, df = 10)
rchisq(10, df = 5)
rf(10, df1 = 5, df2 = 10)

# Statistical tests
t.test(x)
t.test(x, y = rnorm(100))
wilcox.test(x)
chisq.test(table(sample(1:3, 100, replace = TRUE)))
cor.test(x, rnorm(100))
shapiro.test(x)
ks.test(x, "pnorm")

# Linear regression
model <- lm(mpg ~ wt + hp, data = mtcars)
summary(model)
coef(model)
fitted(model)
residuals(model)
predict(model, newdata = data.frame(wt = 3, hp = 150))
confint(model)
anova(model)

# ANOVA
aov_model <- aov(mpg ~ factor(cyl), data = mtcars)
summary(aov_model)
TukeyHSD(aov_model)

# Generalized linear model
glm_model <- glm(am ~ wt + hp, data = mtcars, family = binomial)
summary(glm_model)

# ==============================================================================
# Graphics (Base R)
# ==============================================================================

# Basic plots
x <- 1:10
y <- x^2

# plot(x, y)
# plot(x, y, type = "l")  # Line
# plot(x, y, type = "b")  # Both points and lines
# plot(x, y, type = "h")  # Histogram-like
# plot(x, y, type = "s")  # Steps

# Customization
# plot(x, y,
#   main = "Title",
#   sub = "Subtitle",
#   xlab = "X axis",
#   ylab = "Y axis",
#   col = "blue",
#   pch = 19,
#   cex = 1.5,
#   lwd = 2,
#   lty = 2,
#   xlim = c(0, 12),
#   ylim = c(0, 120)
# )

# Add elements
# lines(x, y * 0.5, col = "red")
# points(5, 25, col = "green", pch = 17, cex = 2)
# abline(h = 50, col = "gray", lty = 2)
# abline(v = 5, col = "gray", lty = 2)
# abline(a = 0, b = 10, col = "purple")
# text(5, 25, "Point", pos = 4)
# legend("topleft", legend = c("Data", "Fit"), col = c("blue", "red"), lty = 1)

# Multiple plots
# par(mfrow = c(2, 2))  # 2x2 grid
# par(mar = c(5, 4, 4, 2))  # Margins

# Other plot types
# hist(rnorm(1000))
# boxplot(mpg ~ cyl, data = mtcars)
# barplot(table(mtcars$cyl))
# pie(table(mtcars$cyl))
# pairs(mtcars[, 1:4])
# heatmap(as.matrix(mtcars[, 1:7]))
# contour(volcano)
# persp(volcano)

# Save plots
# png("plot.png", width = 800, height = 600)
# plot(x, y)
# dev.off()
#
# pdf("plot.pdf", width = 8, height = 6)
# plot(x, y)
# dev.off()

# ==============================================================================
# Package Management
# ==============================================================================

# Install packages
# install.packages("dplyr")
# install.packages(c("ggplot2", "tidyr"))
# install.packages("package", repos = "https://cran.r-project.org")

# Load packages
# library(dplyr)
# require(ggplot2)  # Returns FALSE if not available

# Check if package is installed
# "dplyr" %in% installed.packages()[, "Package"]

# Package information
# packageVersion("dplyr")
# packageDescription("dplyr")
# help(package = "dplyr")

# Update packages
# update.packages()

# Remove packages
# remove.packages("package_name")

# List loaded packages
# (.packages())
# search()

# ==============================================================================
# Tidyverse Style (dplyr example)
# ==============================================================================

# library(dplyr)
#
# # Pipe operator
# mtcars %>%
#   filter(mpg > 20) %>%
#   select(mpg, cyl, wt) %>%
#   mutate(efficiency = mpg / wt) %>%
#   arrange(desc(efficiency)) %>%
#   group_by(cyl) %>%
#   summarise(
#     count = n(),
#     mean_mpg = mean(mpg),
#     sd_mpg = sd(mpg)
#   )
#
# # Native pipe (R 4.1+)
# mtcars |>
#   subset(mpg > 20) |>
#   transform(efficiency = mpg / wt)

# ==============================================================================
# Parallel Processing
# ==============================================================================

# library(parallel)
#
# # Detect cores
# num_cores <- detectCores()
#
# # mclapply (Unix only)
# result <- mclapply(1:10, function(x) x^2, mc.cores = 4)
#
# # parLapply (cross-platform)
# cl <- makeCluster(4)
# result <- parLapply(cl, 1:10, function(x) x^2)
# stopCluster(cl)
#
# # foreach with doParallel
# library(foreach)
# library(doParallel)
# registerDoParallel(4)
# result <- foreach(i = 1:10, .combine = c) %dopar% {
#   i^2
# }

# ==============================================================================
# Debugging
# ==============================================================================

# Print debugging
print("Debug message")
cat("Value:", x, "\n")
message("Informational message")

# Browser
debug_function <- function(x) {
  # browser()  # Uncomment to debug
  y <- x * 2
  z <- y + 1
  z
}

# Debug mode
# debug(debug_function)
# undebug(debug_function)
# debugonce(debug_function)

# Traceback
# traceback()

# Options
# options(error = recover)
# options(error = NULL)
# options(warn = 2)  # Turn warnings into errors

# ==============================================================================
# Memory Management
# ==============================================================================

# Object size
object.size(mtcars)
format(object.size(mtcars), units = "Kb")

# Memory usage
# memory.size()  # Windows only
# memory.limit()  # Windows only
gc()  # Garbage collection

# Remove objects
rm(x)
rm(list = ls())  # Remove all

# ==============================================================================
# Attributes and Metadata
# ==============================================================================

x <- 1:10

# Set attributes
attr(x, "custom") <- "value"
names(x) <- letters[1:10]
dim(x) <- c(2, 5)
class(x) <- "myclass"

# Get attributes
attr(x, "custom")
attributes(x)

# Common attributes
comment(x) <- "This is a comment"
comment(x)

# ==============================================================================
# Metaprogramming
# ==============================================================================

# Quote expression
expr <- quote(x + y)
expr[[1]]  # +
expr[[2]]  # x
expr[[3]]  # y

# Substitute
f <- function(x) {
  substitute(x)
}
f(a + b)

# Evaluate
eval(expr, envir = list(x = 1, y = 2))

# Parse and deparse
parsed <- parse(text = "1 + 2")
eval(parsed)
deparse(quote(x + y))

# Call manipulation
call_obj <- call("sum", 1, 2, 3)
eval(call_obj)

# Match call
f <- function(x, y) {
  match.call()
}
f(1, y = 2)

# ==============================================================================
# End of R Sample
# ==============================================================================
