data <- read.table('USCrime.txt', header = TRUE)
print(names(data))

myfit <- lm(R ~ ., data)

# Get 'model frame'.
# This is the data.frame with Y and all X's,
# but with the intercept column.
XY <- model.frame(myfit)

# Get design matrix.
# This is the data.frame with all X columns,
# including the intercept column, i.e. the column
# of 1's (which is the first column).
X <- model.matrix(myfit)

# A scatter plot matrix would be handy for checking
# the relations between the predictors and the response
# as well as between the predictors.
# In here however there are too many predictors, which
# will make the plot very crowded.
# For illustration, we show the plot of 'Ex0 vs Ex1'
# only, since we know these two are suspects of
# high correlation.
pdf(file = 'part10-pairs.pdf', width = 5, height = 5)
plot(data$Ex0, data$Ex1)
dev.off()

# SS involving 'Ex0' and 'Ex1'.
sse.full <- deviance(myfit)

myfit.red <- update(myfit, . ~ . - Ex0 - Ex1)
    # Drop 'Ex0' and 'Ex1'.
sse.red <- deviance(myfit.red)

myfit.ex0 <- update(myfit.red, . ~ . + Ex0)
    # Include 'Ex0', but not 'Ex1'.
myfit.ex1 <- update(myfit.red, . ~ . + Ex1)
    # Include 'Ex1', but not 'Ex0'.

sse.ex0 <-deviance(myfit.ex0)
sse.ex1 <-deviance(myfit.ex1)

# SSR(Ex0 | others but not Ex1)
print(sse.red - sse.ex0)

# SSR(Ex0 | others incl. Ex1)
# This should be much smaller than the above,
# if 'Ex1' is highly correlated with 'Ex0'.
print(sse.ex1 - sse.full)

# SSR(Ex1 | others but not Ex0)
print(sse.red - sse.ex1)

# SSR(Ex1 | others incl. Ex0)
# This should be much smaller than the above,
# if 'Ex1' is highly correlated with 'Ex0'.
print(sse.ex0 - sse.full)

# SSR(Ex0, Ex1 | others)
# This should be much smaller than
#   SSR(Ex0 | others but not Ex1)
# +
#   SSR(Ex1 | others but not Ex0)
# if 'Ex0' and 'Ex1' are highly correlated.
print(sse.red - sse.full)


# Which predictors would be significant
# if we performed a 't' test?
print(summary(myfit)$coefficients, digits = 2)


# VIF
cc <- cor(X[, -1])
    # Correlation matrix, excluding the intercept column.
print(diag(solve(cc)))
    # 'solve' finds the inverse.
    # 'diag' extracts diagonal elements.

# xfit <- lm(Ex0 ~ . - R, data)
# xfit.red <- lm(Ex0 ~ 1, data)
# R2 <- 1 - sum(resid(xfit)^2) / sum(resid(xfit.red)^2)
# print(1 / (1 - R2))
