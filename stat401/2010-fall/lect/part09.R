data <- read.table('USCrime.txt', header = TRUE)

print(names(data))

# 'R' is the reponse (crime rate)
# Since we know some predictors are highly correlated,
# and we don't deal with that problem now,
# we'll focus on the following predictors:
#  Age: number of male aged 14--24 per 1000 population
#  Ed:  mean # of years of schooling
#  Ex0: per capita expenditure on police by government
#  N:   state population
#  U1:  unemployment rate of urban males
#  W:   median family goods

y <- data$R
X <- as.matrix(data[, c('Age', 'Ed', 'Ex0', 'N', 'U1', 'W')])
X <- cbind(1, X)   # Add the constant predictor.
colnames(X)[1] <- 'Const'
n <- length(y)
p <- ncol(X)
alpha <- .05

##### BLOCK 1 #####

# Take a look at the data.
pdf(file = 'USCrime-scm.pdf', width = 8, height = 8)
pairs(cbind(y, X[, -1]))
dev.off()


print(sstotal <- sum(y * y))
print(syy <- sstotal.corrected <- sum((y - mean(y))^2))
    # Does the assignment and printing on one line, to be lazy.
    # 'print' applies on 'syy'.
    # Compare sst and syy!


##### BLOCK 2 #####

# Let's fit a model with the intercept only.
z <- lm.fit(x = X[, 'Const', drop = FALSE], y = y)
    # Selecting a single row or col will return a simple vector
    # by default; use 'drop = FALSE' will keep it a matrix.
    # 'drop' means dropping the dimension info.
print(coef(z))

# We know this estimate should equal the mean.
# Does it?
print(mean(y))

# Calc SSR and SSE.
print(ssr.Const <- sum(fitted(z) ^2))    # SSR
print(sse.Const <- sum(residuals(z) ^2)) # SSE.
    # This should equal syy. Does it?
print(syy)
print(ssr.Const + sse.Const)
    # This should equal sstotal. Does it?
print(sstotal)

# Let's test the significance of this 'pure-intercept' model.
print((ssr.Const / 1) / (sse.Const / (n - 1)))
print(qf(1 - alpha, 1, n - 1))
    # Is the test statistic greater than the critical value?



##### BLOCK 3 #####

# Let's add predictor 'Age'.
z <- lm.fit(x = X[, c('Const', 'Age')], y = y)
print(coef(z))

# Calc SSR and SSE.
print(ssr.ConstAge <- sum(fitted(z) ^2))    # SSR
print(sse.ConstAge <- sum(residuals(z) ^2)) # SSE.
print(ssr.ConstAge + sse.ConstAge)
    # This should equal sstotal. Does it?
print(sstotal)

# Let's test the significance of the coef for 'Age'.
print(((ssr.ConstAge - ssr.Const)/ 1) / (sse.ConstAge / (n - 2)))
print(qf(1 - alpha, 1, n - 2))
    # Is the test statistic greater than the critical value?

# Turns out to be insignificant.
# Compare the sse's:
print(sse.Const)
print(sse.ConstAge)
    # The decrease is indeed small.

# ssr.ConstAge - ssr.Const should be equal to
# sse.Const - sse.ConstAge.
# Is it?
print(ssr.ConstAge - ssr.Const)
print(sse.Const - sse.ConstAge)
    # Both are SSR(Age | Const)

# Out of curiosity,
# is the extra SS of 'Age' the same as the SSR of 'Age' alone?
z <- lm.fit(x = X[, 'Age', drop = FALSE], y = y)
print(ssr.Age <- sum(fitted(z) ^ 2))

# Compare the above with SSR(Age | Const).
# The SSR of 'Age' alone is much larger than its extra contribution
# on top of 'Const'.

# Now, adding 'Age' on top of 'Const' is not significant.
# Does 'Age' alone makes a significant model?
sse.Age <- sum(residuals(z) ^ 2)
print( (ssr.Age / 1) / (sse.Age / (n - 1)) )
print(qf(1 - alpha, 1, n - 1))
    # Is the test statistic greater than the critical value?


##### BLOCK 4 #####

# Does 'Const' and 'Age' as a group make a significant model?
# The answer must be 'yes', given the preceding results.
# But let's do a test anyway.
# The things we need are already computed.
print( (ssr.ConstAge / 2) / (sse.ConstAge / (n - 2)) )
print(qf(1 - alpha, 2, n - 2))



##### BLOCK 5 #####

# Keeping 'Const' and 'Age' in the model,
# let's add 'Ed' and 'Ex0' at once.
z <- lm.fit(x = X[, c('Const', 'Age', 'Ed', 'Ex0')], y = y)
print(coef(z))

ssr.CAEE <- sum(fitted(z) ^ 2)
sse.CAEE <- sum(residuals(z) ^ 2)
print(ssr.CAEE + sse.CAEE)  # This should be equal to 'sstotal'.
print(sstotal)

# Let's test the group.
print( ((ssr.CAEE - ssr.ConstAge) / 2) / (sse.CAEE / (n - 4)) )
print(qf(1 - alpha, 2, n - 4))


