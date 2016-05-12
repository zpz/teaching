data <- read.table('senic.txt', header = FALSE)
print(names(data))
data <- data[, c(3, 4, 9)]
names(data) <- c('age', 'risk', 'region')

# 'region' is a numerical now, noticing
# its values are 1, 2, 3, 4.
# Convert it to categorical.
data$region <- as.factor(data$region)
print(is.factor(data$region))
print(class(data$region))

# Make a plot.
pdf(file = 'part16-a.pdf', width = 7, height = 5)
plot(x = data$region, y = data$risk, xlab = 'region', ylab = 'risk')
dev.off()
    # Whoops! Not what you hoped for!
    # Since 'x' is of 'factor' type,
    # R does not think a numerical scale makes sense to it.
    # R makes a boxplots of all 'y' values with a common level
    # of the factor 'x'. This plot actually IS quite informative.

# To make something similar to  Figure 16.3, page 686:
pdf(file = 'part16-b.pdf', width = 7, height = 5)
plot(x = as.numeric(data$region), y = data$risk, xlab = 'region', ylab = 'risk')
dev.off()



# Let's fit a linear model and take a look.
lmfit <- lm(risk ~ region, data)
print(lmfit)
print(summary(lmfit))
    # Nothing we didn't see before.

# The cell-means formulation does not use intercept.
# So let's refit.
myfit <- lm(risk ~ -1 + region, data)
print(myfit)
print(summary(myfit))


# Since we know the predictors are all qualitative,
# hence it is a ANOVA model, it is more natural to do that
# directly:
aovfit <- aov(risk ~ region, data)
print(aovfit)
aovsummary <- summary(aovfit)
print(aovsummary)
    # This gives us the ANOVA table that we pretty much
    # can copy and use directly for testing
    #   mu1 = mu2 = ....
    #
    # If you need to make inferences about individual treatment effects,
    # you need the estimate and standard error of each coefficient.
    # In that case you need to use 'myfit'.

# If all this looks confusing, perhaps the following
# brute force procedure is better.
fit.x <- lm(risk ~ -1 + region, data)
fit.0 <- lm(risk ~ 1, data)
sse.x <- deviance(fit.x)
sse.0 <- deviance(fit.0)
df.x <- length(coef(fit.x))  # 4
df.0 <- 1
df.ssr <- df.x - df.0
df.sse <- nrow(data) - df.x
ssr <- sse.0 - sse.x            # extra SSR
msr <- ssr / df.ssr
mse <- sse.x / df.sse
f.star <- msr / mse
f.crit <- qf(.95, df.ssr, df.sse)
p.val <- 1 - pf(f.star, df.ssr, df.sse)
print(c(df.ssr = df.ssr, df.sse = df.sse))
print(c(
    ssr = ssr, sse = sse.x,
    msr = msr, mse = mse,
    f.star = f.star, f.crit = f.crit, p.value = p.val)
    )
