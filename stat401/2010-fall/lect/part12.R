data <- read.table('RealEstateSales.txt', header = TRUE)
data <- data[data$Style %in% c(1,2,3,7), ]
data$Style <- factor(LETTERS[data$Style])
data$Price <- data$Price / 100000
    # Change unit of price so that the numbers are not so big.

print(names(data))

data <- data[, c('Price', 'SquareFeet', 'Style',
            'NumBed', 'NumBath', 'GarageSize', 'AC')]
    # 'Style': qualitative
    # 'AC': 0 or 1
    # 'GarageSize': number of car spots, integer.

pdf(file = 'part12-pairs.pdf', width = 7, height = 7)
pairs(data)
dev.off()


myfit <- lm(Price ~ ., data)
print(myfit)


#################
#### Block 1 ####
#################

print(anova(myfit))

# 'aov' provides the same info in a more concise form.
# Compare the numbers below with the output of 'anova'.
print(aov(myfit))

# For 'aov', you can provide the formula directly
# instead of the fitted object:
print(aov(Price ~ ., data))

# You can not do this with 'anova'.
# 'anova' needs a fitted model object as its first argument.

# The result of 'anova' and 'aov'
# depends on the order in which the predictors
# enter the model.
# Note the order of the predictors above.
# How did R choose this order?

print(names(data))
    # This shows the order of predictors is simply
    # the order in which they appear in the 'data' data.frame.
    # R did not do any alphabetic ordering etc.

# If we change the order of the predictors,
# the result is different:
print(aov(lm(Price ~ AC + SquareFeet + NumBath
    + GarageSize + NumBed + Style, data)))


#################
#### Block 2 ####
#################

print(drop1(myfit))

# Although rarely necessary,
# we could specify what terms to consider dropping
# by the 'scope' argument.
# Without this, each term will be dropped in turn.
print(drop1(myfit, scope = ~ SquareFeet + Style + NumBed))

# To understand what these numbers are,
# let's verify several of them.

# The first row contains the SSE (Residual Sum of Squares, 'RSS')
# and AIC of the full model:
print(deviance(myfit))

n <- nrow(data)
print(n * log(deviance(myfit) / n) + 2 * length(coef(myfit)))

# Each of the other rows drops by term from the full model,
# and shows the extra SSR of the dropped term,
# the SSE ('RSS' in R output) of the reduced model,
# and AIC of the reduced model.

# Note the Df of each dropped term.
# In particular, the 'Df' of 'Style'.

newfit <- update(myfit, . ~ . - SquareFeet)
print(deviance(newfit) - deviance(myfit))
print(deviance(newfit))
print(n * log(deviance(newfit) / n) + 2 * length(coef(newfit)))

newfit <- update(myfit, . ~ . - NumBed)
print(deviance(newfit) - deviance(myfit))
print(deviance(newfit))
print(n * log(deviance(newfit) / n) + 2 * length(coef(newfit)))

# There is an analogous function 'add1',
# which begins with a simpler model, e.g.
#   lm(Price ~ 1, data)
# and considers adding each candidate term specified
# by 'scope'.

m1 <- lm(Price ~ 1 + SquareFeet, data)
print(add1(m1, scope = ~ . + NumBed + Style + AC))
    # Note the 'scope' argument.
    # On the RHS of '~' we use '.' to mean what's already
    # in the model.
    # To the left of `~' you can write '.' or omit it.

# For some technical reason,
# we prefer 'drop1' to 'add1'.


