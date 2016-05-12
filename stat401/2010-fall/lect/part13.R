data <- read.table('USCrime.txt', header = TRUE)

# 'R' is the reponse (crime rate).
# We'll focus on the following predictors:
#  Age: number of male aged 14--24 per 1000 population
#  Ed:  mean # of years of schooling
#  Ex0: per capita expenditure on police by government
#  N:   state population
#  U1:  unemployment rate of urban males
#  W:   median family goods

data <- data[, c('R', 'Age', 'Ed', 'Ex0', 'N', 'U1', 'W')]

n <- nrow(data)
print(n)

myfit <- lm(R ~ ., data)

# 'h_ii', or 'leverage' for each observation:
print(hatvalues(myfit))

# Studentized residuals:
rst <- rstudent(myfit)
print(rst)

# Make a normal probability plot.
pdf(file = 'part13-rstud-qq.pdf', width = 5, height = 5)
qqnorm(rst, main = 'Norm QQ plot of studentized residuals')
qqline(rst)
dev.off()

# Cook's distance of each case:
print(cooks.distance(myfit))

