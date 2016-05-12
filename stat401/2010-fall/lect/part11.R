library(lattice)

data <- read.table('RealEstateSales.txt', header = TRUE)
data <- data[data$Style %in% c(1,2,3,7), ]
data$Style <- LETTERS[data$Style]
trellis.device(pdf, color = FALSE,
    file = 'part11-sales.pdf', width = 7, height = 5)
print(xyplot(
    Price ~ SquareFeet | Style,
    data,
    prepanel = prepanel.lmline,
    panel = function(x, y, ...) {
        panel.xyplot(x, y, cex = .5)
        panel.lmline(x, y)
        }
    ))
dev.off()

print(names(data))
print(data$Style)

print(lm(Price ~ SquareFeet + Style, data))

data$Style <- factor(data$Style)
    # Or 'as.factor' in this case.
fit <- lm(Price ~ SquareFeet + Style, data)
print(fit)

print(summary(fit), digits = 2)

# library(MASS)
#     # The 'cats' datasets is in the package 'MASS'.
# print(cats)
# 
# levels(cats$Sex) <- c('Female', 'Male')
# trellis.device(pdf, color = FALSE,
#     file = 'part11-cat1.pdf', width = 7, height = 5)
# print(xyplot(Hwt ~ Bwt | Sex, cats,
#     prepanel = prepanel.lmline,
#     panel = function(x, y, ...) {
#         panel.xyplot(x, y, cex = .5)
#         panel.lmline(x, y)
#         },
#     xlab = 'Body weight (kg)',
#     ylab = 'Heart weight (gm)'
#     ))
# dev.off()

