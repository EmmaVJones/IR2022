x <- c(3.44, 4.55, 4.93, 9.93, 5.50, 4.59, 
       3.24, 3.25, 1.62, 1.02, 1.04, 1.02, 
       3.45, 4.05, 2.26, 4.52, 3.22, 1.21, 4.08)
threshold <- 5.0
# since threshold has 2 sig figs then 2 exceedance in 19 measures

( sum(round(x, digits = 0) > round(threshold, digits = 0)) / length(x) ) * 100


( sum(round(x, digits = 1) > round(threshold, digits = 2)) / length(x) ) * 100


( sum(round(x, digits = 2) > round(threshold, digits = 2)) / length(x) ) * 100

( sum(round(x) > round(threshold, digits = 2)) / length(x) ) * 100


( sum(round(x, digits = 3) > round(threshold, digits = 2)) / length(x) ) * 100


x1 <- c(3.44, 3.50, 3.51, 3.8)
threshold1 <- 3.5

round(x1, 1) > threshold1

signif(x1, digits = 2)


options(digits = 5)


round(x, digits = 0)
round(x, digits = 1)
round(x, digits = 2)


# for the decision point
y <- c(4.5, 5, 9.5, 10, 10.5, 10.555, 10.6, 11.0, 11.1, 11)
ceiling(y)
floor(y)
trunc(y, 3)

DescTools::RoundTo(y, 0.5, floor)
rbind(y= y, round = round(y))
rbind(y= y, round0 = round(y, digits = 0))
rbind(y= y, round1 = round(y, digits = 1))
rbind(y= y, round2 = round(y, digits = 2))

round(y)
