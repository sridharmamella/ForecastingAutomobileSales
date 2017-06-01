#Script made by FWE

#Loading required R packages
require(httr)
require(XML)
require(xlsx)
require(stringr)
require(tidyr)
require(dplyr)
require(lubridate)
require(zoo)
require(ggplot2)
require(caret)
require(randomForest)
require(doParallel)


data <- data.frame(TOTALSALES_GDPUNEMPLOYMENT_GER_10yrs_FWE2_manipulated_without_rownumbers4, row.names = NULL, check.rows = FALSE,
           check.names = TRUE, fix.empty.names = TRUE,  
           stringsAsFactors = default.stringsAsFactors()) #added by FWE, these lines are not part of the tutorial.

# Script taken from the Tutorial
n <- dim(data)[1]
in.test <- seq(n - (n %/% 10 * 2), n)  # integer division
test <- data[in.test, ]
train <- data[-in.test, ]
rm(list = c('n', 'in.test'))


############################################## Linear Model  ##############################################################

linear <- lm(SalesM ~ ., data = train)

MSE <- function(actual, pred) {   #Mean standard error
  error <- actual - pred
  return(sum(error**2) / length(error))
}

SE_bounds <- function(pred, se) {   #Confidence intrerval
  return(list(upper = pred + 1.96 * se, lower = pred - 1.96 * se))
}

linfit <- predict(linear, newdata = test, se.fit = TRUE)

lm.pred <- data.frame(date = rownames(data), actual = data$SalesM, 
                      predicted = c(rep(NA, dim(data)[1] - length(linfit$fit)), 
                                    linfit$fit),
                      se_l = c(rep(NA, dim(data)[1] - length(linfit$fit)),
                               SE_bounds(linfit$fit, linfit$se.fit)$lower),
                      se_u = c(rep(NA, dim(data)[1] - length(linfit$fit)),
                               SE_bounds(linfit$fit, linfit$se.fit)$upper))

tick.dates <- c('2007-01-01', '2008-01-01', '2009-01-01', '2010-01-01',
                '2011-01-01', '2012-01-01', '2013-01-01', '2014-01-01', '2015-01-01', '2016-01-01', '2016-12-01')  # are not shown in the graph until now.


ggplot(lm.pred, aes(x = date, y = actual)) + geom_point(alpha = .7) + 
  geom_point(aes(y = predicted), color = 'red', alpha = .7) +
  scale_x_discrete(breaks = tick.dates) + 
  geom_smooth(aes(y = predicted, ymin = se_l, ymax = se_u, group = 1), 
              , color = 'red', linetype = 0 , stat = 'identity') +
  labs(title = 'Linear model actual (black) and predicted (red) sales', x = 'Month', 
       y = 'Auto Sales (m)')



############################################## Random forests ##############################################################

cl <- makePSOCKcluster(4)
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)

randForest <- train(SalesM ~ ., data = train, method = 'rf',
                    trControl = trainControl(method = 'cv', number = 10),
                    prox = TRUE, allowParallel = TRUE, importance = TRUE)


rf.fit <- predict(randForest, test)

rf.pred <- data.frame(Month = rownames(data), actual = data$SalesM,
                      predicted = c(rep(NA, dim(data)[1] - length(rf.fit)), 
                                    rf.fit))
ggplot(rf.pred, aes(x = Month, y = actual)) + geom_point(alpha = .7) + 
  geom_point(aes(y = predicted), color = 'red', alpha = .7) +
  scale_x_discrete(breaks = tick.dates) + 
  labs(title = 'Cross-validated random forest actual (black) and predicted (red) sales', 
       x = 'Month', y = 'Auto Sales (m)')


############################################## 



