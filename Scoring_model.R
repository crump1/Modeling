
# Scoring Model
# Load data
data<- read.csv("https://d18ky98rnyall9.cloudfront.net/_1750936296f8224de16fad133923d350_purchases.txt?Expires=1545782400&Signature=ez839isqnRDLcHdWwmwQbJa9~jmgI3AgzaFqMiwCeuoYWeYsdQxbS8HoSWjP-g-nNJsizi49D9DehE3YkDUBHlyBi5yueETMGGBzJlvwbG605w8MLUXsZMqB6q8hI7UvrC89ZzVCrgUtPjjX2G08x3B1DeqJszd-5pzIBGtwix4_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A", 
                header = FALSE, sep = '\t', dec = '.')

# Manipulate some data
colnames(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase <- as.Date(data$date_of_purchase, "%Y-%m-%d")
data$year_of_purchase <- as.numeric(format(data$date_of_purchase, "%Y"))
data$days_since <- as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))
# Now write a SQL query to extract and format the first object
library(sqldf)

customers_2014 <- sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'avg_amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")
# Revenue for 2015
revenue_2015 <- sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")
# Merge the two
in_sample <- merge(customers_2014, revenue_2015, all.x = TRUE)
# Convert NA to 0
in_sample$revenue_2015[is.na(in_sample$revenue_2015)] = 0
# Create new variable
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)

head(in_sample)
summary(in_sample)

library(nnet)

# Based on the output, build a model using multinom()  
prob.model <- multinom(active_2015 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)
coef <- summary(prob.model)$coefficients
std  <- summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std)

# Pull just those who bought something
z <- which(in_sample$active_2015 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])

amount.model <- lm(revenue_2015 ~ avg_amount + max_amount, data = in_sample[z, ])
summary(amount.model)

plot(x = in_sample[z, ]$revenue_2015, y = amount.model$fitted.values)
# Not looking good...let's log the model and plot again
amount.model <- lm(log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample[z, ])
summary(amount.model)

plot(x = log(in_sample[z, ]$revenue_2015), y = amount.model$fitted.values)

# Today's RFM variables
customers_2015 <- sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount'
                        FROM data GROUP BY 1")

# Make predictions based on today's data
customers_2015$prob_predicted    <- predict(prob.model, newdata = customers_2015, type = "probs")
customers_2015$revenue_predicted <- exp(predict(amount.model, newdata = customers_2015))
customers_2015$score_predicted   <- customers_2015$prob_predicted * customers_2015$revenue_predicted

summary(customers_2015$prob_predicted)
summary(customers_2015$revenue_predicted)
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)

z <- which(customers_2015$score_predicted > 50)
print(length(z))