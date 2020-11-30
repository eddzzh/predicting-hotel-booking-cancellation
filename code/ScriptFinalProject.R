# Import library ====
library(tidyverse)
library(glmnet)
library(psych)
library(pROC)
library(randomForest)
library(caret)
library(broom)
library(tree)
library(e1071)
library(MASS)
library(ggpubr)

data = read.csv("data/hotel_bookings.csv")
str(data)
missData <- function(x) {
  sum(is.na(x))
}
# Children has 4 missing data
sapply(data, missData)

# Replace children with babies for these 4 observations
for (i in 1:nrow(data)) {
  if (is.na(data[i, "children"])) {
    data[i, "children"] = data[i, "babies"]
  }
}
sum(is.na(data))


# locate character NULL values 
countNull = function(x) {
  sum(x=="NULL")
}
countNullPercent = function (x) {
  sum(x=="NULL") / nrow(data)
}
# $ country                       : int 488     0.00409
# $ agent                         : int 16340   0.137
# $ company                       : int 112593  0.943
str(lapply(data, countNull))
str(lapply(data, countNullPercent))

# Preserve original data by creating a new `df`
df = data
# feature engineering. attempt to answer two questions:
# 1. Are company bookings more/less likely to cancel?
# 2. Are bookings by travel agencies more or less likely to cancel?
df <- df %>% 
  mutate(is_company = ifelse(company == "NULL", 0, 1)) %>%
  mutate(is_agent = ifelse(agent == "NULL", 0, 1)) %>%
  mutate(country_is_missing = ifelse(country == "NULL", 1, 0))

# Transform all character variables into factors. 
df[sapply(df, is.character)] = lapply(df[sapply(df, is.character)], as.factor)


# EDA charts ====
# Reserved room type
two_tone = c('#c1c1c1', '#40ddee')

a1 = df %>% 
  group_by(is_canceled, reserved_room_type) %>% 
  summarise(num = n()) %>%
  group_by(reserved_room_type) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=reserved_room_type, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by reserved room type", x="Proportion", y="Room type") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75) +
  theme(legend.position="none")

# Distribution channel
a2 = df %>% 
  group_by(is_canceled, distribution_channel) %>% 
  summarise(num = n()) %>%
  group_by(distribution_channel) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=distribution_channel, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by distribution channel", x="Proportion", y="Distribution channel") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75) +
  theme(legend.position="right")

# Market segment
a3 = df %>% 
  group_by(is_canceled, market_segment) %>% 
  summarise(num = n()) %>%
  group_by(market_segment) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=market_segment, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by market segment", x="Proportion", y="Market segment") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75) +
  theme(legend.position="none")

# Deposit type
a4 = df %>% 
  group_by(is_canceled, deposit_type) %>% 
  summarise(num = n()) %>%
  group_by(deposit_type) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=deposit_type, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by desposit type", x="Proportion", y="Deposit type") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)


# Meal
a5 = df %>% 
  group_by(is_canceled, meal) %>% 
  summarise(num = n()) %>%
  group_by(meal) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=meal, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by meal type", x="Proportion", y="Meal type") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

# Previous cancellation
a6 = df %>% 
  group_by(is_canceled, arrival_date_month) %>% 
  summarise(num = n()) %>%
  group_by(arrival_date_month) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(arrival_date_month, levels=c("January", "February", "March", "April", "May", 
                                                   "June", "July", "August", "September", "October", 
                                                   "November", "December")), 
             x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by arrival month", x="Proportion", y="Month") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

ggarrange(a1, a2, a6, a4, a5, a3, nrow=3, ncol=2)

# Cancels by hotel type
c1 = df %>% 
  group_by(is_canceled, hotel) %>% 
  summarise(num = n()) %>%
  group_by(hotel) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=hotel, x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by hotel type", x="Proportion", y="Hotel") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

c2 = df %>% 
  group_by(is_canceled, total_of_special_requests) %>% 
  summarise(num = n()) %>%
  group_by(total_of_special_requests) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(total_of_special_requests), x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by number of special requests", x="Proportion", y="Number of special requests") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75) +
  theme(legend.position="right")

c3 = df %>% 
  group_by(is_canceled, is_repeated_guest) %>% 
  summarise(num = n()) %>%
  group_by(is_repeated_guest) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(is_repeated_guest), x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by returning guest", x="Proportion", y="Is returning guest") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

c4 = df %>% 
  group_by(is_canceled, required_car_parking_spaces) %>% 
  summarise(num = n()) %>%
  group_by(required_car_parking_spaces) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(required_car_parking_spaces), x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by required parking spaces", x="Proportion", y="Number of required parkings") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

ggarrange(c1, c2, c3, c4, nrow=2, ncol=2)

# Feature Engineering ====
str(df)
library(lubridate)
e1 = df %>%
  dplyr::mutate(arrival_date = ymd(paste(as.character(df$arrival_date_year),
                                  as.character(df$arrival_date_month),
                                  as.character(df$arrival_date_day_of_month)))) %>%
  dplyr::mutate(day_of_week = wday(arrival_date)) %>% 
  group_by(is_canceled, day_of_week) %>% 
  summarise(num = n()) %>%
  group_by(day_of_week) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(day_of_week), x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by day of week", x="Proportion", y="Day of week") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)
  
df %>% 
  group_by(is_canceled, arrival) %>% 
  summarise(num = n()) %>%
  group_by(required_car_parking_spaces) %>%
  mutate(prop = num/sum(num)) %>%
  ggplot(aes(y=factor(required_car_parking_spaces), x=prop, fill=factor(is_canceled))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=num), size=3.3, position=position_stack(vjust=0.5)) +
  labs(title="Cancels by required parking spaces", x="Proportion", y="Number of required parkings") +
  scale_fill_manual(values=two_tone,
                    name="Booking Status",
                    breaks=c("0", "1"),
                    labels=c("Not canceled", "Cancelled")) + 
  guides(fill=FALSE) +
  theme_light() +
  geom_vline(xintercept=0.3704, color = "black", size = 0.75)

# Model fit ====

# remove features with too many levels
# drop `reservation_status` because it's the same as `is_canceled`
# drop `reservation_status_data` because date of cancellation/checkout/checkin does not correlate to `is_canceled`
# drop `assigned_room_type`, `arrival_date_year` because they do not help with predicting future cancellation
variable_drop = c("reservation_status_date", "reservation_status", "arrival_date_year", "assigned_room_type")
df_ridge_1 <- df %>% dplyr::select(-variable_drop)


# naive ridge ====
# Not run
# x1 <- model.matrix(is_canceled ~ ., data = df_ridge_1)
# y1 <- df_lasso_1$is_canceled
# dim(x1)
# hotel_ridge_cv_1 = cv.glmnet(x1, y1, alpha=0)
# plot(hotel_ridge_cv_1)
# 
# library(broom)
# coef(hotel_ridge_cv_1, s = "lambda.1se") %>%
#   tidy() %>%
#   filter(row != "(Intercept)") %>%
#   top_n(80, wt = abs(value)) %>%
#   ggplot(aes(value, reorder(row, value), color = value > 0)) +
#   geom_point(show.legend = FALSE) +
#   ggtitle("Influential variables") +
#   xlab("Coefficient") +
#   ylab(NULL)
# 
# coeff_ridge_1 <- coef(hotel_ridge_cv_1, s = "lambda.1se") %>%
#   broom::tidy() %>%
#   filter(row != "(Intercept)") %>%
#   arrange(desc(abs(value)))
# 
# options(dplyr.print_max = 1000)
# 
# company_cancel_rate <- df %>%
#   select(is_canceled, company) %>%
#   group_by(company) %>%
#   summarise(n = n(), cancel_rate = sum(is_canceled) / n()) %>%
#   arrange(desc(cancel_rate))
# 
# agent_cancel_rate <- df %>%
#   select(is_canceled, agent) %>%
#   group_by(agent) %>%
#   summarise(n = n(), cancel_rate = sum(is_canceled) / n()) %>%
#   arrange(desc(cancel_rate))
# 
# country_cancel_rate <- df %>%
#   select(is_canceled, country) %>%
#   group_by(country) %>%
#   summarise(n = n(), cancel_rate = sum(is_canceled) / n()) %>%
#   arrange(desc(cancel_rate))
# 
# write_excel_csv(company_cancel_rate, "company_cancel_rate.csv")
# write_csv(agent_cancel_rate, "agent_cancel_rate.csv")
# write_csv(country_cancel_rate, "country_cancel_rate.csv")
# write_csv(coeff_ridge_1, "coeff-ridge1.csv")



# train test split ====
set.seed(123)
n <- nrow(df_ridge_1)
train_ind <- sample(1:n, floor(0.8 * n))
train = df_ridge_1[train_ind, ]
test = df_ridge_1[-train_ind, ]

x_train <- train %>% select(-is_canceled)
x_train_mat <- model.matrix(is_canceled ~ ., data = train)[,-1]
y_train <- train$is_canceled

x_test <- test %>% select(-is_canceled)
x_test_mat <- model.matrix(is_canceled ~ ., data = test)[,-1]
y_test <- test$is_canceled
# 95512   922
dim(x_train_mat)

# train ridge 2 ====
hotel_ridge_cv_2 = cv.glmnet(x_train_mat, y_train, alpha=0, family="binomial")

# Plot full list of influential variables
coef(hotel_ridge_cv_2, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(80, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Full influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

# Plot selected list of influential variables excluding company, agent, country
coef(hotel_ridge_cv_2, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  filter(substring(row, 1, 3) != "com") %>%
  filter(substring(row, 1, 5) != "count") %>%
  filter(substring(row, 1, 5) != "agent") %>%
  top_n(80, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Other influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

# All coeff
coeff_ridge_2 <- coef(hotel_ridge_cv_2, s = "lambda.1se") %>%
  broom::tidy() %>%
  filter(row != "(Intercept)") %>%
  arrange(desc(abs(value)))

# Coeffs excluding company, agent, and country
other_coeff_ridge_2 <- coeff_ridge_2 %>%
  filter(substring(row, 1, 3) != "com") %>%
  filter(substring(row, 1, 5) != "count") %>%
  filter(substring(row, 1, 5) != "agent")

write.csv(other_coeff_ridge_2, "other_coeff_ridge_2.csv")
write.csv(coeff_ridge_2, "full_coeff_ridge_2.csv")

# Prediction
y_pred_ridge_cv_2 <- predict(hotel_ridge_cv_2, s = hotel_ridge_cv_2$lambda.1se, x_test_mat, type="response")

# ROC AUC
ridge_roc <- roc(response = y_test, predictor = y_pred_ridge_cv_2)
plot(ridge_roc, main = "ROC - Ridge regression (AUC = 0.9011)", col = "blue", 
     ylab = "TPR (Sensitivity)", xlab = "FPR (Specificity)")

# Ridge evaluation metrics
#             threshold specificity sensitivity  accuracy    tn   tp   fn
# threshold -0.5872482   0.8106619   0.8355077 0.8198342 12211 7365 1450
rets <- c("threshold", "specificity", "sensitivity", "accuracy", "tn", "tp", "fn", "fp")
ridge_bench <- coords(ridge_roc, "best", "threshold", ret=rets)
ridge_bench$auc <- round(as.numeric(ridge_roc$auc), 3)
str(ridge_bench)




# Variable selection ====
# Randomly select a small subset of raw data for speed
n <- nrow(df_ridge_1)
smallSample <- df_ridge_1[sample(1:n, 3000), ]
train_size <- floor(0.8 * 3000)
train_ind_small <- sample(1:3000, train_size)
train_small <- smallSample[train_ind_small, ]
test_small <- smallSample[-train_ind_small, ]

temp_logistic <- glm(is_canceled ~ ., data=train_small, family="binomial")
pred_temp_logistic <- predict(temp_logistic, newdata=test_small, type="response")
options(max.print = .Machine$integer.max) # print all coefficients
summary(temp_logistic)  # many insignificant variables

# Choose variable using step wise function to optimize AIC
# Call:  glm(formula = is_canceled ~ lead_time + arrival_date_week_number + 
#              stays_in_weekend_nights + country + market_segment + is_repeated_guest + 
#              previous_cancellations + previous_bookings_not_canceled + 
#              booking_changes + deposit_type + days_in_waiting_list + customer_type + 
#              adr + required_car_parking_spaces + total_of_special_requests, 
#            family = "binomial", data = train_small)
# Degrees of Freedom: 2399 Total (i.e. Null);  2308 Residual
# Null Deviance:	    3128 
# Residual Deviance: 1706 	AIC: 1890
stepAIC(temp_logistic, direction="both", trace=T)

temp_logistic_step <- glm(formula = is_canceled ~ lead_time + arrival_date_week_number + 
                           stays_in_weekend_nights + market_segment + is_repeated_guest +
                           previous_cancellations + previous_bookings_not_canceled +
                           booking_changes + deposit_type + days_in_waiting_list + customer_type +
                           adr + required_car_parking_spaces + total_of_special_requests,
                          family="binomial",
                          data=train_small)
summary(temp_logistic_step)
pred_temp_logistic_step <- predict(temp_logistic_step, newdata=test_small, type="response")
# summary statistics of temp logistic model
roc_temp_logistic_step <- roc(test_small$is_canceled, pred_temp_logistic_step)
roc_temp_logistic_step
#           threshold specificity sensitivity  accuracy  tn  tp fn fp
# threshold 0.3996396   0.8364116   0.7285068 0.7966667 317 161 60 62
coords(roc_temp_logistic_step, "best", "threshold", ret=rets)



# What is the output of tree()?
# Error in tree(is_canceled ~ ., data = train_small, mindev = 0.001) : 
# factor predictors must have at most 32 levels
# remove high level variable when fitting classification tree and random forest
high_levels_var <- c("agent", "country", "company")
# fit a classification tree with all variable
temp_tree <- tree(is_canceled~., data=dplyr::select(train_small, -high_levels_var))
pred_temp_tree <- predict(temp_tree, newdata=dplyr::select(test_small, -high_levels_var))
roc_temp_tree <- roc(test_small$is_canceled, pred_temp_tree)
plot(roc_temp_tree)
#           threshold specificity sensitivity accuracy  tn  tp fn fp
# threshold  0.473765   0.9287599   0.6470588    0.825 352 143 78 27
coords(roc_temp_tree, "best", ret=rets)
# Area under the curve: 0.8368
roc_temp_tree$auc
# step wise AIC for classification tree
# Error in UseMethod("extractAIC") : 
#   no applicable method for 'extractAIC' applied to an object of class "tree"
# use variable that are selected by logistic step wise function to see if score would improve 
temp_tree_step <- tree(is_canceled~lead_time + arrival_date_week_number + 
                    stays_in_weekend_nights + market_segment + is_repeated_guest +
                    previous_cancellations + previous_bookings_not_canceled +
                    booking_changes + deposit_type + days_in_waiting_list + customer_type +
                    adr + required_car_parking_spaces + total_of_special_requests,
                  data=train_small)
pred_temp_tree_step <- predict(temp_tree_step, newdata=test_small)
head(pred_temp_tree_step)
roc_temp_tree_step <- roc(test_small$is_canceled, pred_temp_tree_step)
plot(roc_temp_tree_step)
#           threshold specificity sensitivity accuracy  tn  tp fn fp
# threshold 0.4738215   0.8944591    0.638009      0.8 339 141 80 40
coords(roc_temp_tree_step, "best", ret=rets)
# CONCLUSION: use full set of variable except for the high-level categorical variables for tree and random forest


# Test random forest before cv
# Error in randomForest.default(m, y, ...) : 
#   Can not handle categorical predictors with more than 53 categories.
temp_randomForest <- randomForest(factor(is_canceled)~., data=dplyr::select(train_small, -high_levels_var), ntree=50)
pred_temp_randomForest <- predict(temp_randomForest, newdata=dplyr::select(test_small, -high_levels_var), type="prob")
roc_temp_randomForest <- roc(test_small$is_canceled, pred_temp_randomForest[,2])
plot(roc_temp_randomForest)
coords(roc_temp_randomForest, "best", ret=rets)
as.numeric(roc_temp_randomForest$auc)


# Test out SVM before CV
temp_svm <- svm(is_canceled ~ ., data=train_small)
pred_temp_svm <- predict(temp_svm, newdata=test_small)
roc_temp_svm <- roc(test_small$is_canceled, pred_temp_svm)
plot(roc_temp_svm)
roc_temp_svm
#           threshold specificity sensitivity  accuracy  tn  tp fn fp
# threshold 0.1609396   0.8047493    0.760181 0.7883333 305 168 53 74
coords(roc_temp_svm, "best", ret=rets)

# use AIC step variables:
temp_svm_step <- svm(is_canceled ~ lead_time + arrival_date_week_number + 
                  stays_in_weekend_nights + market_segment + is_repeated_guest +
                  previous_cancellations + previous_bookings_not_canceled +
                  booking_changes + deposit_type + days_in_waiting_list + customer_type +
                  adr + required_car_parking_spaces + total_of_special_requests, data=train_small)
pred_temp_svm_step <- predict(temp_svm_step, newdata=test_small)
head(pred_temp_svm_step)
roc_temp_svm_step <- roc(test_small$is_canceled, pred_temp_svm_step)
plot(roc_temp_svm_step)
# Area under the curve: 0.8525
roc_temp_svm_step
#           threshold specificity sensitivity  accuracy  tn  tp fn fp
# threshold 0.2302288   0.8627968   0.7149321 0.8083333 327 158 63 52
coords(roc_temp_svm_step, "best", ret=rets)



# 10-Fold CV ====
nfold <- 10
# 119390
n <- nrow(df_ridge_1)
foldid <- rep(1:nfold, each=floor(n/nfold))[sample(1:n)]
# create a empty data frame to hold out of sample performance metrics
OOS_accuracy <- data.frame(logistic=rep(NA, nfold), random_forest=rep(NA, nfold), tree=rep(NA, nfold), svm=rep(NA, nfold))
OOS_auc <- data.frame(logistic=rep(NA, nfold), random_forest=rep(NA, nfold), tree=rep(NA, nfold), svm=rep(NA, nfold))

for (k in 1:nfold) {
  this_train_ind <- which(foldid != k)  # train on all but fold `k`
  
  # fit four models and one null model for comparison
  model_logistic <- glm(is_canceled ~ lead_time + arrival_date_week_number + 
                          stays_in_weekend_nights + market_segment + is_repeated_guest +
                          previous_cancellations + previous_bookings_not_canceled +
                          booking_changes + deposit_type + days_in_waiting_list + customer_type +
                          adr + required_car_parking_spaces + total_of_special_requests,
                        data=train[this_train_ind, ],
                        family="binomial")
  model_randomForest <- randomForest(factor(is_canceled)~.,
                                     data=dplyr::select(train[this_train_ind, ], -high_levels_var), 
                                     ntree=50,
                                     na.action=na.roughfix)
  model_tree <- tree(is_canceled~.,
                     data=dplyr::select(train[this_train_ind, ], -high_levels_var))
  model_svm <- svm(is_canceled ~ lead_time + arrival_date_week_number + 
                     stays_in_weekend_nights + market_segment + is_repeated_guest +
                     previous_cancellations + previous_bookings_not_canceled +
                     booking_changes + deposit_type + days_in_waiting_list + customer_type +
                     adr + required_car_parking_spaces + total_of_special_requests,
                   data=train[this_train_ind, ])
  
  pred_logistic <- predict(model_logistic, newdata=train[-this_train_ind, ], type="response")
  pred_randomForest <- predict(model_randomForest, newdata=train[-this_train_ind, ], type="prob")
  pred_tree <- predict(model_tree, newdata=train[-this_train_ind, ])
  pred_svm <- predict(model_svm, newdata=train[-this_train_ind, ])
  
  roc_logistic <- roc(train[-this_train_ind, ]$is_canceled, pred_logistic)
  roc_randomForest <- roc(train[-this_train_ind, ]$is_canceled, pred_randomForest[,2])
  roc_tree <- roc(train[-this_train_ind, ]$is_canceled, pred_tree)
  roc_svm <- roc(train[-this_train_ind, ]$is_canceled, pred_svm)
  
  OOS_accuracy$logistic[k] <- coords(roc_logistic, "best", ret="accuracy")
  OOS_accuracy$random_forest[k] <- coords(roc_randomForest, "best", ret="accuracy")
  OOS_accuracy$tree[k] <- coords(roc_tree, "best", ret="accuracy")
  OOS_accuracy$svm[k] <- coords(roc_svm, "best", ret="accuracy")
  
  OOS_auc$logistic[k] <- as.numeric(roc_logistic$auc)
  OOS_auc$random_forest[k] <- as.numeric(roc_randomForest$auc)
  OOS_auc$tree[k] <- as.numeric(roc_tree$auc)
  OOS_auc$svm[k] <- as.numeric(roc_svm$auc)

  print( paste("Iteration ", k, " of ", nfold, "============")  )
}
  
#     logistic random_forest      tree       svm
# 1  0.7934874     0.8407563 0.7929622 0.7870798
# 2  0.7988786     0.8585817 0.7991901  0.794933
# 3  0.8152026     0.8586535 0.8070359 0.8041043
# 4   0.802139     0.8515256 0.7981546 0.8016148
# 5  0.8061364     0.8556268 0.8003573 0.8007776
# 6  0.7982631      0.846186 0.7964843 0.8015067
# 7  0.8066374     0.8533291 0.8049623 0.7990997
# 8  0.8119228     0.8647145 0.8073048 0.8172754
# 9  0.7985777     0.8523321 0.7983685 0.7752562
# 10 0.8062153     0.8587423 0.8018207 0.8008789
OOS_accuracy
#     logistic random_forest      tree       svm
# 1  0.8495086     0.9205392 0.8155882 0.8509948
# 2  0.8469611     0.9248909 0.8175418 0.8575730
# 3  0.8502057     0.9253330 0.8191377 0.8636570
# 4  0.8462211     0.9199655 0.8111780 0.8559121
# 5  0.8500767     0.9234127 0.8189645 0.8586782
# 6  0.8476098     0.9224448 0.8224671 0.8547498
# 7  0.8484701     0.9214276 0.8238227 0.8552903
# 8  0.8530113     0.9255446 0.8233266 0.8605037
# 9  0.8442701     0.9184045 0.8116086 0.8538949
# 10 0.8556146     0.9232828 0.8255791 0.8605070
OOS_auc


colMeans(OOS_auc)
colSums(OOS_accuracy)

OOS_accuracy_numeric = matrix(nrow=10, ncol=4)
for (i in 1:10) {
  for (j in 1:4) {
    OOS_accuracy_numeric[i, j] <- as.numeric(OOS_accuracy[i,j])
  }
}

colnames(OOS_accuracy_numeric) <- c("logistic", "random_forest", "tree", "svm")
colMeans(OOS_accuracy_numeric)
cv <- rbind(colMeans(OOS_accuracy_numeric), colMeans(OOS_auc))
rownames(cv) <- c("accuracy", "AUC")
cv_ridge <- matrix(nrow=2, ncol=1)
cv_ridge[1,1] <- 0.8198342
cv_ridge[2,1] <- 0.9011
cv <- cbind(cv, cv_ridge)
colnames(cv) <- c("logistic", "random_forest", "tree", "svm", "ridge")
#           logistic random_forest      tree       svm     ridge
# accuracy 0.8037460     0.8540448 0.8006641 0.7982526 0.8198342
# AUC      0.8491949     0.9225245 0.8189214 0.8571761 0.9011000
cv

write.csv(OOS_auc, "1.csv")
write.csv(OOS_accuracy_numeric, "2.csv")
write.csv(cv, "OOS-10FoldCV.csv")



# Influential coefficients: reserved_room_type, distribution_channel, deposit_type, market_segment, customer_type, 
# babies, children, meal, arrival_data_month, required_car_parking_spaces



cancel_rate_reserved_room_type_ <- df_ridge_1 %>%
  select(reserved_room_type, is_canceled) %>%
  group_by(reserved_room_type) %>%
  summarise(n = n(), canceled = sum(is_canceled), cancel_rate = canceled / n)
            
write_csv(cancel_rate_reserved_room_type_, "cancel_rate_reserved_room_type_.csv")

str(df_ridge_1)