# How Can Hotel Manager Develop Prescient Knowledge of Room Cancellation?

## Quick Overview
Traditionally, occupancy rate is considered to be one of the top three most useful KPI's the efficiency of hotel's revenue management strategy. One of the culprit of hotel revenue loss is cancellation on short notice. The objective of this project is to build a binary classifier to forecast whether a hotel booking will be cancelled or completed based on range of booking features. The analysis could help hotel management to develop prescient knowledge of early indicators for cancellation. The prediction results can be used to devise overbooking strategies to recuperate revenue losses from bookings with high chance of cancellation.

- Optimized Logistic, Ridge, Lasso, Elastic Net, and Random Forest Regressors using Step-wise AIC, GridsearchCV, 10-Fold CV.
- Random forest model performed the best in predicting cancellation using unseen data, achieving an AUROC of 0.9225 and an accuracy of 0.9225.
- This README provides a snapshot of the full report. Checkout my website if you want to know more - https://eddie-zhang.com/

## Credit
**Course**: DESC 520Q - Data Science for Business

**Professor**: Natesh Pillai

**Team**: Eddie Zhang, Mindy Li, Yuenan Liu, Tong Yu, and Ruojin Xu

**Data Source**: Nuno Antonio, Ana Almeida, and Luis Nunes (https://www.sciencedirect.com/science/article/pii/S2352340918315191)

## Data Cleaning
The dataset contains 119,386 observations with 32 variables of which 14 are categorical variables 18 are numerical variables. In this predictive analysis, the target is the binary variable, is_canceled. Although the original authors have extensively pre-processed the data for modeling, the data set is not completely clean. I have found two types of missing data. One type is the system-defined NA values. The other type was harder to identify because they were entered as characters in different ways, such as 'NaN' and 'NULL'. These variables are `country` (0.409% missing), `agent` (13.7% missing), `company` (94.3% missing), `children` (~0% missing). Below is a summary of the missing values and their corresponding treatments:

| Variable | # Missing | % Missing | Treatment |
| --- | --- | --- | --- |
| `country` | 488 | 0.409% | Feature engineer - `country_is_missing` |
| `agent` | 16340 |13.7% | Feature engineer - `agent_is_missing` |
| `company` | 1122594 |94.3%| Drop in modeling |
| `children` | 4 |0| Impute with the value of `babies` |

## Math and Metrics
The metrics we select to measure the performance of models need to achieve two objectives: 1) The metric needs to be robust and not prone to random sampling error or the choice of threshold values and 2) is straight-forward, has practical interpretation, and easy to explain to stakeholders. To achieve the first objective, we will use ROC AUC; for the second, we will use accuracy since is easy to understand and has high interpretability for the management level. 

![\Large x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}](https://latex.codecogs.com/svg.latex?\Large&space;Accuracy=\frac{TP+TN}{T+N}) 

To establish ROC AUC, let's first define so me basic classification benchmarks:

![\Large x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}](https://latex.codecogs.com/svg.latex?\Large&space;TPR=\frac{TP}{FP+TP}=Precision) 

![\Large x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}](https://latex.codecogs.com/svg.latex?\Large&space;ROC=\frac{TPR}{FPR}=\frac{\frac{TP}{TP+FN}}{\frac{FP}{FP+TN}}) 

![\Large AUC=\int_{x=0}^1TPR(FPR^{-1}(x))dx](https://latex.codecogs.com/svg.latex?\Large&space;AUC=\int_{x=0}^1TPR(FPR^{-1}(x))dx) 

An ROC curve has TPR on y-axis and FPR on x-axis for all classification threshold in [0,1]. The benefit of ROC curve is that it visualizes model performance on all possible thresholds, whereas a single misclassification rate is error rate for a single threshold. For every possible threshold value, draw a point (x, y) = (FPR,TPR) to form a ROC curve.

A poor classification model will have a ROC curve very close to the diagonal line and AUC (area under curve) close to 0.5 at which a purely random classifier will have an AUC of 0.5 = random guessing.

All that ROC AUC care about is how well the model separate the two classes, and thus it is said to be only sensitive to "rank ordering". You can think of AUC as the probability that a classifier will rank a randomly chose positive observation higher than a randomly chosen negative observation.

## Model Performance
K-fold cross validation method evaluates a model's performance on different subsets of the training data and then calculates the average benchmark. A K-fold CV is a robust way of measuring a model's performance with lower bias than other methods. To measure each model’s capability in predicting cancellation, we performed out-of-sample 10-fold cross validation on each model and calculated average accuracy and AUC. The results are displayed in Exhibit 9. By all accounts, the random forest model performed the best out of the five models we considered with an out-of-sample accuracy of 0.8540 and AUC of 0.9255.

|  |AIC-optimized logistic|random forest|classification tree|svm|ridge|
|---|---|---|---|---|---|
|Accuracy|0.8037|0.8540|0.8007|0.7983|0.8198|
|AUROC|0.8492|0.9225|0.8189|0.8572|0.9011|

## Model Deployment
Our model’s key idea is to help hotel managers in both city hotels and resort hotels deal with booking cancellation. In the hotels’ system, managers equipped with our model will learn the customer behaviors and deploy the booking strategy in advance. For the hotel and tourism industry, attracting customers to book the room is the first step. The hotels earn a profit if the customers visit the hotel. This model aims to analyze the features that may cause the customers to cancel the hotel rooms. We will mainly focus on two audiences for our business model:

First, the hotel manager can use our business model to target customers. Our business model provides detailed features for predicting customer behaviors. For example, customers who have special requests will be less likely to cancel the reservation. Therefore, the hotel manager would be able to forecast the cancellation rate based on customer features and then carry out strategies such as overbooking accordingly. In a word, the hotel can optimize its profit by applying our business model.

Second, the hotel’s senior VPs can use our business model to visualize the hotel's performance. They can use our business model as a benchmark to supervise the hotel managers' strategy to make decisions that will be necessary to keep the company financially healthy.
