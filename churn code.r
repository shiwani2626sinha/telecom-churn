1. Churn_data <- read.csv(file.choose())
2.  
3. str(Churn_data)
4.  
5. Churn_data$Churn <- factor(Churn_data$Churn)
6. str(Churn_data)
7.  
8. #In sample prediction
9. logit_model <- glm(Churn ~ ., data = Churn_data, family = "binomial")
10.  
11. #Predict Churn column
12. Churn_data$Churn_Predicted <- predict(logit_model, data = Churn_data, type = "response")
13.  
14. #Change probabilities to class (0 or 1/Yes or No)
15. Churn_data$Churn_Predicted <- ifelse(Churn_data$Churn_Predicted > 0.5,1,0)
16.  
17. #Confusion Matrix
18. table(Churn_data$Churn,Churn_data$Churn_Predicted)
19. misClassError <- mean(Churn_data$Churn_Predicted != Churn_data$Churn)
20.  
21. #Accurancy
22. print(paste('Accuracy =',1-misClassError))
23.  
24. ###################################################################################
25. #Out of sample error
26. Churn_data <- read.csv(file.choose())
27.  
28. str(Churn_data)
29.  
30. Churn_data$Churn <- factor(Churn_data$Churn)
31. str(Churn_data)
32.  
33. # Step 1:Split data in train and test data
34. #install.packages("caTools")
35. library(caTools)
36. set.seed(390)
37. split <- sample.split(Churn_data, SplitRatio = 0.7)
38. split
39.  
40. train <- subset(Churn_data, split== "TRUE")
41. test <- subset(Churn_data, split== "FALSE")
42. str(train)
43. str(test)
44.  
45. # Step 2:Train model with logistics regression using glm function
46. logit_model <- glm(Churn ~ ., data = train, family = "binomial")
47. logit_model
48.  
49. summary(logit_model)
50.  
51.  
52. logit_model <- glm(Churn ~ ContractRenewal + CustServCalls + RoamMins , data = train, family = binomial)
53. summary(logit_model)
54. # Null Deviance = (fit dependent variable only with intercept)
55. # Residual Deviance = (fit dependent variable with all the independent variable)
56. # AIC (lesser the better, used for comparing different models)
57.  
58.  
59. # Step 3:Predict test data based on trained model -logit_model
60. fitted.results <- predict(logit_model, test, type = "response")
61.  
62. fitted.results # Predicted Result
63. test$Churn   # Actual Result
64.  
65.  
66. fitted.results.new <- fitted.results
67. # Step 4: Change probabilities to class (0 or 1/Yes or No)
68. # If prob > 0.5 then 1, else 0. Threshold can be set for better results
69. #fitted.results <- ifelse(fitted.results > 0.5,1,0)
70. fitted.results.new <- ifelse(fitted.results.new > 0.3,1,0)
71. #fitted.results <- ifelse(fitted.results > 0.25,1,0)
72.  
73. fitted.results.new # Predicted Result
74. test$Churn    # Actual Result
75.  
76.  
77. # Step 5: Evauate Model Accuracy using Confusion matrix
78. table(test$Churn, fitted.results.new)
79. misClassError <- mean(fitted.results.new != test$Churn  )
80. print(paste('Accuracy =',1-misClassError))
81.  
82. #Model evaluation using confusion matrix function
83. library(caret)
84. confusionMatrix(table(test$Churn, fitted.results.new))
85.  
86. # ROC-AUC Curve
87. #install.packages("ROCR")
88. library(ROCR)
89. ROCRPred <- prediction(fitted.results.new, test$Churn  )
90. ROCRPerf <- performance(ROCRPred, measure ="tpr", x.measure ="fpr")
91. plot(ROCRPerf)
92. plot(ROCRPerf, colorize = TRUE)
93. plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
94. plot(ROCRPerf, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1),main = "ROC CURVE")
95. abline(a=0, b=1)
96.  
97. auc <- performance(ROCRPred, measure = "auc")
98. auc <- auc@y.values[[1]]
99. auc
100. auc <- round(auc, 4)
101. legend (.5,.4,auc, title = "AUC", cex =1)
102. #################################################################################

