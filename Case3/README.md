Compared classifiers in identifying whether a bank is about to go bankrupt or not, on the basis of 10 internal factors.

1. Confusion matrix for **Decision Tree using CART algorithm**:

| Prediction | Bankrupt | Still operating |
| ---------- | --------- | ------------- |
| Bankrupt | 60 | 28 |
| Still operating | 50 | 262 |

Accuracy : 80.5 %           
95% CI : (0.7627, 0.8427)
No Information Rate : 0.725           
P-Value [Acc > NIR] : 0.0001352       
Kappa : 0.4786          
Mcnemar's Test P-Value : 0.0174171       
Sensitivity : 0.5455          
Specificity : 0.9034          
Pos Pred Value : 0.6818          
Neg Pred Value : 0.8397
Prevalence : 0.2750          
Detection Rate : 0.1500          
Detection Prevalence : 0.2200          
Balanced Accuracy : 0.7245  

2. Confusion matrix for **K-Nearest Neighbor algorithm** using centered and scaled attributes: 

| Prediction | Bankrupt | Still operating |
| ---------- | --------- | ------------- |
| Bankrupt | 59 | 29 |
| Still operating | 51 | 261 |

Optimal value : k = 29
Accuracy : 80 %          
95% CI : (0.7574, 0.8381)
No Information Rate : 0.725           
P-Value [Acc > NIR] : 0.0003348       
Kappa : 0.4652          
Mcnemar's Test P-Value : 0.0188810       
Sensitivity : 0.5364          
Specificity : 0.9000          
Pos Pred Value : 0.6705          
Neg Pred Value : 0.8365          
Prevalence : 0.2750          
Detection Rate : 0.1475          
Detection Prevalence : 0.2200          
Balanced Accuracy : 0.7182  

3. Confusion matrix for **Bayesian Generalized Logistic Regression** algorithm:

| Prediction | Bankrupt | Still operating |
| ---------- | --------- | ------------- |
| Bankrupt | 37 | 8 |
| Still operating | 73 | 262 |

Accuracy : 79.75 %         
95% CI : (0.7547, 0.8358)
No Information Rate : 0.725           
P-Value [Acc > NIR] : 0.0005153       
Kappa : 0.3781          
Mcnemar's Test P-Value : 1.151e-12       
Sensitivity : 0.3364          
Specificity : 0.9724          
Pos Pred Value : 0.8222          
Neg Pred Value : 0.7944          
Prevalence : 0.2750          
Detection Rate : 0.0925          
Detection Prevalence : 0.1125          
Balanced Accuracy : 0.6544 

**Result:** The KNN model performed the best, at 80.5 %
