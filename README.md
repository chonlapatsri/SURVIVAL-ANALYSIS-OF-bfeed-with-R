# SURVIVAL-ANALYSIS-OF-bfeed-with-R
In this report, the general prediction model is, firstly, created using every feature provided 
in the data frame with the Kaplan Meier (KM) estimator to determine the survival function. Then, 
the hypotheses testing is carried to enhance the model, by dropping and merging statistically 
insignificant features with testing methods including log-rank test on KM model, and Wald test 
and likelihood ratio test on cox proportional hazard model (Coxph) to address two Types of errors.
Type-I error is when the null hypothesis cannot be rejected when the feature has no impact, whereas 
the Type-II error the null hypothesis is rejected when the feature has an impact. Finally, the model’s 
quality is tested with the residual analysis.

The bfeed data frame (Klein and Moeschberger , 1997) from KMSurv library contains 927 
rows and 10 columns of data about breastfeeding completion. The principal parameters of bfeed 
for survival analysis are “delta” and the temporal variable “duration”
