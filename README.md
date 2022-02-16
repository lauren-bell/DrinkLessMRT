# DrinkLessMRT
Micro-Randomized Trial to Optimize Notification Policy of Drink Less

This repository includes the R code to estimate the marginal excursion effect, for the primary and secondary objectives of the Drink Less MRT. 
For details of the Estimator of the Marginal Excursion Effect, see https://doi.org/10.1093/biomet/asaa070

The datafiles for the MRT are available from the Open Science Framework site: https://osf.io/w3szp/ This code uses the datafile "FINAL Dataset_A.rds". 

The file "pooled estimates.R" calculates the effect of both notifications, pooled together, compared to no notificiation. This file requires the source code "estimator_EMEE.R". 

The file "by notification type.R" calculates the effect of the two different types of notifications (standard or new message type), both compared to no notification. This file  requires the course code "estimator_EMEE.R" and "estimator_multiple.trt.R". 



