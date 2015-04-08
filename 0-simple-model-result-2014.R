Call:
glm(formula = dif.score ~ ., family = "binomial", data = myData)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.1729  -0.6879   0.1517   0.8267   1.7966  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)  0.34090    0.38861   0.877   0.3804  
dif.fgm      0.74233    0.42482   1.747   0.0806 .
dif.fga     -0.54874    0.28012  -1.959   0.0501 .
dif.fgm3     1.97545    0.94670   2.087   0.0369 *
dif.fga3    -0.59559    0.35361  -1.684   0.0921 .
dif.ftm     -0.10522    0.35271  -0.298   0.7655  
dif.fta     -0.26887    0.28759  -0.935   0.3498  
dif.or       0.76337    0.32980   2.315   0.0206 *
dif.dr       0.55601    0.26718   2.081   0.0374 *
dif.ast     -0.51623    0.25907  -1.993   0.0463 *
dif.to      -0.59024    0.28384  -2.079   0.0376 *
dif.stl     -0.08596    0.26274  -0.327   0.7435  
dif.blk      0.56494    0.34864   1.620   0.1051  
dif.pf      -0.08143    0.19766  -0.412   0.6804  
---
Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 91.669  on 66  degrees of freedom
Residual deviance: 58.290  on 53  degrees of freedom
AIC: 86.29

Number of Fisher Scoring iterations: 6