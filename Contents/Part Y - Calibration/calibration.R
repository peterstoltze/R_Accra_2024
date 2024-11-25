## Introduction to model assisted estimation {.columns-2}

- Sometimes we have **auxiliary information** available, ie. information $x$ known for both the sampled units but **also** at the population level $\tau_x$
  - Trivial example: Export is a fixed ratio of the turnover
$$
  \hat{\tau}_y^{ratio} = \frac{\tau_x}{\hat{\tau}_x^{HT}}\hat{\tau}_y^{HT}
  $$
    
    <p class="forceBreak"></p>
      
      ```{r echo=FALSE, fig.width=4}
    library(png)
    library(grid)
    img <- readPNG("pics/ratio.png")
    grid.raster(img)
    ```
    
    
    ## The regression estimate
    
    * The purpose of the regression estimate is to use an **auxiliary variable** $x$ to get a better estimate of our **variable of interest** $y$
      * **Better** means either
    * smaller standard error
    * or reduction of bias
    * One can show that the regression etimate may be though of as an **adjusted** HT-estimate:
      
      $$
      \hat{\tau}_y^{GREG} = \hat{\tau}_y^{HT} + (\tau_x - \hat{\tau}_x)\hat{\beta}
    $$
      
      ***
      
      > * Assume we  know the **totals** of the auxiliary variable at the **population level**
      $$
      \tau_x = \sum_{i \in \mathcal{U}}x_i
    $$
      
      > * We **could** also estimate these quantities from the sample applying the HT-estimator
    $$
      \hat{\tau}_x^{HT}=\sum_{i \in \mathcal{S}}d_i x_i
    $$
      where $d_i$ is the design weight for sample unit $i$, ie. $\pi _i^{-1}$
      
      > * Hopefully $\hat{\tau}_x^{HT} \approx \tau_x$ but generally the two quantities will not be exactly equal
    
    ***
      <!-- ## Calibration equation -->
      
      > * In principle, this is unimportant, since we already know the true value
    > * But since the design weight $d$ is also used for estimating totals for our variables of interest $y$, we must expect these to be a little off, too -- especially if $y$ and $x$ are correlated
    > * Hence, we seek a **calibrated weight** $w_i$ such that the following calibration equation is satisfied
    $$
      \sum_{i \in \mathcal{S}} w_i x_i = \tau_x
    $$
      
      ***
      <!-- ## Minimization and distance function -->
      
      > * The calibrated weights should ideally be close to the original design weights, so the calibrated weights are found as the solution to a **minimization** problem:
      $$
      \min \sum_{i \in \mathcal{S}} G(w_i,d_i) \quad \textrm{subject to} \quad \sum_{i \in \mathcal{S}} w_i x_i = \tau_x
    $$
      
      > * $G$ is a function measuring the distance between the design weight and the calibrated weight -- for the regression estimate we use a quadratic distance function
    $$
      G(w,d) = \frac{(w-d)^2}{2d}
    $$
      
      ***
      
      > * Now the regression estimate for our variable of interest $y$ can simply be calculated as a weighted sum
    $$
      \hat{\tau}_y^{GREG} = \sum_{i \in \mathcal{S}} w_i y_i
    $$
      
      > * The calibrated weight can be regarded as the design weight $d$ multiplied with a $g$-weight (a factor introducing individual corrections)
    $$
      w_i = g_i d_i \Rightarrow g_i = \frac{w_i}{d_i}
    $$
      
      > * Always check the $g$-weights -- if there are **extreme values** check again what you are trying to accomplish with the calibration equations
    
    ***
      
      > * A third (and final) interpretation of the GREG-estimate (giving more prominence to an underlying linear model) is as the sum of predicted (or fitted) values for the entire population **plus** the design weighted residuals
    $$
      \hat{\tau}_y^{GREG} = \sum_{i \in \mathcal{U}} \hat{y}_i + \sum_{i \in \mathcal{S}} d_i (y_i - \hat{y}_i)
    $$
      
      > * Calculating $V(\hat{\tau}_y^{GREG})$ involves approximation by Taylor expansion and is generally best left to some suitable package in R...
    
    
    ## The regression estimate with R
    
    * We create a small ($N=10$) and very simple population with two variables and no strata
    * Variable $x$ is an auxiliary variable available from a register (say land area measured in hectare), while $y$ is our main variable of interest only available from a sample (say crop production measured in ton)
    
    ```{r}
    my.pop <- data.frame(id=1:10,
                         x=c(99,111,114,95,110,102,94,83,96,102),
                         y=c(647,654,674,637,664,653,644,625,654,647))
    ```
    
    ***
      
      * There is a strong positive correlation between $x$ and $y$ ($\rho =$ `r cor(my.pop$x, my.pop$y)`)
    
    ```{r echo=FALSE}
    plot(my.pop$x, my.pop$y, col="blue")
    ```
    
    ***
      
      * Now we draw a simple random sample (SRS) of size 3 and estimate both $\tau_x$ and $\tau_y$ using the `survey` package:
      ```{r message=FALSE}
    s <- c(1,3,8)
    my.sample <- my.pop[s,]
    my.sample$N <- nrow(my.pop)
    
    library(survey)
    srs.svyobj <- svydesign(ids = ~1,
                            fpc = ~N,
                            data = my.sample)
    
    ```
    
    ***
      ```{r}
    (y.hat <- svytotal (~y, srs.svyobj))
    (x.hat <- svytotal (~x, srs.svyobj))
    sum(my.pop$x)
    ```
    
    ***
      * We now turn to calibration (the regression estimate) and start by setting up a target  consisting of the known population size and the known total of $x$
      
      ```{r}
    (cgoal <- c('(Intercept)'=nrow(my.pop), 'x'=sum(my.pop$x)))
    ```
    
    * Don't worry too much about using named vectors -- it is just syntax...

***
* The calibration is done by an aptly named function
```{r}
cal.svyobj <- calibrate (srs.svyobj,
                         formula=~x,
                         population=cgoal)
```

* We use the new survey object to calculate a regression estimate of $\tau_x$, although the result is known by definition in advance
```{r}
(x.hat.cal <- svytotal (~x, cal.svyobj))
```

*** 
* More interesting, we calculate the regression estimate of $y$
```{r}
(y.hat.cal <- svytotal (~y, cal.svyobj))
```

* Remember that the true (but principally unknown) value of $\tau_y$ is `r sum(my.pop$y)` -- in this case the power from the correlation between $x$ and $y$ is used to reduce the standard error rather than the bias

***
* We can get the calibrated weight (and also the $g$-weights) to gain some insight

```{r}
my.sample$dw <- nrow(my.pop) / nrow(my.sample)
my.sample$cw <- weights(cal.svyobj)
my.sample$gw <- my.sample$cw / my.sample$dw

```

***
```{r}
my.sample
```

* $\tau_x$ was estimated slightly smaller than the target value with the HT-estimator -- therefore $g>1$ for the largest unit (in terms of $x$) while $g<1$ for the smallest unit

***

* Also note the following proporties

```{r}
sum(my.sample$dw)
sum(my.sample$cw)
mean(my.sample$gw)
```

***

* Now imagine that an updated frame population shows that the true number of units is 12 rather than 10, and that the true total value of x is 1100 hectares rather than 1006
* Adjusting the target to these figures allows us to raise the sample to the level of the updated frame population

```{r}
cgoal.new <- c('(Intercept)'=12,'x'=1100)

cal.svyobj.new <- calibrate (srs.svyobj, formula=~x, 
                   population=cgoal.new)
(x.hat.cal.new <- svytotal (~x, cal.svyobj.new))

```



***
```{r}
(y.hat.cal.new <- svytotal (~y, cal.svyobj.new))

cv(y.hat.cal.new)
```

* The result of using the target from the updated frame population that the estimate of $\tau_y$ has been increased significantly
* Notice also that the CV is also higher –- raising the estimates relative to the original frame comes at a cost

***
* Finally we may derive and inspect the new correction weights.
```{r echo=FALSE}
my.sample$cw2 <- weights(cal.svyobj.new)
my.sample$gw2 <- my.sample$cw2 / my.sample$dw
my.sample
```

```{r}
sum(my.sample$cw2)
mean(my.sample$gw2)
```


## Other topics
* Stratification (choice of variables, number of strata, stratum boundaries
* Non-response (bias correction)
* Estimation of more complex population attributes (rather than just means and totals)
* Recurrent (longitudinal) surveys