---
author: Lukas Dargel
title: test
description: old project that is used for the test
---

<!-- # Part 2: High dimensional dataset -->

**Instructions:**

This work is interested in a problem of certification of a production chain
of cookies. It is necessary to control the amount of each ingredient in the preparation before baking in an industrial oven. The purpose is to guarantee a good proportion rate of lipids, sugar, floor, water so that the preparation is guaranteed to be close to the nominal recommendation for the recipe. It is also
important to detect as soon as possible the presence of a gap between the preparation and the nominal recommendation. The measures and their analyses
are performed in a chemistry laboratory and are generally long and costly.

It is however possible to perform a spectral analysis with infra-red signals in
the cookie-firm. Hence, a “simplification" of the NIR analysis is needed : the
selection of a few amount of spectral frequencies and a robust model of regression can bypass a whole chemistry analysis This kind of problem is classical
in food-processing industry and corresponds to a ”chemistrymetry” calibration. 

**_Data_**

Measures are performed on two samples, the sizes of the training and test on the test set and compute the error of the method.
sets are respectively 40 and 32 samples. For each of the 72 cookies, the amount
of lipids, sugar, floor and water are measured with a classical chemistry approach, although in the same time a spectral infra-red analysis is made from 
the frequency 1100 nm to the frequency 2498 nm, regularly sampled with a step-size of 2nm. We hence have in our hands 700 observed values for each 
cookie. 
All of these values may be informative (or not) to explain the cookie preparation. 
This study is thus a typical example of a high dimensional problem with p >> n. 
The main objective is to answer the following question : 
is it possible to infer from this spectrum the composition of the cookie preparation?
In case of a favourable answer, the time and money saved would be important.
The practical session focuses on the modelization of sugar and how 
to build a good prediction of this sugar rate.

The dataset can be loaded with R through the use of the package `ppls`.
You can load the dataset using this list of commands.

```{r, "P2-data"}
library(ppls)
data(cookie)

# Extraction of the sugar rate and spectrum
cook = data.frame(cookie[,702],cookie[,1:700])
names(cook)= c("sucre",paste("X",1:700,sep=""))
print(help(cookie))
rm(cookie)
```


```{r, "P2-Lib",eval=TRUE,echo=TRUE,cache=FALSE}
# libraries used to conduct the analysis related to Part 2

# data manipulation
library(dplyr)

# summaries
library(skimr)
library(stargazer)

# models
library(caret)
library(pls)
library(glmnet)

# graphics
library(ggplot2)

# tables
library(kableExtra)
```


<!-- ```{r "P2-cheats",include=FALSE,eval=TRUE} -->
<!-- save("P2_MODELS.Rds") -->
<!-- load("P2_MODELS.Rds") -->

<!-- ``` -->



## Question 8 : Warning : one observation seems to be an outlier (as pointed in the litterature). Why ?

```{r "P2-cookie-help",include=FALSE}
help(cookie)
```

Calling the help of cookie reveals the following information:

* The first 40 observations should be considered as training set.
  * With sample 23 being an outlier.
* The second 32 observations should be considered as validation set.
  * With sample 21 being an outlier.
  
In addition  Osbone et al. (1984) who conduct analysis on the same dataset identify only observation 23 in the training set as an outlier.

```{r "P2-outliers",fig.show = 'hold',cache=FALSE}
# highlight the potential outlyers
# observetion 23 and 24 in the dataset train set
color_obs <- rep(1, 72)
pch_obs <- rep(1, 72)
color_obs[23] <- 2
pch_obs[23] <- 2

# sample among the first 150 variables just for illustration
vars <- c(1, sample(2:150, replace = FALSE, size = 5))

# look at bivariate distributions for some of the covariates
cook[, vars] %>% pairs.default(pch = pch_obs, col= color_obs)
# remove the observation
cook <- cook[-23, ]
```

The above figure illustrates that sample 23 is extreme in its X values and outlying in the relationship between the response variable and the target variable.
In the sequel this observation is discarded from the dataset.

```{r ,"P2-cat-help", echo=FALSE,eval=FALSE, include=FALSE, tidy=TRUE}
help_cookie <- "
Description \n
This data set contains measurements from quantitative NIR spectroscopy. The example studied arises from an experiment done to test the feasibility of NIR spectroscopy to measure the composition of biscuit dough pieces (formed but unbaked biscuits). Two similar sample sets were made up, with the standard recipe varied to provide a large range for each of the four constituents under investigation: fat, sucrose, dry flour, and water. The calculated percentages of these four ingredients represent the 4 responses. There are 40 samples in the calibration or training set (with sample 23 being an outlier) and a further 32 samples in the separate prediction or validation set (with example 21 considered as an outlier).

An NIR reflectance spectrum is available for each dough piece. The spectral data consist of 700 points measured from 1100 to 2498 nanometers (nm) in steps of 2 nm.
"

cat(writeLines(strwrap(help_cookie, width = 90)))
```

## Question 9 : Create a training set and a test set to learn your algorithm and measure the efficiency of your method.

```{r, "P2-test-split",cache=FALSE}
# chose the same split as the one proposed in the literature
train_index <- 1:39

cook_train <- cook[train_index, ]
cook_test <- cook[-train_index, ]
```


## Question 10 : Use a ridge regression to learn the sugar rate, optimize the ridge regression with a suitable choice of the penalty parameter. Discuss on the quality of the model on the learning set and provide a prediction on the test set and a computation of the error. (precise its definition). Note the error of the method so that we can compare it with other methods.

The `caret` framework is used to train the train the ridge regression model.
The model is implemented in the `glmnet` package.

```{r, "P2-train-RegRidge",cache=TRUE}
# use caret for ridge regression

# Initialise training parameters
Tconrl_RegRidge <- trainControl(method = "cv",
                                 number = 3,
                                 verboseIter = FALSE, 
                                 search = "grid",
                                 returnData = FALSE,
                                 trim = TRUE)

# Define a tune grid for the ridge regression
tune_RegRidge <- expand.grid(.lambda = 2, # L2 norm for ridge
                              .alpha = seq(0,0.02,length.out = 25)
                              )

# Estimate the model (proceed with the best)
M1_RegRidge <- train(x = cook_train[,-1],
                      y = cook_train[,1],
                      trControl = Tconrl_RegRidge,
                      tuneGrid = tune_RegRidge,
                      method = "glmnet",
                      family= c("gaussian")
                      )

plot(M1_RegRidge, main = "Tuning the Ridge Regression")
```

The predictive accuracy of the trained model measured by the root mean square error and the mean absolute error.
The data frame object which stores the results will be updated with new entries whenever an additional model is estimated.

```{r "P2-performance-RR",cache=FALSE}
# create a structure to store and compare model performance
VALIDATION_ERRORS_DF <- data.frame()

# write a short function to create an entry based on model
PredErrors_get <- function(model = NULL, truth = cook_test[,1], new_data = cook_test[,-1], M_name = NULL){
  
  pred_temp <- predict(model,new_data) 
  
  RMSE <-  mean((pred_temp - truth)^2)^0.5
  MAE <- mean(abs(pred_temp - truth))
  
  return(data.frame(
    "Model" = M_name,
    "Root Mean Square Error" = RMSE,
    "Mean Absolute Error" = MAE,
    check.names = FALSE
    ))
}

# Add an entry for Rige Regression in the performance comparison
VALIDATION_ERRORS_DF <- 
  VALIDATION_ERRORS_DF %>%
  bind_rows(PredErrors_get(M1_RegRidge,
                           M_name = "Ridge Regression",
                           truth = cook_test[, 1],
                           new_data = cook_test[, -1]))
VALIDATION_ERRORS_DF
```

## Question 11 : Investigate the PLS regression on the www, explain the model,and its resolution. 

Partial least square regression (PLS) is an estimation technique that allows to predict a quantitative target variable Y using a large number of potentially highly correlated predictors.
The set of predictors is denoted as $X^j, j = 1,2,...,p$.
Similar to a regression on principal components, the partial least squares uses a dimensionality reduction into orthogonal factors. 
These factors are constructed such that each of them maximizes the empirical covariance with the outcome variable.
So unlike in principal components, they do not maximize the total variance captured in the original predictors.

Let $X_{(nxp)}$, be the matrix of the centered variables. 
We are looking a matrix $U_{(pxr)}$ of loading vectors that define the r components $\Phi_h ,h = 1,2,...,r$ by linear combination of the $X^j$ :
$$ \Phi = XU $$
The U matrix can be found by the following maximization problem:

$$ u_h = arg max_u Cov(Y,\Phi_h)^2 \qquad ,\text{for} \quad h = 1,2,...,r $$
with $\qquad u_h'u_h = 1$  
and  $\qquad u'X'YY'Xu = 0$  


In practice one resorts to cross validation to determine the optimal number of components r.
<!-- The PLS1 algorithm has been proposed to solve the previous problem in the case of a scalar response variable.  -->

<!-- PLS1 algorithm  | -->
<!-- =============== | -->
<!-- $X$ is the matrix of centered predictors | -->
<!-- Calculate the loading matrix $U$ as:     | -->
<!-- *for* $h = 1,2,...,r$ *do:* | -->
<!-- $u_h = \frac{X'Y}{||X'Y ||}$ | -->
<!-- $\eta_h = Xu_h $| -->
<!-- Deflation of $X: X = X - \eta_h'\eta_hXS $| -->
<!-- *end for* | -->


## Question 12 : Optimize the use of the PLS regression (optimal number of components) and estimate the model with this number of components. Predict on the test set and compute the error of the method.

The `caret` framework is used to train the PLS regression model.
The model is implemented in the `pls` package.

```{r "P2-train-RegPLS",cache=TRUE}
# use caret for partial least squares
set.seed(284)
seed_PLS <- matrix(sample(1:1785486,size = 30, replace = FALSE),
                   nrow = 5)


# Initialise training parameters
Tconrl_RegPLS <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE,
                              returnData = FALSE,
                              trim = TRUE,
                              seeds = seed_PLS)

# Train PLS model (proceed with the best)
# Use tuneLegth to optimize the number of components
M1_RegPLS <- train(x = cook_train[,-1],
                   y = cook_train[, 1],
                   trControl = Tconrl_RegPLS,
                   preProcess = c("center", "scale"),
                   method = "pls",
                   tuneLength = 30)
#
# Check CV profile
plot(M1_RegPLS,main = "Tuning the PLS model")
```

The optimal number of components is found by cross validation.
Using the above random seed seven components minimize the RMSE during the cross validation procedure. 

```{r "P2-performance-PLS",cache=TRUE}
# Add an entry for Rige Regression in the performance comparison
VALIDATION_ERRORS_DF <- 
  VALIDATION_ERRORS_DF %>%
  bind_rows(PredErrors_get(M1_RegPLS,
                           M_name = "Partial Least Squares Regression",
                           truth = cook_test[, 1] %>% as.numeric(),
                           new_data = cook_test[, -1]))

VALIDATION_ERRORS_DF
```

## Question 13 : Check that on this dataset, the aggregation models with trees are not very strong. (Explain the meaning of an "agregation" method).

Aggregation methods are so called Meta algorithms which can be implemented on top of any statistical model. 
They are used to aggregate the decision rules of many weak learners (which are at least a bit better than random guessing) into strong ones.

One example is bootstrap aggregation or bagging.
Here one draws several bootstrap samples from the original data and fits a model to each of them.
The final decision is then obtained by aggregating the decisions of all models.
In case of regression problems this aggregation step is usually just a weighted mean.
For classification results are aggregated by a majority vote.
Bagging allows to artificially increase the sample size and is hence a technique to reduce the variance of an estimator.

Boosting is another example of an aggregation method.
Unlike bootstrap aggregation this procedure builds new samples sequentially. Here the first model is trained on a random subset of the training data. However, for the next sample observations which were poorly predicted are drawn with higher probability. 
This technique can be used when one primarily wants to reduce the bias of an estimator.

To test the above statement a random forest regression, which uses bootstrap aggregation.

```{r "P2-train-forest",cache=TRUE}
# use caret for partial least squares

# Initialise training parameters
Tconrl_RForest <- trainControl(method = "cv",
                               verboseIter = FALSE,
                               number = 5,
                               trim = TRUE,
                               returnData = FALSE)

# Train Random Forest Regression model (proceed with the best)
M1_RForest <- train(x = cook_train[, -1],
                    y = cook_train[, 1],
                    trControl = Tconrl_RForest,
                    method = "ranger",
                    tuneLength = 10)

# Check CV profile
plot(M1_RForest,main = "Tuning the Random Forest model")
```


The performance is clearly below the one of the ridge regression and the partial least squares regression.

```{r "P2-performance-rforest",cache=TRUE}
# Add an entry for Rige Regression in the performance comparison
VALIDATION_ERRORS_DF <- 
  VALIDATION_ERRORS_DF %>%
  bind_rows(PredErrors_get(M1_RForest,
                           M_name = "Random Forest Regression",
                           truth = cook_test[, 1],
                           new_data = cook_test[, -1])
    )

VALIDATION_ERRORS_DF
```




## Question 14 : Explain the Bayesian exponential weighted aggregation and program it with R. You are assumed to read carefully the paper " Sparse Regression Learning by Aggregation and Langevin Monte-Carlo " of Dalalyan and Tsybakov.

Exponential Weighted Aggregate (EWA) was developed to solve regression models high dimensional contexts.
It is build on the assumption that the relation ship between the predictors X and the dependent variable can be captured by a much simpler model than the dimensionality of X would suggest.
This sparsity is incerted using a sparctity prior.


The Exponential Weighted Aggregate (EWA) is a tool introduced to deal with high dimensional estimations. Despite the important number of covariates available to predict a variable Y, the underlying assumption of the EWA is that the inherent complexity of Y is small. To insert this sparsity assumption in the model, EWA gathered a family of prior-estimators, each linked to a weight which exponentially underlines the low complexity object. 



Let   $f$ : $X \rightarrow \mathbb{R} , \quad x\mapsto f(x)$  
With  $f(x) = \mathbb{E}[Y | X=x]$  
And   $f_{\theta} = \Sigma^p_j\theta_j f_j, \quad \forall \theta \in \mathbb{R}^p$  

The function $f$ is aproximated by $f_{\theta o}$.  
With $\theta_o = (\theta_{o,1},...,\theta_{o,p})'$ 
Where  $\theta_o = argmin_{\theta \in \mathbb{R}^p}\mathbb{E}[F(f_\theta,y)]$, and $F()$ the loss function.

The goeal is to estimate $\theta_o$ in a high-dimensional setup (p>>n) using  the prior $\theta_o$ belongs to a low-dimension model subset.

In addition to that, we want to ensure theoretical guarantees of the estimator performance.

One can prove that the EWA satisfies the Oracle Inequalities (PAC Bayesian bound), which are valid for any prior and bound the risk of the EWA estimate. 
An additional inequality can be established under some  conditions on the prior.
These are called the Sparse Oracle Inequality. 
Under these assumptions the estimator comes with the following theoretical guarantees:

1. Correct model selection
2. Estimation guarantee
3. Prediction guarantee 

*EWA estimation:*

Let $\mathcal{F_\Theta}$ = {$f_\theta : X \rightarrow \mathbb{R} ; \theta \in \Theta$} a dictionnary where $\Theta$ is $f_\theta(x)$ measurable.  

We estimate :
$$ \hat{\mu}_n(d\theta) = \frac{exp(-F(f_\theta,y)/\beta)\pi(d\theta)}{\int_\Theta exp(-F(f_w,y)/\beta)\pi(dw)} = \frac{exp(-F(X\theta,y)/\beta)\pi(d\theta)}{\int_\Theta exp(-F(Xw,y)/\beta)\pi(dw)}    $$ 
Where:

* $\beta >0$ is the temperature parameter (weights the choice of the data loss/prior) 
* $\pi$ is the prior probability measure on $\Theta$. We define it as follows :

$$ \pi(d \lambda)  = \frac{\tau^{2M}}{C_{\alpha,\tau,R}}(\prod_{j=1}^{M} \frac{e^{\bar{w}(\alpha \lambda)}}{(\tau^2 + \lambda^2)^2}) \mathbb{1}(||\lambda||_1 \leqslant R)d\lambda $$

Where:

* $\bar{w}$ is the Hubert function
* ($\tau,\alpha)\in\mathbb{R}^px\mathbb{R}^p$
* $C_{\alpha,\tau,R}$ is the normalizing constant

The aggregate is defined as:

$$ \hat{f}_n = f_{\hat{\theta}n}^{EWA}  $$

With 

$$\hat{\theta}_n^{EWA}= \int_\Theta \theta_{\hat{\mu}n}(d\theta)$$
The follwing code presents an implementation of this estimator.

```{r 'P2-EWA-algo'}
OMEGA <- function(t) {
  if (abs(t) <= 1) {
    return(t^2)
    } else {
    return(abs(t) - 1)
  }
}

bewa <- function(Y, X, omega, alpha, beta, tao, h, tt) {
  n <- nrow(X)
  M <- ncol(X)
  L <- matrix(0, M, 1)
  lambda <- matrix(0, M, 1)
  H <- 0
  XX <- t(X) %*% X
  Xy <- t(X) %*% y

  while (H < T) {
    nablaV <- (2 / beta) * (Xy - XX %*% L) - alpha * deriv(OMEGA, "alphaL")
    nablaV <- nablaV - 4 * L. / (tao^2 + L.^2)
    L <- L + h * nablaV + sqrt(2 * h) * rnorm(1, M)
    H <- H + h
    lambda <- lambda + h * L / tt
  }
  return(lambda)
}


```

## Question 15 : Use the Lasso estimator with the lars algorithm.

```{r "P2-train-lars",cache=TRUE}
# use caret for partial least squares

# Initialise training parameters
Tconrl_RegLARS <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE,
                              returnData = FALSE,
                              trim = TRUE)

# Train PLS model (proceed with the best)
# Use tuneLegth to optimize the number of components
M1_RegLARS <- train(x = cook_train[,-1],
                   y = cook_train[, 1],
                   trControl = Tconrl_RegPLS,
                   preProcess = c("center", "scale"),
                   method = "lars",
                   tuneLength = 30,
                   use.Gram=FALSE
                 )

# Check CV profile
plot(M1_RegLARS,main = "Tuning the Lasso model")
```

```{r "P2-performance-LARS",cache=TRUE}
# Add an entry for Rige Regression in the performance comparison
VALIDATION_ERRORS_DF <- 
  VALIDATION_ERRORS_DF %>%
  bind_rows(PredErrors_get(M1_RegLARS,
                           M_name = "Lasso - LARS",
                           truth = cook_test[, 1],
                           new_data = cook_test[, -1])
    )

VALIDATION_ERRORS_DF
```

## Question 16 : What is a deep learning neural network method ? Explain the theoretical difficulties and the practical ones. Use a neural network method with several hidden layers.

Neural networks are algorithms that are modelled after the functioning of a brain.
A set of input neurons receives information and transmits in re-weighted form to the next layer of neurons. 
There the inputs are again re-weighted and transferred to the next layer of neurons.
This process continues until the final layer or the output layer is reached, which eventually produces a prediction.

Deep neural network are neural networks with several layers in between input and output layer, called hidden layers.

*Theoretical difficulties:* 

+ One major issue deep neural network is that there are no theoretical guarantees for the estimation procedure. Thus it is never clear if one obtains a globally optimal prediction or if it is just a local one. Additionally this type of model is a black-box, thus it is not possible to comprehend how it has reached a decision.

*Practical difficulties:* 

+ Due to the lag of theoretical properties, one has to resort to empirical procedures to choose a good specification for a model. Which can be a problem or even be impossible as the computational burden for training such is model can be enormous.

```{r "P2-neuralnet-cheat",cache=FALSE, eval=TRUE,include=FALSE}
#save(M1_NeuralNet,file = "MODEL_NEURALNET.Rds")
load("MODEL_NEURALNET.Rds")
```


```{r "P2-train-neuralnet-spec",cache=TRUE, eval=FALSE}
Tconrl_NeuralNet <- trainControl(method = "cv",
                                 number = 2,
                                 verboseIter = TRUE,
                                 search = "grid",
                                 returnData = FALSE,
                                 trim = TRUE)

# Define a tune grid for the ridge regression
neurons_per_layer <- seq(1,19,by = 3)
tune_NeuralNet <- expand.grid(.layer1 = neurons_per_layer, 
                              .layer2 = neurons_per_layer, 
                              .layer3 = neurons_per_layer)

# do this in parallel
cl <- makePSOCKcluster(3)
registerDoParallel(cl)

M1_NeuralNet <- train(x = cook_train[,-1],
                   y = cook_train[, 1],
                   trControl = Tconrl_NeuralNet,
                   preProcess = c("center", "scale"),
                   method = "neuralnet",
                   tuneGrid =  tune_NeuralNet
                 )
## When you are done:
stopCluster(cl)
stopImplicitCluster()

```


```{r "P2-performance-neuralnet",cache=TRUE}
# Check CV profile
plot(M1_NeuralNet,main = "Tuning the Neural Network")

# Add an entry for Rige Regression in the performance comparison
VALIDATION_ERRORS_DF <-
  VALIDATION_ERRORS_DF %>%
  bind_rows(PredErrors_get(M1_NeuralNet,
                           M_name = "Neural Network",
                           truth = cook_test[, 1],
                           new_data = cook_test[, -1])
    )

VALIDATION_ERRORS_DF
```


## Question 17 : Provide a summary of the results, what are the good methods,what are the useful variables ?


The model performance on the test set is summarised in the table below.
The best model is clearly the partial least squares regression.

```{r, "P2-performance-summary",cache=TRUE}
VALIDATION_ERRORS_DF
```

The highest predictive accuracy is achieved with the PLS regression.
The regularized regression models did also performed reasonably well, however the Random Forest's performance is much poorer than the one of all other methods.

The graph below shows that the coefficients around 490 contributes most to the predictive power of the PLS model.

```{r, "P2-varimp-summary",cache=TRUE}
# the best variables in the best model
plot(varImp(M1_RegPLS),top = 30,
     main = "Variable importance in the PLS model")

```





