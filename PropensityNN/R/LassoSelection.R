#'Use Lasso algorithm to select predictors for propensity score estimation.
#'@param formula Lasso regression formula.
#'@param data Data frame for building model.
#'@param nfolds number of folds, default is 10.
#'@param standardize Standardize covariates or not.
#'@param family Response type. Only "gaussian" and "binomial" are supported. If the family = "gaussian", then the criterion for cross-validation is "mse", otherwise, the criterion is the area under roc curve.
#'@export
LassoSelection <- function(formula, data, nfolds = 10, standardize = T, family = "gaussian"){
  #Check the family argument. Only "gaussian" and "binomial" are supported.

  if (family == "gaussian"){
    type.measure = "mse"
  } else if (family == "binomial"){
    type.measure = "auc"
  } else {
    stop("Error input vaule for \"family\". Only \"gaussian\" and \"binomial\" are supported")
  }


  #Extract Xs and y according to the specified formula
  x <- as.matrix(data[ ,all.vars(formula[[3]])])
  y <- data[ ,all.vars(formula[[2]])]
  #Compute lambda through cross validation (lambda.1se)
  lassofit <- glmnet::cv.glmnet(x = x,
                                y = y,
                                nfolds = nfolds,
                                type.measure = type.measure,
                                standardize = standardize,
                                family = family)

  #fit lasso model with lambda.1se
  lassofitmodel <- glmnet::glmnet(x = x,
                                  y = y,
                                  standardize = standardize,
                                  family = family,
                                  lambda = lassofit$lambda.1se
                                  )




  #The coefficient matrix of the fitting result.
  lassoCoef <- coef(lassofitmodel)
  #The names of the covariates kept in the Lasso model
  keptCovariates <- c(1, rownames(lassoCoef)[as.vector(lassoCoef != 0)][-1])
  #The names of the covariates droped from the model
  droppedCovariates <- rownames(lassoCoef)[as.vector(lassoCoef == 0)]

  #Build a formula
  covarFomula <- as.formula(paste(formula[[2]],"~",paste0(keptCovariates, collapse = " + ")))
  #Return both the selection results and the formula
  return(list("keptCovariates" = keptCovariates,"droppedCovariates" = droppedCovariates, "formula" = covarFomula, "cv_model" = lassofit, "model" =  lassofitmodel))
}

