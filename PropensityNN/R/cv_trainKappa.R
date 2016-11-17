
#'Generates a plot of cross-validated prediction error against decay values.
#'
#'@param ps_formula The propensity score formula.
#'@param data The data set.
#'@param folds Number of folds for k-fold cross validation.
#'@param dec_vals The vector of decay values to try.
#'@param size The number of hidden layers.
#'@param ncores The number of processors to use.
#'@param num_reps The number of random starts to average over.
#'@export

cv_trainKappa <-
  function(ps_formula, data, folds = 10,
           decay = 2^(seq(-10, 0, 0.5)),
           size = 8, ncores = 1,
           MaxNWts = 2000, maxit = 2000)
  {
    # ::: cv_train generates a plot of cross-validated prediction error against
    # ::: decay values to help choose the best decay tuning parameter
    # ::: value to use with neural networks for these data.
    # ::: The output, in addition to a plot,
    # ::: is the output of the train() function from package 'caret'.

    # ::: ps_formula: the propensity score formula.
    # ::: data: the data set
    #       (which should contain all the variables in ps_formula).
    # ::: folds: number, k, of folds for k-fold cross validation.
    # ::: dec_vals: the vector of decay values
    #       (lamda tuning parameter for nn) to try.
    # ::: size: the number of hidden layers
    #       (only a scalar is accepted).
    # ::: ncores: the number of processors to use
    #       (MUST HAVE PACKAGE doMC INSTALLED if ncores > 1).
    # ::: num_reps: the number of random starts to average over
    # ::: (only meaningful if nn_avgs = TRUE;
    # ::: WARNING: lengthens computational time almost multiplicatively).





    ### Rescale the covariates to [0,1] prior to running neural networks
    data[,all.vars(ps_formula [[3]] )] <- sc.nn(data[ ,all.vars(ps_formula [[3]] )])



    # all.vars extracts out the covariate names from ps_formula
    # so that the names can be used to subset out the useful data colunms



    #Transform the selection variable(assignment indicator) into factors

    outcome_factor <- as.factor(data[[ as.character(ps_formula[[2]]) ]] )
    data[ as.character(ps_formula[[2]]) ] <- outcome_factor

    # The selection variable is the vector of selection varible so it must be binary (0,1) for the
    # convenience of computation; if it is not, the error message will be given
    if(length(unique(outcome_factor)) !=2){
      stop("The selection variable must be binary")
    } else if(!(all(c(0,1)== unique(outcome_factor))|all(c(1,0) == unique(outcome_factor)))){
      stop("The selection variable must be numeric binary whose value is 0 or 1")
    }

    #I have to rename the levels for using ROC
    levels(data[[ as.character(ps_formula[[2]]) ]]) <-  c("n","y")
    #data[ as.character(ps_formula[[2]]) ]  <- relevel(data[[ as.character(ps_formula[[2]]) ]], ref = "y")

    ctrl <- caret::trainControl(method = "cv" , number = folds)
    nnet_grid <- expand.grid(.decay = decay, .size = size)
    nnfit <- caret::train(form = ps_formula,
                          data = data,
                          method = 'nnet',
                          MaxNWts = 4000,
                          maxit = 2000,
                          trControl = ctrl,
                          tuneGrid = nnet_grid,
                          metric = "Kappa",
                          ncores = ncores
    )

    Maxindex <- which.max(nnfit$result$Kappa)

    print(ggplot2::ggplot(data = nnfit$result, aes(x = decay, y = Kappa, group = size, color = factor(size))) +
            geom_path()+
            geom_point(size = 1.5)+
            geom_point( x = nnfit$result$decay[Maxindex], y = nnfit$result$Kappa[Maxindex], color = "red", size = 4)+
            theme_bw() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            ggtitle(paste0("Max Kappa = ", format(nnfit$result$Kappa[Maxindex], digits = 3), "; Best Decay = ",  format(nnfit$result$decay[Maxindex], digits = 4), "; Best Size = ",  format(nnfit$result$size[Maxindex], digits = 3)))+
            labs(color = "Size", x = "Decay", y = "Kappa Value")
    )

    output <- list("PropensityScore" = predict(nnfit$finalModel, data), "Performance" = nnfit$result,"BestPerformance" = nnfit$result[Maxindex,], "FittedModel" = nnfit, "BestTune" = nnfit$bestTune)

    return(output)
  }
