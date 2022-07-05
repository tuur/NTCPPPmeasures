#install.packages("torch")
#install.packages("mltools")
library('torch')
library('mltools')
library(data.table)  


# A logistic regression implementation torch R (to allow easy updating of coefficients and intercept separately): includes data normalization
torch_lr <- nn_module(
  clasname = "LR",
  # the initialize function tuns whenever we instantiate the model
  initialize = function(X_names, y_name) {
    
    self$X_names=c(X_names)
    self$y_name=y_name

    self$coeffs <- nn_parameter(torch_randn(length(X_names), 1), requires_grad = TRUE)
    self$intercept <- nn_parameter(torch_zeros(1), requires_grad = TRUE)
    self$slope <- nn_parameter(torch_tensor(1.0), requires_grad = TRUE)
    self$sigmoid <- nn_sigmoid()
    self$mean <- 0
    self$std <- 1
    self$lp_noise <- 0
    self$lp_noise_outcome_prev <- 0.5
    return(self)
  },
  
  # ==== FORWARD FUNCTION ====
  forward = function(x, LP=FALSE) {
    x <- (x-self$mean) / self$std # always standardize (mean = 0 and std=1 if no standardization is applied)
    if(self$lp_noise > 0){ # adds some random noise to the prediction
      bx = torch_mm(x, self$coeffs)
      #print(c('bx',bx))
      

      # V1 interpolate linear predictor with normally distributed noise variable (mean=0, std=1)
      #noise_mean = exp(self$lp_noise_outcome_prev)/(1+exp(self$lp_noise_outcome_prev))
      noise_mean = 0
      noise = torch_reshape(torch_tensor(rnorm(nrow(x), noise_mean, 1)), list(nrow(x), 1))
      noisy_bx <- (bx * (1-self$lp_noise)) + (noise * self$lp_noise)
      linear_predictor <- (self$slope * (noisy_bx)) + self$intercept
      
      
      
    }else{
      linear_predictor <- (self$slope * (torch_mm(x, self$coeffs))) + self$intercept
    }
    
    if (LP){
      return(torch_flatten(linear_predictor))
    }else{
      return(torch_flatten(self$sigmoid(linear_predictor)))
    }
    
  },
  
  # this function sets a new value for ONE coefficient
  set_coefficient_value  = function(name, value){
    # 1. get the index of the predictor
    index <- match(name,self$X_names )
    # 2. set the coefficient at that index to the new value
    r_coeffs <- as.numeric(self$coeffs)
    r_coeffs[index][1] <- value
    self$coeffs <- nn_parameter(torch_tensor(matrix(r_coeffs, nrow=length(self$X_names), ncol=1)), requires_grad = TRUE)
  },
  
  # this functions allows setting the values of individual coefficients, or the intercept.
  set_parameters = function(coeffs=FALSE, intercept=FALSE, slope=FALSE){
    if (!is.logical(coeffs)) { # if coefficient values are provided
      for (name in names(coeffs)){
        self$set_coefficient_value(name, coeffs[[name]][1])
      }
    }
    if (!is.logical(intercept)) { # if an intercept value is given
      self$intercept <- nn_parameter(torch_tensor(intercept), requires_grad = TRUE)
    }
    if (!is.logical(slope)) { # if a slope value is given
      self$slope <- nn_parameter(torch_tensor(slope), requires_grad = TRUE)
    }
  },
  
  # ==== TRAIN/FIT FUNCTION ====
  fit = function(df, num_iterations=1000, update_intercept=TRUE, update_coeffs=TRUE, update_slope=FALSE, normalize=TRUE, lr=0.1, patience=5, alternative_outcome=FALSE, plot_loss_curve=FALSE){
    # 1. Prep the data
    predictor_df <- df %>% select(unlist(self$X_names))
    X <- torch_tensor(matrix(unlist(predictor_df), ncol = dim(predictor_df)[2]),dtype=torch_float())
    y <- torch_tensor(unlist(df %>% select(self$y_name)),dtype=torch_float())
    if (is.character(alternative_outcome)){
      y <- torch_tensor(unlist(df %>% select(!!as.symbol(alternative_outcome))),dtype=torch_float())
    }
    pt <- patience
    
    #1.1 Normalization of input to mean=0 and std=1 (if set TRUE)
    if (normalize){
      self$mean <- torch_mean(X,1,TRUE)
      self$std <- torch_std(X, 1, TRUE)      
    }
    
    # 2. Determine what parameters to fit / update
    to_be_updated_params <- c()
    if (update_intercept){to_be_updated_params <- c(to_be_updated_params, self$intercept)}
    if (update_coeffs){to_be_updated_params <- c(to_be_updated_params, self$coeffs)}
    if (update_slope){to_be_updated_params <- c(to_be_updated_params, self$slope)}
    
    #optimizer <- optim_adam(to_be_updated_params, lr=lr)
    #optimizer <- optim_sgd(to_be_updated_params, lr=1, nesterov = TRUE, momentum = .1, dampening = 0)
    #optimizer <- optim_adadelta(to_be_updated_params)
    optimizer <- optim_lbfgs(to_be_updated_params,line_search_fn = "strong_wolfe", max_iter = 100)  # works well for logistic regression (and very fast)
    
    calc_loss <- function(){
      # make sure gradient updates are calculated from a fresh start
      optimizer$zero_grad()
      # get model predictions
      output <- self$forward(X, LP=TRUE)
      # get the loss
      loss <- nnf_binary_cross_entropy_with_logits(output, y)
      # calculate gradients
      loss$backward()
      return(loss)
    }
    
    loss_curve <- c()
    prev_loss <- 100000000

    # 3. Optimize the objective
    for (epoch in 1:num_iterations) {
      
      # get the loss
      loss <- calc_loss()

      # perform an optimization step 
      optimizer$step(calc_loss) 
      
      # track losses
      current_loss <- as.numeric(loss)
      loss_curve <- c(loss_curve, current_loss)
      
      if (current_loss >= prev_loss){ # early stopping 
        if (pt == 0){
          #print(c('converged at epoch',epoch))
          break
          } 
        else {pt <- pt - 1} # no change in loss for patience epochs
      } else{ pt <- patience}
      
      prev_loss <- as.numeric(loss)
    }
    #print(c('loss',current_loss))
    if(plot_loss_curve){
      print(loss_curve)
    }
    return(loss_curve)
  },
  
  # ==== PREDICT FUNCTION ====
  predict = function(df, LP=FALSE){
    # 1. Select the relevant predictor variables
    predictor_df <- df %>% select(unlist(self$X_names))
    X <- torch_tensor(matrix(unlist(predictor_df), ncol = dim(predictor_df)[2]),dtype=torch_float())
    # 2. Make the predictions
    predictions <-  self$forward(X, LP=LP)
    return(c(as.numeric(predictions)))
  },

  get_model_coefficients = function(){
    coefficients <- as.numeric(self$coeffs * self$slope)
    names(coefficients) <- self$X_names
    return(coefficients)
  },
  
  get_model_intercept = function(){
    return(as.numeric(self$intercept))
  },
  get_model_slope = function(){
    return(as.numeric(self$slope))
  },
  recalibrate = function(df, target_lp, update_slope=TRUE, update_intercept=TRUE){
    current_lp = self$predict(df, LP=TRUE)
    
    if (update_slope & update_intercept){
      calibration_model = lm(target_lp ~ current_lp)
      calibration_intercept = coef(calibration_model)[['(Intercept)']]
      calibration_slope = coef(calibration_model)[['current_lp']]
      self$set_parameters(slope = self$get_model_slope() * calibration_slope, intercept=(self$get_model_intercept() * calibration_slope) + calibration_intercept)
    }
    if (update_intercept & !update_slope){
      calibration_model = lm(target_lp ~ current_lp)
      calibration_intercept = coef(calibration_model)[['(Intercept)']]
      calibration_slope = coef(calibration_model)[['current_lp']]
      #self$intercept = self$intercept + (mean(target_lp) - mean(current_lp))
      self$set_parameters(intercept=(self$get_model_intercept() * calibration_slope) + calibration_intercept)
    }
    if (!update_intercept & update_slope){
      print('NOT POSSIBLE CURRENTLY')
    }

  }
)

# encodes data to a matrix useable for 
encode_features = function(df, list_of_variable_names=FALSE, normalize = FALSE){
  if (typeof(list_of_variable_names)=='list') name_order <- list_of_variable_names else name_order<-names(df)
  used_data = data.table(df[,c(list_of_variable_names)])
  print(used_data)
  new_data <- one_hot(used_data)
  return(new_data)
}



