my_compute_xgboost_model <- function(
  params = list(),
  data,   # categorical variables have been replaced by dummies.
  label_var,
  feature_vars,
  offset_var = NULL,
  nb_fold = 10,
  contraintes= NULL,
  seed = 8484,
  max_iter = 2e3,
  forced_max_iter = FALSE,
  get_fold_pred = TRUE
) {

  function_return <- list()
  function_return$label_var = label_var
  function_return$feature_vars = feature_vars
  function_return$offset_var = offset_var
  function_return$nb_fold = nb_fold
  function_return$contraintes = contraintes
  function_return$seed = seed
  function_return$max_iter =max_iter
  function_return$forced_max_iter = forced_max_iter
  function_return$get_fold_pred = get_fold_pred

  if(!forced_max_iter){
    message("Calcul du meilleur nombre d'itérations")
    optimal_number_of_iter <- my_get_optimal_number_of_iteration(
      params = params,
      data= data,
      label_var = label_var,
      offset_var = offset_var,
      feature_vars= feature_vars,
      nb_fold = nb_fold,
      contraintes = contraintes)
    function_return$optimal_number_of_iter = optimal_number_of_iter
  }
  else{
    message(paste0("on impose ", max_iter, " arbres"))
    optimal_number_of_iter= max_iter
    function_return$optimal_number_of_iter = NULL}

  if(get_fold_pred){
    message("Calcul de la prédiction fold")
    out_of_fold_pred_model <- get_xgboost_fold_pred_model(
      params = params,
      data= data,
      label_var = label_var,
      offset_var = offset_var,
      feature_vars= feature_vars,
      nb_fold = nb_fold,
      nround = optimal_number_of_iter,
      contraintes = contraintes)

    function_return$out_of_fold_pred <- out_of_fold_pred_model$pred
    function_return$out_of_fold_models <- out_of_fold_pred_model$model
  } else {
    function_return$out_of_fold_pred <- NULL
    function_return$out_of_fold_models <- NULL}

  message("Calcul du full model et  prédiction sur toute la population ")

  train_fold_xgbmatrix <- xgb.DMatrix(
    data = data %>% select(feature_vars) %>% as.matrix,
    label = data %>% pull(label_var),
    missing = "NAN")


  if(!is.null(offset_var)){
    setinfo(train_fold_xgbmatrix,"base_margin",
            data %>%  pull(offset_var) %>% log() )}

  if(!is.null(contraintes)){
    booster <- xgb.train(
      params = params,
      data = train_fold_xgbmatrix,
      nround = optimal_number_of_iter,
      monotone_constraints= contraintes$sens)
  }
  else {
    booster <- xgb.train(
      params = params,
      data = train_fold_xgbmatrix,
      nround = optimal_number_of_iter)
  }


  function_return$full_model_pred <-predict(booster, newdata= train_fold_xgbmatrix) %>% as_tibble()  %>% pull(value)

  function_return$full_model <- booster

  return(function_return)
}

