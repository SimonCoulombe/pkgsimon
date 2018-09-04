#' @title get_xgboost_fold_pred_model()
#'
#' @description get_xgboost_fold_pred_model() retourne les prédictions et les modèles de chaque fold d'un xgboost.  Pourquoi utiliser ça plutôt que caret?  Parce que je gère le offset et les contraintes de monotonicité.
#' @param params les paramètre du xgboost,
#' @param data la base de données (categorical variables have been replaced by dummies.)
#' @param label_var  le nom de la variable à prédire
#' @param feature_vars le vecteur des noms des variables explicatives
#' @param offset_var  le nom de la variable qui sera mise dans le base_margin (ex:exposure), default= NULL
#' @param nb_fold le nombre de fold dans notre validation croisée
#' @param nround le nombre d'arbres à créer avec le xgboost
#' @param contraintes vecteur des contraints de monotonicité


get_xgboost_fold_pred_model <- function(
  params = list(),
  data,   # categorical variables have been replaced by dummies.
  label_var,
  feature_vars,
  offset_var = NULL,
  nb_fold = 10,
  nround = 1000,
  contraintes= NULL) {

  result_get_xgboost_fold_pred_model <- list()
  myfolds <- caret::createFolds(
    data %>% pull(label_var),
    k = nb_fold, list = FALSE)

  # inspired by  Codes/Fonctions/get_expected_lift.R
  map_result <- seq_len(nb_fold) %>% purrr::map(~{
    function_result  <- list()
    message(paste0("calcul du fold ", .x))
    train_fold_xgbmatrix <- xgb.DMatrix(
      data = data[myfolds != .x,] %>% select(feature_vars) %>% as.matrix,
      label = data[myfolds != .x,] %>% pull(label_var),
      missing = "NAN")

    test_fold_xgbmatrix <- xgb.DMatrix(
      data = data[myfolds == .x,] %>% select(feature_vars) %>% as.matrix,
      label = data[myfolds == .x,] %>% pull(label_var),
      missing = "NAN")

    if(!is.null(offset_var)){
      setinfo(train_fold_xgbmatrix,"base_margin", data[myfolds != .x,] %>%
                pull(offset_var) %>% log() )
      setinfo(test_fold_xgbmatrix,"base_margin", data[myfolds == .x,] %>%
                pull(offset_var) %>% log() )}

    if(!is.null(contraintes)){
      booster <- xgb.train(
        params = params,
        data = train_fold_xgbmatrix,
        nround = nround,
        monotone_constraints= contraintes$sens)
    }
    else {
      booster <- xgb.train(
        params = params,
        data = train_fold_xgbmatrix,
        nround = nround)
    }


    function_result$pred <-predict(booster, newdata= test_fold_xgbmatrix) %>% as_tibble() %>%
      bind_cols( data %>% mutate(rownum = row_number())%>%.[myfolds ==.x,] %>%
                   select(rownum)) %>%
      mutate(fold = .x)

    function_result$model <- booster


    return(function_result)
  })



  result_get_xgboost_fold_pred_model$pred <-  map_df(map_result, "pred") %>%
    arrange(rownum) %>%
    pull(value)

  result_get_xgboost_fold_pred_model$model <- map(map_result, "model")

  return(result_get_xgboost_fold_pred_model)
}
