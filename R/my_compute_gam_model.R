#' @title my_compute_gam_model()
#' @export

my_compute_gam_model <- function(
  data,   # categorical variables have been replaced by dummies.
  label_var,
  offset_var,
  feature_vars,
  nb_fold = 10,
  get_fold_pred = TRUE
) {

  function_return <- list()
  function_return$label_var = label_var
  function_return$feature_vars = feature_vars
  function_return$offset_var = offset_var
  function_return$nb_fold = nb_fold
  function_return$get_fold_pred = get_fold_pred

  formula = get_gam_formula(label_var = label_var,
                            offset_var = offset_var,
                            feature_vars = feature_vars)


  if(get_fold_pred){
    message("Calcul de la prédiction fold")
    out_of_fold_pred_model <- get_gam_fold_pred_model(
      data = data,
      formula = formula)

    function_return$out_of_fold_pred <- out_of_fold_pred_model$pred
    function_return$out_of_fold_models <- out_of_fold_pred_model$model
  } else {
    function_return$out_of_fold_pred <- NULL
    function_return$out_of_fold_models <- NULL}

  message("Calcul du full model et  prédiction sur toute la population ")

  mod.gam <- data %>%
    mgcv::bam(family= "poisson" (link="log"),
              formula = formula, discrete=TRUE)

  function_return$full_model_pred <-
    predict.bam(mod.gam, newdata= data, type = "response") %>% as_tibble() %>% pull(value)
  function_return$full_model <- mod.gam

  return(function_return)
}

