#' @title my_get_optimal_number_of_iteration()
#'
#' @description my_get_optimal_number_of_iteration calcule le point où le modèle cesse de ds pour le test sample de chacun des nb_fold et retourne la médiane du nombre optimal d'itérations pour les nb_fold.
#' @param params les paramètre du xgboost,
#' @param data la base de données qui contient les données (label, features et offset) qui ont déjà été dummifiées avec caret::dummyVars
#' @param label_var  le nom de la variable à prédire
#' @param feature_vars le vecteur des noms des variables explicatives
#' @param offset_var  le nom de la variable qui sera mise dans le base_margin (ex:exposure)
#' @param nb_fold le nombre de fold dans notre validation croisée
#' @param nround le nombre d'arbres à créer avec le xgboost
#' @param contraintes vecteur des contraints de monotonicité
#' @param seed le numéro de seed
#' @param maxiter le nombre maximum d'arbres à construire
#' @export

my_get_optimal_number_of_iteration <-   function(
  params = list(),
  data,   # categorical variables have been replace with dummies
  label_var = "GPERTS",
  feature_vars,
  offset_var = "pred_saison",
  contraintes = NULL,
  nb_fold = 10,
  seed = 8484,
  max_iter = 2e3)
{

  set.seed(seed)

  myfolds <- caret::createFolds(
    data %>% pull(label_var),
    k = nb_fold, list = FALSE)

  result_fold <- seq_len(nb_fold) %>% purrr::map_df(~{
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

    watchlist <- list(eval = test_fold_xgbmatrix, train = train_fold_xgbmatrix)
    ## todo : comment faire pour avoir des contraintes propres au lieu de if else?
    if(!is.null(contraintes)){
      booster <- xgb.train(
        params = params,
        data = train_fold_xgbmatrix,
        nround = max_iter,
        watchlist,
        verbose=0,
        callbacks = list(cb.early.stop(metric_name = "eval_poisson_nloglik", stopping_rounds = 50)),
        monotone_constraints= contraintes$sens)
    }
    else{
      booster <- xgb.train(
        params = params,
        data = train_fold_xgbmatrix,
        nround = max_iter,
        watchlist,
        verbose=0,
        callbacks = list(cb.early.stop(metric_name = "eval_poisson_nloglik", stopping_rounds = 50)))}

    message("Best iter fold # ", .x, "\t",  booster$best_iteration, "\t",  Sys.time())
    data_frame(Fold = .x, best_iter = booster$best_iteration, best_score = booster$best_score)
  })
  result_fold %>% summarise(m = median(best_iter)) %>% as.numeric %>% floor
}

