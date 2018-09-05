#' @title get_gam_fold_pred_model()
#'
#' @description get_gam_fold_pred_model() retourne les prédictions et les modèles de chaque fold d'un gam.  Pourquoi utiliser ça plutôt que caret?  Je pense que c'est parce que caret ne fait pas de gam.. ?  En tout cas, mon modèle calcule présentement des poissons.
#' @param data la base de données (categorical variables have been replaced by dummies.)
#' @param nb_fold le nombre de fold dans notre validation croisée
#' @param formula la formule, probablement générée par get_gam_formula()
#' @export



# bam fait des gros gains avec discrete=TRUE, et des petits gains ensuite avec nthreds >1.
# avec plusieurs modèles on est mieux de faire pleins de modèles avec parlapply avec discrete=TRUE et nthreads=1
get_gam_fold_pred_model <- function(
  data,   # categorical variables have been replaced by dummies.
  nb_fold = 10,
  formula
){
  result_get_gam_fold_pred_model <- list()


  myfolds <- caret::createFolds(
    data %>% pull(label_var),
    k = nb_fold,
    list = FALSE)

  data$fold <- myfolds
  data <- data %>%
    mutate(rownum = row_number())
  nCores <- parallel::detectCores()
  nCores_used <- min(nCores/2-1 , nb_fold)
  my_cluster <- makeForkCluster(nCores_used)

  parLapply_result <- parLapply(
    cl = my_cluster,
    X = seq_len(nb_fold),
    fun = function(X) {
      function_result <- list()

      train_fold <- data %>% filter(fold != X)
      test_fold <- data%>% filter(fold == X)
      mod.gam <- train_fold %>%
        mgcv::bam(family= "poisson" (link="log"),
                  formula = formula, discrete= TRUE, nthreads = 1)

      function_result$pred <-
        predict.bam(mod.gam, newdata= test_fold, type = "response") %>% as_tibble()

      function_result$pred <- function_result$pred %>% bind_cols(test_fold %>% select(rownum))
      function_result$mod.gam <- mod.gam


      return(function_result)
    })
  stopCluster(my_cluster)
  result_get_gam_fold_pred_model$pred <-  map_df(parLapply_result, "pred") %>%
    arrange(rownum) %>%
    pull(value)

  result_get_gam_fold_pred_model$mod.gam <- map(parLapply_result, "mod.gam")

  return(result_get_gam_fold_pred_model)
}


