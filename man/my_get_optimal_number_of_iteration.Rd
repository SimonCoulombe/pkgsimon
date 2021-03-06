% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_get_optimal_number_of_iteration.R
\name{my_get_optimal_number_of_iteration}
\alias{my_get_optimal_number_of_iteration}
\title{my_get_optimal_number_of_iteration()}
\usage{
my_get_optimal_number_of_iteration(params = list(), data,
  label_var = "GPERTS", feature_vars, offset_var = "pred_saison",
  contraintes = NULL, nb_fold = 10, seed = 8484, max_iter = 2000)
}
\arguments{
\item{params}{les paramètre du xgboost,}

\item{data}{la base de données qui contient les données (label, features et offset) qui ont déjà été dummifiées avec caret::dummyVars}

\item{label_var}{le nom de la variable à prédire}

\item{feature_vars}{le vecteur des noms des variables explicatives}

\item{offset_var}{le nom de la variable qui sera mise dans le base_margin (ex:exposure)}

\item{contraintes}{vecteur des contraints de monotonicité}

\item{nb_fold}{le nombre de fold dans notre validation croisée}

\item{seed}{le numéro de seed}

\item{nround}{le nombre d'arbres à créer avec le xgboost}

\item{maxiter}{le nombre maximum d'arbres à construire}
}
\description{
my_get_optimal_number_of_iteration calcule le point où le modèle cesse de ds pour le test sample de chacun des nb_fold et retourne la médiane du nombre optimal d'itérations pour les nb_fold.
}
