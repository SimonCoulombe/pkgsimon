#' @title get_gam_formula()
#'
#' @description get_gam_formula() crée une formule à passer à un gam.  Les feature_vars seront toutes splinées et le log de la offset_Var sera mis en offset.
#' @param label_var  le nom de la variable à prédire
#' @param offset_var la variable dont le log sera mis en offset
#' @param feature_vars un vecteur de noms de variables pour lesquelles une spline sera créée
#' @export
get_gam_formula <- function(label_var, offset_var, feature_vars){

  as.formula(paste0(label_var, "~",
                    paste0( paste0("s(",feature_vars,", bs = 'cr')"), collapse=" + "),
                    "+  offset(log(" , offset_var, "))"))
}
