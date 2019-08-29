#' @title save_with_date()
#'
#' @description Cette fonction crée sauvegarde un objet avec un prefixe correspondant à la date. voir aussi load_latest()
#' @param object le nom de l'obect à sauvegarder
#' @param path le path où sauvegarder l'objet (par rapport à here::here)
#' @examples
#' save_with_date(expo_claim_count, "Result/ON")
#' @export

save_with_date <- function(object, path){
  enquo_object <- enquo(object)
  enquo_name <- quo_name(enquo_object)

  today <- Sys.Date() %>% str_extract_all("\\d") %>% unlist() %>%
    paste0(collapse = "")

  message("Saving as ",here::here(path,paste0(today,"_", enquo_name,".rds")))
  write_rds(
    x = object,
    path = here::here(
      path,
      paste0(today,"_", enquo_name,".rds")
    )
  )

}
