#' @title save_with_date()
#'
#' @description Cette fonction crée charge un object sauvegardé avec le pattern de save_with_date()
#' @param object le nom de l'objet sauvegardé
#' @param path le path où a été sauvegardé l'object  (par rapport à here::here)
#' @examples
#' expo_claim_count <- load_latest(expo_claim_count, "Result/ON")
#' @export

load_latest <- function(object, path){
  #expo3 <- load_latest(expo_claim_count, "Result/ON")
  enquo_object <- enquo(object)
  enquo_name <- quo_name(enquo_object)
  last_save <- list.files(
    here::here(path),
    pattern = enquo_name
  ) %>% str_extract("\\d+") %>%
    as.numeric %>% max

  message("Loading ", here::here(path, paste0(last_save,"_", enquo_name, ".rds" )
  ))

  read_rds(
    here::here(
      path,
      paste0(last_save,"_", enquo_name, ".rds"
      )
    )
  )
}
