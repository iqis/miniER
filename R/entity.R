#' Entity Type, Entity
#'
#' @param entity_type_name
#' @param rule validation rule; <language>
#'
#' @return
#' @export
#'
#' @examples
entity_type <- function(entity_type_name, subconstructor = function(values){values}) {
  stopifnot(is.character(entity_type_name))
  stopifnot(length(entity_type_name) == 1)
  stopifnot(!is.na(entity_type_name))
  stopifnot(is.function(subconstructor))

  # entity type
  ret <- structure(function(...){
    values <- c(...)

    # use empty chr to represent nothingness
    if (is.null(values)) {
      values <- ""
    }

    stopifnot(is.character(values))

    structure(subconstructor(values),
              class = "miniER.entity",
              entity_type_name = entity_type_name)
  },
  class = "miniER.entity_type",
  entity_type_name = entity_type_name)

  register(ret)
}


anything <- structure(NA, class = "miniER.entity", entity_type_name = "anything")
nothing <- structure(NA, class = "miniER.entity", entity_type_name = "nothing")
