#' Relationship Kind, Relationship Type and Relationships
#'
#' @param relationship_kind_name
#' @param rule
#'
#' @name relationship
#' @return
#'
#' @examples
relationship_kind <- function(relationship_kind_name, rule){
  stopifnot(is.character(relationship_kind_name))
  stopifnot(length(relationship_kind_name) == 1)
  stopifnot(is.language(rule))

  # relationship kind
  ret <- function(relationship_type_name, entity_type_1_name, entity_type_2_name){
    stopifnot(is.character(relationship_type_name))
    stopifnot(length(relationship_type_name) == 1)

    stopifnot(entity_type_1_name %in% ls(workspace$entity_types))
    stopifnot(entity_type_2_name %in% ls(workspace$entity_types))

    # relationship type
    ret <- structure(function(entity_1, entity_2){
      stopifnot(attr(entity_1, "entity_type_name") %in% entity_type_1_name)
      stopifnot(attr(entity_2, "entity_type_name") %in% entity_type_2_name)

      eval(rule)

      ret <- structure(list(entity_1 = entity_1,
                            entity_2 = entity_2),
                       class = "miniER.relationship",
                       entity_type_1_name = entity_type_1_name,
                       entity_type_2_name = entity_type_2_name,
                       relationship_type_name = relationship_type_name,
                       relationship_kind_name = relationship_kind_name)

      register(ret)
      entity_1
    },
    class = "miniER.relationship_type",
    entity_type_1_name = entity_type_1_name,
    entity_type_2_name = entity_type_2_name,
    relationship_type_name = relationship_type_name,
    relationship_kind_name = relationship_kind_name)

    register(ret)
  }

  structure(ret,
            class = "miniER.relationship_kind")
}


#' @rdname relationship
#' @export
one_one <- relationship_kind("one_one",
                             rule = quote({
                               stopifnot(length(entity_1) == 1)
                               stopifnot(length(entity_2) == 1)
                             }))


#' @rdname relationship
#' @export
one_many <- relationship_kind("one_many",
                              rule = quote({
                                stopifnot(length(entity_1) == 1)
                              }))

#' @rdname relationship
#' @export
many_many <- relationship_kind("many_many",
                               rule = quote({}))
