#' Register an Item to miniER's Workspace
#'
#' @name register
#'
#' @param item item; <miniER.entity_type; miniER.relationship_type; miniER.relationship>
#' @param ... dot-dot-dot
#'
#' @return item
#'
#' @examples
register <- function(item, ...){
  UseMethod("register")
}

#' @rdname register
#' @method register miniER.entity_type
register.miniER.entity_type <- function(item, ...){
  # to global env
  assign(attr(item, "entity_type_name"),
         item,
         globalenv())

  # to workspace
  assign(attr(item, "entity_type_name"),
         item,
         workspace$entity_types)

  invisible(item)
}

#' @rdname register
#' @method register miniER.relationship_type
register.miniER.relationship_type <- function(item, ...){

  # initialize empty relationship entry under workspace$relationships
  assign(attr(item, "relationship_type_name"),
         local({
           ret <-  tibble::as_tibble(data.frame(character(), character()))
           colnames(ret) <- c(attr(item, "entity_type_1_name"),
                              attr(item, "entity_type_2_name"))
           structure(ret,
                     class = c("miniER.relationship_entry",
                               "data.frame"))
         }),
         workspace$relationships)

  # to workspace
  assign(attr(item, "relationship_type_name"),
         item,
         workspace$relationship_types)

  # to global env
  assign(attr(item, "relationship_type_name"),
         item,
         globalenv())
  invisible(item)
}

#' @rdname register
#' @method register miniER.relationship
register.miniER.relationship <- function(item, ...){

  existing_register <- get(attr(item, "relationship_type_name"),
                           workspace$relationships)

  # for one_one, one_many relationship kinds
  if (attr(item, "relationship_kind_name") %in% c("one_one", "one_many")) {
    # entity_1 cannot already exist in the register
    if (item$entity_1 %in% existing_register[[attr(item, "entity_type_1_name")]]) {
      stop("one_one or one_many relationship already has the same entity.")
    }
  }

  new_entry <- expand_relationship(item)

  updated_register <- dplyr::bind_rows(existing_register,
                                       new_entry)

  assign(attr(item, "relationship_type_name"),
         updated_register,
         workspace$relationships)
  invisible(item)
}


# expand relationship to data frame
#' @import dplyr
expand_relationship <- function(rel){
  stopifnot(inherits(rel, "miniER.relationship"))

  rel_df <-
    expand.grid(unlist(unclass(rel$entity_1)),
                unlist(unclass(rel$entity_2)),
                KEEP.OUT.ATTRS = FALSE) %>%
    tibble::as_tibble() %>%
    mutate(across(everything(), as.character))


  colnames(rel_df) <- c(attr(rel, "entity_type_1_name"),
                          attr(rel, "entity_type_2_name"))

  rel_df
}


#' miniER Workspace
#'
#' @export
workspace <- new.env()

initialize_workspace <- function(workspace_env){
  # build workspace environment
  eval(quote({
    entity_types <- new.env()
    entities <- new.env()
    relationship_types <- new.env()
    relationships <- new.env()
  }),
  workspace_env)

  # link workspace
  makeActiveBinding("entity_types", function(){
    workspace_env$entity_types
  }, workspace)
  makeActiveBinding("relationship_types", function(){
    workspace_env$relationship_types
  }, workspace)
  makeActiveBinding("relationships", function(){
    workspace_env$relationships
  }, workspace)
}
