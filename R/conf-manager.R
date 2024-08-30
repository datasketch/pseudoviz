#' @export
selector_choices_var <- function(conf, viz) {
  choices <- NULL
  data_list <- conf$available_viz[[viz]]
  if (!is.null(data_list[["cat-var"]])) {
    cat_choices <- setNames(
      sapply(data_list[["cat-var"]], function(x) x$id),
      sapply(data_list[["cat-var"]], function(x) x$label)
    )
    choices[["categories"]] <- cat_choices
  }
  if (!is.null(data_list[["num-var"]])) {
    num_choices <- setNames(
      sapply(data_list[["num-var"]], function(x) x$id),
      sapply(data_list[["num-var"]], function(x) x$label)
    )
    choices[["numerics"]] <- num_choices
  }
  if (!is.null(data_list[["dat-var"]])) {
    dat_choices <- setNames(
      sapply(data_list[["dat-var"]], function(x) x$id),
      sapply(data_list[["dat-var"]], function(x) x$label)
    )
    choices[["date"]] <- dat_choices
  }
  if (!is.null(data_list[["txt-var"]])) {
    cat_choices <- setNames(
      sapply(data_list[["txt-var"]], function(x) x$id),
      sapply(data_list[["txt-var"]], function(x) x$label)
    )
    choices[["categories"]] <- cat_choices
  }
  choices
}