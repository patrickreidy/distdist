

#' S4 Class: Distribution
#'
#' The \code{Distribution} class represents discrete probability distributions.
#'
#' @slot .Data A data frame, with \code{n > 1} columns, that defines a
#'   probability mass function that associates probabilities to a finite number
#'   of values assumed by a random variable. Columns \code{1} to \code{n-1}
#'   determine the values assumed by the random variable; none of these columns
#'   should be named \code{"P"}. Column \code{n} defines the probability
#'   assigned to each value assumed by the random variable; this column must be
#'   named \code{"P"}.
#'
#' @name Distribution-class
#' @exportClass Distribution
methods::setClass(
  Class = "Distribution",
  contains = c("data.frame")
)

methods::setValidity(
  Class = "Distribution",
  method = function(object) {
    # P must be a numeric variable that sums to 1.
    p_msg <- "A Distribution must contain a numeric variable named 'P', whose values are positive and sum to 1."
    p_error <- ifelse(!("P" %in% names(object)) ||
                        class(object[["P"]]) != "numeric" ||
                        length(which(object[["P"]] < 0)) > 0 ||
                        abs(sum(object[["P"]]) - 1) > .Machine$double.eps,
                      p_msg, "")
    # A distribution must be defined over a random variable---a variable other than P.
    rv <-
      as_tibble(object) %>%
      dplyr::select(rlang::UQS(rlang::syms(setdiff(names(object), "P"))))
    rv_msg <- "A Distribution must contain at least one variable other than 'P'."
    unique_msg <-
      stringr::str_c("The values of the random variable (columns ",
                     stringr::str_c(setdiff(names(object), "P"), collapse = ", "),
                     ") must be unique.")
    rv_error <- ifelse(length(setdiff(names(object), "P")) == 0, rv_msg,
                       ifelse(nrow(dplyr::distinct(rv)) < nrow(rv),
                              unique_msg, ""))
    errors <- c(p_error, rv_error) %>% stringr::str_subset(".+")
    if (length(errors) > 0) {return(errors)} else {return(TRUE)}
  }
)





#' Initialize a Distribution
#'
#' Initialize a distribution from a data frame or a tibble.
#'
#' @param x A data frame or tibble.
#' @param p An unquoted column name in \code{x}, which will be renamed to
#'   \code{P} to comply with the validity method for the Distribution class.
#'   If the values of this column in \code{x} do not sum to 1, then it is
#'   normalized by its sum, so that it may be treated as the values of a
#'   probability mass function.
#'
#' @return A \code{\link[=Distribution-class]{Distribution}}
#'
#' @name Distribution
#' @export
methods::setGeneric(
  name = "Distribution",
  def = function(x, ...) {
    standardGeneric("Distribution")
  }
)

#' @usage \S4method{Distribution}{data.frame}(x, p = .data$P)
#' @name Distribution,data.frame-method
#' @rdname Distribution
methods::setMethod(
  f = "Distribution",
  signature = c(x = "data.frame"),
  definition = function(x, p = .data$P) {
    p_quo <- rlang::enquo(p)
    x_rv <- x %>% dplyr::select(-rlang::UQ(p_quo))
    if ("P" %in% names(x_rv)) {
      stop("The dimension names of the state space must not include 'P'.")
    }
    x <- x %>% dplyr::mutate(P = rlang::UQ(p_quo) / sum(rlang::UQ(p_quo)))
    methods::new(Class = "Distribution", as.data.frame(x))
  }
)





#' Coerce a Distribution to a Tibble
#'
#' Coerce a distribution to a tibble. This function is used internally in many
#' other functions in order to capitalize on functionality from other packages
#' such as \code{dplyr}.
#'
#' @param x A \code{\link[=Distribution-class]{Distribution}}.
#'
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @export
as_tibble.Distribution <- function(x) {
  methods::getDataPart(x) %>%
    purrr::set_names(names(x)) %>%
    tibble::as_tibble()
}





#' Arrange a Distribution
#'
#' Arrange a Distribution by alphabetizing its first \code{n-1} columns and
#' by arranging the values of these alphabetized columns, in increasing order.
#'
#' @param x A \code{\link[=Distribution-class]{Distribution}}.
#'
#' @return A \code{\link[=Distribution-class]{Distribution}} that is like
#'   \code{x}, except that its first \code{n-1} columns are arranged in
#'   alphabetical order, and its rows are arranged such that the values of the
#'   alphabetized columns are in increasing order.
#'
#' @name Arrange
#' @export
methods::setGeneric(
  name = "Arrange",
  def = function(x, ...) {
    standardGeneric("Arrange")
  }
)

#' @usage \S4method{Arrange}{Distribution}(x)
#' @name Arrange,Distribution-method
#' @rdname Arrange
methods::setMethod(
  f = "Arrange",
  signature = c(x = "Distribution"),
  definition = function(x) {
    rv_names <- names(x) %>% setdiff("P") %>% sort()
    arranged <-
      as_tibble(x) %>%
      dplyr::select(rlang::UQS(rlang::syms(c(rv_names, "P")))) %>%
      dplyr::arrange(rlang::UQS(rlang::syms(rv_names))) %>%
      as.data.frame()
    methods::new(Class = "Distribution", arranged)
  }
)





#' Extend a Distribution
#'
#' Extend a distribution to new values of a random variable. The extended
#' distribution is zero-valued at the new points.
#'
#' @param x,to \code{\link[=Distribution-class]{Distributions}} that are defined
#'   for random variables that have the same dimension names.
#'
#' @return A \code{\link[=Distribution-class]{Distribution}} that is like
#'   \code{x} except that it has additional rows that correspond to the rows
#'   of \code{to} that were not originally in \code{x}.
#'
#' @name Extend
#' @noRd
methods::setGeneric(
  name = "Extend",
  def = function(x, to, ...) {
    standardGeneric("Extend")
  }
)

#' @usage \S4method{Extend}{Distribution,Distribution}(x, to)
#' @name Extend,Distribution,Distribution-method
#' @rdname Extend
#' @noRd
methods::setMethod(
  f = "Extend",
  signature = c(x = "Distribution", to = "Distribution"),
  definition = function(x, to) {
    x_rv <- as_tibble(x) %>% dplyr::select(-.data$P)
    to_rv <- as_tibble(to) %>% dplyr::select(-.data$P)
    if (!identical(names(x_rv) %>% sort(), names(to_rv) %>% sort())) {
      stop(stringr::str_c(substitute(x, env = rlang::call_frame(2)[["env"]]),
                          "and",
                          substitute(to, env = rlang::call_frame(2)[["env"]]),
                          "must be defined on random variables with",
                          "the same dimension names.",
                          sep = " "))
    }
    extension <-
      suppressMessages(dplyr::anti_join(to_rv, x_rv)) %>%
      dplyr::arrange(rlang::UQS(rlang::syms(names(x_rv)))) %>%
      dplyr::mutate(P = 0)
    extended <-
      as_tibble(x) %>%
      dplyr::bind_rows(extension) %>%
      as.data.frame()
    methods::new(Class = "Distribution", extended)
  }
)




#' Test Identity Between Distributions
#'
#' Test identity between various components of two distributions, e.g. the
#' names of the state space of the random variable, the states of the random
#' variable, and the probabilities assigned to the states.
#'
#' @param x,y \code{\link[=Distribution-class]{Distributions}}.
#' @param names A logical. If \code{TRUE}, then the names of \code{x} and
#'   \code{y} are compared.
#' @param states A logical. If \code{TRUE}, then the states of the random
#'   variables associated with \code{x} and \code{y} (i.e., the values of the
#'   columns other than \code{P}) are compared.
#' @param probs A logical. If \code{TRUE}, then the probabilities assigned by
#'   \code{x} and \code{y} (i.e., the values of the column \code{P}) are
#'   compared.
#' @param arrange A logical. If \code{TRUE}, then \code{x} and \code{y} are
#'   respectively passed to \code{\link{Arrange}} before being compared.
#'   Arranging allows for more lenient comparison between distributions that
#'   may denote the same set-theoretic information that happens to be arranged
#'   differently due to their computational implementation as data frames.
#'
#' @return A logical.
#'
#' @name Identical
#' @export
methods::setGeneric(
  name = "Identical",
  def = function(x, y, ...) {
    standardGeneric("Identical")
  }
)

#' @usage \S4method{Identical}{Distribution,Distribution}(x, y, names = TRUE, states = TRUE, probs = TRUE, arrange = TRUE)
#' @name Identical,Distribution,Distribution-method
#' @rdname Identical
methods::setMethod(
  f = "Identical",
  signature = c(x = "Distribution", y = "Distribution"),
  definition = function(x, y, names = TRUE, states = TRUE, probs = TRUE, arrange = TRUE) {
    x <- if (arrange) {Arrange(x)} else {x}
    y <- if (arrange) {Arrange(y)} else {y}
    n <- !names || identical(setdiff(names(x), "P"), setdiff(names(y), "P"))
    s <- !states || identical(dplyr::select(as_tibble(x), -.data$P),
                              dplyr::select(as_tibble(y), -.data$P))
    p <- !probs || identical(dplyr::select(as_tibble(x), .data$P),
                             dplyr::select(as_tibble(y), .data$P))
    return(n && s && p)
  }
)

