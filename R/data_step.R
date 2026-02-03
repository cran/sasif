#' @import data.table
NULL

#-------------------------------------------------
# DATA step engine
#-------------------------------------------------

#' SAS IF-style data step logic for data.table
#'
#' Provides SAS-style IF/ELSE chains, independent IF rules, and DELETE logic
#' for fast, vectorized transformations on data.table objects. This enables
#' clinical programmers to express SDTM and ADaM-style derivations in familiar
#' SAS-like syntax while leveraging data.table performance.
#'
#' @param dt A data.table.
#' @param ... One or more rule objects created by if_do(), else_if_do(),
#'   else_do(), if_independent(), or delete_if().
#' @param copy Logical. If TRUE (default), a copy of dt is modified and returned.
#'
#' @return A data.table with applied transformations.
#'
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   AGE = c(40, 60, 80),
#'   SEX = c("M", "F", "M")
#' )
#'
#' out <- data_step(
#'   dt,
#'   if_do(AGE <= 45, GROUP = 1),
#'   else_if_do(AGE <= 70, GROUP = 2),
#'   else_do(GROUP = 3),
#'   if_independent(SEX == "M", MALE = 1)
#' )
#'
#' out
#'
#' @export
data_step <- function(dt, ..., copy = TRUE) {

  if (!data.table::is.data.table(dt)) {
    stop(
      "`dt` must be a data.table. Use setDT() or as.data.table() before calling.",
      call. = FALSE
    )
  }

  out   <- if (copy) data.table::copy(dt) else dt
  rules <- list(...)
  n     <- nrow(out)

  matched  <- rep(FALSE, n)
  in_chain <- FALSE

  for (rule in rules) {

    # ---- Rule validation ----
    if (!is.list(rule) || is.null(rule$type)) {
      stop(
        "All arguments to data_step() must be rules created by if_do(), else_if_do(), else_do(), if_independent(), or delete_if().",
        call. = FALSE
      )
    }

    #-------------------------------
    # DELETE
    #-------------------------------
    if (rule$type == "delete") {

      del <- eval(rule$cond, out, parent.frame())

      if (!is.logical(del)) {
        stop("delete_if() condition must return a logical vector.", call. = FALSE)
      }
      if (length(del) != nrow(out)) {
        stop("delete_if() condition must return one value per row.", call. = FALSE)
      }

      del[is.na(del)] <- FALSE
      out     <- out[!del]
      matched <- matched[!del]

      in_chain <- FALSE
      next
    }

    #-------------------------------
    # Determine active mask
    #-------------------------------
    if (rule$type %in% c("if", "else_if")) {

      in_chain <- TRUE

      mask <- eval(rule$cond, out, parent.frame())

      if (!is.logical(mask)) {
        stop("if_do()/else_if_do() condition must return a logical vector.", call. = FALSE)
      }
      if (length(mask) != nrow(out)) {
        stop("if_do()/else_if_do() condition must return one value per row.", call. = FALSE)
      }

      mask[is.na(mask)] <- FALSE
      active <- mask & !matched

    } else if (rule$type == "else") {

      if (!in_chain) {
        stop("else_do() must follow if_do() / else_if_do().", call. = FALSE)
      }

      active   <- !matched
      in_chain <- FALSE

    } else if (rule$type == "independent") {

      in_chain <- FALSE

      mask <- eval(rule$cond, out, parent.frame())

      if (!is.logical(mask)) {
        stop("if_independent() condition must return a logical vector.", call. = FALSE)
      }
      if (length(mask) != nrow(out)) {
        stop("if_independent() condition must return one value per row.", call. = FALSE)
      }

      mask[is.na(mask)] <- FALSE
      active <- mask

    } else {
      stop("Unknown rule type.", call. = FALSE)
    }

    if (!any(active)) next

    #-------------------------------
    # ASSIGNMENTS
    #-------------------------------
    for (nm in names(rule$assign)) {

      value <- eval(rule$assign[[nm]], out[active], parent.frame())

      if (!(length(value) == 1L || length(value) == sum(active))) {
        stop(
          sprintf(
            "Assignment to '%s' must be length 1 or match number of active rows (%d).",
            nm, sum(active)
          ),
          call. = FALSE
        )
      }

      out[active, (nm) := value]
    }

    #-------------------------------
    # Update matched
    #-------------------------------
    if (rule$type != "independent") {
      matched[active] <- TRUE
    }
  }

  out
}

#-------------------------------------------------
# Rule constructors
#-------------------------------------------------

#' Create a SAS-style IF rule
#'
#' Creates a mutually exclusive IF rule for use inside data_step().
#'
#' @param condition Logical condition evaluated on the data.table.
#' @param ... Named assignments to apply when condition is TRUE.
#'
#' @return A rule object for data_step().
#' @export
if_do <- function(condition, ...) {
  list(
    type   = "if",
    cond   = substitute(condition),
    assign = as.list(substitute(list(...)))[-1]
  )
}

#' Create a SAS-style ELSE IF rule
#'
#' Creates an ELSE IF rule for use inside data_step().
#'
#' @param condition Logical condition evaluated on the data.table.
#' @param ... Named assignments to apply when condition is TRUE.
#'
#' @return A rule object for data_step().
#' @export
else_if_do <- function(condition, ...) {
  list(
    type   = "else_if",
    cond   = substitute(condition),
    assign = as.list(substitute(list(...)))[-1]
  )
}

#' Create a SAS-style ELSE rule
#'
#' Creates an ELSE rule for use inside data_step().
#'
#' @param ... Named assignments to apply when no previous IF/ELSE IF matched.
#'
#' @return A rule object for data_step().
#' @export
else_do <- function(...) {
  list(
    type   = "else",
    cond   = NULL,
    assign = as.list(substitute(list(...)))[-1]
  )
}

#' Create a SAS-style DELETE rule
#'
#' Creates a DELETE rule to remove rows from the data.table when condition is TRUE.
#'
#' @param condition Logical condition evaluated on the data.table.
#'
#' @return A rule object for data_step().
#' @export
delete_if <- function(condition) {
  list(
    type = "delete",
    cond = substitute(condition)
  )
}

#' Create an independent SAS-style IF rule
#'
#' Creates an independent IF rule that is evaluated regardless of IF/ELSE chains.
#'
#' @param condition Logical condition evaluated on the data.table.
#' @param ... Named assignments to apply when condition is TRUE.
#'
#' @return A rule object for data_step().
#' @export
if_independent <- function(condition, ...) {
  list(
    type   = "independent",
    cond   = substitute(condition),
    assign = as.list(substitute(list(...)))[-1]
  )
}
