#' Apply Formatting Functions
#'
#' Apply the formatting functions to each of the raw statistics.
#' Function aliases are converted to functions using [alias_as_fmt_fun()].
#'
#' @param x (`data.frame`)\cr
#'   an ARD data frame of class 'card'
#' @param replace (scalar `logical`)\cr
#'   logical indicating whether to replace values in the `'stat_fmt'` column (if present).
#'   Default is `FALSE`.
#'
#' @return an ARD data frame of class 'card'
#' @export
#'
#' @examples
#' ard_summary(ADSL, variables = "AGE") |>
#'   apply_fmt_fun()
apply_fmt_fun <- function(x, replace = FALSE) {
  set_cli_abort_call()

  check_class(x, cls = "card")
  check_scalar_logical(replace)

  # add stat_fmt if not already present, if replace is TRUE overwrite existing stat_fmt column
  if (!"stat_fmt" %in% names(x) || isTRUE(replace)) {
    x <- x |> dplyr::mutate(.after = "stat", stat_fmt = list(NULL))
  }

  stat_fmt <- x[["stat_fmt"]]
  fmt_fun <- x[["fmt_fun"]]
  stat <- x[["stat"]]
  variable <- x[["variable"]]
  stat_name <- x[["stat_name"]]
  n_rows <- nrow(x)

  # identify which rows need formatting
  to_format_idx <- which(
    !vapply(fmt_fun, is.null, logical(1L)) &
      vapply(stat_fmt, is.null, logical(1L))
  )

  if (length(to_format_idx) == 0L) {
    return(x)
  }

  # Group eligible rows by fmt_fun object
  # For integerish / character fmt_fun, we can group by their string/numeric value
  # For functions or other types, we can group by identical objects
  fmt_fun_sub <- fmt_fun[to_format_idx]
  
  # Create group keys for fmt_funs
  group_keys <- character(length(to_format_idx))
  fn_registry <- list()
  
  for (i in seq_along(to_format_idx)) {
    fn_item <- fmt_fun_sub[[i]]
    if (is.character(fn_item) && length(fn_item) == 1L) {
      group_keys[i] <- paste0("chr:", fn_item)
    } else if (is.numeric(fn_item) && length(fn_item) == 1L) {
      group_keys[i] <- paste0("num:", fn_item)
    } else {
      # Function or other object: find match in fn_registry
      matched <- FALSE
      for (k in seq_along(fn_registry)) {
        if (identical(fn_item, fn_registry[[k]])) {
          group_keys[i] <- paste0("obj:", k)
          matched <- TRUE
          break
        }
      }
      if (!matched) {
        new_k <- length(fn_registry) + 1L
        fn_registry[[new_k]] <- fn_item
        group_keys[i] <- paste0("obj:", new_k)
      }
    }
  }

  groups <- split(to_format_idx, group_keys)

  for (grp_indices in groups) {
    first_idx <- grp_indices[1L]
    raw_fn <- fmt_fun[[first_idx]]
    var_first <- variable[first_idx]
    stat_first <- stat_name[first_idx]

    # Resolve format function (and validate format alias)
    resolved_fn <- tryCatch(
      alias_as_fmt_fun(raw_fn, var_first, stat_first),
      error = function(e) {
        cli::cli_abort(
          c("There was an error applying the formatting function to
             statistic {.val {stat_first}} for variable {.val {var_first}}.",
            "i" = "Perhaps try formmatting function {.fun as.character}? See error message below:",
            "x" = conditionMessage(e)
          ),
          call = get_cli_abort_call()
        )
      }
    )

    # Extract stats for all rows in this group
    stats_list <- stat[grp_indices]

    # Check if stats are all scalars and can be unlisted into a vector
    # (or if any is non-scalar/list/etc.)
    can_vectorize <- is.function(resolved_fn) &&
      all(vapply(stats_list, function(s) length(s) == 1L && !is.list(s), logical(1L)))

    formatted_res <- NULL
    if (can_vectorize) {
      stats_vec <- unlist(stats_list, recursive = FALSE, use.names = FALSE)
      # Try vectorized call
      formatted_res <- tryCatch(
        as.list(do.call(resolved_fn, list(stats_vec))),
        error = function(e) NULL
      )
      if (!is.null(formatted_res) && length(formatted_res) != length(grp_indices)) {
        formatted_res <- NULL
      }
    }

    if (!is.null(formatted_res)) {
      stat_fmt[grp_indices] <- formatted_res
    } else {
      # Fallback to row-by-row invocation within this group with proper error handling
      for (idx in grp_indices) {
        var_i <- variable[idx]
        stat_i <- stat_name[idx]
        stat_fmt[[idx]] <- tryCatch(
          {
            fn_i <- if (identical(raw_fn, fmt_fun[[idx]])) resolved_fn else alias_as_fmt_fun(fmt_fun[[idx]], var_i, stat_i)
            do.call(fn_i, args = list(stat[[idx]]))
          },
          error = function(e) {
            cli::cli_abort(
              c("There was an error applying the formatting function to
                 statistic {.val {stat_i}} for variable {.val {var_i}}.",
                "i" = "Perhaps try formmatting function {.fun as.character}? See error message below:",
                "x" = conditionMessage(e)
              ),
              call = get_cli_abort_call()
            )
          }
        )
      }
    }
  }

  x[["stat_fmt"]] <- stat_fmt
  x
}

#' Convert Alias to Function
#'
#' @description
#' Accepted aliases are non-negative integers and strings.
#'
#' The integers are converted to functions that round the statistics
#' to the number of decimal places to match the integer.
#'
#' The formatting strings come in the form `"xx"`, `"xx.x"`, `"xx.x%"`, etc.
#' The number of `x`s that appear after the decimal place indicate the number of
#' decimal places the statistics will be rounded to.
#' The number of `x`s that appear before the decimal place indicate the leading
#' spaces that are added to the result.
#' If the string ends in `"%"`, results are scaled by 100 before rounding.
#'
#' @param x (`integer`, `string`, or `function`)\cr
#'   a non-negative integer, string alias, or function
#' @param variable (`character`)\cr the variable whose statistic is to be formatted
#' @param stat_name (`character`)\cr the name of the statistic that is to be formatted
#'
#' @return a function
#' @export
#'
#' @examples
#' alias_as_fmt_fun(1)
#' alias_as_fmt_fun("xx.x")
alias_as_fmt_fun <- function(x, variable, stat_name) {
  set_cli_abort_call()

  if (is.function(x)) {
    return(x)
  }
  if (is_integerish(x) && x >= 0L) {
    return(label_round(digits = as.integer(x)))
  }
  if (is_string(x)) {
    .check_fmt_string(x, variable, stat_name)
    scale <- ifelse(endsWith(x, "%"), 100, 1)
    decimal_n <-
      ifelse(
        !grepl("\\.", x),
        0L,
        gsub("%", "", x) |> # remove percent sign if it is there
          strsplit(split = ".", fixed = TRUE) |> # split string at decimal place
          unlist() %>%
          `[`(2) %>% # get the string after the period
          {ifelse(is.na(.), 0L, nchar(.))} # styler: off
      )
    width <- nchar(x) - endsWith(x, "%")

    return(label_round(digits = decimal_n, scale = scale, width = width))
  }

  # if the above conditions are not met, return an error -----------------------
  if (!missing(variable) && !missing(stat_name)) {
    error_message <-
      c("The value in {.arg fmt_fun} cannot be converted into a function for
         statistic {.val {stat_name}} and variable {.val {variable}}.",
        "i" = "Value must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.",
        "*" = "See {.help cards::alias_as_fmt_fun} for details."
      )
  } else {
    error_message <-
      c("The value in {.arg fmt_fun} cannot be converted into a function.",
        "i" = "Value must be a function, a non-negative integer, or a formatting string, e.g. {.val xx.x}.",
        "*" = "See {.help cards::alias_as_fmt_fun} for details."
      )
  }

  cli::cli_abort(
    message = error_message,
    call = get_cli_abort_call()
  )
}

#' Generate Formatting Function
#'
#' Returns a function with the requested rounding and scaling schema.
#'
#' @param digits (`integer`)\cr
#'   a non-negative integer specifying the number of decimal places
#'   round statistics to
#' @param scale (`numeric`)\cr
#'   a scalar real number. Before rounding, the input will be scaled by
#'   this quantity
#' @param width (`integer`)\cr
#'   a non-negative integer specifying the minimum width of the
#'   returned formatted values
#'
#' @return a function
#' @export
#'
#' @examples
#' label_round(2)(pi)
#' label_round(1, scale = 100)(pi)
#' label_round(2, width = 5)(pi)
label_round <- function(digits = 1, scale = 1, width = NULL) {
  round_fun <- .get_round_fun()

  function(x) {
    # round and scale vector
    res <-
      ifelse(
        is.na(x),
        NA_character_,
        format(round_fun(x * scale, digits = digits), nsmall = digits) |> str_trim()
      )


    # if width provided, pad formatted result
    if (!is.null(width)) {
      res <-
        ifelse(
          nchar(res) >= width | is.na(res),
          res,
          paste0(strrep(" ", width - nchar(res)), res)
        )
    }

    # return final formatted vector
    res
  }
}

.get_round_fun <- function() {
  switch(getOption("cards.round_type", default = "round-half-up"),
    "round-half-up" = round5,
    "round-to-even" = round
  ) %||%
    cli::cli_abort(
      "The {.arg cards.round_type} {.emph option} must be one of
         {.val {c('round-half-up', 'round-to-even')}}.",
      call = get_cli_abort_call()
    )
}


#' Check 'xx' Format Structure
#'
#' @description
#' A function that checks a **single** string for consistency.
#' String must begin with 'x' and only consist of x's, a single period or none,
#' and may end with a percent symbol.
#'
#' If string is consistent, `TRUE` is returned. Otherwise an error.
#'
#' @param x (`string`)\cr
#'   string to check
#' @param variable (`character`)\cr the variable whose statistic is to be formatted
#' @param stat_name (`character`)\cr the name of the statistic that is to be formatted
#'
#' @return a logical
#' @keywords internal
#'
#' @examples
#' cards:::.check_fmt_string("xx.x") # TRUE
#' cards:::.check_fmt_string("xx.x%") # TRUE
.check_fmt_string <- function(x, variable, stat_name) {
  set_cli_abort_call()

  # perform checks on the string
  fmt_is_good <-
    grepl("^x[x.%]+$", x = x) && # string begins with 'x', and consists of only x, period, or percent
      sum(unlist(gregexpr("\\.", x)) != -1) %in% c(0L, 1L) && # a period appears 0 or 1 times
      sum(unlist(gregexpr("%", x)) != -1) %in% c(0L, 1L) && # a percent appears 0 or 1 times
      (sum(unlist(gregexpr("%", x)) != -1) %in% 0L || grepl(pattern = "%$", x = x)) # if there is a % it appears at the end

  if (isFALSE(fmt_is_good)) {
    cli::cli_abort(
      message =
        "The format {.val {x}} for `fmt_fun` is not valid for the
         variable {.val {variable}} for the statistic {.val {stat_name}}.
         String must begin with 'x' and only consist of x's, a single period or
         none, and may end with a percent symbol.",
      call = get_cli_abort_call()
    )
  }
  fmt_is_good
}
