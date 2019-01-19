#' @title Small utility functions for Cleland Ecoregions
#'
#' @description
#'
#' Some functions that are required for package processing or just useful to have
#' when working with the ClelandEcoregions dataset.
#'
#' @name utils
NULL
#' @describeIn utils Key function for the FIA codes
#' @export
KeyClelandCode <- function(x, lvl = 'province', long = F, rev = F) {
  out_df <- data.frame()
  if (lvl == 'province') {
    df <- Cleland_meta_df[, c('province_code', 'province_name')]
    if (rev) {
      for (i in x) {
        stop('implement/check this this')
        grep(pattern = i, x = df$province_name)
      }
    } else {
      l <- df$province_name[match(x, df$province_code)]
      if (long) return(l)
      sp <- sapply(strsplit(l, ' - '), function(x) x[1])
      s <- sapply(strsplit(sp, ' Province'), function(x) x[1])
    }
    return(s)
  } else if (lvl == 'section') {
    df <- Cleland_meta_df[, c('section_code', 'section_name')]
    if (rev) {
      sp <- df$section_code[match(x, df$section_name)]
    } else {
      l <- df$section_name[match(x, df$section_code)]
      if (long) return(l)
      sp <- sapply(strsplit(l, ' - '), function(x) x[1])
    }
    return(sp)
  } else if (lvl == 'subsection') {
    stop('implement this')
    df <- Cleland_meta_df[, c('subsection_code', 'subsection_name')]
  } else {
    stop('bad lvl input')
  }
  invisible()
}
#' @describeIn utils Scales up the Cleland code
#' @export
ScaleUpClelandCode <- function(x, in_lvl = 'subsection', out_lvl = 'section') {
  # Scaling subsection to section:
  if (in_lvl == 'subsection') {
    a0 <- sapply(strsplit(x, ''), function(z) z[1:(length(z) - 1)])
    a1 <- sapply(a0, function(z) paste(z, collapse = ''))
    x <- sapply(a1, function(z) {
      z0 <- unlist(strsplit(z, ''))
      r <- ifelse(z0[length(z0)] %in% letters,
                  paste(z0[1:(length(z0) - 1)], collapse = ''),
                  z)
      r
    })
  }
  if (all(x %in% Cleland_meta_df$section_code)) {
    if (out_lvl == 'section') {
      return(x)
    }
  } else {
    stop('section scaling failed')
  }

  # Scaling section to province:
  b0 <- sapply(strsplit(x, ''), function(z) z[1:(length(z) - 1)])
  b1 <- sapply(b0, function(z) paste(z, collapse = ''))
  x <- b1
  if (all(x %in% Cleland_meta_df$province_code)) {
    if (out_lvl == 'province') {
      return(x)
    }
  } else {
    stop('province scaling failed')
  }
  stop('scaling failed - bad inputs?')
  invisible()
}
#' @describeIn utils Prints the Cleland subsection name for a code
#' @export
NameClelandCode <- function(code) {
  m <- which(Cleland_meta_df$subsection_code == code)
  if (length(m) == 0) stop('Cleland code not found')
  cat('\nSubsection name:', Cleland_meta_df$subsection_name[m[1]])
  cat('\nSection name:', Cleland_meta_df$section_name[m[1]])
  cat('\nProvince name:', Cleland_meta_df$province_name[m[1]])
  invisible()
}
#' @describeIn utils Rrims a character by 1 character for each element
#' @export
SnipSingleCharacter <- function(v, side = 'front') {
  stopifnot(class(v) == 'character' && is.vector(v))
  y <- strsplit(v, '')
  if (side == 'front') {
    y <- lapply(y, function(x) x[2:(length(x))])
  } else if (side == 'back') {
    y <- lapply(y, function(x) x[1:(length(x) - 1)])
  } else {
    stop('Side must be either front or back.')
  }
  y <- lapply(y, function(x) paste(x, collapse = ''))
  return(unlist(y))
}
