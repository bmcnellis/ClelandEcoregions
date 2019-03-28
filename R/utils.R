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
  out_df <- data.frame(stringsAsFactors = F)
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
  mdf <- ClelandEcoregions::Cleland_meta_df
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
  if (all(x %in% mdf$section_code)) {
    if (out_lvl == 'section') {
      return(x)
    }
  } else {
    stop('section scaling failed - check to make sure input is CODE, not NAME')
  }

  # Scaling section to province:
  b0 <- sapply(strsplit(x, ''), function(z) z[1:(length(z) - 1)])
  b1 <- sapply(b0, function(z) paste(z, collapse = ''))
  x <- b1
  if (all(x %in% mdf$province_code)) {
    if (out_lvl == 'province') {
      return(x)
    }
  } else {
    stop('province scaling failed - make sure input is CODE, not NAME')
  }
  stop('scaling failed - bad inputs?')
  invisible()
}
#' @describeIn utils Scales up Cleland name by 1 level
#' @export
ScaleUpClelandName <- function(x, in_lvl = 'subsection', debug = F, belt = 'M332D') {
  mdf <- ClelandEcoregions::Cleland_meta_df
  z <- character(length(x))
  im <- character()
  oc <- character()
  bw <- 1
  if (in_lvl == 'subsection') {
    im <- 'subsection_name'
    oc <- 'section_code'
    kl <- 'section'
  } else if (in_lvl == 'section') {
    im <- 'section_name'
    oc <- 'province_code'
    kl <- 'province'
  } else {
    if (debug) browser()
    stop('Bad in_lvl input (set debug = T?)')
  }
  for (i in 1:length(x)) {
    if (x[i] == 'Belt Mountains') {
      if (!(in_lvl == 'section')) {
        if (debug) browser()
        stop('Belt Mountain exception broke')
      }
      if (as.logical(bw)) {
        belt_warn <- paste0('Belt Mountains is a section in 2 provs, keying as ', belt)
        bw <- bw - 1
        warning(belt_warn)
        cat('\n')
      }
      if (belt == 'M332D') {
        z[i] <- mdf$province_code[which(mdf$section_code == 'M332D')[1]] 
      } else if (belt == '331N') {
        z[i] <- mdf$province_code[which(mdf$section_code == '331N')[1]]
      } else {
        if (debug) browser()
        stop('Bad belt input')
      }
      if (debug) if (is.na(z[i])) browser() 
      next
    }
    xx <- paste0('^', x[i], '$')
    uq <- unique(mdf[[oc]][grep(xx, mdf[[im]])])
    if (length(uq) == 1) {
      if (debug) if (is.na(z[i])) browser()
      z[i] <- uq
    } else {
      if (debug) browser()
      stop('Name grep failed (set debug = T?)')
    }
  }
  zz <- ClelandEcoregions::KeyClelandCode(z, lvl = kl)
  if (all(!(any(is.na(z))), any(is.na(zz)))) {
    if (debug) browser()
    stop('introduced NAs with KeyClelandCode')
  }
  zz
}
#' @describeIn utils Prints the Cleland subsection name for a code
#' @export
NameClelandCode <- function(code) {
  mdf <- ClelandEcoregions::Cleland_meta_df
  m <- which(mdf$subsection_code == code)
  if (length(m) == 0) stop('Cleland code not found')
  cat('\nSubsection name:', mdf$subsection_name[m[1]])
  cat('\nSection name:', mdf$section_name[m[1]])
  cat('\nProvince name:', mdf$province_name[m[1]])
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
#' @describeIn utils Scales down Cleland code
#' @export
ScaleDownClelandCode <- function(x, in_lvl = 'province', out_lvl = 'subsection') {

  mdf0 <- ClelandEcoregions::Cleland_meta_df
  mdf <- mdf0
  xx <- x
  if (in_lvl == 'province') {
    mdf <- mdf[which(mdf$province_code %in% xx), ]
    xx <- mdf[, 'section_code']
    if (out_lvl == 'section') {
      return(mdf[, c('province_code', 'section_code')])
    }
  }
  
  if (out_lvl == 'subsection') {
    mdf <- mdf[which(mdf$section_code %in% xx), ]
    return(mdf[, c('province_code', 'section_code', 'subsection_code')])
  } else {
    return(mdf[, c('province_code', 'section_code', 'subsection_code')])
  }
  
  stop('scaling failed')
}
#' @describeIn utils Cleland subsection integer key for .nc storage
#' @export
SubAsIntger <- function(x, rev = F) {
  full_subs <- unique(ClelandEcoregions::Cleland_meta_df$subsection_code)
  full_subs <- full_subs[order(full_subs)]
  if (is.integer(x)) {
    return(full_subs[x])
  } else if (is.character(x)) {
    return(match(x, full_subs))
  } else {
    stop('bad input')
  }
}