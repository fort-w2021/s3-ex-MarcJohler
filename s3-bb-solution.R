# helper function for user to generate bb objects
bb <- function(object) {
  # first input checking
  checkmate::assert(
    checkmate::check_character(object),
    checkmate::check_list(object),
    checkmate::check_factor(object)
  )
  UseMethod("bb", object)
}

# constructor function to convert text into bb-text
new_bb <- function(text) {
  # replace ((leading consonants) (1-2 vowels) (trailing consonants)) with
  # ((leading consonants) (vowels) b (same vowels again) (trailing consonants)):
  match <- "([^aeiouäöüAEIOUÄÜÖ]*)([aeiouäöü]{1,2})([^aeiouäöü]*)"
  gsub(pattern = match, replacement = "\\1\\2b\\2\\3", x = text)
}

# Default Constructor-function for class bb
bb.default <- function(object) {
  # loop over all the elements and replace strings
  for (i in seq_len(length(object))) {
    object[i] <- new_bb(object[i])
  }
  # add new class and return
  structure(object,
    class = c("bb", class(object))
  )
}

# Constructor function for class bb based on a factor/ordered
bb.factor <- function(object) {
  # remember if it's an ordered factor
  is_ordered <- FALSE
  if (is.ordered(object)) {
    is_ordered <- TRUE
  }
  # remember the levels in original order as well as their occurences
  levels <- levels(object)
  occurences <- unclass(object)
  # loop over all the levels and replace the respective strings
  for (i in seq_len(length(levels))) {
    levels[i] <- new_bb(levels[i])
  }
  # create new factor with new levels
  object <- factor(levels[occurences], levels = levels, ordered = is_ordered)
  # generate vector of classes
  classes <- c("bb", "ordered", "factor")
  # add new class and return
  structure(object,
    class = classes[c(TRUE, is_ordered, TRUE)]
  )
}

# Constructor function for class bb based on a matrix
bb.matrix <- function(object) {
  # loop over all the elements and replace strings
  for (i in seq_len(nrow(object))) {
    for (j in seq_len(ncol(object))) {
      object[i, j] <- new_bb(object[i, j])
    }
  }
  # add new class and return
  structure(object,
    class = c("bb", "matrix", "array")
  )
}

# Constructor function for class bb based on a list
bb.list <- function(object) {
  # loop over all the elements and apply helper function, since the class of a
  # list element can vary
  for (i in seq_len(length(object))) {
    object[[i]] <- bb(object[[i]])
  }
  # add new class and return
  structure(object,
    class = c("bb", "list")
  )
}
