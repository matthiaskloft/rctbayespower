#' Update Properties of an S7 Object with `...`
#'
#' Updates properties of an S7 object by replacing values with those passed
#' via `...`. Only existing properties of the object's class are updated;
#' non-matching names in `...` trigger a warning.
#'
#' @param object An S7 object whose class defines properties via `S7::new_class()`.
#' @param ... Named arguments corresponding to properties to update.
#'
#' @return The modified S7 object with updated property values.
#'
#' @keywords internal
update_S7_with_dots <- function(object, ...) {
  dots <- list(...)
  
  # Get all property names of the object
  prop_names <- S7::prop_names(object)
  
  # Replace any matching properties with those from dots
  for (name in names(dots)) {
    if (name %in% prop_names) {
      S7::prop(object = object,name =  name) <- dots[[name]]
    } else {
      warning(sprintf("'%s' is not a property of class '%s'", name, class(object)))
    }
  }
  # revalidate the object to ensure it meets class requirements
  S7::validate(object)
  
  return(object)
}



#' S7 Class Validator for Data Frames
#'
#' Defines an S7 class `class_data_frame` that accepts only S3 `data.frame` objects.
#' This is useful for validating properties in S7 classes that should hold standard
#' data frame values. Used when `S7::class_data.frame` fails to match S3 objects.
#'
#' @format An S7 class object for validating standard data frames.
#'
#' @return An S7 class validator that passes only if the object is a `data.frame`.
#'
#' @keywords internal
class_data_frame <- S7::new_class(
  "class_data_frame",
  parent = S7::new_S3_class("data.frame"),
  validator = function(self) {
    if (!is.data.frame(self)) {
      stop("Must be a data.frame")
    }
    NULL
  }
)
