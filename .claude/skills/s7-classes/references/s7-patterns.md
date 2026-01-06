# S7 Patterns Reference

Detailed S7 patterns for properties, validation, generics, and methods.

## Property Defaults

### Static Default

```r
count = S7::new_property(S7::class_numeric, default = 0)
```

### Dynamic Default (Quoted Expression)

Evaluated at construction time:

```r
created_at = S7::new_property(
  class = S7::class_POSIXct,
  default = quote(Sys.time())  # Fresh timestamp each time
)
```

### Required Property (No Default)

Force users to provide value:

```r
id = S7::new_property(

  class = S7::class_character,
  default = quote(stop("'id' is required"))
)
```

## Computed Properties

### Read-Only (Getter Only)

```r
length = S7::new_property(
  getter = function(self) self@end - self@start
)
```

### Read-Write (Getter + Setter)

```r
length = S7::new_property(
  class = S7::class_double,
  getter = function(self) self@end - self@start,
  setter = function(self, value) {
    self@end <- self@start + value
    self  # Must return self
  }
)
```

## Special Property Patterns

### Frozen Property (Immutable After Creation)

```r
birth_date = S7::new_property(

  class = S7::class_Date,
  setter = function(self, value) {
    if (!is.null(self@birth_date)) {
      stop("'birth_date' cannot be modified after creation")
    }
    self@birth_date <- value
    self
  }
)
```

### Deprecated Property (Backward Compatibility)

```r
# Old property name, redirect to new one
old_name = S7::new_property(
  getter = function(self) {
    warning("'old_name' is deprecated, use 'new_name'")
    self@new_name
  },
  setter = function(self, value) {
    warning("'old_name' is deprecated, use 'new_name'")
    self@new_name <- value
    self
  }
)
```

## Batch Property Updates

Avoid intermediate validation failures when updating related properties:

```r
# Instead of:
obj@start <- 10  # May fail if end < start temporarily
obj@end <- 20

# Use batch update:
S7::props(obj) <- list(start = 10, end = 20)  # Single validation
```

## Validation Patterns

### Class-Level Validator

For property interactions (runs after all properties set):

```r
validator = function(self) {
  if (self@end < self@start) {
    return("'end' must be >= 'start'.")
  }
  NULL  # Must return NULL on success
}
```

### Property-Level Validator

For single property rules:

```r
prop = S7::new_property(
  class = S7::class_numeric,
  validator = function(value) {
    if (length(value) != 1) "must be length 1"
    # Return string on error, NULL on success
  }
)
```

### rctbayespower Style (cli::cli_abort in Setters)

```r
setter = function(self, value) {
  if (!value %in% valid_options) {
    cli::cli_abort(c(
      "Invalid {.arg param_name}",
      "x" = "You supplied {.val {value}}",
      "i" = "Must be one of: {.val {valid_options}}"
    ))
  }
  self@param_name <- value
  self
}
```

## Method Registration

### Basic Method

```r
S7::method(print, rctbp_classname) <- function(x, ...) {
  cli::cli_h2("rctbp_classname")
  cli::cli_alert_info("Property: {.val {x@property}}")
  invisible(x)
}
```

### Method with Parameters

```r
S7::method(summary, rctbp_classname) <- function(object, verbose = FALSE, ...) {
  if (verbose) {
    # detailed output
  }
  invisible(object)
}
```

### Multiple Dispatch

```r
speak <- S7::new_generic("speak", c("x", "y"))
S7::method(speak, list(Dog, English)) <- function(x, y) "Woof"
S7::method(speak, list(Cat, French)) <- function(x, y) "Miaou"
```

## Creating Generics

### Basic Generic

```r
process <- S7::new_generic("process", "x")
```

### Generic with Custom Body

Must include `S7_dispatch()`:

```r
validate <- S7::new_generic("validate", "x", function(x, strict = FALSE) {
  # Pre-dispatch logic
  result <- S7::S7_dispatch()
  # Post-dispatch logic
  result
})
```

### Generic with Required Arguments

```r
compare <- S7::new_generic("compare", "x", function(x, y, ...) {
  S7::S7_dispatch()
})
```

## Method Compatibility Rules

Methods must satisfy:
1. All generic arguments must exist in the method
2. Dispatch arguments **cannot have defaults** in methods
3. Methods may add arguments only if generic includes `...`

```r
# Generic with ...
process <- S7::new_generic("process", "x", function(x, ...) S7::S7_dispatch())

# Method can add extra arguments
S7::method(process, MyClass) <- function(x, ..., verbose = FALSE) {
  # verbose is allowed because generic has ...
}
```

## Accessing Properties

```r
# Get property
value <- obj@property_name

# Set property (triggers validation)
obj@property_name <- new_value
```

## Special Classes for Dispatch

| Class | Purpose |
|-------|---------|
| `S7::class_any` | Matches any class |
| `S7::class_missing` | Matches missing arguments |
| `S7::S7_object` | Base class for all S7 objects |

## Abstract Classes

S7 doesn't have formal abstract classes. To prevent direct instantiation:
- Don't export the constructor
- Add a validator that checks for subclass

```r
# Base class - not exported
rctbp_base <- S7::new_class(
  "rctbp_base",
  properties = list(...),
  validator = function(self) {
    if (identical(class(self)[[1]], "rctbp_base")) {
      return("rctbp_base cannot be instantiated directly")
    }
    NULL
  }
)

# Subclass - exported
rctbp_concrete <- S7::new_class(
  "rctbp_concrete",
  parent = rctbp_base,
  properties = list(...)
)
```

## Official Documentation

- S7 basics: https://cran.r-project.org/web/packages/S7/vignettes/S7.html
- Classes/objects: https://cran.r-project.org/web/packages/S7/vignettes/classes-objects.html
- Generics/methods: https://cran.r-project.org/web/packages/S7/vignettes/generics-methods.html
