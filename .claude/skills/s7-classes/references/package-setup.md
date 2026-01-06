# S7 Package Setup Reference

How to integrate S7 classes into an R package.

## Package Setup (zzz.R)

**Required** - register methods in `.onLoad`:

```r
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
```

This is S7's way of registering methods, rather than using NAMESPACE export directives.

## S3 Wrapper Pattern

**Required for base R dispatch** - without these, `print(obj)` won't find S7 methods.

File: `R/s3_wrappers.R`

```r
#' @export
print.rctbp_classname <- function(x, ...) {
  S7::method(print, rctbp_classname)(x, ...)
}

#' @export
summary.rctbp_classname <- function(object, ...) {
  S7::method(summary, rctbp_classname)(object, ...)
}

#' @export
plot.rctbp_classname <- function(x, ...) {
  S7::method(plot, rctbp_classname)(x, ...)
}
```

## Inheritance

### Define Child Class

```r
rctbp_child <- S7::new_class(
  "rctbp_child",
  parent = rctbp_parent,
  properties = list(
    extra_prop = S7::class_character
  )
)
```

### Call Parent Method with super()

```r
S7::method(process, rctbp_child) <- function(x) {
  # Call parent implementation
  parent_result <- S7::super(x, to = rctbp_parent)
  # Additional processing
  parent_result
}
```

## Custom Constructor

Override default construction:

```r
rctbp_range <- S7::new_class(
  "rctbp_range",
  properties = list(
    start = S7::class_numeric,
    end = S7::class_numeric
  ),
  constructor = function(x) {
    S7::new_object(S7::S7_object(),
      start = min(x, na.rm = TRUE),
      end = max(x, na.rm = TRUE)
    )
  }
)

# Usage: rctbp_range(c(5, 2, 8)) creates start=2, end=8
```

**Key requirement**: Constructor must end with `new_object()` call.

## Exporting Classes

If users should instantiate your class:

1. Export the constructor function
2. Document it (roxygen for arguments = properties)
3. Set `package` argument to disambiguate:

```r
rctbp_design <- S7::new_class(
  "rctbp_design",
  package = "rctbayespower",
  properties = list(...)
)
```

## R < 4.3.0 Compatibility

The `@` operator only works for S4 in older R. Options:
- Use `S7::prop(obj, "property")` instead of `obj@property`
- S7 auto-attaches `@` support when loaded
- Add to NAMESPACE: `if (getRversion() < '4.3.0') importFrom('S7', '@')`

## S3/S4 Compatibility

S7 builds on S3, so S7 objects are S3 objects with extra attributes.

### Register S7 Method for S3 Generic

```r
# S3 generic from base R
S7::method(print, rctbp_classname) <- function(x, ...) {
  # works automatically
}
```

### Register S7 Method for S4 Generic

```r
# S4 generic from another package
S7::method(show, rctbp_classname) <- function(object) {
  # S7 handles registration
}
```

### Extend S3 Class with S7

```r
# S7 class with S3 parent
rctbp_enhanced_lm <- S7::new_class(

  "rctbp_enhanced_lm",
  parent = S7::class_list,  # lm objects are lists
  properties = list(
    extra_info = S7::class_character
  )
)
```

### Convert S3 Class to S7

Two approaches for list-based S3 classes:

**Option 1: Preserve structure** (use `class_list` as parent)
```r
rctbp_rle <- S7::new_class(
  "rctbp_rle",
  parent = S7::class_list,
  constructor = function(x) {
    rle_obj <- rle(x)
    S7::new_object(rle_obj)
  }
)
```

**Option 2: Natural properties** (define properties directly)
```r
rctbp_rle <- S7::new_class(
  "rctbp_rle",
  properties = list(
    lengths = S7::class_integer,
    values = S7::class_any
  )
)
```

## Performance Notes

S7 dispatch has ~2-3x overhead vs S3/S4 (package vs primitive implementation).

- Single dispatch: ~2-3 microseconds
- Double dispatch: ~4-5 microseconds
- Even with 100-deep hierarchies: <20 microseconds

Negligible for most use cases. Only optimize if profiling shows dispatch as bottleneck.

## Official Documentation

- Package usage: https://cran.r-project.org/web/packages/S7/vignettes/packages.html
- S3/S4 compatibility: https://cran.r-project.org/web/packages/S7/vignettes/compatibility.html
- Performance: https://cran.r-project.org/web/packages/S7/vignettes/performance.html
