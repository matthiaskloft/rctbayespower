---
name: s7-classes
description: Write S7 class definitions and methods for rctbayespower. Use when creating new S7 classes, adding properties, defining validators, registering methods, or setting up S3 compatibility.
---

# S7 Classes in rctbayespower

## Class Definition Template

```r
#' @importFrom S7 new_class class_character class_numeric new_property new_union
rctbp_classname <- S7::new_class(
  "rctbp_classname",
  properties = list(
    # Simple typed property
    name = S7::class_character,

    # Property with default
    count = S7::new_property(S7::class_numeric, default = 0),

    # Nullable property (union with NULL)
    optional = S7::class_character | NULL,

    # Property with setter validation (rctbayespower style)
    backend = S7::new_property(
      class = S7::class_character,
      default = "brms",
      setter = function(self, value) {
        if (!value %in% c("brms", "bf")) {
          cli::cli_abort(c(
            "{.arg backend} must be 'brms' or 'bf'",
            "x" = "You supplied {.val {value}}"
          ))
        }
        self@backend <- value
        self
      }
    )
  ),

  validator = function(self) {
    if (self@count < 0) return("'count' must be non-negative.")
    NULL
  }
)
```

## Property Types

| Type | Usage |
|------|-------|
| `S7::class_character` | Character vector |
| `S7::class_numeric` | Numeric (double or integer) |
| `S7::class_double` | Double only |
| `S7::class_integer` | Integer only |
| `S7::class_logical` | Logical |
| `S7::class_list` | List |
| `S7::class_function` | Function |
| `S7::class_any` | Any type (use sparingly) |
| `Type \| NULL` | Nullable (union) |

## Naming Convention

All S7 classes use `rctbp_*` prefix: `rctbp_design`, `rctbp_conditions`, `rctbp_power_analysis`

## References

For detailed patterns, see:
- [Computed properties, validation, methods](references/s7-patterns.md)
- [Package setup, S3 wrappers, inheritance](references/package-setup.md)
- [S7 Reference Manual (complete API)](S7_0-2-1_manual.html)
