# S7 Coding Guide

A comprehensive guide to using *S7*, a modern OOP system in R, for package development and migrating from **S3**.

---

## 1. 🚀 Introduction

* **S7** blends the simplicity of S3 with the structure of S4, offering formal classes, generics, validation, inheritance, and multiple dispatch.
* It remains fully compatible with S3/S4 and is being considered for inclusion in base R.

---

## 2. Defining S7 Classes

```r
library(S7)

MyClass <- new_class(
  name       = "MyClass",
  parent     = S7_object,
  properties = list(
    foo = class_character,
    bar = class_double
  ),
  validator  = function(self) {
    if (nchar(self@foo) == 0) "@foo must be non-empty"
  }
)
```

* `new_class()` creates both a class and constructor.
* Properties are type-checked and validated automatically.

---

## 3. Properties

* Access via `@`, inspect via `prop_names(class)`
* Use `new_property()` for lazy defaults, computed, frozen, or derived values.
* Setters can run at initialization (since v0.2.0).

---

## 4. Generics & Methods

```r
greet <- new_generic("greet", dispatch_args = "x")
method(greet, MyClass) <- function(x, ...) {
  paste0("Hello, ", x@foo)
}
```

* `new_generic()` + `method()` define dispatch functions.
* Supports single and multiple dispatch.
* Introspect with `method_explain()`.

---

## 5. Inheritance & Dispatch

* Supports single/multiple inheritance via `parent = list(A, B)`
* Instance methods auto-fallback along inheritance chain.

---

## 6. Converting & Super Calls

* `convert(from, to)` enables up/down casting between classes.
* Use `super(generic, self, ...)` to invoke parent methods.

---

## 7. Package Integration

Place in **`R/zzz.R`**:

```r
.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
```

* Registers S7 methods on load.
* Document and export constructors, generics, and properties.

---

## 8. Migrating S3 → S7

1. Convert S3 `list/class` to `new_class()` with properties.
2. Shift `UseMethod()` to `new_generic()` + `method()`.
3. Drop S3 scaffolding; optionally re-export S3-class compatibility via `new_S3_class()`.
4. Retain `class()` attribute for compatibility.

---

## 9. Utilities & Introspection

```r
cls <- S7_class(obj)
names(cls@parent)
prop_names(cls)
method_explain(generic, object = obj)

get_all_parents <- function(cls) {
  ps <- cls@parent
  if (!length(ps)) return(character())
  c(names(ps), unlist(lapply(ps, get_all_parents)))
}
```

---

## 10. Example: S3 → S7 Migration

```r
# S3 style
Point <- function(x,y) structure(list(x = x, y = y), class = "Point")
dist.Point <- function(a,b) sqrt((a$x-b$x)^2 + (a$y-b$y)^2)

# S7 style
Point <- new_class("Point",
  properties = list(x = class_double, y = class_double)
)
dist <- new_generic("dist", c("a","b"))
method(dist, list(Point, Point)) <- function(a,b) {
  sqrt((a@x - b@x)^2 + (a@y - b@y)^2)
}
```

---

## 11. ⏹ Summary

S7 provides:

* Formal, validated, inheritable classes (with multiple dispatch)
* Structured generics with clear syntax
* S3/S4 conversion tools
* Seamless package integration

---

## 📚 Further Reading

* Vignettes: `vignette("classes-objects")`, `vignette("generics-methods")`, `vignette("compatibility")`, `vignette("packages")`
* Reference docs: `new_class`, `new_generic`, `new_property`, `convert`, `methods_register`
* Release notes (v0.2.0 enhancements): [https://www.tidyverse.org/blog/2024/11/s7-0-2-0/](https://www.tidyverse.org/blog/2024/11/s7-0-2-0/)
* Official website: [https://rconsortium.github.io/S7/](https://rconsortium.github.io/S7/)
