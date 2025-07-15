| | `base::ifelse()` | `ifelse::ifelse1()` | `dplyr::if_else()` | `hutils::if_else()` |	`data.table::fifelse()` | `kit::iif(tprom = TRUE)` |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| `is.na(test)` replacement | not supported | supported | supported | supported | supported | supported |
| constraint on `typeof(test)` | none | none | only `"logical"` | only `"logical"` | only `"logical"` | only `"logical"` |
| constraint on `class(test)` | coercible to `"logical"` | coercible to `"logical"` | none | none | none | none |
| constraint on `length(test)` | none | none | none | none | none | none |
| constraint on `dim(test)` | none | none | none | none | none | none |
| constraint on `typeof(yes\|no)` | none | none | vector type excluding `"expression"` | vector type excluding `"complex"`, `"list"`, `"expression"`; identical | vector type excluding `"raw"`, `"expression"`; identical or mixture of `"integer"`, `"double"` | vector type excluding `"raw"`, `"expression"` |
| constraint on `class(yes\|no)` | has methods for `[`, `[<-`, `length`, `rep` (or internal default methods work) | has methods for `[`, `[<-`, `c`, `length`, `rep` (or internal default methods work) | `vctrs::vec_ptype_common` | none | identical `oldClass(.)` | identical `oldClass(.)` (but not enforced, wrongly) |
| constraint on `length(yes\|no)` | none | none | `1` or `if (is.array(.)) k*NROW(test) else length(test)` | `1` or `length(test)` | `1` or `length(test)` | `1` or `length(test)` |
| constraint on `dim(yes\|no)` | none | none | `NROW(.) == NROW(test)` and more due to `vctrs::vec_size_common` | none | none | none |
| `typeof(value)` | type after subassignment from `yes`, `no` to `test` | type of `c(yes, no)` | `vctrs::vec_ptype_common` | common type of `yes`, `no` | common type of `yes`, `no` | higher of types of `yes`, `no` |
| `class(value)` | class after subassignment from `yes`, `no` to `test` | class of `c(yes, no)` | `vctrs::vec_ptype_common` | class after subassignment from `yes` to `no` if `length(no) == length(test)` | common class of `yes`, `no` | common class of `yes`, `no` |
| `length(value)` | `length(test)` | `length(test)` | length of longest argument | `length(test)` | `length(test)` | `length(test)` |
| `names(value)` | `if (is.atomic(test)) names(test) else NULL` | `names(test)` | splicing of `names(yes)`, `names(no)` | `if (length(no) == length(test)) names(no) else NULL` | `names(test)` | `names(test)` |
| `dim(value)` | `if (is.atomic(test)) dim(test) else NULL` | `dim(test)` | dim of longest argument | `if (length(no) == length(test)) dim(no) else NULL` | `NULL` | `NULL` |
| `dimnames(value)` | `if (is.atomic(test)) dimnames(test) else NULL` | `dimnames(test)` | dimnames of longest argument | `if (length(no) == length(test)) dimnames(no) else NULL` | `NULL` | `NULL` |
| other attributes | from `test` if atomic | from `c(yes, no)` | none | from `no` if `length(no) == length(test)` | from first of `yes`, `no` not identical to `NA` | from `yes` or `no` or both depending on coercions |
| support for factors `yes`, `no` | none; uses integer representation | full; uses `levels(c(yes, no))` | full; uses `levels(c(yes, no))` | partial; requires `identical(levels(yes), levels(no))` | partial; requires `identical(levels(yes), levels(no))` | partial; requires `identical(levels(yes), levels(no))` |
| support for non-basic S4 classes | full with suitable implementation | full with suitable implementation | none | none | none | none |
