makeMinimalX <- function(code, varNames, varTypes = NULL) {
  if (is.null(varTypes)) varTypes <- rep("variable", length(varNames))
  declarations <- matrix(
    c(varNames, rep("", length(varNames)), rep("", length(varNames)), varTypes),
    nrow = length(varNames), ncol = 4,
    dimnames = list(rep("core", length(varNames)), c("names", "sets", "description", "type"))
  )
  list(code = code, declarations = declarations, not_used = NULL)
}

test_that("checkAppearance does not match variable names inside double-quoted strings", {
  code <- c(
    "pm_corevar = 1;",
    "display \"vm_modulevar is used in module\";",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(result$appearance["vm_modulevar", "core"],
               label = "vm_modulevar must not appear in core (only inside double-quoted string)")
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
  expect_true(result$appearance["pm_corevar", "core"])
  expect_true(result$appearance["pm_corevar", "fancymodule"])
})

test_that("checkAppearance does not match variable names inside single-quoted strings", {
  code <- c(
    "pm_corevar = 1;",
    "pm_corevar.l = 'vm_modulevar is here';",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(result$appearance["vm_modulevar", "core"],
               label = "vm_modulevar must not appear in core (only inside single-quoted string)")
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
})

hasCapWarning <- function(result, varName) {
  any(grepl(paste0("capitalization.*", varName, "|", varName, ".*capitalization"), names(result$warnings)))
}

test_that("checkAppearance warns when a symbol appears with inconsistent capitalisation", {
  code <- c(
    "vm_Example = 1;",
    "vm_example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressWarnings(suppressMessages(checkAppearance(x)))

  expect_true(hasCapWarning(result, "vm_Example"),
              label = "capitalization warning expected for vm_Example")
})

test_that("checkAppearance does not warn when capitalisation is consistent", {
  code <- c(
    "vm_Example = 1;",
    "vm_Example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressMessages(checkAppearance(x))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "no capitalization warning expected when casing is consistent")
})

test_that("checkAppearance does not warn for symbols in capitalExclusionList", {
  code <- c(
    "vm_Example = 1;",
    "vm_example = 2;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressMessages(checkAppearance(x, capitalExclusionList = "vm_Example"))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "excluded symbol must not produce a capitalization warning")
})

test_that("checkAppearance does not warn when mixed casing is only inside string literals", {
  code <- c(
    "vm_Example = 1;",
    "pm_corevar = 'vm_example is just a label';"
  )
  names(code) <- c("core", "core")

  x <- makeMinimalX(code, c("vm_Example", "pm_corevar"),
                    c("variable", "parameter"))

  result <- suppressMessages(checkAppearance(x))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "casing difference inside a string literal must not trigger a warning")
})

test_that("checkAppearance ignores casing differences inside display statements", {
  # display statements are stripped before tokenization, so a casing difference that
  # only occurs inside a display statement must not be flagged.
  code <- c(
    "vm_Example = 1;",
    "display vm_example;"
  )
  names(code) <- c("core", "core")

  x <- makeMinimalX(code, "vm_Example", "variable")

  result <- suppressMessages(checkAppearance(x))

  expect_false(hasCapWarning(result, "vm_Example"),
               label = "casing difference inside a display statement must not trigger a warning")
})

test_that("checkAppearance detects a symbol located between two string literals on one line", {
  # Regression test: a greedy string-stripping regex (".*") would match from the first
  # quote to the last quote on the line and delete the real token (fm_croparea) sitting
  # between the two "y1995" string literals. String literals must be stripped individually.
  code <- c(
    "pm_corevar = 1;",
    "  pm_corevar = f59_topsoilc_density(\"y1995\",j) * fm_croparea(\"y1995\",j,w,kcr);"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "fm_croparea", "f59_topsoilc_density"),
                    c("parameter", "parameter", "parameter"))

  result <- suppressMessages(checkAppearance(x))

  expect_true(result$appearance["fm_croparea", "fancymodule"],
              label = "fm_croparea sits between two string literals and must still be detected")
  expect_true(result$appearance["f59_topsoilc_density", "fancymodule"])
})

test_that("checkAppearance still detects actual variable usage outside strings", {
  code <- c(
    "pm_corevar = 1;",
    "vm_modulevar = pm_corevar;"
  )
  names(code) <- c("core", "fancymodule")

  x <- makeMinimalX(code, c("pm_corevar", "vm_modulevar"),
                    c("parameter", "variable"))

  result <- suppressMessages(checkAppearance(x))

  expect_true(result$appearance["pm_corevar", "core"])
  expect_true(result$appearance["pm_corevar", "fancymodule"])
  expect_false(result$appearance["vm_modulevar", "core"])
  expect_true(result$appearance["vm_modulevar", "fancymodule"])
})
