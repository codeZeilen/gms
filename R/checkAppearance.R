#' checkAppearance
#'
#' Checks for all declared objects in which parts of the model they appear and
#' calculates the type of each object (core object, interface object, module
#' object of module xy,...)
#'
#'
#' @param x A code list as returned by \code{\link{codeExtract}}
#' @param capitalExclusionList A vector of names that should be ignored when
#' checking for unified capitalization of variables
#' @return A list with four elements: appearance, setappearance, type and
#' warnings. Appearance is a matrix containing values which indicate whether an
#' object appears in a part of the code or not (e.g. indicates whether "vm_example"
#' appears in realization "on" of module "test" or not.). 0 means that it does not appear,
#' 1 means that it appears in the code and 2 means that it appears in the
#' not_used.txt. setappearance contains the same information but for sets instead of other
#' objects. Type is a vector containing the type of each object (exluding sets). And warnings
#' contains a list of warnings created during that process.
#' @author Jan Philipp Dietrich

#' @export
#' @seealso \code{\link{codeCheck}},\code{\link{readDeclarations}}
checkAppearance <- function(x, capitalExclusionList = NULL) {
  w <- NULL
  ptm <- proc.time()["elapsed"]
  message("  Running checkAppearance...")
  moduleNames <- unique(names(x$code))
  objectNames <- unique(x$declarations[, "names"])
  if (!is.null(x$not_used)) objectNames <- unique(c(objectNames, x$not_used[, "name"]))

  # check for variables with different capitalization in declarations
  if (length(objectNames[duplicated(tolower(objectNames))]) > 0) {
    w <- .warning(paste0(
      "Found variables with more than one capitalization in declarations and not_used.txt files: ",
      paste0(objectNames[duplicated(tolower(objectNames))], collapse = ", ")
    ), w = w)
  }

  # remove right-hand sides in execute_load statements as there are non-module-related
  # object names allowed (here one refers to the names in the gdx file, but these
  # could come from other modules)
  tmp <- grep("execute_load", x$code, ignore.case = TRUE)
  x$code[tmp] <- gsub("=[^,]*", "", x$code[tmp])

  # add empty entry in tmp for module realization which do not contain any code but have a not_used.txt
  modulesWithNotUsedFile <- unique(dimnames(x$not_used)[[1]])
  missing <- modulesWithNotUsedFile[!(modulesWithNotUsedFile %in% moduleNames)]
  if (length(missing) > 0) {
    moduleNames <- c(moduleNames, missing)
  }

  # Strip string literals so that variable names inside strings are not matched.
  # Both double-quoted and single-quoted GAMS strings are removed. The patterns use a
  # negated character class (not a greedy ".*").
  code <- x$code
  code <- gsub("\"[^\"]*\"", "", code)
  code <- gsub("'[^']*'", "", code)
  code <- gsub("display.*", "", code)

  message("  Start variable matching...            (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  # Tokenize all code lines at once and build an inverted (token -> module) index.
  # This is O(code size) rather than O(symbols x code size), replacing the grepl sweep.
  allTokenLists <- strsplit(code, "[^[:alnum:]_]+", perl = TRUE)
  lineLengths   <- lengths(allTokenLists)
  tokenVec      <- unlist(allTokenLists, use.names = FALSE)
  lowerTokenVec <- tolower(tokenVec)
  moduleVec     <- rep(names(code), lineLengths)

  # keep only non-empty tokens that are declared symbols
  isSymbol      <- nzchar(tokenVec) & (tokenVec %in% objectNames)
  symbolTokens  <- tokenVec[isSymbol]
  symbolModules <- moduleVec[isSymbol]

  objectsToModules <- matrix(FALSE, nrow = length(objectNames), ncol = length(moduleNames),
                             dimnames = list(objectNames, moduleNames))
  if (length(symbolTokens) > 0) {
    objectsToModules[cbind(match(symbolTokens, objectNames),
                           match(symbolModules, moduleNames))] <- TRUE
  }

  message("  Finished variable matching...         (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  message("  Start var capitalization check...     (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  # Find symbols that appear with more than one capitalisation variant in the code.
  # Uses the prebuilt token vector (O(tokens) build + O(1) per symbol lookup).
  # tapply groups tokenVec by lowerTokenVec (its grouping key, same length) and counts the
  # distinct casings within each group; any group with more than one casing is a duplicate.
  casingCounts <- tapply(tokenVec, lowerTokenVec, function(v) length(unique(v)))
  multiCaseSet <- names(casingCounts)[casingCounts > 1L]
  duplicates   <- tolower(objectNames) %in% multiCaseSet
  names(duplicates) <- objectNames

  if (length(objectNames[setdiff(objectNames[duplicates], capitalExclusionList)] > 0)) {

    duplicateNames <- unname(setdiff(objectNames[duplicates], capitalExclusionList))

    msg <- paste0(
      "Found variables with more than one capitalization in the codebase: ",
      paste0(duplicateNames, collapse = ", "), "\n"
    )

    for (dup in duplicateNames) {
      msg <- paste0(msg, "- Suspicious lines found for item '", dup, "':\n")
      dupRegex <- paste("(^|[^[:alnum:]_])", escapeRegex(dup), "($|[^[:alnum:]_])", sep = "")
      chunks <- code[grepl(dupRegex, code, ignore.case = TRUE, perl = TRUE)]

      tokens <- strsplit(chunks, "[^[:alnum:]_]+", perl = TRUE)
      correctTokenCounts <- vapply(tokens, function(line) sum(dup == line), integer(1))
      allTokenCounts <- vapply(tokens, function(line) sum(dup == tolower(line)), integer(1))
      suspectLines <- chunks[correctTokenCounts != allTokenCounts]

      msg <- paste0(msg, paste0(paste(" - ", suspectLines), collapse = "\n"))
      msg <- paste0(msg, "\n")
    }
    w <- .warning(msg, w = w)

  }

  message("  Finished var capitalization check...  (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  if (!is.null(x$not_used)) {
    for (i in seq_len(dim(x$not_used)[1])) {
      if (objectsToModules[x$not_used[i, "name"], dimnames(x$not_used)[[1]][i]]) {
        w <- .warning(x$not_used[i, "name"], " appears in not_used.txt of module ", dimnames(x$not_used)[[1]][i],
                      " but is used in the GAMS code of it!", w = w)
      }
      objectsToModules[x$not_used[i, "name"], dimnames(x$not_used)[[1]][i]] <- 2
    }
  }

  sets <- x$declarations[x$declarations[, "type"] == "set", "names"]
  aSets <- objectsToModules[sets, , drop = FALSE]
  objectsToModules <- objectsToModules[!(rownames(objectsToModules) %in% sets), , drop = FALSE]
  type <- sub("^(o|)[^_]*?(m|[0-9]{2}|)_.*$", "\\1\\2", dimnames(objectsToModules)[[1]])
  names(type) <- dimnames(objectsToModules)[[1]]
  return(list(appearance = objectsToModules, setappearance = aSets, type = type, warnings = w))
}
