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

  message("  Start variable matching...            (time elapsed: ",
          format(proc.time()["elapsed"] - ptm, width = 6, nsmall = 2, digits = 2), ")")

  # Tokenize all code lines at once and build an inverted (token -> module) index.
  # This is O(code size) rather than O(symbols x code size), replacing the grepl sweep.
  nonIdentifierSetItems <- grep("[+-]", unique(unlist(x$sets, use.names = FALSE)), value = TRUE)
  allTokenLists <- .tokenizeGams(code, objectNames, nonIdentifierSetItems)
  lineLengths   <- lengths(allTokenLists)
  tokenVec      <- unlist(allTokenLists, use.names = FALSE)
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

  # Find symbols that appear with more than one capitalisation variant in the code. A lowercase
  # form has mixed casing exactly when more than one distinct token shares it.
  uniqueTokens <- tokenVec[!duplicated(tokenVec)]
  lowerUnique  <- tolower(uniqueTokens)
  multiCaseSet <- lowerUnique[duplicated(lowerUnique)]
  duplicates   <- tolower(objectNames) %in% multiCaseSet
  names(duplicates) <- objectNames

  if (length(objectNames[setdiff(objectNames[duplicates], capitalExclusionList)] > 0)) {
    duplicateNames <- unname(setdiff(objectNames[duplicates], capitalExclusionList))

    msg <- paste0(
      "Found variables with more than one capitalization in the codebase: ",
      paste0(duplicateNames, collapse = ", "), "\n"
    )

    lineOfToken <- rep(seq_along(code), lineLengths)
    for (dup in duplicateNames) {
      msg <- paste0(msg, "- Suspicious lines found for item '", dup, "':\n")

      # lines holding the item in a casing other than the declared one
      miscased     <- tokenVec %in% setdiff(uniqueTokens[lowerUnique == tolower(dup)], dup)
      suspectLines <- code[unique(lineOfToken[miscased])]

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

# Tokenizes each line like strsplit(lines, "[^[:alnum:]_]+"), but keeps a run like
# "SMIP7-H-SSP2-sens" together and drops it, as it is one set item (a GAMS label) and not code,
# whose pieces (here "H") would otherwise count as symbols. nonIdentifierSetItems are the parsed
# set items that no identifier could be, i.e. the ones containing "+" or "-"; passing the remaining
# items would be harmless but pointless, as a run of one piece is never dropped. A run of
# identifiers joined by "+"/"-" (no spaces) is dropped if it equals one of those items, or, for set
# items readSetItems could not parse, if its first piece does not start with a digit and is no
# symbol (case-insensitive) and no piece is exactly a symbol. So "1-vm_x", "vm_a-vm_b" and
# "vm_x(t-1)" are still split. Two gaps remain for a set item that is not among
# nonIdentifierSetItems: one starting with a number or a symbol is still split, and a mis-cased
# symbol inside it stays hidden from the capitalization check (indistinguishable from the piece "H"
# above). Unlike plain strsplit the signs around a run leave no empty tokens behind.
.tokenizeGams <- function(lines, symbols, nonIdentifierSetItems = NULL) {
  tokens <- strsplit(lines, "[^[:alnum:]_]+", perl = TRUE)
  # the cheap grep first, the backtracking one only on the few lines that can match at all
  maybe      <- grep("[+-]", lines, perl = TRUE)
  candidates <- maybe[grepl("[[:alnum:]_][+-]+[[:alnum:]_]", lines[maybe], perl = TRUE)]
  if (length(candidates) == 0) return(tokens)

  coarse     <- strsplit(lines[candidates], "[^[:alnum:]_+-]+", perl = TRUE)
  runs       <- gsub("^[+-]+|[+-]+$", "", unlist(coarse, use.names = FALSE), perl = TRUE)
  runLine    <- rep(seq_along(candidates), lengths(coarse))
  pieces     <- strsplit(runs, "[+-]+", perl = TRUE)
  pieceVec   <- unlist(pieces, use.names = FALSE)
  pieceRun   <- rep(seq_along(runs), lengths(pieces))
  # match(), as a run stripped to "" contributes no piece at all
  firstPiece <- pieceVec[match(seq_along(runs), pieceRun)]

  hasSymbol <- tabulate(pieceRun[pieceVec %in% symbols], nbins = length(runs)) > 0
  isKnown   <- tolower(runs) %in% tolower(nonIdentifierSetItems)
  isGuessed <- !grepl("^[0-9]", firstPiece) & !(tolower(firstPiece) %in% tolower(symbols)) & !hasSymbol
  isSetItem <- lengths(pieces) > 1 & (isKnown | isGuessed)

  keep <- !isSetItem[pieceRun]
  tokens[candidates] <- unname(split(pieceVec[keep], factor(runLine[pieceRun[keep]],
                                                            levels = seq_along(candidates))))
  tokens
}
