#'
#' @importFrom checkmate assert assertString checkNumeric checkClass assertFunction assertSubset assertList checkCount
#' @importFrom BBmisc stopf makeS3Obj isScalarNumeric
#'
makeParam = function(id, type, learner.param, len = 1L, lower = NULL, upper = NULL, values = NULL, cnames = NULL, allow.inf = FALSE, default,
                     trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list(), when) {

  assertString(id)
  assert(
    checkCount(len, na.ok = learner.param),
    checkClass(len, "expression")
  )
  if (isNumericTypeString(type, include.int = TRUE)) {
    assert(
      checkNumeric(lower, any.missing = FALSE),
      checkClass(lower, "expression")
    )
    assert(
      checkNumeric(upper, any.missing = FALSE),
      checkClass(upper, "expression")
    )
    # the following check also ensures that if len=NA, the lower and upper must be scalars
    if (!is.expression(len) && !is.expression(lower)) {
      if (length(lower) %nin% c(1L, len)) {
        stopf("For param '%s' length 'lower' must be either 1 or length of param, not: %i", id, length(lower))
      }
    }
    if (!is.expression(len) && !is.expression(upper)) {
      if (length(upper) %nin% c(1L, len)) {
        stopf("For param '%s' length 'upper' must be either 1 or length of param, not: %i", id, length(upper))
      }
    }
  }
  if (isDiscreteTypeString(type)) {
    # values = checkValuesForDiscreteParam(id, values)
    values = NULL
  }
  if (missing(default)) {
    has.default = FALSE
    default = NULL
  } else {
    has.default = TRUE
  }
  if (!is.null(trafo)) {
    assertFunction(trafo)
  }
  if (!is.null(requires)) {
    requires = convertExpressionToCall(requires)
    assertSubset(mode(requires), c("call", "name"))
  }
  assertList(special.vals)

  if (isNumericTypeString(type, include.int = TRUE)) {
    if (!is.expression(len) && !is.na(len) && len > 1L) {
      if (isScalarNumeric(lower)) {
        lower = rep(lower, len)
      }
      if (isScalarNumeric(upper)) {
        upper = rep(upper, len)
      }
    }
    if (!is.expression(lower) && !is.expression(upper)) {
      if (any(upper < lower)) {
        stopf("For param '%s' some component of 'upper' is smaller than the corresponding one in 'lower'", id)
      }
    }
  }
  p = makeS3Obj("Param",
                id = id,
                type = type,
                len = len,
                lower = lower,
                upper = upper,
                values = values,
                cnames = cnames,
                allow.inf = allow.inf,
                has.default = has.default,
                default = default,
                trafo = trafo,
                requires = requires,
                tunable = tunable,
                special.vals = special.vals
  )
  if (learner.param) {
    p = makeLearnerParam(p, when)
  }
  if (has.default && !is.expression(default)) {
    if (!isFeasible(p, default)) {
      stop(p$id, " : 'default' must be a feasible parameter setting.")
    }
  }
  return(p)
}

convertExpressionToCall = function(req) {
  if (is.expression(req)) {
    if (length(req) == 1) {
      return(req[[1]])
    } else {
      return(substitute(eval(x), list(x = req)))
    }
  }
  req
}

#'
#' @importFrom checkmate assertChoice
#'
makeLearnerParam = function(p, when) {
  assertChoice(when, c("train", "predict", "both"))
  p$when = when
  class(p) = c("LearnerParam", "Param")
  return(p)
}

#'
#' @importFrom checkmate assertList
#' @importFrom BBmisc makeS3Obj extractSubList vlapply addClasses
#'
makeParamSet = function(..., params = NULL, forbidden = NULL, keys = NULL) {

  pars = list(...)
  if (length(pars) > 0 && !is.null(params)) {
    stop("You can only use one of ... or params!")
  }
  if (!is.null(params)) {
    assertList(params, types = "Param")
    pars = params
  } else {
    assertList(pars, types = "Param")
  }
  ns = extractSubList(pars, "id")
  if (anyDuplicated(ns)) {
    stop("All parameters must have unique names!")
  }
  names(pars) = ns
  par.set = makeS3Obj("ParamSet", pars = pars, forbidden = forbidden)

  if (length(pars) > 0L) {
    if (all(vlapply(pars, inherits, what = "LearnerParam"))) {
      par.set = addClasses(par.set, classes = "LearnerParamSet")
      keys = union(keys, c("task", "n", "p", "k", "type"))
    }
    if (!is.null(keys) && (hasExpression(par.set))) {
      checkExpressionFeasibility(par.set = par.set, keys = keys)
    }
  }
  return(par.set)
}

isNumericTypeString <- function(type, include.int = TRUE) {
  numeric_types <- c("numeric", "double")
  if (include.int) {
    numeric_types <- c(numeric_types, "integer")
  }
  return(type %in% numeric_types)
}

isDiscreteTypeString <- function(type, include.logical = TRUE) {
  discrete_types <- c("integer", "factor", "character", "logical")
  if (!include.logical) {
    discrete_types <- discrete_types[discrete_types != "logical"]
  }
  return(type %in% discrete_types)
}

#' @importFrom BBmisc vlapply
isSpecialValue = function(par, x) {
  any(vlapply(par$special.vals, function(special.val) isTRUE(all.equal(x, special.val))))
}

isFeasible = function(par, x, use.defaults = FALSE, filter = FALSE) {
  return(TRUE)
}

hasExpression = function(obj) {
  return(TRUE)
}

checkLength = function(par, x) {
  (is.expression(par$len) || is.na(par$len) || length(x) == par$len)
}

checkExpressionFeasibility = function(par.set, keys) {
  return(TRUE)
}

#'
#' @importFrom checkmate assertString asInt assertNumeric assertFlag
makeNumericLearnerParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE, default,
                                   when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numeric", learner.param = TRUE, lower = lower,
            upper = upper, allow.inf = allow.inf,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeNumericVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
                                         upper = Inf, allow.inf = FALSE, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numericvector", learner.param = TRUE, len = len, lower = lower,
            upper = upper, allow.inf = allow.inf,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeIntegerLearnerParam = function(id, lower = -Inf, upper = Inf,
                                   default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integer", learner.param = TRUE, lower = lower,
            upper = upper,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}
makeIntegerVectorLearnerParam = function(id, len = as.integer(NA), lower = -Inf,
                                         upper = Inf, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integervector", learner.param = TRUE, len = len, lower = lower,
            upper = upper,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeDiscreteLearnerParam = function(id, values, default,
                                    when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discrete", learner.param = TRUE, values = values,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeDiscreteVectorLearnerParam = function(id, len = as.integer(NA), values, default,
                                          when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discretevector", learner.param = TRUE, len = len, values = values,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeLogicalLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logical", learner.param = TRUE, values = values,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeLogicalVectorLearnerParam = function(id, len = as.integer(NA), default, when = "train",
                                         requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logicalvector", learner.param = TRUE, len = len, values = values,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeUntypedLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "untyped", learner.param = TRUE,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeFunctionLearnerParam = function(id, default, when = "train", requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "function", learner.param = TRUE,
            default = default, requires = requires, tunable = tunable, special.vals = special.vals, when = when)
}

makeNumericParam = function(id, lower = -Inf, upper = Inf, allow.inf = FALSE,
                            default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numeric", learner.param = FALSE, lower = lower,
            upper = upper, allow.inf = allow.inf,
            default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeNumericVectorParam = function(id, len, lower = -Inf, upper = Inf, cnames = NULL,
                                  allow.inf = FALSE, default, trafo = NULL, requires = NULL,
                                  tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "numericvector", learner.param = FALSE, len = len, lower = lower,
            upper = upper, cnames = cnames, allow.inf = allow.inf,
            default = default, trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeIntegerParam = function(id, lower = -Inf, upper = Inf, default, trafo = NULL,
                            requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integer", learner.param = FALSE, lower = lower,
            upper = upper, default = default,
            trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeIntegerVectorParam = function(id, len, lower = -Inf, upper = Inf, cnames = NULL,
                                  default, trafo = NULL, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "integervector", learner.param = FALSE, len = len, lower = lower, upper = upper,
            cnames = cnames, default = default, trafo = trafo,
            requires = requires, tunable = tunable, special.vals = special.vals)
}

makeLogicalParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logical", learner.param = FALSE,
            values = values, default = default,
            trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeLogicalVectorParam = function(id, len, cnames = NULL, default,
                                  requires = NULL, tunable = TRUE, special.vals = list()) {
  values = list("TRUE" = TRUE, "FALSE" = FALSE)
  makeParam(id = id, type = "logicalvector", learner.param = FALSE, len = len,
            values = values, cnames = cnames, default = default,
            trafo = NULL, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeDiscreteParam = function(id, values, trafo = NULL, default,
                             requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discrete", learner.param = FALSE,
            values = values, default = default,
            trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeDiscreteVectorParam = function(id, len, values, trafo = NULL, default, requires = NULL,
                                   tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "discretevector", learner.param = FALSE, len = len,
            values = values, default = default,
            trafo = trafo, requires = requires, tunable = tunable, special.vals = special.vals)
}

makeFunctionParam = function(id, default = default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "function", learner.param = FALSE,
            values = NULL, default = default, trafo = NULL,
            requires = requires, tunable = FALSE, special.vals = special.vals)
}

makeUntypedParam = function(id, default, requires = NULL, tunable = TRUE, special.vals = list()) {
  makeParam(id = id, type = "untyped", learner.param = FALSE,
            values = NULL, default = default, trafo = NULL,
            requires = requires, tunable = TRUE, special.vals = special.vals)
}

makeCharacterParam = function(id, default, requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "character", learner.param = FALSE,
            default = default, trafo = NULL,
            requires = requires, tunable = FALSE, special.vals = special.vals)
}

makeCharacterVectorParam = function(id, len, cnames = NULL, default,
                                    requires = NULL, special.vals = list()) {
  makeParam(id = id, type = "charactervector", learner.param = FALSE, len = len,
            cnames = cnames, default = default,
            trafo = NULL, requires = requires, tunable = FALSE, special.vals = special.vals)
}
