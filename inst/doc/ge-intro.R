## ----echo=FALSE---------------------------------------------------------------
#options(width=70)
#options(prompt="R> ")
options(warn = (-1))

## ----echo=FALSE---------------------------------------------------------------
set.seed(0)

## ----tidy=FALSE---------------------------------------------------------------
library("gramEvol")

ruleDef <- list(expr  = grule(op(expr, expr), func(expr), var),
                func  = grule(sin, cos, log, sqrt),
                op    = grule(`+`, `-`, `*`),
                var   = grule(distance, distance^n, n),
                n     = grule(1, 2, 3, 4))

grammarDef <- CreateGrammar(ruleDef)

## -----------------------------------------------------------------------------
print(grammarDef)

## -----------------------------------------------------------------------------
ruleDef <- list(expr  = gsrule("<expr><op><expr>", "<func>(<expr>)", "<var>"),
                func  = gsrule("sin", "cos", "log", "sqrt"),
                op    = gsrule("+", "-", "*"),
                var   = grule(distance, distance^n, n),
                n     = grule(1, 2, 3, 4))

CreateGrammar(ruleDef)

## ----tidy=FALSE---------------------------------------------------------------
planets <- c("Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus")
distance <- c(0.72, 1.00, 1.52, 5.20, 9.53, 19.10)
period <- c(0.61, 1.00, 1.84, 11.90, 29.40, 83.50)

SymRegFitFunc <- function(expr) {
  result <- eval(expr)

  if (any(is.nan(result)))
    return(Inf)

  return (mean(log(1 + abs(period - result))))
}

## -----------------------------------------------------------------------------
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc,  
                           terminationCost = 0.021)
ge

## -----------------------------------------------------------------------------
best.expression <- ge$best$expression

data.frame(distance, period, Kepler = sqrt(distance^3), 
           GE = eval(best.expression))

## ----eval=FALSE---------------------------------------------------------------
#  customMonitorFunc <- function(results){
#    cat("-------------------\n")
#    print(results)
#  }
#  
#  ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc,
#                             terminationCost = 0.021,
#                             monitorFunc = customMonitorFunc)

## ----eval=FALSE---------------------------------------------------------------
#  ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc,
#                             terminationCost = 0.021,
#                             monitorFunc = print)

## ----echo=FALSE---------------------------------------------------------------
set.seed(0)

## ----tidy=FALSE---------------------------------------------------------------
re <- "^(\\+|-)?[[:digit:]]+(\\.[[:digit:]]+)?$"

## ----tidy=FALSE---------------------------------------------------------------
grepl(re, "+1.1")
grepl(re, "1+1")

## ----tidy=FALSE---------------------------------------------------------------
matching <- c("1", "11.1", "1.11", "+11", "-11", "-11.1")
non.matching <- c("a", "1.", "1..1", "-.1", "-", "1-", "1.-1", 
                  ".-1", "1.-", "1.1.1", "", ".", "1.1-", "11-11")

## ----tidy=FALSE---------------------------------------------------------------
re.score <- function(re) {
  score <- sum(sapply(matching, function(x) grepl(re, x))) + 
           sum(sapply(non.matching, function(x)  !grepl(re, x)))
  return (length(matching) + length(non.matching) - score)
}

## -----------------------------------------------------------------------------
fitfunc <- function(expr) re.score(eval(expr))

## ----tidy=FALSE---------------------------------------------------------------
library("rex")
library("gramEvol")
grammarDef <- CreateGrammar(list(
  re    = grule(rex(start, rules, end)),
  rules = grule(rule, .(rule, rules)),
  rule  = grule(numbers, ".", or("+", "-"), maybe(rules))))
grammarDef

## ----eval=FALSE---------------------------------------------------------------
#  GrammaticalExhaustiveSearch(grammarDef, fitfunc, max.depth = 7, terminationCost = 0)

## ----eval=FALSE---------------------------------------------------------------
#  system.time(GrammaticalExhaustiveSearch(grammarDef, fitfunc,
#                                          max.depth = 7, terminationCost = 0))

## ----tidy=FALSE---------------------------------------------------------------
grammarDef <- CreateGrammar(list(
  expr = gsrule("(<expr>)<op>(<expr>)", "<coef>*<var>"),
  op   = gsrule("+", "-", "*", "/"),
  coef = gsrule("c1", "c2"),
  var  = gsrule("v1", "v2")))

grammarDef

## ----tidy=FALSE---------------------------------------------------------------
GrammarMap(c(0, 1, 0, 0, 1, 1, 0, 0), grammarDef)

## ----tidy=FALSE---------------------------------------------------------------
GrammarMap(c(0, 1, 0, 0, 1, 1, 0, 0), grammarDef, verbose = TRUE)

## ----tidy=FALSE---------------------------------------------------------------
GrammarMap(c(0, 1, 0, 0, 1, 1), grammarDef, verbose = TRUE)

## ----tidy=FALSE---------------------------------------------------------------
summary(grammarDef)

## ----tidy=FALSE---------------------------------------------------------------
GetGrammarDepth(grammarDef)
GetGrammarDepth(grammarDef, max.depth = 10)

## ----tidy=FALSE---------------------------------------------------------------
grammarDef2 <- CreateGrammar(list(
  expr    = gsrule("(<subexpr>)<op>(<subexpr>)"),
  subexpr = gsrule("<coef>*<var>"),
  op      = gsrule("+", "-", "*", "/"),
  coef    = gsrule("c1", "c2"),
  var     = gsrule("v1", "v2")))

GetGrammarDepth(grammarDef2)

## ----tidy=FALSE---------------------------------------------------------------
GetGrammarDepth(grammarDef2, startSymb = "<subexpr>")
GetGrammarDepth(grammarDef2, startSymb = "<coef>")

## ----tidy=FALSE---------------------------------------------------------------
GetGrammarMaxRuleSize(grammarDef)

## ----tidy=FALSE---------------------------------------------------------------
GetGrammarNumOfExpressions(grammarDef)
GetGrammarNumOfExpressions(grammarDef, max.depth = 2)
GetGrammarNumOfExpressions(grammarDef, startSymb = "<coef>")

## ----tidy=FALSE---------------------------------------------------------------
GetGrammarMaxSequenceLen(grammarDef)
GetGrammarMaxSequenceLen(grammarDef, max.depth = 3) 
GetGrammarMaxSequenceLen(grammarDef2, startSymb = "<subexpr>")

## ----eval=FALSE---------------------------------------------------------------
#  GrammaticalEvolution(grammarDef, evalFunc,
#                       numExpr = 1,
#                       max.depth = GrammarGetDepth(grammarDef),
#                       startSymb = GrammarStartSymbol(grammarDef),
#                       seqLen = GrammarMaxSequenceLen(grammarDef, max.depth, startSymb),
#                       wrappings = 3,
#                       suggestions = NULL,
#                       optimizer = c("auto", "es", "ga"),
#                       popSize = 8, newPerGen = "auto", elitism = 2,
#                       mutationChance = NA,
#                       iterations = 1000, terminationCost = NA,
#                       monitorFunc = NULL,
#                       plapply = lapply, ...)
#  

## ----eval=FALSE---------------------------------------------------------------
#  library("parallel")
#  options(mc.cores = 4)
#  ge <- GrammaticalEvolution(grammarDef, evalFunc,
#                             plapply = mclapply)

## ----eval=FALSE---------------------------------------------------------------
#  library("parallel")
#  cl <- makeCluster(type = "PSOCK", c("127.0.0.1",
#                                      "127.0.0.1",
#                                      "127.0.0.1",
#                                      "127.0.0.1"))
#  clusterEvalQ(cl, library("gramEvol"))
#  clusterExport(cl, c("evalFunc"))
#  ge <- GrammaticalEvolution(grammarDef, evalFunc,
#                             plapply = function(...) parLapply(cl, ...))
#  stopCluster(cl)

## ----tidy=FALSE---------------------------------------------------------------
df <- data.frame(c1 = c(1, 2),
                 c2 = c(2, 3),
                 v1 = c(3, 4),
                 v2 = c(4, 5))

quad.expr <- expression(c1 * v1, c1 * v2, c2 * v1, c2 * v2)
EvalExpressions(quad.expr, envir = df)

## ----eval=FALSE---------------------------------------------------------------
#  result1 <- GrammaticalExhaustiveSearch(grammarDef, evalFunc)
#  result2 <- GrammaticalRandomSearch(grammarDef, evalFunc)

## -----------------------------------------------------------------------------
gvrule(1:5)

## -----------------------------------------------------------------------------
grule(1,2,3,4,5)

## -----------------------------------------------------------------------------
grule(1:5)

## -----------------------------------------------------------------------------
CreateGrammar(list(assignment = gsrule("A = B", "A = C"),
                   comma      = gsrule("A, B", "B, C")))

## -----------------------------------------------------------------------------
CreateGrammar(list(assignment = grule(.(A = B), .(A = C)),
                   comma      = grule(.(A, B), .(B, C))))

