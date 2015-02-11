### R code from vignette source 'ge-intro.Rnw'

###################################################
### code chunk number 1: ge-intro.Rnw:33-36
###################################################
options(width=70)
options(warn = (-1))
options(prompt="R> ")


###################################################
### code chunk number 2: ge-intro.Rnw:39-40
###################################################
set.seed(0)


###################################################
### code chunk number 3: ge-intro.Rnw:340-349
###################################################
library("gramEvol")

ruleDef <- list(expr  = grule(op(expr, expr), func(expr), var),
                 func  = grule(sin, cos, log, sqrt),
                 op    = grule(`+`, `-`, `*`),
                 var   = grule(distance, distance^n, n),
                 n     = grule(1, 2, 3, 4))

grammarDef <- CreateGrammar(ruleDef)


###################################################
### code chunk number 4: ge-intro.Rnw:358-359
###################################################
print(grammarDef)


###################################################
### code chunk number 5: ge-intro.Rnw:365-372
###################################################
ruleDef <- list(expr  = gsrule("<expr><op><expr>", "<func>(<expr>)", "<var>"),
                 func  = gsrule("sin", "cos", "log", "sqrt"),
                 op    = gsrule("+", "-", "*"),
                 var   = grule(distance, distance^n, n),
                 n     = grule(1, 2, 3, 4))

CreateGrammar(ruleDef)


###################################################
### code chunk number 6: ge-intro.Rnw:389-401
###################################################
planets <- c("Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus")
distance <- c(0.72, 1.00, 1.52, 5.20, 9.53, 19.10)
period <- c(0.61, 1.00, 1.84, 11.90, 29.40, 83.50)

SymRegFitFunc <- function(expr) {
  result <- eval(expr)

  if (any(is.nan(result)))
    return(Inf)

  return (mean(log(1 + abs(period - result))))
}


###################################################
### code chunk number 7: ge-intro.Rnw:417-420
###################################################
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc,  
                           terminationCost = 0.021)
ge


###################################################
### code chunk number 8: ge-intro.Rnw:424-428
###################################################
best.expression <- ge$best$expression

data.frame(distance, period, Kepler = sqrt(distance^3), 
           GE = eval(best.expression))


###################################################
### code chunk number 9: ge-intro.Rnw:488-496 (eval = FALSE)
###################################################
## customMonitorFunc <- function(results){
##   cat("-------------------\n")
##   print(results)
## }
## 
## ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, 
##                            terminationCost = 0.021,
##                            monitorFunc = customMonitorFunc)


###################################################
### code chunk number 10: ge-intro.Rnw:500-503 (eval = FALSE)
###################################################
## ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, 
##                            terminationCost = 0.021,
##                            monitorFunc = print)


###################################################
### code chunk number 11: ge-intro.Rnw:546-547
###################################################
set.seed(0)


###################################################
### code chunk number 12: ge-intro.Rnw:593-594
###################################################
re <- "^(\\+|-)?[[:digit:]]+(\\.[[:digit:]]+)?$"


###################################################
### code chunk number 13: ge-intro.Rnw:598-600
###################################################
grepl(re, "+1.1")
grepl(re, "1+1")


###################################################
### code chunk number 14: ge-intro.Rnw:604-607
###################################################
matching <- c("1", "11.1", "1.11", "+11", "-11", "-11.1")
non.matching <- c("a", "1.", "1..1", "-.1", "-", "1-", "1.-1", 
                   ".-1", "1.-", "1.1.1", "", ".", "1.1-", "11-11")


###################################################
### code chunk number 15: ge-intro.Rnw:621-626
###################################################
re.score <- function(re) {
  score <- sum(sapply(matching, function(x) grepl(re, x))) + 
           sum(sapply(non.matching, function(x)  !grepl(re, x)))
  return (length(matching) + length(non.matching) - score)
}


###################################################
### code chunk number 16: ge-intro.Rnw:631-632
###################################################
fitfunc <- function(expr) re.score(eval(expr))


###################################################
### code chunk number 17: ge-intro.Rnw:640-647
###################################################
library("rex")
library("gramEvol")
grammarDef <- CreateGrammar(list(
                re    = grule(rex(start, rules, end)),
                rules = grule(rule, .(rule, rules)),
                rule  = grule(numbers, ".", or("+", "-"), maybe(rules))))
grammarDef


###################################################
### code chunk number 18: ge-intro.Rnw:670-671 (eval = FALSE)
###################################################
## GrammaticalExhaustiveSearch(grammarDef, fitfunc, max.depth = 7, terminationCost = 0)


###################################################
### code chunk number 19: ge-intro.Rnw:683-685 (eval = FALSE)
###################################################
## system.time(GrammaticalExhaustiveSearch(grammarDef, fitfunc, 
##                                          max.depth = 7, terminationCost = 0))


###################################################
### code chunk number 20: ge-intro.Rnw:702-709
###################################################
grammarDef <- CreateGrammar(list(
                expr = gsrule("(<expr>)<op>(<expr>)", "<coef>*<var>"),
                op   = gsrule("+", "-", "*", "/"),
                coef = gsrule("c1", "c2"),
                var  = gsrule("v1", "v2")))

grammarDef


###################################################
### code chunk number 21: ge-intro.Rnw:714-715
###################################################
GrammarMap(c(0, 1, 0, 0, 1, 1, 0, 0), grammarDef)


###################################################
### code chunk number 22: ge-intro.Rnw:718-719
###################################################
GrammarMap(c(0, 1, 0, 0, 1, 1, 0, 0), grammarDef, verbose = TRUE)


###################################################
### code chunk number 23: ge-intro.Rnw:725-726
###################################################
GrammarMap(c(0, 1, 0, 0, 1, 1), grammarDef, verbose = TRUE)


###################################################
### code chunk number 24: ge-intro.Rnw:735-736
###################################################
summary(grammarDef)


###################################################
### code chunk number 25: ge-intro.Rnw:746-748
###################################################
GetGrammarDepth(grammarDef)
GetGrammarDepth(grammarDef, max.depth = 10)


###################################################
### code chunk number 26: ge-intro.Rnw:753-761
###################################################
grammarDef2 <- CreateGrammar(list(
                    expr    = gsrule("(<subexpr>)<op>(<subexpr>)"),
                    subexpr = gsrule("<coef>*<var>"),
                    op      = gsrule("+", "-", "*", "/"),
                    coef    = gsrule("c1", "c2"),
                    var     = gsrule("v1", "v2")))

GetGrammarDepth(grammarDef2)


###################################################
### code chunk number 27: ge-intro.Rnw:765-767
###################################################
GetGrammarDepth(grammarDef2, startSymb = "<subexpr>")
GetGrammarDepth(grammarDef2, startSymb = "<coef>")


###################################################
### code chunk number 28: ge-intro.Rnw:772-773
###################################################
GetGrammarMaxRuleSize(grammarDef)


###################################################
### code chunk number 29: ge-intro.Rnw:780-783
###################################################
GetGrammarNumOfExpressions(grammarDef)
GetGrammarNumOfExpressions(grammarDef, max.depth = 2)
GetGrammarNumOfExpressions(grammarDef, startSymb = "<coef>")


###################################################
### code chunk number 30: ge-intro.Rnw:795-798
###################################################
GetGrammarMaxSequenceLen(grammarDef)
GetGrammarMaxSequenceLen(grammarDef, max.depth = 3) 
GetGrammarMaxSequenceLen(grammarDef2, startSymb = "<subexpr>")


###################################################
### code chunk number 31: ge-intro.Rnw:883-887 (eval = FALSE)
###################################################
## library("parallel")
## options(mc.cores = 4)
## ge <- GrammaticalEvolution(grammarDef, evalFunc, 
##                             plapply = mclapply)


###################################################
### code chunk number 32: ge-intro.Rnw:897-907 (eval = FALSE)
###################################################
## library("parallel")
## cl <- makeCluster(type = "PSOCK", c("127.0.0.1",
##                                      "127.0.0.1",
##                                      "127.0.0.1",
##                                      "127.0.0.1"))
## clusterEvalQ(cl, library("gramEvol"))
## clusterExport(cl, c("evalFunc"))
## ge <- GrammaticalEvolution(grammarDef, evalFunc, 
##    plapply = function(...) parLapply(cl, ...))
## stopCluster(cl)


###################################################
### code chunk number 33: ge-intro.Rnw:926-933
###################################################
df <- data.frame(c1 = c(1, 2),
                  c2 = c(2, 3),
                  v1 = c(3, 4),
                  v2 = c(4, 5))

quad.expr <- expression(c1 * v1, c1 * v2, c2 * v1, c2 * v2)
EvalExpressions(quad.expr, envir = df)


###################################################
### code chunk number 34: ge-intro.Rnw:947-949 (eval = FALSE)
###################################################
## result1 <- GrammaticalExhaustiveSearch(grammarDef, evalFunc)
## result2 <- GrammaticalRandomSearch(grammarDef, evalFunc)


###################################################
### code chunk number 35: ge-intro.Rnw:968-970
###################################################
CreateGrammar(list(assignment = gsrule("A = B", "A = C"),
                    comma      = gsrule("A, B", "B, C")))


###################################################
### code chunk number 36: ge-intro.Rnw:973-975
###################################################
CreateGrammar(list(assignment = grule(.(A = B), .(A = C)),
                    comma      = grule(.(A, B), .(B, C))))


