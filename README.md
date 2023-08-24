# Diet_overlap_FSA_Schoener
´´´´´´

dietOverlap <- function(diet1,diet2=NULL,
                        type=c("Horn","Levins","Morisita","Pianka","Schoener"),
                        prey=NULL,num1=NULL,num2=NULL,N1=NULL,N2=NULL) {
  ## Internal functions
    dietProp <- function(diet) { diet/sum(diet) }

    DOHorns <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      if (any(c(prop1,prop2)==0))
        stop("'Horn's' diet overlap index cannot be used when the proportion of any diet item is 0.",
             call.=FALSE)
      doi <- (sum((prop1+prop2)*log(prop1+prop2))-sum(prop1*log(prop1))-sum(prop2*log(prop2)))/(2*log(2))
      list(type="Horns",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOHorns internal function

    DOLevins <- function(diet1,diet2){
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      res12 <- sum(prop1*prop2)/sum(prop2^2)       # overlap of 1 on 2
      res21 <- sum(prop1*prop2)/sum(prop1^2)       # overlap of 2 on 1
      list(type="Levins",doi=c(res12,res21),propdiet=cbind(prop1,prop2))
    } # end DOLevins internal function

    DOMorisita <- function(diet1,diet2,num1,num2,N1,N2){
      if (is.null(num1)|is.null(num2))
        stop("'num1' and 'num2' must not be NULL if type='Morisita'.",call.=FALSE)
      if (is.null(N1)|is.null(N2))
        stop("'N1' and 'N2' must not be NULL if type='Morisita'.",call.=FALSE)
      if (length(num1)!=length(num2))
        stop("Lengths of 'num1' and 'num2' must be equal.",call.=FALSE)
      if (length(num1)!=length(diet1))
        stop("Lengths of 'diet1', 'diet2', 'num1', and 'num2' must be equal.",call.=FALSE)
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- (2*sum(prop1*prop2))/(sum(prop1*((num1-1)/(N1-1)))+sum(prop2*((num2-1)/(N2-1))))
      list(type="Morisita",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOMorisita internal function

    DOPianka <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- sum(prop1*prop2)/sqrt(sum(prop1^2)*sum(prop2^2))
      list(type="Pianka",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOPianka internal function

    DOSchoener <- function(diet1,diet2) {
      prop1 <- dietProp(diet1)                     # proportion of each prey in first diet
      prop2 <- dietProp(diet2)                     # proportion of each prey in second diet
      doi <- 1-0.5*sum(abs(prop1-prop2))
      list(type="Schoener",doi=doi,propdiet=cbind(prop1,prop2))
    } # end DOSchoener internal function

  ## Start of main function
  type <- match.arg(type)
  if (!is.vector(diet1)) {
    if (!(is.matrix(diet1) | is.data.frame(diet1)))
      stop("'diet1' must be either a vector, matrix, or data.frame.",call.=FALSE)
    if (!is.null(diet2))
      stop("'diet2' should be null if 'diet1' is a matrix or data.frame.",call.=FALSE)
    if (ncol(diet1)>2)
      stop("'diet1' must contain only two columns (corresponding to two predators)",call.=FALSE)
    diet2 <- as.vector(diet1[,2])
    diet1 <- as.vector(diet1[,1])
  }
  if (length(diet1)!=length(diet2))
    stop("Length (number of diet items) of 'diet1' and 'diet2' must be the same.",call.=FALSE)
  switch(type,
         Horn={res <- DOHorns(diet1,diet2)},
         Levins={res <- DOLevins(diet1,diet2)},
         Morisita={res <- DOMorisita(diet1,diet2,num1,num2,N1,N2)},
         Pianka={res <- DOPianka(diet1,diet2)},
         Schoener={res <- DOSchoener(diet1,diet2)}
         ) # end switch
  if (!is.null(prey)) {
    if (length(diet1)!=length(prey))
      stop("Number of prey categories in 'prey' must be same as number of items in 'diet1' and 'diet2'.",
           call.=FALSE)
    rownames(res[["propdiet"]]) <- prey
  }
  class(res) <- "dietOverlap"
  res
}

#' @rdname dietOverlap
#' @export
print.dietOverlap <- function(x,...) { print(x$doi,...) }

#' @rdname dietOverlap
#' @export
summary.dietOverlap <- function(object,verbose=TRUE,digits=getOption("digits"),...) {
  if (verbose) {
    if (object$type=="Levins") {
      message("The ",object$type," diet overlap is ",round(object$doi[1],digits),
              " for predator 1 on predator 2\n",
              " and ",round(object$doi[2],digits)," for predator 2 on predator 1.\n")
    } else { message("The ",object$type," diet overlap index is ",
                     round(object$doi,digits),".\n")
    }
    message("The following observed proportional diets were used:")
    print(round(object$propdiet,digits))
  } else round(object$doi,digits)
}
