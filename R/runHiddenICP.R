runHiddenICP <- function(X, environment, interventions, parentsOf, alpha, 
                   variableSelMat, excludeTargetInterventions, confBound, 
                   setOptions, directed, verbose, ...){
  
  dots <- list(...)
  if(length(dots) > 0){
    warning("options provided via '...' not taken")
  }
  
  result <- vector("list", length = length(parentsOf))
  
  # additional options for hiddenICP
  optionsList <- list("mode"="asymptotic", "selfselect"=NULL)
  
  # adjust according to setOptions if necessary
  optionsList <- adjustOptions(availableOptions = optionsList, 
                               optionsToSet = setOptions)
  
  if(all((1:ncol(X)) %in% parentsOf) & is.null(interventions) & excludeTargetInterventions) 
    warning(
      "hiddenICP requires that no interventions occured on the target variables. 
      In the current function call 
      (a) all variables are considered as target variables (parentsOf=1:ncol(X)) and 
      (b) the interventions are equal to NULL (and can thus not be removed for 
      each variable).

      The results are likely to be misleading. Either target just specific 
      variables by specifying 'parentsOf' or add the list where interventons 
      occured (using argument 'interventions').\n ")
  
  # use data from different environments/interventions
  if(excludeTargetInterventions & !is.null(interventions)){
    removeObsTarget <- list()
    for (parentsOfC in 1:length(parentsOf)){
      removeObsTarget[[parentsOfC]] <- 
        which(sapply(interventions, 
                      function(x,a) a %in% x, a=parentsOf[parentsOfC]))
    }
  }
  
  for (k in 1:length(parentsOf)){
    if(round(k/100)==(k/100)) cat(" ",k)
    allobs <- 1:nrow(X)
    if(excludeTargetInterventions & !is.null(interventions)){
      if(length(removeObsTarget[[k]])>0) 
        allobs <- allobs[-removeObsTarget[[k]]]
    }
    
    possibleVar <- (1:ncol(X))
    removeVar <- parentsOf[k]
    if(!is.null(variableSelMat) & is.null(optionsList$selfselect)){
      if(ncol(variableSelMat)==length(parentsOf)){
        selc <- k 
      }else{
        selc <- which( (1:ncol(X)) == parentsOf[k])
    }
      removeVar <- 
        unique(c(removeVar,which( !variableSelMat[,selc] )))
    }else{
      if(!is.null(optionsList$selfselect)){
        gl <- glmnet::glmnet(X[, possibleVar[-removeVar],drop=FALSE], 
                             as.numeric(X[, parentsOf[k]]))
        nnz <- apply(coef(gl)!=0, 2,sum)
        beta <- 
          coef(gl, s= gl$lambda[sum(nnz<=optionsList$selfselect)])[-1]
        removeVar <- 
          c(removeVar,(possibleVar[-removeVar])[which(beta==0)])
      }
    }
    if(length(removeVar)>0) 
      possibleVar <- possibleVar[-removeVar]
    
    numUniqueInt <- length(unique(environment[allobs]))
    if(numUniqueInt <= 1)
      stop(paste(
        "After excluding observations where interventions occured on\n", 
        "target variable (variable ", k, ") only", numUniqueInt, "unique environment(s)\n", 
        "remained. At least 2 unique environments are required by hiddenICP."), 
        call. = FALSE)
    
    
    res <- InvariantCausalPrediction::hiddenICP(
       X[allobs,possibleVar,drop=FALSE], 
       as.numeric(X[allobs,parentsOf[k]]), 
       environment[allobs],
       alpha=alpha, mode=optionsList$mode)
    
    parents <- possibleVar[wh <- which(res$maximinCoefficients !=0)]
    result[[k]] <- parents
    attr(result[[k]],"parentsOf") <- parentsOf[k]
    
    if(confBound) 
      attr(result[[k]],"coefficients") <- res$maximinCoefficients[ wh ]
  }
  
  if(directed){
    for (k in 1:length(parentsOf)){
      parentsVar <- result[[k]]
      
      if(confBound)
        coefsParentsVar <-  as.numeric(attr(result[[k]],"coefficients"))
      
      childVar <- as.numeric(attr(result[[k]],"parentsOf"))
      parentsVarOrig <- parentsVar
      for(p in 1:length(parentsVarOrig)){
        idxParentsOfParent <- which(sapply(result, 
                                           function(j) 
                                             is.element(parentsVarOrig[p], 
                                                        as.numeric(attr(j, "parentsOf")))))
        
        if(length(idxParentsOfParent) == 0)
          next
        
        parentsOfParentP <- result[[idxParentsOfParent]]
        
        if(confBound)
          coefsParentsOfParentP <- as.numeric(attr(result[[idxParentsOfParent]],"coefficients"))
        
        
        if(is.element(childVar, parentsOfParentP)){
          
          if(confBound){
            coefsParentsVar <-  coefsParentsVar[parentsVar != parentsVar[p]]
            coefsParentsOfParentP <- coefsParentsOfParentP[parentsOfParentP != childVar]
          }
          
          parentsVar <- parentsVar[parentsVar != parentsVar[p]]
          parentsOfParentP <- parentsOfParentP[parentsOfParentP != childVar]
          
          result[[k]] <- parentsVar
          attr(result[[k]],"parentsOf") <- parentsOf[k]
          
          result[[idxParentsOfParent]] <- parentsOfParentP
          attr(result[[idxParentsOfParent]],"parentsOf") <- parentsVarOrig[p]
          
          if(confBound){
            attr(result[[k]],"coefficients") <- coefsParentsVar
            attr(result[[idxParentsOfParent]],"coefficients") <- coefsParentsOfParentP
          }
          
        }
        
      }
    }
  }
  
  list(resList = result, resMat = NULL)
}
  
