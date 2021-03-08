#' @title abovebelow
#' @description This will change the alternative hypotheses based on preference
#' @param x is a variable from markdown. i.e. less = TRUE, alt=Logical. If true, will give alternate text 
#' @export
abovebelow = function(x,alt=FALSE){
  if (alt==FALSE){
    if(x==TRUE) return("below")
    if(x==FALSE) return("above")
  }
  if (alt==TRUE){
    if(x==TRUE) return("less than")
    if(x==FALSE) return("greater than")
  }  
}

#' @title typeTest
#' @description more alternate wording for alternative hypotheses. 
#' @param x variable from markdown with a value of "less" "more" "either". alt is a logical response. If true it will give alternate text
#' @export
typeTest = function(x,alt=FALSE){
  if (alt==FALSE){
    if(x=="less") return("decrease")
    if(x=="more") return("increase")
    if(x=="either") return("different")
    
  }
  if (alt==TRUE){
    if(x=="less") return("less than")
    if(x=="more") return("greater than")
    if(x=="either") return("not equal to") 
    
  }  
}

#' @title tailTest
#' @description Gives what type of tail test you use. 
#' @param x is a variable from the markdown. "less" "more" "either"
#' @export
tailTest = function(x){
  if(x=="less") return("one tail")
  if(x=="more") return("one tail")
  if(x=="either") return("two tailed")
}

#' @title symbolTest
#' @description gives the symbol that matches the alternative hypothesis
#' @param x is a variable from the markdown. "less" "more" "either"
#' @export
symbolTest = function(x){
  if(x=="less") return("<")
  if(x=="more") return(">")
  if(x=="either") return("\\neq") 
  
}

#' @title numberText
#' @description replaces 1 with one and so on
#' @param variable with a number
#' @export
numberText = function(x){
  transfer = c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine","Ten")
  transfer[x]
}

#' @title updown
#' @description inputs 'increase' or 'decrease' depending on desired test
#' @param x as logical function 
#' @export
updown = function(x){
  if(x==TRUE) return("decrease")
  if(x==FALSE) return("increase")
}

#' @title failorNot
#' @description returns whether or not the null should be rejected
#' @param p = p-value, alpha = level of significance
#' @export
failOrNot = function(p, alpha){
  if(p < alpha) return("reject the null hypothesis")
  if(p >= alpha) return("fail to reject the null hypothesis")
}

#' @title sufficientOrNot
#' @description returns whether or not there is sufficient evidence for the alternative
#' @param x text"reject the null hypothesis" hint: just put the failOrNot function inside this function. 
#' @export
sufficientOrNot = function(x){
  if(x=="reject the null hypothesis") return("sufficient")
  if(x=="fail to reject the null hypothesis") return("insufficient")
}

#' @title pValue
#' @description input the zscore and type of test and it will give you the pvalue
#' @param score is zScore, tail = alternative hyp
#' @export 
pValue = function(score,tail=c("less","more","either")[2]){
  if(tail=="either") out = pnorm(score, lower.tail = TRUE) * 2 
  if(tail=="less") out = pnorm(score, lower.tail = TRUE)
  if(tail=="more") out = pnorm(score, lower.tail = FALSE)
  out
} 

#' @title zScore
#' @description returns the zScore
#' @param x = sample mean/observed value, trueMean = trueMean, sigma = sigma, n = sample size
#' @export
zScore = function(x, trueMean, sigma,n){
  (x-trueMean)/(sigma/sqrt(n))
}

#' @title zStar
#' @description returns the zStar from a confidence level
#' @param confLevel. Input a variable that equals 90, 95 or 99
#' @export
zStar = function(confLevel){
  if(confLevel==90) return(1.645)
  if(confLevel==95) return(1.960)
  if(confLevel==99) return(2.576)
}

#' @title marginError
#' @description returns margin of error
#' @param zStar, sigma and sample size
#' @export
marginError = function(z,sigma,n){
  z * (sigma/sqrt(n))
}

#' @title confInterval
#' @description will return confidence interval
#' @param sample mean, zStar, sigma and sample size
#' @export
confInterval = function(mean, z, sigma, n){
  lowerBound = (mean - (z * (sigma/sqrt(n))))
  upperBound = (mean + (z * (sigma/sqrt(n))))
  return(paste(round(lowerBound,3), ",", round(upperBound,3)))
}

#' @title confIntervalString
#' @description will return a confidence interval as a string
#' @param sample mean, zStar, sigma and sample size
#' @export
confIntervalString = function(mean, z, sigma, n){
  lowerBound = (mean - (z * (sigma/sqrt(n))))
  upperBound = (mean + (z * (sigma/sqrt(n))))
  return(paste(round(lowerBound,3), " and ", round(upperBound,3)))
}

#' @title sampleSize
#' @description will return necessary sample size 
#' @param zStar, sigma, desired margin of error
#' @export
sampleSize = function(z,sigma,error){
  ceiling(((z*sigma)/error)^2)
}

#' @title label
#' @description Not sure, Hathaway's genius
#' @param x
#' @export 
f <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

#' @title sdProportion
#' @description Will return the sigma of a proportion
#' @param Population proportion, sample size
#' @export
sdProportion = function(p,n){
  sqrt((p * (1-p))/n)
}

#' @title zProportion
#' @description will return the z score of a proportion
#' @param sample proportion, true proportion, sample size
#' @export
zProportion = function(phat,trueP,n){
  (phat - trueP)/(sqrt(trueP * (1-trueP)/(n)))
}

#' @title requirementsProp
#' @description Will give you the smallest necessary sample size to meet the requirements of a one proportion test
#' @param true proportion
#' @export
requirementsProp = function(p){
  n1 = 10/p
  n2 = 10/(1-p)
  
  if(n1 > n2) return(n1)
  if(n1 < n2) return(n2)
  
} 

#' @title ConditionsMetPropHyp
#' @description Will return a string in the answer key that says if the conditions are met for a one proportion hypothesis test 
#' @param con1 (from a variable n*p) con2 (from a variable n * (1-p))
#' @export
ConditionsMetPropHyp = function(con1,con2){
  if(con1 & con2 >= 10) return("Since both conditions are true, we conclude that $n$ is sufficiently large so that $\\hat{p}$ will be approximately distributed.")
  if(con1 | con2 < 10) return("Since one or both conditions are false, we conclude that $n$ is not sufficiently large so that $\\hat{p}$ will not be approximately distributed")
}

#' @title ConditionsMetPropConf
#' @description returns a string in answer key that says if the conditions are met for a one proportion confidence interval test
#' @param con1 and con2
#' @export
ConditionsMetPropConf = function(con1,con2){
  if(con1 & con2 >= 10) return("The requirements are met.")
  if(con1 | con2 < 10) return("The requirements are not met")
}

#' @title necessarySampleProp
#' @description Will return necessary sample size for a proportion test without a prior estimate
#' @param zStar, desired margin of error
#' @export
necessarySampleProp = function(z,m){
  (z/(2 * m))^2
}

#' @title marginErrorProp
#' @description will return the margin of error for a one proportion confidence interval
#' @param zStar, sample proportion and sample size
#' @export
marginErrorProp = function(z,phat,n){
  z * sqrt(phat * (1-phat)/n)
}

#' @title zTwoProp
#' @description will return the zScore of a two proportion test
#' @param sample proportion 1, sample proportion 2, sample size 1 and sample size 2
#' @export
zTwoProp = function(phat,phat1,phat2,n1,n2){
  top = phat1 - phat2
  bottom = sqrt(phat*(1-phat)*((1/n1)+(1/n2)))
  return(top/bottom)
}

#' @title confIntTwoProp
#' @description will return the confidence interval for a two proportion test
#' @param sample proportion 1, sample proportion 2, sample size 1 and sample size 2, zStar
#' @export
confIntTwoProp = function(phat1,phat2,n1,n2,zStar){
  LowerBound = (phat1 - phat2) - (zStar * sqrt(((phat1*(1-phat1))/(n1+2)) + (phat2*(1-phat2))/(n2+2)))
  UpperBound = (phat1 - phat2) + (zStar * sqrt(((phat1*(1-phat1))/(n1+2)) + (phat2*(1-phat2))/(n2+2)))
  return(list(LowerBound,UpperBound))
}

#' @title reqMetChi
#' @description returns a string in the answer key that indicates if the requirements are met for a chi squared test
#' @param chiobject$expected
#' @export
reqMetChi = function(expectedCount){
  expectedCount >= 5
  if(FALSE >= 1) return("The requirements are not met because at least one expected count is less than five")
  if(FALSE == 0) return("The requirements are met because all expected counts are greater than or equal to five")
}

#' @title dfChi
#' @description returns the degrees of freedom of a chi matrix
#' @param the matrix you are performing your chi test on
#' @export
dfChi = function(matrix){
  (nrow(matrix) - 1) * (ncol(matrix) - 1)
}

#' @title zUsual
#' @description Returns the word "Unusual" or "Not Unusual" (let's ignore the double negative) based on 2 or -2.
#' @param z is the normal z-score
#' @export
zUsual = function(z){
  out = "Not Unusual"
  if(abs(z) >=2) out = "Unusual"
  out
}


necessarySamplePropPrior = function(zvalue,medesired,assumedP=.5){
  ceiling((zvalue/medesired)^2*(assumedP*(1-assumedP)))
}











































