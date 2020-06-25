digits = 9:1


add = function(){return("+")}
subtract = function(){return("-")}
multiply = function(){return("*")}
divide = function(){return("/")}


operations = c(add, subtract, multiply, divide)
allPermutations = permutations(length(operations),length(digits)-1,v=1:length(operations), repeats.allowed = T)

insertString <- function(old, newChar, n){
  return(paste(substr(old, 1, n-1), newChar, substr(old, n, nchar(old)), sep = ""))
}

insertBracket <- function(formula){
  formulaNew=formula
  randomNumbers = numeric(0)
  while(sum(randomNumbers)<length(digits)){
    newNumber = sample(1:(length(digits)),1)
    if(sum(randomNumbers)+newNumber<=length(digits)){
      randomNumbers=c(randomNumbers,newNumber)
    }
  }
  
  startingIndex = nchar(formulaNew)+1
  for(i in 1:length(randomNumbers)){
    randomNumber = randomNumbers[i]
    if(randomNumber==1){
      startingIndex = startingIndex - 2*randomNumber
    } else {
      formulaNew=insertString(formulaNew,")",startingIndex)
      startingIndex = startingIndex - 2*randomNumber+1
      formulaNew=insertString(formulaNew,"(",startingIndex)
      startingIndex = startingIndex-1
    }
  }
  
  firstNumber = randomNumbers[1]
  
  return(formulaNew)
}


guessResult = function(){
  formula = "9"
  for(i in 2:length(digits)){
    operation = sample(operations,1)[[1]]
    formula = paste0(formula,operation(),digits[i])
  }
  formula = insertBracket(formula)
  
  total = eval(parse(text = formula))
  
  return(data.frame(Total = total, Formula = formula, Length = nchar(formula)))
}

total = 10000;
output = NULL

results = NULL
for(index in 1:10000000){
  output = guessResult()
  if(!is.na(output$Total[1]) & output$Total[1] == 2001){
    results = rbind(results, output)
    print(output$Formula) 
  }

  
  if(index %% 100000 == 0) {
    print(index)
  }
}
