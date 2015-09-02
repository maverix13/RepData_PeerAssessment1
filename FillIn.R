# Copy of FillIn by Christopher Gandrud. This is copied from his repository and print statements are removed.

FillIn <- function(D1, D2, Var1, Var2 = NULL, KeyVar = c("iso2c", "year"))
{
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(Var2)){
    Var2 <- Var1
  } else {
    Var2 <- Var2
  }
  
  # Give var a generic name
  names(D1)[match(Var1, names(D1))] <- "VarGen"
  names(D2)[match(Var2, names(D2))] <- "VarGen.1"
  
  # Convert data frames to data.table type objects
  D1Temp <- data.table::data.table(D1, key = KeyVar)
  D2Temp <- data.table::data.table(D2, key = KeyVar)
  
  # Merge data.tables
  OutDT <- D2Temp[D1Temp]
  
  # Tell the user how many values will be filled in
  SubNA <- OutDT[, list(VarGen, VarGen.1)]
  SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
  #print(paste(nrow(SubNA), "NAs were replaced."))
  
  # Fill in missing values from D1 with values from D2
  OutDT <- OutDT[is.na(VarGen), VarGen := VarGen.1]
  
  # Convert back to data frame
  OutDF <- data.frame(OutDT)
  
  # Tell the user what the correlation coefficient is between the variables
  SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
  HowMany <- nrow(SubNoNA)
  CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
  #print(paste("The correlation between", Var1, "and", Var2, "is", round(CORR, digits = 3), "based on", HowMany, "shared observations." ))
  
  # Remove uncombined variable and return main variable's name
  names(OutDF)[match("VarGen", names(OutDF))] <- Var1
  Keepers <- setdiff(names(OutDF), "VarGen.1")
  OutDF <- OutDF[, Keepers]
  OutDF
}
