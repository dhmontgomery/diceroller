#' Roll polyhedral dice
#' 
#' Roll X dice with Y sides and add a fixed bonus.
#' 
#' @param dice The number of dice to roll. Defaults to 1
#' @param sides The number of sides on each die. Defaults to 20.
#' @param bonus The fixed bonus to add to the result. Defaults to 0.
#' 
#' @return Strings reflecting the number of dice rolled of which kinds, the number rolled, and the total with bonus.
#' 
#' @examples 
#' roll()
#' 
#' roll(2, 6)
#' 
#' roll(3, 8, 2)
roll <- function(dice = 1, sides = 20, bonus = 0) {
	result <- sample(1:sides, dice, replace = T)
	if(bonus == 0) {
		diceroll <- paste0(dice, "d", sides)
		cat("Rolled", diceroll, "and got" , result)
		cat("\nTotal:", sum(result))
	} else {
		diceroll <- paste0(dice, "d", sides, "+", bonus)
		cat("Rolled", diceroll, "and got" , result)
		cat("\nTotal:", sum(result), "+", bonus, "=", sum(result) + bonus)	
	}
}