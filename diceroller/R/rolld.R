#' Roll XdY+Z dice
#' 
#' When given an input in the form of a string "XdY" or "XdY+Z", where X = the number of dice, Y = the number of sides per dice, and Z = any bonus to the roll, computes the result of that die roll.
#' 
#' @param diceroll A number of dice to roll, in a string with the format "XdY" or "XdY+Z", where X = the number of dice, Y = the number of sides per dice, and Z = any bonus to the roll.
#' 
#' @return String restating the dice roll, giving the numbers rolled, and showing the total value with bonuses
#' 
#' @examples 
#' diceroll("3d6+2")
rolld <- function(diceroll) {
	tmp <- strsplit(as.character(diceroll), "d")
	dice <- tmp[[1]][1]
	if(grepl("\\+",tmp)) { 
		tmp2 <- strsplit(as.character(tmp[[1]][2]), "\\+")
		sides <- tmp2[[1]][1]
		bonus <- tmp2[[1]][2]
	} else {
		sides <- tmp[[1]][2]
		bonus <- 0
	}
	roll(as.numeric(dice), as.numeric(sides), as.numeric(bonus))
}