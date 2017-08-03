#' Roll with advantage
#' 
#' Rolls a d20 with the advantage or disadvantage mechanic from Dungeons & Dragons 5th Edition, where you roll two d20s and take the better or worse result.
#' 
#' @param advantage Boolean. If FALSE will roll with disadvantage (lower of two dice), otherwise will roll with advantage (better of two dice).
#' @param bonus A fixed value to add to the final result.
#' 
#' @return 
#' 
#' @examples
#' rolladv()
#' 
#' rolladv(F)
#' 
#' rolladv(,2)
#' 
#' rolladv(F, 4)
rolladv <- function(advantage = T, bonus = 0) {
	result <- sample(1:20, 2, replace = T)
	if(advantage == T & bonus == 0) {
		cat(
			"Rolled 2d20 with advantage and got:", 
			result, 
			"for a result of", 
			max(result), 
			if(max(result) <= 5) {"\nSadvantage!"}
			)
	} else if(advantage == F & bonus == 0) {
		cat(
			"Rolled 2d20 with disadvantage and got:", 
			result, 
			"for a result of", 
			min(result)
			)
	} else if(advantage == T & bonus != 0) {
		cat(
			"Rolled 2d20 +", 
			bonus, 
			"and got:", 
			result, 
			"for a result of", 
			max(result), 
			"+", 
			bonus, 
			"=", 
			max(result)+bonus, 
			if(max(result) <= 5) {"\nSadvantage!"})
	} else {
		cat(
			"Rolled 2d20 +", 
			bonus, 
			"and got:", 
			result, 
			"for a result of", 
			min(result), 
			"+", 
			bonus, 
			"=", 
			min(result)+bonus)
	}
}
