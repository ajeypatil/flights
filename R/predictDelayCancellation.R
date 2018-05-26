#' @param airline  Airline IATA_CODE
#' @param origin_airport  Origin Airport IATA_CODE
#' @param destination_airport  DESTINATION Airport IATA_CODE
#' @param flght_month   Month of travel
#' @param flght_day_of_week Day of week
#' @param flght_time  Time in 24 (HHMM) hour format
#'
#' @examples
#'   predictDelayCancellation("UA", "SJC", "DEN", 10, 4, 1500)
#' @import jsonlite
#' @import rpart
#' @export

predictDelayCancellation <- function(airline,origin_airport,destination_airport,flght_month,flght_day_of_week,flght_time) {

  if( ! airline %in% levels(airlines$IATA_CODE) )
    return("[Unknown Airline IATA_CODE]")

  if( ! origin_airport %in% levels(airports$IATA_CODE) )
    return("[Unknown Origin Airport IATA_CODE]")

  if( ! destination_airport %in% levels(airports$IATA_CODE) )
    return("[Unknown Destination Airport IATA_CODE]")

  if( ! flght_month %in% c(1,2,3,4,5,6,7,8,9,10,11,12) )
    return("[Month input error")

  if( ! flght_day_of_week %in% c(1,2,3,4,5,6,7) )
    return("[Day of week input error]")

  input <- data.frame(AIRLINE=airline, ORIGIN_AIRPORT=origin_airport,
            DESTINATION_AIRPORT=destination_airport,MONTH=flght_month,
            DAY_OF_WEEK=flght_day_of_week,DEPARTURE_TIME=flght_time)

  input2 <- rbind(data_prototype, input)
  pred <-  predict(tree.model, input2, type="prob")
  printpred <- ifelse((pred[,2]>0.8),TRUE,FALSE)
  return(jsonlite::toJSON(printpred))

}
