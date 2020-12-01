########################################
# Lists for selectInput objects
# lists.R
########################################

states <- c("Utah"="UT")

# states <- c(
#   "Alabama"="AL", 
#   "Alaska"="AK", 
#   "Arizona"="AZ", 
#   "Arkansas"="AR", 
#   "California"="CA", 
#   "Colorado"="CO", 
#   "Connecticut"="CT", 
#   "Delaware"="DE", 
#   "Florida"="FL", 
#   "Georgia"="GA", 
#   "Hawaii"="HI", 
#   "Idaho"="ID", 
#   "Illinois"="IL", 
#   "Indiana"="IN", 
#   "Iowa"="IA", 
#   "Kansas"="KS", 
#   "Kentucky"="KY", 
#   "Louisiana"="LA", 
#   "Maine"="ME", 
#   "Maryland"="MD", 
#   "Massachusetts"="MA", 
#   "Michigan"="MI", 
#   "Minnesota"="MN", 
#   "Mississippi"="MS", 
#   "Missouri"="MO", 
#   "Montana"="MT", 
#   "Nebraska"="NE", 
#   "Nevada"="NV", 
#   "New Hampshire"="NH", 
#   "New Jersey"="NJ", 
#   "New Mexico"="NM", 
#   "New York"="NY", 
#   "North Carolina"="NC", 
#   "North Dakota"="ND", 
#   "Ohio"="OH", 
#   "Oklahoma"="OK", 
#   "Oregon"="OR", 
#   "Pennsylvania"="PA", 
#   "Rhode Island"="RI", 
#   "South Carolina"="SC", 
#   "South Dakota"="SD", 
#   "Tennessee"="TN", 
#   "Texas"="TX", 
#   "Utah"="UT", 
#   "Vermont"="VT", 
#   "Virginia"="VA", 
#   "Washington"="WA", 
#   "West Virginia"="WV", 
#   "Wisconsin"="WI", 
#   "Wyoming"="WY", 
#   "-----"="", 
#   "American Samoa"="AS", 
#   "District of Columbia"="DC", 
#   "Federated States of Micronesia"="FM", 
#   "Guam"="GU", 
#   "Marshall Islands"="MH", 
#   "Northern Mariana Islands"="MP", 
#   "Palau"="PW", 
#   "Puerto Rico"="PR", 
#   "Virgin Islands"="VI"
# )

directions <- c(
  "-----"="", 
  "North"="N", 
  "East"="E", 
  "South"="S", 
  "West"="W", 
  "Northeast"="NE", 
  "Southeast"="SE", 
  "Southwest"="SW", 
  "Northwest"="NW"
)

directions2 <- c(
  "-----"="", 
  "Northeast"="NE", 
  "Southeast"="SE", 
  "Southwest"="SW", 
  "Northwest"="NW", 
  "North"="N", 
  "East"="E", 
  "South"="S", 
  "West"="W"
)

weekdays <- c(
  "Sunday"="Sun", 
  "Monday"="Mon", 
  "Tuesday"="Tue", 
  "Wednesday"="Wed", 
  "Thursday"="Thu", 
  "Friday"="Fri", 
  "Saturday"="Sat"
)

hours2 <- c(
  "12 midnight"="00", 
  "01 AM"="01", 
  "02 AM"="02", 
  "03 AM"="03", 
  "04 AM"="04", 
  "05 AM"="05", 
  "06 AM"="06", 
  "07 AM"="07", 
  "08 AM"="08", 
  "09 AM"="09", 
  "10 AM"="10", 
  "11 AM"="11", 
  "12 noon"="12", 
  "01 PM"="13", 
  "02 PM"="14", 
  "03 PM"="15", 
  "04 PM"="16", 
  "05 PM"="17", 
  "06 PM"="18", 
  "07 PM"="19", 
  "08 PM"="20", 
  "09 PM"="21", 
  "10 PM"="22", 
  "11 PM"="23"
)

hours <- as.character(c("00", "01", "02", "03" ,"04", "05", "06", "07", "08", "09", 10:23))

minutes <- as.character(c("00", "01", "02", "03" ,"04", "05", "06", "07", "08", "09", 10:59))

seconds <- as.character(c("00", "01", "02", "03" ,"04", "05", "06", "07", "08", "09", 10:59))

dirX1 <- c("-----"="", "North"="N", "Northeast"="NE")
dirX2 <- c("-----"="", "East"="E", "Southeast"="SE")
dirX3 <- c("-----"="", "South"="S", "Southwest"="SW")
dirX4 <- c("-----"="", "West"="W", "Northwest"="NW")
dirC1 <- c("-----"="", "Northeast"="NE", "East"="E")
dirC2 <- c("-----"="", "Southeast"="SE", "South"="S")
dirC3 <- c("-----"="", "Southwest"="SW", "West"="W")
dirC4 <- c("-----"="", "Northwest"="NW", "North"="N")

BOlist <- c("-----"="", "Bicycle"="BIKE", "Scooter"="SCOOT", "Skateboard"="SKATE", "Wheelchair"="WHEEL", "Other"="OTHER", 
            "Pedestrian (outside crosswalk)"="PEDOUT", 
            "Start not visible"="NOVIS1", "End not visible"="NOVIS2")

########################################
# END
########################################