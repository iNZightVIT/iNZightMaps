#' Earthquakes in New Zealand
#'
#' A dataset containing locations and measurements of earthquakes in the
#' New Zealand region.
#'
#' @format A data frame with 334 rows and 12 variables:
#' \describe{
#'   \item{Order}{For ordering the observations}
#'   \item{Latitude}{The latitude ("Y") positions of the earthquakes}
#'   \item{Longitude}{The longitude ("X") positions of the earthquakes}
#'   \item{Depth}{Depth of the earthquake in kilometres}
#'   \item{Felt}{Was the earthquake felt?}
#'   \item{Magnitude}{Magnitude of the earthquake in Richter magnitude}
#'   \item{RMS}{Root mean square amplitude}
#'   \item{NorMidSth}{Region of New Zealand the earthquake occurred in (North, Mid, South)}
#'   \item{Month}{The month the earthquake occurred in (1--12)}
#'   \item{Day}{Day of the month the earthquake occurred on (1--31)}
#'   \item{Hour}{Hour of the day the earthquake occurred on (0--23)}
#'   \item{Minute}{Minute of the hour the earthquake occurred in (0--59)}
#' }
"nzquakes"

#' Cardiac-related mortality in New Zealand DHBs
#' 
#' Number of cardiac-related deaths in each DHB for a single year. 
#' @format A data frame with 21 rows and 5 variables:
#' \describe{
#'   \item{NAME}{DHB name}
#'   \item{Char.Code}{Character code of the DHB}
#'   \item{heart}{Number of cardiac-related deaths in the total DHB population}
#'   \item{maori}{Number of cardiac-related deaths in the Maori population}
#'   \item{total}{Number of deaths in the total DHB population due to any cause}
#' }
"heart"

#' National Survey of Substance Abuse Treatment Services
#' 
#' A dataset containing the proportion of substance abuse treatment facilities in 
#' a given state that meet a criteria given by the variable name. 
#' 
#' @format A data frame with 57 rows and 25 variables:
#' \describe{
#'   \item{STATE}{Two character state code}
#'   \item{OfferNonEnglish}{Proportion of facilities offering non-English language treatment}
#'   \item{SpecificGroupAdolescent}{Proportion of facilities offering specific groups for adolescents}
#'   \item{SpecificGroupWomen}{Proportion of facilities offering specific groups for women}
#'   \item{SpecificGroupPregnant}{Proportion of facilities offering specific groups for pregnant women}
#'   \item{SpecificGroupMen}{Proportion of facilities offering specific groups for men}
#'   \item{SpecificGroupSeniors}{Proportion of facilities offering specific groups for seniors}
#'   \item{SpecificGroupLGBT}{Proportion of facilities offering specific groups for LGBT}
#'   \item{SpecificGroupVeterans}{Proportion of facilities offering specific groups for veterans}
#'   \item{SpecificGroupMentalIllness}{Proportion of facilities offering specific groups for individuals with mental illness}
#'   \item{SpecificGroupTrauma}{Proportion of facilities offering specific groups for individuals with trauma}
#'   \item{SlidingFeeScale}{Proportion of facilities offering a sliding fee scale}
#'   \item{OffersFreeTreatment}{Proportion of facilities offering free treatment to those who cannot pay}
#'   \item{AcceptsMedicare}{Proportion of facilities that accept Medicare as a payment option}
#'   \item{AcceptsMedicaid}{Proportion of facilities that accept Medicaid as a payment option}
#'   \item{AcceptsStateInsurance}{Proportion of facilities that accept state insurance as a payment option}
#'   \item{AcceptsMilitaryInsurance}{Proportion of facilities that accept military insurance as a payment option}
#'   \item{AcceptsPrivateInsurance}{Proportion of facilities that accept private insurance as a payment option}
#'   \item{LicensedByAuthority}{Proportion of facilities licensed/certified/accredited by hospital or state authority}
#'   \item{PrivateForProfit}{Proportion of facilities that are private for-profit organisations}
#'   \item{PrivateNonProfit}{Proportion of facilities that are private non-profit organisations}
#'   \item{StateGovt}{Proportion of facilities owned by the state government}
#'   \item{DUIProgram}{Proportion of facilities offering a program for DUI/DWI offenders}
#'   \item{OfferSignLanguage}{Proportion of facilities offering treatment in sign language}
#'   \item{AnyPublicFunding}{Proportion of facilities that receive any federal, state, county, or local funds}
#' }
"NSSATS"