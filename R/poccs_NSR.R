
#' @title poccs_NSR Identify non-native occurrences
#' @description This function flags and optionally excludes plants by native status in a region.
#'
#' @details
#' This function calls the NSR part of the BIEN workflow to flag plant observations as native vs. nonnative.
#'
#' @param occs data frame of cleaned occurrences obtained from component occs: Obtain occurrence data
#' @param removeID the ID of the occurrence to be removed from the occurrences dataframe
#' @param logger Stores all notification messages to be displayed in the Log Window of Wallace GUI. Insert the logger reactive list here for running in shiny,
#'  otherwise leave the default NULL
#' @param spN data frame of cleaned occurrences obtained from component occs: Obtain occurrence data. Used to obtain species name for logger messages
#' @examples
#' spN <- "Xanthium strumarium"
#'out.gbif <- occs_queryDb(spName = spN, occDb = "gbif", occNum = 100)
#'occs <- as.data.frame(out.gbif[[1]]$cleaned)
#'occs <- poccs_NSR(occs, spN = spN)
#'
#' @return A new occurence dataframe without the user selected occurrence maintaining all
#' columns from original dataframe for further analyses.

#' @author Brian Maitner <bmaitner@@gmail.com>
#' @author Cory Merow <Cory.Merow@@connecticut.edu>
#' @export
poccs_NSR <- function(occs, logger = NULL, spN = NULL) {

  #Check whether there are any occurrences
  if (is.null(occs)) {
    logger %>% writeLog(type = 'error',
                        "Before processing occurrences, you need to obtain them (see the OccData tab).")
    return()
  }

  #########

  #Check whether NSR has already been run.  If so, strip NSR fields
  if (any(c("scrubbed_country","scrubbed_state_province","species","native_status","native_status_reason","native_status_sources","isIntroduced") %in% colnames(occs))) {

    logger %>% writeLog( "The NSR has already been run for this species.  Removing old results and running again.")
    occs <- occs[which(!colnames(occs)%in%c("scrubbed_country","scrubbed_state_province","species","native_status","native_status_reason","native_status_sources","isIntroduced"))]

  }

  #########

  #Convert any NAs to "" to facilitate merging

  occs$country[which(is.na(occs$country))]  <-""
  occs$state_province[which(is.na(occs$state_province))]<-""



  #First, get unique set of country x states and send to GNRS

  #Note: need to message user or show progress bar or something


  smartProgress(logger, message = "Standardizing political division names ...", {

    poldivs <- unique(occs[c("country","state_province")])
    gnrs_out <- GNRS::GNRS_super_simple(country = poldivs$country,state_province = poldivs$state_province)
    gnrs_out <- gnrs_out[c("country","state_province","country_verbatim","state_province_verbatim")]
    colnames(gnrs_out)[grep(pattern = "verbatim",x = colnames(gnrs_out),invert = T)] <- paste("scrubbed_",colnames(gnrs_out)[grep(pattern = "verbatim",x = colnames(gnrs_out),invert = T)],sep = "")

  })

  smartProgress(logger, message = "Checking native status ...", {

  # Second, append the standardized poldiv names to the occ data
  occs <- merge(x = occs,y = gnrs_out,by.x = c("country","state_province"),by.y=c("country_verbatim","state_province_verbatim"),all.x = T)
  #occs_merged <- merge(x = occs,y = gnrs_out,by.x = c("country","state_province"),by.y=c("country_verbatim","state_province_verbatim"),all.x = T)

  rm(gnrs_out)

  #Push cleaned country names (along with species) through the NSR
    sp_x_poldiv <- unique(occs[c("scrubbed_country","scrubbed_state_province")])
    sp_x_poldiv$species <- spName(spN)

    #Don't try to run things through the NSR without a country.  It will break
    sp_x_poldiv <- sp_x_poldiv[which(sp_x_poldiv$scrubbed_country!=""),]


    nsr_out <- NSR::NSR_super_simple(species = sp_x_poldiv$species,
                                     country = sp_x_poldiv$scrubbed_country,
                                     state_province = sp_x_poldiv$scrubbed_state_province)

    #prune nsr output for purposes of wallace
    nsr_out <- nsr_out[c("country","state_province","native_status","native_status_reason","native_status_sources","isIntroduced" )]
    rm(sp_x_poldiv)

  #Append NSR output to occs
  occs <- merge(x = occs,
                y = nsr_out,
                by.x = c("scrubbed_country","scrubbed_state_province"),
                by.y=c("country","state_province"),all.x = T)
  rm(nsr_out)

  })

  return(occs)
}
