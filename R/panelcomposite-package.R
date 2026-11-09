## usethis namespace: start
#' @import magick
#' @importFrom rlang abort warn
## usethis namespace: end
NULL




## Helper function for dealing with different systems when comparing images
fetch_acceptable_distortion <- function(verbose = TRUE) {
    if (Sys.info()[["sysname"]] == "Linux") {
        if (verbose) {cli_alert_info("Linux detected. Setting font to 'Liberation Sans' for Arial/Helvetica metric compability.")}
        ggplot2::theme_update(text = element_text(family = "Liberation Sans"))
    }

    if (Sys.info()[["sysname"]] == "Darwin" && Sys.info()[["user"]] %in% c("evelyn", "ejad042")) {
        if (verbose) {cli_alert_info("Evelyn's macbook detected, using extremely strict matching")}
        acceptable_distortion <- 0.000001
    } else if (Sys.getenv("NOT_CRAN") == "false" || Sys.getenv("GITHUB_ACTIONS") == "true") {
        if (verbose) {cli_alert_info("GitHub actions/CRAN environment detected. Allowing lenience in plot matching.")}

        if (Sys.info()[["sysname"]] == "Linux") {
            acceptable_distortion <- 0.05
        }  else if (Sys.info()[["sysname"]] == "Windows") {
            if (verbose) {cli_alert_info("Windows detected. Not taking any special action.")}
            acceptable_distortion <- 0.05
        } else if (Sys.info()[["sysname"]] == "Darwin") {
            if (verbose) {cli_alert_info("MacOS (Darwin) detected. Giving a little less lenience in plot matching as Helvetica is available.")}
            acceptable_distortion <- 0.025
        } else {
            abort("Operating system not Linux/Windows/Darwin. Don't know what to do. Evelyn should take a look at this.", class = "unrecognised_OS")
        }

    } else {
        if (verbose) {cli_alert_info("Unknown local system detected, using lenient matching")}
        acceptable_distortion <- 0.05
    }
    if (verbose) {cli_alert_info(paste("Current acceptable distortion:", acceptable_distortion))}

    return(acceptable_distortion)
}

