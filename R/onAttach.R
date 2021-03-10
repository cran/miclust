### welcome message
#' @importFrom utils packageDescription
.onAttach <- function(lib, pkg) {
  meta <- utils::packageDescription("miclust")
  attachmsg <- paste0("\nThis is miclust ", meta$Version,
                      ". For details, use:\n",
                      "> help(package = 'miclust')\n\n",
                      "To cite the methods in the package use:\n",
                      "> citation('miclust')\n")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}
