library("iSEEindex")
library("BiocFileCache")
bfc <- BiocFileCache(cache = tempdir())


##
# iSEEindex ----
##
dataset_fun <- function() {
  x <- yaml::read_yaml("tonsil_package.yml")
  x$datasets
}
initial_fun <- function() {
  x <- yaml::read_yaml("tonsil_package.yml")
  x$initial
}
app <- iSEEindex_runr(bfc, dataset_fun, initial_fun,
                      default.add = FALSE,
                      app.title = "iSEE ❤️ Tonsil Data Atlas")
if (interactive()) {
  shiny::runApp(app, port = 1234)
}
app <- iSEEindex_runr(bfc, dataset_fun, initial_fun,
                      default.add = TRUE,
                      default.position = "last",
                      app.title = "iSEE ❤️ Tonsil Data Atlas")
if (interactive()) {
  shiny::runApp(app, port = 1234)
}





# the logic:



iSEEindex_runr

.landing_page_runr

specifically into .create_launch_observers_runr

.launch_isee_runr

iSEEindexRunrResource -> no need to precache


