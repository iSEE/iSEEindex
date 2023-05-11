
dataset_fun <- function() {
    x <- yaml::read_yaml(system.file(package = "iSEEindex", "example.yaml"))
    x$datasets
}
