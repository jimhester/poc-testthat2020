#' @export
run_tests <- function() {
  processes <- list()
  test_files <- list.files("tests", pattern = "test[-_].+[.][Rr]$", full.names = TRUE)

  for (i in seq_along(test_files)) {
    processes[[i]] <- callr::rscript_process$new(callr::rscript_process_options(script = test_files[[i]]))
  }
  processes
}

results <- new.env(parent = baseenv())

#' @export
test_that <- function(title, code) {
  withCallingHandlers(
    testthat_success = success_handler,
    testthat_failure = failure_handler,
  tryCatch(
    error = error_handler,
    code
  )
  )
  if (!isTRUE(results$finalizer_set)) {
    reg.finalizer(parent.frame(), function(e) {
      testthat_results()
  }, onexit = TRUE)

    results$finalizer_set <- TRUE

  }
}

#' @export
testthat_results <- function() {
  cat(sprintf(
      "successes: %s\nfailures: %s\nerrors: %s\n", length(results$successes), length(results$failures), length(results$errors)))
  if (!interactive() && (length(results$failure) > 0 || length(results$errors) > 0)) {
    q(save = "no", status = 1, runLast = FALSE)
  }
}

#' @export
expect_equal <- function(x, y, ...) {
  res <- waldo::compare(x, y, ...)

  if (length(res) == 0) {
    signalCondition(structure(list(message = "succeeded"), class = c("testthat_success", "condition")))
    return()
  }

  signalCondition(structure(list(message = "failed"), class = c("testthat_failure", "condition")))
}

error_handler <- function(e) {
  results$errors <- append(results$errors, list(e))

}

failure_handler <- function(e) {
  results$failures <- append(results$failures, list(e))
}

success_handler <- function(e) {
  results$successes <- append(results$successes, list(e))
}
