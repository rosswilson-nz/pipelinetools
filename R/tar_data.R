#' Figure image file output target
#'
#' Target factory for data import targets. Creates a pair of targets prefixed
#'     `file_` and `raw_` for the data file and imported data objects,
#'     respectively.
#'
#' See [targets::tar_target()] for details of all arguments other than
#'     `filename` and those passed through `...`.
#'
#' @param name Symbol, name stub for the target. Will be given prefixes `file_`
#'     and `raw_` for the file and imported raw data targets, respectively.
#' @param filename String, filename stub for the saved image files.
#' @param fn Function to read the data file into R.
#' @param ... Additional arguments passed on to `fn`.
#' @param tidy_eval Logical, whether to enable tidy evaluation when interpreting
#'     command and pattern.
#' @param packages Character vector of packages to load right before the target
#'     runs.
#' @param library Character vector of library paths to try when loading
#'     packages.
#' @param format Optional storage format for the data target's return value.
#' @param error Character of length 1, what to do if the target
#'   stops and throws an error. Options:
#'   * `"stop"`: the whole pipeline stops and throws an error.
#'   * `"continue"`: the whole pipeline keeps going.
#'   * `"null"`: The errored target continues and returns `NULL`.
#'     The data hash is deliberately wrong so the target is not
#'     up to date for the next run of the pipeline. In addition,
#'     as of `targets` version 1.8.0.9011, a value of `NULL` is given
#'     to upstream dependencies with `error = "null"` if loading fails.
#'   * `"abridge"`: any currently running targets keep running,
#'     but no new targets launch after that.
#'   * `"trim"`: all currently running targets stay running. A queued
#'     target is allowed to start if:
#'
#'       1. It is not downstream of the error, and
#'       2. It is not a sibling branch from the same [targets::tar_target()] call
#'         (if the error happened in a dynamic branch).
#'
#'     The idea is to avoid starting any new work that the immediate error
#'     impacts. `error = "trim"` is just like `error = "abridge"`,
#'     but it allows potentially healthy regions of the dependency graph
#'     to begin running.
#'   (Visit <https://books.ropensci.org/targets/debugging.html>
#'   to learn how to debug targets using saved workspaces.)
#' @param memory Character of length 1, memory strategy. Possible values:
#'   * `"auto"` (default): equivalent to `memory = "transient"` in almost
#'     all cases. But to avoid superfluous reads from disk,
#'     `memory = "auto"` is equivalent to `memory = "persistent"` for
#'     for non-dynamically-branched targets that other targets
#'     dynamically branch over. For example: if your pipeline has
#'     `tar_target(name = y, command = x, pattern = map(x))`,
#'     then `tar_target(name = x, command = f(), memory = "auto")`
#'     will use persistent memory for `x`
#'     in order to avoid rereading all of `x`
#'     for every branch of `y`.
#'   * `"transient"`: the target gets unloaded
#'     after every new target completes.
#'     Either way, the target gets automatically loaded into memory
#'     whenever another target needs the value.
#'   * `"persistent"`: the target stays in memory
#'     until the end of the pipeline (unless `storage` is `"worker"`,
#'     in which case `targets` unloads the value from memory
#'     right after storing it in order to avoid sending
#'     copious data over a network).
#' @param garbage_collection Logical: TRUE to run base::gc() just before the
#'     target runs, in whatever R process it is about to run (which could be a
#'     parallel worker). FALSE to omit garbage collection.
#' @param deployment Character of length 1. If deployment is "main", then the
#'     target will run on the central controlling R process. Otherwise, if
#'     deployment is "worker" and you set up the pipeline with
#'     distributed/parallel computing, then the target runs on a parallel
#'     worker.
#' @param resources Object returned by tar_resources() with optional settings
#'     for high-performance computing functionality, alternative data storage
#'     formats, and other optional capabilities of targets.
#' @param storage Character string to control when the output of the target
#'   is saved to storage. Only relevant when using `targets`
#'   with parallel workers (<https://books.ropensci.org/targets/crew.html>).
#'   Must be one of the following values:
#'   * `"worker"` (default): the worker saves/uploads the value.
#'   * `"main"`: the target's return value is sent back to the
#'     host machine and saved/uploaded locally.
#'   * `"none"`: `targets` makes no attempt to save the result
#'     of the target to storage in the location where `targets`
#'     expects it to be. Saving to storage is the responsibility
#'     of the user. Use with caution.
#' @param retrieval Character string to control when the current target
#'   loads its dependencies into memory before running.
#'   (Here, a "dependency" is another target upstream that the current one
#'   depends on.) Only relevant when using `targets`
#'   with parallel workers (<https://books.ropensci.org/targets/crew.html>).
#'   Must be one of the following values:
#'   * `"auto"` (default): equivalent to `retrieval = "worker"` in almost all
#'     cases. But to avoid unnecessary reads from disk, `retrieval = "auto"`
#'     is equivalent to `retrieval = "main"` for dynamic branches that
#'     branch over non-dynamic targets. For example: if your pipeline has
#'     `tar_target(x, command = f())`, then
#'     `tar_target(y, command = x, pattern = map(x), retrieval = "auto")`
#'     will use `"main"` retrieval in order to avoid rereading all of `x`
#'     for every branch of `y`.
#'   * `"worker"`: the worker loads the target's dependencies.
#'   * `"main"`: the target's dependencies are loaded on the host machine
#'     and sent to the worker before the target runs.
#'   * `"none"`: `targets` makes no attempt to load its
#'     dependencies. With `retrieval = "none"`, loading dependencies
#'     is the responsibility of the user. Use with caution.
#' @param cue An optional object from tar_cue() to customize the rules that
#'     decide whether the target is up to date.
#' @param description Character of length 1, a custom free-form human-readable
#'     text description of the target.
#'
#' @returns A list of two [targets::tar_target()] objects: one with 'file'
#'     storage format (data file), one with standard target format (a
#'     tibble/data frame).
#'
#' @export
tar_data <- function(
  name,
  filename,
  fn,
  ...,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
  deployment = targets::tar_option_get("deployment"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  name_base <- targets::tar_deparse_language(substitute(name))
  targets::tar_assert_chr(name_base)
  targets::tar_assert_nzchar(name_base)

  targets::tar_assert_lgl(tidy_eval)

  envir <- targets::tar_option_get("envir")
  filename <- as.expression(substitute(filename))
  targets::tar_assert_nonmissing(
    filename[[1]],
    paste("target", name_base, "has no filename.")
  )
  filename <- targets::tar_tidy_eval(filename, envir, tidy_eval)
  filename <- as.character(filename)
  targets::tar_assert_chr(filename)
  targets::tar_assert_nzchar(filename)

  pattern <- as.expression(substitute(pattern))
  pattern <- targets::tar_tidy_eval(pattern, envir, tidy_eval)
  garbage_collection <- isTRUE(garbage_collection)

  name_file <- paste0("file_", name_base)
  name_raw <- paste0("raw_", name_base)
  file <- as.symbol(name_file)

  targets::tar_assert_nonmissing(fn)
  fn <- substitute(fn)
  filename <- fs::path("raw_data", filename)

  command <- as.expression(substitute(fn(file, ...)))
  targets::tar_assert_nonmissing(
    command[[1]],
    paste("target", name_base, "has no `fn`.")
  )
  command <- targets::tar_tidy_eval(command, envir, tidy_eval)
  list(
    targets::tar_target_raw(
      name = name_file,
      command = filename,
      packages = union(packages, "pipelinetools"),
      library = library,
      format = "file",
      repository = targets::tar_option_get("repository"),
      iteration = targets::tar_option_get("iteration"),
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      resources = resources,
      storage = storage,
      retrieval = retrieval,
      cue = cue,
      description = description
    ),
    targets::tar_target_raw(
      name = name_raw,
      command = command,
      packages = union(packages, "pipelinetools"),
      library = library,
      format = format,
      repository = targets::tar_option_get("repository"),
      iteration = targets::tar_option_get("iteration"),
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = deployment,
      resources = resources,
      storage = storage,
      retrieval = retrieval,
      cue = cue,
      description = description
    )
  )
}
