#' @importFrom R6 R6Class

## An rcmdcheck_process that runs `R CMD check` with another R build.
##
## rcmdcheck::rcmdcheck_process always runs the R of the current session.
## This subclass takes the path of an R executable as well, and only accepts
## a source package tarball, which is all we need. Results are parsed by the
## inherited parse_results() method.

rdev_check_process <- R6Class(
  "rdev_check_process",
  inherit = rcmdcheck::rcmdcheck_process,
  public = list(
    initialize = function(
      path,
      args = character(),
      libpath = .libPaths(),
      repos = getOption("repos"),
      env = character(),
      arch = "same"
    ) {
      rdev_process_init(
        self,
        private,
        super,
        path,
        args,
        libpath,
        repos,
        env,
        arch
      )
    }
  )
)

rdev_process_init <- function(
  self,
  private,
  super,
  path,
  args,
  libpath,
  repos,
  env,
  arch
) {
  path <- normalizePath(path, mustWork = TRUE)
  if (file.info(path)$isdir) {
    stop("`path` must be a source package tarball", call. = FALSE)
  }

  ## Same layout as rcmdcheck: the tarball is checked from a scratch directory
  check_dir <- tempfile("rdev-check-")
  dir.create(check_dir, recursive = TRUE)
  targz <- file.path(check_dir, basename(path))
  file.copy(path, targz)

  description <- desc::desc(file = path)
  package <- description$get("Package")[[1]]

  private$description <- description
  private$path <- path
  private$check_dir <- check_dir
  private$targz <- targz
  ## so that the inherited parse_results() removes the scratch directory
  private$tempfiles <- check_dir

  chkenv <- callr::rcmd_safe_env()
  if (length(env)) {
    chkenv[names(env)] <- env
  }

  libdir <- file.path(check_dir, paste0(package, ".Rcheck"))
  options <- callr::rcmd_process_options(
    cmd = "check",
    cmdargs = c(basename(targz), args),
    libpath = c(libdir, libpath),
    repos = repos,
    user_profile = FALSE,
    stderr = "2>&1",
    env = chkenv,
    arch = arch
  )

  ## Skip rcmdcheck's initialize(), which would build the package and start
  ## the current R, and go straight to callr's
  callr_init <- super$.__enclos_env__$super$initialize
  withr::with_dir(check_dir, callr_init(options))

  invisible(self)
}
