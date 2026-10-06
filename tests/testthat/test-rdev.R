## A stand-in for an R executable: rdev_init() only checks that it exists
## and is executable, the real one is only needed by rdev_check()
fake_r <- function(dir = tempfile("fake-r-")) {
  dir.create(file.path(dir, "bin"), recursive = TRUE)
  path <- file.path(dir, "bin", "R")
  writeLines(c("#!/bin/sh", "exit 0"), path)
  Sys.chmod(path, "0755")
  path
}

fake_pkg <- function(dir = tempfile("fake-pkg-")) {
  dir.create(dir)
  writeLines(c("Package: fakepkg", "Version: 0.0.1"), file.path(dir, "DESCRIPTION"))
  dir
}

test_that("rdev_init() creates a root that pkg_check() and dir_find() accept", {
  skip_on_os("windows")

  root <- tempfile("rdev-root-")
  r_old <- fake_r()
  r_new <- fake_r()

  expect_identical(rdev_init(root, r_old, r_new), normalizePath(root))
  on.exit(db_disconnect(normalizePath(root)), add = TRUE)
  root <- normalizePath(root)

  expect_true(is_rdev(root))
  expect_identical(pkg_check(root), root)
  expect_identical(rdev_root(root), root)

  config <- rdev_config(root)
  expect_identical(config$r_old, normalizePath(r_old))
  expect_identical(config$r_new, normalizePath(r_new))
  expect_false(config$bioc)
  expect_null(config$cache_dir)

  ## The root is the check directory itself, and there is no package under
  ## test with old/new libraries
  expect_identical(dir_find(root, "root"), root)
  expect_identical(dir_find(root, "db"), file.path(root, "data.sqlite"))
  expect_null(dir_find(root, "old"))
  expect_null(dir_find(root, "new"))
  expect_identical(dir_find(root, "pkgold", "foo"), dir_find(root, "pkg", "foo"))
  expect_identical(dir_find(root, "pkgnew", "foo"), dir_find(root, "pkg", "foo"))
  expect_true(startsWith(dir_find(root, "tools"), root))
  expect_true(startsWith(dir_find(root, "cache"), root))

  expect_true(db_exists(root))
  expect_identical(db_metadata_get(root, "todo"), "install")
  expect_length(db_metadata_get(root, "package"), 0)

  opts <- rdev_options(root)
  expect_identical(opts$r_old, normalizePath(r_old))
  expect_identical(opts$cache, dir_find(root, "cache"))
  expect_true(opts$reuse_old)
  expect_identical(rdev_cache_env(opts), c(CRANCACHE_DIR = opts$cache))
  expect_identical(rdev_cache_env(NULL), character())
  expect_identical(
    rdev_lib_env(c("/a", "/b")),
    c(R_LIBS_USER = "/a:/b", R_LIBS_SITE = "/a:/b")
  )
})

test_that("rdev_init() keeps the database of an existing root", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  db_todo_add(root, "foo")
  db_metadata_set(root, "todo", "done")

  rdev_init(root, fake_r(), fake_r(), cache_dir = tempfile("cache-"))
  expect_identical(db_todo(root), "foo")
  expect_identical(db_metadata_get(root, "todo"), "install")
  expect_identical(rdev_options(root)$cache, rdev_config(root)$cache_dir)
})

test_that("rdev_r_binary() accepts an R home and rejects missing files", {
  skip_on_os("windows")

  r <- fake_r()
  expect_identical(rdev_r_binary(r), normalizePath(r))
  expect_identical(rdev_r_binary(dirname(dirname(r))), normalizePath(r))
  expect_error(rdev_r_binary(tempfile()), "not found")
  expect_error(rdev_r_binary(tempdir()), "not found")
})

test_that("rdev roots and package directories are told apart", {
  skip_on_os("windows")

  pkg <- fake_pkg()
  expect_false(is_rdev(pkg))
  expect_error(rdev_root(pkg), "not a directory created by")
  expect_error(rdev_init(pkg, fake_r(), fake_r()), "must not be a package")

  plain <- tempfile()
  dir.create(plain)
  expect_error(pkg_check(plain), "DESCRIPTION file, or be a root")
})

test_that("rdev_tools_built_for() reads the R version from the Built field", {
  tools <- tempfile("tools-")
  expect_null(rdev_tools_built_for(tools))

  ## installed.packages() only looks at the installed metadata
  for (pkg in c("crancache", "withr")) {
    dir.create(file.path(tools, pkg, "Meta"), recursive = TRUE)
    desc <- c(
      Package = pkg,
      Version = "1.0.0",
      Built = "R 4.7.0; ; 2026-02-20 10:00:00 UTC; unix"
    )
    saveRDS(
      list(DESCRIPTION = desc),
      file.path(tools, pkg, "Meta", "package.rds")
    )
  }
  expect_identical(rdev_tools_built_for(tools), "4.7")

  unlink(file.path(tools, "withr"), recursive = TRUE)
  expect_null(rdev_tools_built_for(tools))
})

test_that("rdev_r_info() fingerprints a build by more than its version", {
  skip_on_cran()
  skip_on_os("windows")

  rbin <- file.path(R.home("bin"), "R")
  info <- rdev_r_info(rbin)

  expect_identical(info$version, R.version.string)
  expect_identical(
    info$minor,
    paste(R.version$major, sub("[.].*$", "", R.version$minor), sep = ".")
  )
  expect_true(startsWith(info$fingerprint, paste0(rbin, " | ", info$version)))
  expect_match(info$fingerprint, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")

  ## The same R reached through another path is a different build
  alias <- file.path(tempfile("alias-"), "R")
  dir.create(dirname(alias))
  file.symlink(rbin, alias)
  expect_false(identical(rdev_r_info(alias)$fingerprint, info$fingerprint))
})

test_that("tarball_version()", {
  expect_identical(tarball_version("/x/y/foo_1.2.3.tar.gz"), "1.2.3")
  expect_identical(tarball_version("data.table_1.15.0.tar.gz"), "1.15.0")
  expect_identical(tarball_version("foo_1.0-1.tar.gz"), "1.0-1")
})

test_that("old results are only reused for complete checks of the same version", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))

  db_insert(
    root,
    "foo",
    version = "1.0",
    status = "OK",
    which = "old",
    duration = 1,
    starttime = Sys.time(),
    result = "{}",
    summary = NULL
  )
  expect_true(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))
  expect_false(rdev_old_result_usable(root, "foo", "foo_1.1.tar.gz"))
  expect_false(rdev_old_result_usable(root, "bar", "bar_1.0.tar.gz"))

  for (status in c("TIMEOUT", "PREPERROR")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = status,
      which = "old",
      duration = 1,
      starttime = Sys.time(),
      result = "{}",
      summary = NULL
    )
    expect_false(rdev_old_result_usable(root, "foo", "foo_1.0.tar.gz"))
  }
})

test_that("rdev_invalidate_old() drops old results and re-queues done packages", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  db_todo_add(root, "foo")
  for (which in c("old", "new")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = "OK",
      which = which,
      duration = 1,
      starttime = Sys.time(),
      result = "{}",
      summary = NULL
    )
  }
  expect_identical(db_todo_status(root)$status, "done")

  rdev_invalidate_old(root)

  expect_identical(db_get_results(root, NULL)$old$package, character())
  expect_identical(db_get_results(root, NULL)$new$package, "foo")
  expect_identical(db_todo(root), "foo")
})

test_that("results from an rdev root are tagged for the reports", {
  skip_on_os("windows")

  root <- rdev_init(tempfile("rdev-root-"), fake_r(), fake_r())
  on.exit(db_disconnect(root), add = TRUE)

  check <- structure(
    list(
      package = "foo",
      version = "1.0",
      status = 0L,
      timeout = FALSE,
      errors = character(),
      warnings = character(),
      notes = "N1"
    ),
    class = "rcmdcheck"
  )
  for (which in c("old", "new")) {
    db_insert(
      root,
      "foo",
      version = "1.0",
      status = "NOTE",
      which = which,
      duration = 1,
      starttime = Sys.time(),
      result = unclass(toJSON(check)),
      summary = NULL
    )
  }

  results <- db_results(root, NULL)
  expect_identical(results$foo$new$type, "rdev")
  expect_identical(side_labels(results$foo), list(old = "Old R", new = "New R"))
  expect_identical(
    side_labels(list(new = list(type = NULL))),
    list(old = "CRAN", new = "Devel")
  )
})

test_that("rdev_packages() resolves selections against the repositories", {
  skip_on_cran()
  skip_if_offline()

  expect_identical(rdev_packages(), character())

  expect_warning(
    pkgs <- rdev_packages(c("prettyunits", "notarealpackage123")),
    "notarealpackage123"
  )
  expect_identical(pkgs, "prettyunits")

  pkgs <- rdev_packages(revdeps_of = "prettyunits")
  expect_true("prettyunits" %in% pkgs)
  expect_gt(length(pkgs), 1)
  expect_identical(pkgs, pkgs[order(tolower(pkgs))])
})

test_that("rdev_check_process runs R CMD check with the given R executable", {
  skip_on_cran()
  skip_on_os("windows")

  pkg <- tempfile("rdevtest")
  dir.create(pkg)
  writeLines(
    c(
      "Package: rdevtest",
      "Version: 0.0.1",
      "Title: Test Package",
      "Description: A test package.",
      "License: MIT + file LICENSE",
      "Encoding: UTF-8",
      "Authors@R: person('A', 'B', email = 'a@b.com', role = c('aut', 'cre'))"
    ),
    file.path(pkg, "DESCRIPTION")
  )
  writeLines(c("YEAR: 2026", "COPYRIGHT HOLDER: A B"), file.path(pkg, "LICENSE"))
  writeLines("", file.path(pkg, "NAMESPACE"))
  tarball <- pkgbuild::build(pkg, dest_path = tempdir(), quiet = TRUE)

  out <- tempfile("rdevtest-out")
  dir.create(out)

  ## The current R, but selected by path like a foreign build would be
  px <- rdev_check_process$new(
    tarball,
    args = c("--no-manual", "-o", out),
    arch = file.path(R.home("bin"), "R")
  )
  on.exit(px$kill(), add = TRUE)

  while (px$is_alive()) {
    px$poll_io(1000)
    px$read_output()
  }

  res <- px$parse_results()
  expect_s3_class(res, "rcmdcheck")
  expect_identical(res$package, "rdevtest")
  expect_identical(res$version, "0.0.1")
  expect_length(res$errors, 0)
  expect_true(file.exists(file.path(out, "rdevtest.Rcheck", "00check.log")))
})
