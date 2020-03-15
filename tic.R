do_package_checks(args = "--as-cran")

if (ci_on_ghactions()) {
  do_pkgdown()
}
