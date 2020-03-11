do_package_checks()

if (ci_on_ghactions()) {
  do_pkgdown()
}
