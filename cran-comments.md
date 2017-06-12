## Test environments
* local macOS 10.12.5 installation, R 3.4.0
* ubuntu 14.04 (on travis-ci), R 3.3.3, R 3.4.0, R-devel
* OSX 10.11.6, Xcode 7.3.1 (on travis-ci), R 3.3.3, R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 1 warnings | 0 notes

* checking top-level files ... WARNING
Conversion of 'README.md' failed:
pandoc.exe: Could not fetch https://img.shields.io/badge/R%3E%3D-2.10-6666ff.svg
TlsExceptionHostPort (HandshakeFailed Error_EOF) "img.shields.io" 443

This can be ignored as the README builds fine even with error.

## Reverse dependencies

There are no reverse dependencies.

## Downstream dependencies

"There are currently no downstream dependencies for this package”
