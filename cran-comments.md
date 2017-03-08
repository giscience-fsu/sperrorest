## Test environments
* local macOS 10.12.3 installation, R 3.3.3
* ubuntu 14.04 (on travis-ci), R 3.3.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

New maintainer:
  Alexander Brenning <alexander.brenning@uni-jena.de>
Old maintainer(s):
  Alexander Brenning <brenning@uwaterloo.ca>

## Reverse dependencies

There are no reverse dependencies.

## Downstream dependencies

"There are currently no downstream dependencies for this package”

## Changelog to previous submission attempts

* Uses `parallel` package instead of `snow` ("PSOCK" cluster type)

* removed package `notifier` causing trouble on win-builder

* Confirmation of maintainer e-mail change: See e-mail to CRAN from 2017-03-07 from <alexander.brenning@uni-jena.de>
