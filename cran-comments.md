# CRAN submission for dwctaxon 2.0.4

(resubmission)

This fixes a violation of CRAN's policy on internet access, where a file needed for a vignette could not be downloaded on MKL.

It also adds a dependency on R >= 4.2.0 because the code includes the base pipe.

dwctaxon has no reverse dependencies.

Thank you for reviewing this submission.

## Test environments

* local OS X install, R 4.5.0 (devtools::check(remote = TRUE, manual = TRUE))
* atlas R Under development (unstable) (2025-12-13 r89166) on Fedora Linux 38 (rhub)
* c23 R Under development (unstable) (2025-12-12 r89163) on Ubuntu 22.04.5 LTS (rhub)
* gcc-asan R Under development (unstable) (2025-12-13 r89166) on Fedora Linux 40 (rhub)
* lto R version 4.5.2 (2025-10-31) on Ubuntu 24.04.3 LTS (rhub)
* mkl R Under development (unstable) (2025-12-13 r89166) on Fedora Linux 38 (rhub)
* ubuntu-next R version 4.5.2 Patched (2025-12-12 r89163) on Ubuntu 24.04.3 LTS (rhub)
* valgrind R Under development (unstable) (2025-12-13 r89166) on Fedora Linux 38 (rhub)
* linux (R-devel) (rhub)
* windows (R-devel) (rhub)

rhub run summary: https://github.com/ropensci/dwctaxon/actions/runs/20098165281

No ERRORs were reported.

## NOTEs

local OS install generated this NOTE:

  Found the following (possibly) invalid URLs:
    URL: https://www.gbif.org/
      From: inst/doc/what-is-dwc.html
      Status: 403
      Message: Forbidden
    URL: https://www.gbif.org/darwin-core
      From: inst/doc/what-is-dwc.html
      Status: 403
      Message: Forbidden


Both URLs have been confirmed to properly work in a web browser (Firefox v144.0.2).