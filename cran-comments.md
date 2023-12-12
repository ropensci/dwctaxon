# CRAN submission for dwctaxon 2.0.3

This fixes a violation of CRAN's policy on internet access.

dwctaxon has no reverse dependencies.

Thank you for reviewing this submission.

## Test environments

* local OS X install, R 4.3.2 (devtools::check(remote = TRUE, manual = TRUE))
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* Windows Server 2022, R-devel, 64 bit (rhub)
* Windows Server 2022, x86_64-w64-mingw32, 64-bit (win-builder)

No ERRORs were reported.

## NOTEs

* All test environments issue the following NOTE:

```
New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-07-10 for policy violation.

  On Internet access.
```

This is correct; the package was removed from CRAN because it could not be updated in time. The current submission fixes the violation
(https://github.com/ropensci/dwctaxon/commit/e7f71308871f9b0cf9244b6d852a71039555bc61)

* The following NOTE was found on Ubuntu Linux and Fedora Linux:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This seems to be due to [rhub lacking tidy](https://github.com/r-hub/rhub/issues/548)

* The following NOTE was found on Windows Server 2022, R-devel, 64 bit (rhub):

```
Found the following files/directories:
    'lastMiKTeXException'
```

As previously noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), `'lastMiKTeXException'` has been flagged as a bug in MiKTeK and probably can be safely ignored.
