# CRAN submission for dwctaxon 2.0.2

2023-05-30
## Resubmission

* Reset any global options changed with options() in an example or vignette to their state before examples were run.

dwctaxon has never previously been on CRAN, so it has no reverse dependencies.

Thank you for reviewing this submission.

## Test environments

* local OS X install, R 4.2.0 (devtools::check(remote = TRUE, manual = TRUE))
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* Windows Server 2022, x86_64-w64-mingw32 (64-bit) (win-builder)

No ERRORs were reported.

## NOTEs

* All test environments issue the NOTE that this is a new submission, which is correct.

* The following NOTE was found on Ubuntu Linux and Fedora Linux:

```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```

This seems to be due to [rhub lacking tidy](https://github.com/r-hub/rhub/issues/548)
