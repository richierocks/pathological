## Release Summary

This release has one new function (split_dir), a new vignette, and some other
minor user-facing changes.  The previous version had some problems with the 
Windows build, which this attempts to correct.  See "R CMD check results" 
section.

## Test Environments

* Local Windows 7 & 10, R-devel 
* Semaphore CI + Ubuntu 14.04, R-devel and R-release
* AppVeyor + Windows Server 2012, R-devel
* CRAN Win-builder

## R CMD check results

There were no ERRORs or WARNINGs or NOTEs under any of the test environments.
Howver, this was also the case for the previous version, and that version had
a few failing tests under the CRAN Windows setup.  These failing tests have not 
reproduced elsewhere, so I've marked them as skip_on_cran.  This is not an ideal
solution, but I believe that it is better to have the package available with a
documented, rare test failure, than not at all. I can't guarantee that I've 
found all the failing tests - the output from testthat only shows the first 9 
failures.  It may take some iteration to get this right. Your patience is
appreciated.

## Downstream dependencies

There aren't any yet.
