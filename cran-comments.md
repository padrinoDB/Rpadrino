This is a patched version of Rpadrino 0.0.1, which errored on Solaris machines because Pandoc is not available for Solaris. We have updated our tests to run on Solaris in the absence of Pandoc, and a note about imported packages on Fedora.

Additional information on this submission:

## Test environments
* local R installation, Windows 10, R 4.1.0
* ubuntu 20.04.1 (GitHub Actions), devel, release, and oldrelease
* macOS Catalina 10.15.7 (GitHub Actions), release and oldrelease
* Windows server 2019 x64 (Github Actions), release, and oldrelease
* Windows server 2008 x86_64 (Win Builder), devel, release, and oldrelease
* Fedora Clang, gfortran (R-Hub) devel


## R CMD check results

0 errors | 0 warnings | 1 note on all platforms

Days since last update: 1

This patch is submitted as requested by a CRAN team member to fix issues on a Solaris machine. 
