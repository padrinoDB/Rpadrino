This is a submission of Rpadrino 0.0.3. It (hopefully) corrects an error on tests run on linux x86-64.

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

Possibly misspelled words in DESCRIPTION:
  Easterling (38:57)
  IPM (2:36)
  Levin (41:64)
  Merow (39:58)
  Metcalf (41:7)
  Rees (40:34)
  al (38:71, 39:67, 40:42, 41:18, 41:73)
  et (38:68, 39:64, 40:39, 41:15, 41:70)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2021-11-30 as check problems were not
    corrected in time.
    
As stated above, this is an attempt to un-archive the package based on the failing test.
