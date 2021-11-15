This is a resubmission of the first submission of Rpadrino. We have added "'"s around software names in the DESCRIPTION, updated the documentation to include \value tags for all exported functions, and ensured our tests now reset user graphical options in the testing suite. 

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

Possibly mis-spelled words in DESCRIPTION:
  IPM (2:34)
  PADRINO (2:26, 12:14)
  Rpadrino (15:33)
  
IPM is shorthand for Integral Projection Model. PADRINO and Rpadrino are names.
