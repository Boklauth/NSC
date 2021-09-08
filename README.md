# NSC

A Function to Create a National Student Clearinghouse Data Layout

## Description

This package contains the NSC function `NSC()`, which formats your data layout according to the National Student Clearinghouse system to obtain the information you need. Also, it checks some essential points so that your data are accepted by the NSC system's standards. Some of the features the NSC does are:
- remove white space and special character in names in names, 
- check for required search dates allowed by the NSC and suggested a search date, 
- provides many ways to read your data set (i.e., txt, xlsx, xls, csv, and an R object), and 
- produced a tab delimited output (txt) file, which is ready for submission. 

## Examples and evaluated help files are available in the R

Various examples are provided in the help file in R by typing:

```{r}
?NSC()
```

## Installing from source

It's recommended to install from Github:

1) Install the `devtools` package (if necessary). In R, paste the following into the console:

```r
install.packages('devtools')
```

2) Load the `devtools` package (requires version 1.4+) and install from the Github source code.

```r
library('devtools')
install_github('Boklauth/NSC')
```

### Installing from source via git

If the `devtools` approach does not work on your system, then you can download and install the
repository directly. 

1) Obtain recent gcc, g++, and gfortran compilers (see above instructions).

2) Install the [git command line tools](https://git-scm.com/downloads).

3) Open a terminal/command-line tool. The following code will download the repository 
code to your computer, and install the package directly using R tools 
(Windows users may also have to add R and git to their 
[path](https://www.computerhope.com/issues/ch000549.htm))

```
git clone https://github.com/Boklauth/NSC
R CMD INSTALL NSC
```

## Licence

This package is free and open source software, licensed under [MIT](https://opensource.org/licenses/MIT).

## Bugs and Questions

Bug reports are always welcome and the preferred way to address these bugs is through
the Github 'issues'. Feel free to submit issues or feature requests on the site, and I'll
address them ASAP.

## What doy you want to see in the package?

This is a simple package. If you wish the package can do more things, shoot me with your ideas.