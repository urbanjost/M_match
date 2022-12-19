# M_match
subset of Regular Expressions implemented in Fortran

A basic implementation in Fortran of a subset of regular expressions as
described in "Software Tools" by Kernighan and Plauger, 1976.

The original used different metacharacters than are commonly used today.

    > ORIG. NOW
    > ?      .  any single character
    > *         any number (including zero) of repeats of previous character
    > %      ^  beginning of line
    > $         end of line
    > @      \  escape next metacharacter
    > []        a set of characters. In a set of characters
    >           -  specifies a range of characters in a set of characters.
                   Must be subsets of a-z, A-Z, or 0-9
    >           ^  negates the set if the first character inside a set of characters.

This code functions, but a more complete (by todays' expectations)
pure-Fortran implementation is planned.

It is hoped this particular module will be useful primarily because it
is a native implementation in Fortran, albeit it is not as optimized or
as feature-rich as several common regex C libraries that, when available,
can be accessed via an ISO_C_BINDING in modern Fortran compilers.

```bash
    git clone https://github.com/urbanjost/M_match.git
    cd M_calculator/src
    # change Makefile if not using one of the listed compilers
     
    # for gfortran
    make clean
    make F90=gfortran gfortran
     
    # for ifort
    make clean
    make F90=ifort ifort

    # for nvfortran
    make clean
    make F90=nvfortran nvfortran

    # run simple example program
    PROGRAMS/findchange '%!' <M_match.f90
```

A simple example that creates a grep(1)-like filter program:

```fortran
program demo_m_match
use M_match,   only : getpat, match, regex_pattern
use M_match,   only : YES, NO, ERR
implicit none
character(len=1024) :: line='', argument=''
type(regex_pattern) :: p
integer             :: ios
   call get_command_argument(1,argument)
   if(argument.eq.'')stop 'missing regular expression'
   if (getpat(trim(argument), p%pat) .eq. ERR) then
      stop '*M_match* Illegal pattern.'
   endif
   INFINITE: do
      read(*,'(a)',iostat=ios)line
      if(ios.ne.0)exit INFINITE
      if (match(trim(line)//char(10), p%pat) .eq. YES) then
         write(*,'(*(g0,1x))')trim(line)
      endif
   enddo INFINITE
end program demo_m_match
```
## SUPPORTS FPM ![fpm](docs/images/fpm_logo.gif)

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

   ```bash
        git clone https://github.com/urbanjost/M_match.git
        cd M_match
        fpm build
        fpm test
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_match        = { git = "https://github.com/urbanjost/M_match.git" }
```

## DOCUMENTATION   ![docs](docs/images/docs.gif)

### USER
   - A single page that uses javascript to combine all the HTML
     descriptions of the man-pages is at 
     [BOOK_M_match](https://urbanjost.github.io/M_match/BOOK_M_match.html).

   - a simple index to the man-pages in HTML form for the
   [routines](https://urbanjost.github.io/M_match/man3.html) 

   - There are man-pages in the repository download in the docs/ directory
     that may be installed on ULS (Unix-Like Systems).

   - ![man-pages](docs/images/manpages.gif)
      + [manpages.zip](https://urbanjost.github.io/M_match/manpages.zip)
      + [manpages.tgz](https://urbanjost.github.io/M_match/manpages.tgz)

   - [CHANGELOG](docs/CHANGELOG.md) provides a history of significant changes

### DEVELOPER
   - [ford(1) output](https://urbanjost.github.io/M_match/fpm-ford/index.html).
<!--
   - [doxygen(1) output](https://urbanjost.github.io/M_match/doxygen_out/html/index.html).
-->
   - [github action status](docs/STATUS.md) 
---


## PASSES TESTS
```text
Sat Dec  5 01:26:24 EST 2020

GNU Fortran (GCC) 8.3.1 20191121 (Red Hat 8.3.1-5)
Copyright (C) 2018 Free Software Foundation, Inc.

ifort (IFORT) 19.1.3.304 20200925
Copyright (C) 1985-2020 Intel Corporation.  All rights reserved.

nvfortran 20.7-0 LLVM 64-bit target on x86-64 Linux -tp nehalem 
NVIDIA Compilers and Tools
Copyright (c) 2020, NVIDIA CORPORATION.  All rights reserved.
```
