# M_change
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
    >           ^  negates the set if the first character inside a set of characters.

This code functions, but a more complete (bye todays' expectations)
pure-Fortran implementation is planned.

It is hoped this particular module will be useful primarily because it
is a native implementation in Fortran, albeit it is not as optimized or
as feature-rich as several common regex C libraries that, when available,
can be accessed via an ISO_C_BINDING in modern Fortran compilers.

```bash
    git clone https://github.com/urbanjost/M_change.git
    cd M_calculator/src
    # change Makefile if not using gfortran(1)
    make
    # run simple example program
    PROGRAMS/findchange '%!' <M_change.f90
```
