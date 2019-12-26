# M_change
subset of Regular Expressions implemented in Fortran

An __extremely__ preliminary implementation in Fortran of a subset of
regular expressions as described in "Software Tools" by Kernighan and
Plauger, 1976.

The metacharacters used are currently true to the original code and
vary from those typically used in Unix regular expressions:

    > ?  any single character
    > *  any number (including zero) of repeats of previous character
    > %  beginning of line
    > $  end of line
    > @  escape next metacharacter
    > [] a set of characters. In a set of characters
    >    -  specifies a range of characters in a set of characters.
    >    ^  negates the set if the first character inside a set of characters.

This code functions, but needs a lot of work. This is a placeholder for a more
complete implementation so that others interested in the code can have early
access to it.

There are times when it is inconvenient to depend on external C libraries for
processing regular expressions from Fortran. It is hoped this will be useful
primarily because it is a native implementation in Fortran, albeit it is not
as optimized or as feature-rich as several common regex C libraries.

    git clone https://github.com/urbanjost/M_change.git
    cd M_calculator/src
    # change Makefile if not using gfortran(1)
    make
    # run simple example program
    PROGRAMS/findchange '%!' <M_change.f90
