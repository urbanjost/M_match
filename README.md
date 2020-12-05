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
                   Must be subsets of a-z, A-Z, or 0-9
    >           ^  negates the set if the first character inside a set of characters.

This code functions, but a more complete (by todays' expectations)
pure-Fortran implementation is planned.

It is hoped this particular module will be useful primarily because it
is a native implementation in Fortran, albeit it is not as optimized or
as feature-rich as several common regex C libraries that, when available,
can be accessed via an ISO_C_BINDING in modern Fortran compilers.

```bash
    git clone https://github.com/urbanjost/M_change.git
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
    PROGRAMS/findchange '%!' <M_change.f90
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
## SUPPORTS FPM

   Alternatively, download the github repository and build it with
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

   ```bash
        git clone https://github.com/urbanjost/M_change.git
        cd M_change
        fpm build
        fpm test
   ```

   or just list it as a dependency in your fpm.toml project file.

```toml
        [dependencies]
        M_change        = { git = "https://github.com/urbanjost/M_change.git" }
```
