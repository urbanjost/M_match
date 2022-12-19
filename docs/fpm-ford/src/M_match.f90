!09/22/1980  15:38:34
!04/19/2020  11:05:06
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    M_match(3fp) - [M_MATCH] Basic Regular Expressions
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_match, only: match, amatch, getpat, makpat
!!    use M_match, only: YES, MAXPAT, MAXARG, MAXLINE, EOS, NEWLINE, ERR
!!
!!##DESCRIPTION
!!    Find a string matching a regular expression.
!!
!!       *   zero or more occurrences of the previous character
!!       .   any character
!!       ^   beginning of line
!!       $   end of line
!!       []  class of characters. Inside the braces
!!
!!            ^  at the beginning of the class means to
!!               negate the class.
!!            -  if not the first or last character in
!!               the class, denotes a range of characters
!!       Escape characters:
!!        \\n  newline
!!        \\r  carriage return
!!        \\t  tab
!!        \\b  backspace
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
module M_match
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
implicit none
private
public :: getpat  !....... encode regular expression for pattern matching
public :: match   !....... match pattern anywhere on line
public :: amatch  !....... look for pattern matching regular expression
public :: makpat  !....... encode regular expression for pattern matching
public :: regex_pattern
public :: bpos, epos
private :: omatch
private :: error
private :: addset
private :: dodash
private :: locate
private :: patsiz
private :: stclos
private :: getccl
private :: filset
private :: esc

integer,parameter,public :: MAXTAGS=10

interface getpat;     module procedure getpat_, getpat__;           end interface
interface makpat;     module procedure makpat_          ;           end interface
interface amatch;     module procedure amatch_, amatch__;           end interface
interface match;      module procedure match_,  match__ ;           end interface
interface omatch;     module procedure omatch_          ;           end interface

!========== STANDARD RATFOR DEFINITIONS ==========
!x!integer,parameter :: CHARACTER=INTEGER
integer,parameter :: chr=kind(1)
integer,parameter :: byte=kind(1)
integer,parameter :: def=kind(1)
!x!integer,parameter :: ANDIF=IF

integer(kind=byte),parameter :: EOF=10003_byte
integer(kind=byte),parameter,public :: EOS=10002_byte
integer(kind=byte),parameter,public :: ERR=10001_byte
integer(kind=byte),parameter,public :: YES=1_byte
!x!integer(kind=byte),parameter :: ARB=100_byte

integer(kind=byte),parameter :: ACCENT=96_byte
integer(kind=byte),parameter :: AND=38_byte
integer(kind=byte),parameter :: ATSIGN=64_byte
integer(kind=byte),parameter :: BACKSLASH=92_byte
integer(kind=byte),parameter :: BACKSPACE=8_byte
integer(kind=byte),parameter :: CR=13_byte
integer(kind=byte),parameter :: BANG=33_byte
integer(kind=byte),parameter :: BAR=124_byte
integer(kind=byte),parameter :: BIGA=65_byte
integer(kind=byte),parameter :: BIGB=66_byte
integer(kind=byte),parameter :: BIGC=67_byte
integer(kind=byte),parameter :: BIGD=68_byte
integer(kind=byte),parameter :: BIGE=69_byte
integer(kind=byte),parameter :: BIGF=70_byte
integer(kind=byte),parameter :: BIGG=71_byte
integer(kind=byte),parameter :: BIGH=72_byte
integer(kind=byte),parameter :: BIGI=73_byte
integer(kind=byte),parameter :: BIGJ=74_byte
integer(kind=byte),parameter :: BIGK=75_byte
integer(kind=byte),parameter :: BIGL=76_byte
integer(kind=byte),parameter :: BIGM=77_byte
integer(kind=byte),parameter :: BIGN=78_byte
integer(kind=byte),parameter :: BIGO=79_byte
integer(kind=byte),parameter :: BIGP=80_byte
integer(kind=byte),parameter :: BIGQ=81_byte
integer(kind=byte),parameter :: BIGR=82_byte
integer(kind=byte),parameter :: BIGS=83_byte
integer(kind=byte),parameter :: BIGT=84_byte
integer(kind=byte),parameter :: BIGU=85_byte
integer(kind=byte),parameter :: BIGV=86_byte
integer(kind=byte),parameter :: BIGW=87_byte
integer(kind=byte),parameter :: BIGX=88_byte
integer(kind=byte),parameter :: BIGY=89_byte
integer(kind=byte),parameter :: BIGZ=90_byte
integer(kind=byte),parameter,public :: BLANK=32_byte
integer(kind=byte),parameter :: CARET=94_byte
integer(kind=byte),parameter :: COLON=58_byte
integer(kind=byte),parameter :: COMMA=44_byte
integer(kind=byte),parameter :: DIG0=48_byte
integer(kind=byte),parameter :: DIG1=49_byte
integer(kind=byte),parameter :: DIG2=50_byte
integer(kind=byte),parameter :: DIG3=51_byte
integer(kind=byte),parameter :: DIG4=52_byte
integer(kind=byte),parameter :: DIG5=53_byte
integer(kind=byte),parameter :: DIG6=54_byte
integer(kind=byte),parameter :: DIG7=55_byte
integer(kind=byte),parameter :: DIG8=56_byte
integer(kind=byte),parameter :: DIG9=57_byte
integer(kind=byte),parameter :: DIGIT=2_byte
integer(kind=byte),parameter :: DOLLAR=36_byte
integer(kind=byte),parameter :: DQUOTE=34_byte
integer(kind=byte),parameter :: EQUALS=61_byte
integer(kind=byte),parameter :: ERROUT=2_byte
integer(kind=byte),parameter :: GREATER=62_byte
integer(kind=byte),parameter :: LBRACE=123_byte
integer(kind=byte),parameter :: LBRACK=91_byte
integer(kind=byte),parameter :: LESS=60_byte
integer(kind=byte),parameter :: LETA=97_byte
integer(kind=byte),parameter :: LETB=98_byte
integer(kind=byte),parameter :: LETC=99_byte
integer(kind=byte),parameter :: LETD=100_byte
integer(kind=byte),parameter :: LETE=101_byte
integer(kind=byte),parameter :: LETF=102_byte
integer(kind=byte),parameter :: LETG=103_byte
integer(kind=byte),parameter :: LETH=104_byte
integer(kind=byte),parameter :: LETI=105_byte
integer(kind=byte),parameter :: LETJ=106_byte
integer(kind=byte),parameter :: LETK=107_byte
integer(kind=byte),parameter :: LETL=108_byte
integer(kind=byte),parameter :: LETM=109_byte
integer(kind=byte),parameter :: LETN=110_byte
integer(kind=byte),parameter :: LETO=111_byte
integer(kind=byte),parameter :: LETP=112_byte
integer(kind=byte),parameter :: LETQ=113_byte
integer(kind=byte),parameter :: LETR=114_byte
integer(kind=byte),parameter :: LETS=115_byte
integer(kind=byte),parameter :: LETT=116_byte
integer(kind=byte),parameter :: LETTER=1_byte
integer(kind=byte),parameter :: LETU=117_byte
integer(kind=byte),parameter :: LETV=118_byte
integer(kind=byte),parameter :: LETW=119_byte
integer(kind=byte),parameter :: LETX=120_byte
integer(kind=byte),parameter :: LETY=121_byte
integer(kind=byte),parameter :: LETZ=122_byte
integer(kind=byte),parameter :: LPAREN=40_byte
!x!integer(kind=byte),parameter :: MAXCHARS=20_byte
integer(kind=byte),parameter,public :: MAXLINE=1024_byte       ! TYPICAL LINE LENGTH
!x!integer(kind=byte),parameter :: MAXNAME=30_byte        ! TYPICAL FILE NAME SIZE
integer(kind=byte),parameter :: MINUS=45_byte
integer(kind=byte),parameter :: NEWLINE=10_byte
integer(kind=byte),parameter,public :: NO=0_byte
integer(kind=byte),parameter :: NOERR=0_byte
integer(kind=byte),parameter :: NOT=126_byte           ! SAME AS TILDE
integer(kind=byte),parameter :: OK=-2_byte
integer(kind=byte),parameter :: OR=BAR             ! SAME AS BAR
integer(kind=byte),parameter :: PERCENT=37_byte
integer(kind=byte),parameter :: PERIOD=46_byte
integer(kind=byte),parameter :: PLUS=43_byte
integer(kind=byte),parameter :: QMARK=63_byte
integer(kind=byte),parameter :: RBRACE=125_byte
integer(kind=byte),parameter :: RBRACK=93_byte
integer(kind=byte),parameter :: READ=0_byte
integer(kind=byte),parameter :: READWRITE=2_byte
integer(kind=byte),parameter :: RPAREN=41_byte
integer(kind=byte),parameter :: SEMICOL=59_byte
integer(kind=byte),parameter :: SHARP=35_byte
integer(kind=byte),parameter :: SLASH=47_byte
integer(kind=byte),parameter :: SQUOTE=39_byte
integer(kind=byte),parameter :: STAR=42_byte
integer(kind=byte),parameter :: TAB=9_byte
integer(kind=byte),parameter :: TILDE=126_byte
integer(kind=byte),parameter :: UNDERLINE=95_byte
integer(kind=byte),parameter :: WRITE=1_byte

! HANDY MACHINE-DEPENDENT PARAMETERS, CHANGE FOR A NEW MACHINE
integer(kind=byte),parameter,public  :: MAXPAT=512
integer(kind=byte),parameter,public  :: MAXARG=512
integer(kind=byte),parameter :: MAXSUBS=10

integer(kind=byte),parameter :: COUNT=1
integer(kind=byte),parameter :: PREVCL=2
integer(kind=byte),parameter :: START=3
integer(kind=byte),parameter :: CLOSIZE=4

!x!integer(kind=byte),parameter  :: ESCAPE=ATSIGN
!x!integer(kind=byte),parameter  :: ANY=QMARK
!x!integer(kind=byte),parameter  :: BOL=PERCENT

integer(kind=byte),parameter   :: EOL=DOLLAR
integer(kind=byte),parameter   :: CLOSURE=STAR
integer(kind=byte),parameter   :: DASH=MINUS
integer(kind=byte),parameter   :: ESCAPE=BACKSLASH
integer(kind=byte),parameter   :: ANY=PERIOD
integer(kind=byte),parameter   :: BOL=CARET

integer(kind=byte),parameter :: CCL=LBRACK
integer(kind=byte),parameter :: CCLEND=RBRACK

integer(kind=byte),parameter :: NCCL=LETN
integer(kind=byte),parameter :: CHAR=LETA
integer(kind=byte),parameter :: BOSS=LBRACE        ! <
integer(kind=byte),parameter :: EOSS=RBRACE        ! >

!x!COMMON /CSUBS/ BPOS(MAXSUBS), EPOS(MAXSUBS)
integer(kind=byte) ::  bpos(maxsubs)           ! beginning of partial match
integer(kind=byte) ::  epos(maxsubs)           ! end of corresponding partial match

type :: regex_pattern
   integer :: pat(MAXPAT)
end type regex_pattern

contains
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function f2r(string,isize)

character(len=*),parameter::ident_1="&
&@(#)M_match::f2r(3f): convert Fortran character variable to Ratfor integer array with Ratfor terminator"

character(len=*),intent(in) :: string
integer,intent(in)          :: isize
!!integer                     :: f2r(len(string)+1)
integer                     :: f2r(isize)
integer                     :: i
f2r=blank
   do i=1,len_trim(string)
      f2r(i)=ichar(string(i:i))
   enddo
   f2r(i)=eos
end function f2r
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
function r2f(ints)

character(len=*),parameter::ident_2="@(#)M_match::r2f(3f): convert Ratfor integer array to Fortran character variable"

integer,intent(in)          :: ints(:)
character(len=size(ints)-1) :: r2f
integer                     :: i
intrinsic char
   r2f=' '
   do i=1,size(ints)-1
      if(ints(i).eq.eos)then
         exit
      endif
      r2f(i:i)=char(ints(i))
   enddo
end function r2f
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getpat(3f) - [M_MATCH] convert str into pattern
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getpat(str, pat)
!!##DESCRIPTION
!!    convert str into pattern
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function getpat_(arg, pat)

! ident_1="@(#)M_match::getpat_ convert argument into pattern"

integer(kind=def) :: getpat_
integer(kind=def) :: arg(maxarg)
integer(kind=def) :: pat(maxpat)
   getpat_ = makpat_(arg, 1, EOS, pat)
end function getpat_
!===================================================================================================================================
function getpat__(arg_str, pat)

character(len=*),intent(in)   :: arg_str
integer(kind=def),intent(out) :: pat(maxpat)
integer(kind=def)             :: getpat__
integer(kind=def)             :: arg(maxarg)
integer                       :: len_arg_str

   len_arg_str=len(arg_str)
   if(len_arg_str.gt.MAXARG-1)then
      write(*,*)'*getpat* error: input arg_str too long,',len_arg_str,' > ',MAXARG-1
      stop
   endif
   arg=f2r(arg_str,size(arg))
   getpat__ = makpat_(arg, 1, eos, pat)
end function getpat__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    addset(3f) - [M_MATCH] put c in string(J) if it fits, increment J
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function addset(c, str, j, maxsiz)
!!##DESCRIPTION
!!   put c in string(j) if it fits, increment
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function addset(c, set, j, maxsiz)

! ident_2="@(#)M_match::addset put C in SET(J) if it fits, increment J"

integer(kind=byte)            :: addset
integer(kind=chr),intent(in)  :: c
integer(kind=chr)             :: set(:)
integer(kind=byte)            :: j
integer(kind=byte),intent(in) :: maxsiz
   if (j > maxsiz)then
      addset = NO
   else
      set(j) = c
      j = j + 1
      addset = YES
   endif
end function addset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    dodash(3f) - [M_MATCH] expand array(i-1)-array(i+1) into set(j)... from valid
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine dodash(valid, array, i, set, j, maxset)
!!##DESCRIPTION
!!    expand array(i-1)-array(i+1) into set(j)... from valid
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
! dodash - expand array(i-1)-array(i+1) into set(j)... from valid
subroutine dodash(valid, array, i, set, j, maxset)
integer(kind=def) ::  i, j, junk, k, limit, maxset
character(len=*),intent(in) :: valid
integer(kind=chr) :: array(:)
integer(kind=chr) :: set(:)
intrinsic char
   i = i + 1
   j = j - 1
   limit = index(valid, char(esc(array, i)))
   k=index(valid,char(set(j)))
   do
      if(.not. (k.le.limit)) exit
      junk = addset(ichar(valid(k:k)), set, j, maxset)
      k=k+1
   enddo
end subroutine dodash
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    locate(3f) - [M_MATCH] look for c in char class at pat(offset)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     pure integer function locate(c, pat, offset)
!!##DESCRIPTION
!!    look for c in char class at pat(offset)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
pure function locate(c, pat, offset)

! ident_3="@(#)M_match::locate look for c in char class at pat(offset)"

integer(kind=def)            :: locate
integer(kind=chr),intent(in) :: c
integer(kind=chr),intent(in) :: pat(maxpat)
integer(kind=def),intent(in) :: offset
integer(kind=def)            :: i
   ! size of class is at pat(offset), characters follow

   !x!for (i = offset + pat(offset); i > offset; i = i - 1)

   locate = NO
   LOC: do i = offset + pat(offset), offset+1,  -1
      if (c == pat(i)) then
         locate = YES
         exit LOC
      endif
   enddo LOC
end function locate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    match(3f) - [M_MATCH] find match to a basic regular expression anywhere on input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function match(line, pattern)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: pattern(MAXPAT)
!!
!!##DESCRIPTION
!!  Given a BRE(Basic Regular Expression) converted to a pattern
!!  return whether an input string matches it.
!!
!!##OPTIONS
!!    LIN  string to search for a match to the pattern
!!    PAT  pattern generated from a BRE using getpat(3f) or makpat(3f).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_match
!!     use :: M_match, only : getpat, match
!!     use :: M_match, only : MAXPAT, MAXARG, MAXLINE, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     character(len=MAXLINE-2)     :: line
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           if(ios.ne.0)exit
!!           if(match(trim(line), pat) .eq. YES) then
!!              write(*,'(*(a))')trim(line)
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_match
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
function match_(lin, pat)

! ident_4="@(#)M_match::match find match anywhere on line"

integer(kind=def) :: match_
integer(kind=chr) :: lin(maxline), pat(maxpat)
integer(kind=def) :: i

   if (pat(1) == bol) then            ! anchored match
      if (amatch_(lin, 1, pat) > 0) then
         match_ = yes
         return
      endif
   else               ! unanchored
      !- for (i = 1; lin(i) /= eos; i = i + 1)
      i=1
      do while (lin(i) /= eos)
         if (amatch_(lin, i, pat) > 0) then
            match_ = yes
            return
         endif
         i=i+1
      enddo
   endif
   match_ = no
end function match_
!==================================================================================================================================!
function match__(lin_str, pat)

! ident_5="@(#)M_match::match find match anywhere on line"

character(len=*),intent(in) :: lin_str
integer(kind=def) :: match__
integer(kind=chr) :: lin(maxline), pat(maxpat)
   lin=f2r(lin_str,size(lin))
   match__=match_(lin,pat)
end function match__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    patsiz(3f) - [M_MATCH] returns size of pattern entry at pat(n)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function patsiz(pat, n)
!!##DESCRIPTION
!!  returns size of pattern entry at pat(n)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function patsiz(pat, n)

! ident_6="@(#)M_match::patsiz returns size of pattern entry at pat(n)"

integer(kind=def) :: patsiz
integer(kind=chr) :: pat(MAXPAT)
integer(kind=def) :: n

   select case(pat(n))
    case(CHAR,BOSS,EOSS)
      patsiz = 2
    case(BOL,EOL,ANY)
      patsiz = 1
    case(CCL,NCCL)
      patsiz = pat(n + 1) + 2
    case(CLOSURE)                  ! optional
      patsiz = CLOSIZE
    case default
      call error("in patsiz: cannot happen.")
   end select

end function patsiz
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    stclos(3f) - [M_MATCH] insert CLOSURE entry at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function stclos(pat, j, lastj, lastcl)
!!##DESCRIPTION
!!  insert CLOSURE entry at pat(j)
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function stclos(pat, j, lastj, lastcl)

! ident_7="@(#)M_match::stclos insert closure entry at pat(j)"

integer(kind=def) :: stclos
integer(kind=chr) :: pat(maxpat)
integer(kind=def) :: j, jp, jt, junk, lastcl, lastj

   !- for (jp = j - 1; jp >= lastj; jp = jp - 1) <   ! make a hole
   do jp = j - 1, lastj,  - 1   ! make a hole
      jt = jp + closize
      junk = addset(pat(jp), pat, jt, maxpat)
   enddo
   j = j + closize
   stclos = lastj
   junk = addset(closure, pat, lastj, maxpat)   ! put closure in it
   junk = addset(0, pat, lastj, maxpat)      ! count
   junk = addset(lastcl, pat, lastj, maxpat)   ! prevcl
   junk = addset(0, pat, lastj, maxpat)      ! start
end function stclos
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    getccl(3f) - [M_MATCH] expand char class at arg(i) into pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function getccl(arg, i, pat, j)
!!##DESCRIPTION
!!    expand char class at arg(i) into pat(j)
!!##OPTIONS
!!    ARG  ADE string array
!!    I    index into ARG
!!    PAT  encoded regular expression
!!    J    .
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function getccl(arg, i, pat, j)

! ident_8="@(#)M_match::getccl expand char class at arg(i) into pat(j)"

integer(kind=def) :: getccl
integer(kind=chr)  :: arg(maxarg), pat(maxpat)
integer(kind=def) :: i, j, jstart, junk

   i = i + 1      ! skip over [
   if (arg(i) == tilde .or. arg(i) == caret) then
      junk = addset(nccl, pat, j, maxpat)
      i = i + 1
   else
      junk = addset(ccl, pat, j, maxpat)
   endif
   jstart = j
   junk = addset(0, pat, j, maxpat)      ! leave room for count
   call filset(cclend, arg, i, pat, j, maxpat)
   pat(jstart) = j - jstart - 1
   if (arg(i) == cclend)then
      getccl = ok
   else
      getccl = err
   endif
end function getccl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    filset(3f) - [M_MATCH] expand set at array(i) into set(j), stop at delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! subroutine filset(delim, array, i, set, j, maxset)
!!##DESCRIPTION
!!   expand set at array(i) into set(j), stop at delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
subroutine filset(delim, array, i, set, j, maxset)

! ident_9="@(#)M_match::filset expand set at  array(i)  into  set(j),  stop at  delim"

integer(kind=def) :: i, j, junk, maxset
integer(kind=chr) :: array(:), delim, set(:)

character(len=*),parameter :: digits= "0123456789"
character(len=*),parameter :: lowalf= "abcdefghijklmnopqrstuvwxyz"
character(len=*),parameter :: upalf= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
intrinsic char

   !-  for ( ; array(i) /= delim .and. array(i) /= eos; i = i + 1)
   do while( array(i) /= delim .and. array(i) /= eos)
      if (array(i) == escape) then
         junk = addset(esc(array, i), set, j, maxset)
      elseif (array(i) /= dash) then
         junk = addset(array(i), set, j, maxset)
      elseif (j <= 1 .or. array(i+1) == eos) then   ! literal -
         junk = addset(dash, set, j, maxset)
      elseif (index(digits, char(set (j - 1))) > 0) then
         call dodash(digits, array, i, set, j, maxset)
     elseif (index(lowalf, char(set (j - 1))) > 0) then
         call dodash(lowalf, array, i, set, j, maxset)
      elseif (index(upalf, char(set (j - 1))) > 0) then
         call dodash(upalf, array, i, set, j, maxset)
      else
         junk = addset (DASH, set, j, maxset)
      endif
      i=i+1
   enddo
end subroutine filset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    esc(3f) - [M_MATCH] map array(i) into escaped character if appropriate
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    integer function esc(array, i)
!!    integer,intent(in) :: array(*)
!!    integer            :: i
!!
!!##DESCRIPTION
!!    To support commonly used non-printable characters escaped strings are
!!    supported. When the ESCAPE character is encountered the following
!!    character is examined. If one of the special characters ESC(3f) will
!!    increment I and return the designated non-printable character. Otherwise
!!    it will return the character as-is.
!!
!!    o convert \n to newline
!!    o convert \t to horizontal tab
!!    o convert \r to carriage return
!!    o convert \b to backspace
!!    o convert \nnn to character represented by octal value
!!
!!##OPTIONS
!!    ARRAY  array of ADE (ASCII Decimal Equivalent) values terminated by
!!           an EOS (End-Of-String) character representing a string to scan
!!           for escaped characters.
!!    I      pointer into ARRAY. It is incremented to the position of the
!!           next character in ARRAY on return.
!!
!!##RETURNS
!!    ESC    The ADE for the substituted character
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!
!!##LICENSE
!!   Public Domain
function esc(array, i)

! ident_10="@(#)M_match::esc map  array(i)  into escaped character if appropriate"

integer(kind=chr) :: esc
integer(kind=chr) :: array(:)
integer(kind=def) :: i

   if (array(i) /= escape)then                         ! if not an escape character, return the character as-is
      esc = array(i)
   elseif (array(i+1) == eos)then                      ! if ESCAP is the last character it is left as-is and is not special
      esc = escape
   else
      i = i + 1                                        ! increment I to look at character after ESCAP
      select case(array(i))                            ! make substitution to special character for designated characters
      case(ichar('n'),ichar('N'));  esc = newline
      case(ichar('t'),ichar('T')); esc = tab
      case(ichar('b'),ichar('B')); esc = backspace
      case(ichar('r'),ichar('R')); esc = cr
      case(dig0:dig7)
         !- for (esc = 0; array(i) >= dig0 .and. array(i) <= dig7; i = i + 1)
         esc=0
         do while (array(i) >= dig0 .and.  array(i) <= dig7)
            i = i + 1
            esc = 8*esc + array(i) - dig0
            i = i - 1                                  ! so like other cases
         enddo
      case default
         esc = array(i)                                ! otherwise just copy character
      end select
   endif
end function esc
!----------------------------------------------------------------------------------------------------------------------------------!
! Conventional C Constants
!   Oct  Dec  Hex  Char
!   -----------------------
!   000  0    00   NUL '\0'   Null
!   007  7    07   BEL '\a'   Bell
!  *010  8    08   BS  '\b'   Backspace
!  *011  9    09   HT  '\t'   Horizontal Tab
!  *012  10   0A   LF  '\n'   Line Feed
!   013  11   0B   VT  '\v'   Vertical Tab
!   014  12   0C   FF  '\f'   Form Feed
!  *015  13   0D   CR  '\r'   Carriage Return
!   134  92   5C   \   '\\'   Backslash
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    omatch(3f) - [M_MATCH] try to match a single pattern at pat(j)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function omatch(lin, i, pat, j)
!!##DESCRIPTION
!!    try to match a single pattern at pat(j)
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function omatch_(lin, i, pat, j)

! ident_11="@(#)M_match::omatch_ try to match a single pattern at pat(j)"

integer(kind=def) ::  omatch_
integer(kind=chr)  :: lin(maxline), pat(maxpat)
integer(kind=def) :: bump, i, j, k

   omatch_ = no
   if (lin(i) == eos)then
      return
   endif
   bump = -1
   if (pat(j) == char) then
      if (lin(i) == pat(j + 1))then
         bump = 1
      endif
   elseif (pat(j) == bol) then
      if (i == 1)then
         bump = 0
      endif
   elseif (pat(j) == any) then
      if (lin(i) /= newline)then
         bump = 1
      endif
   elseif (pat(j) == eol) then
      if (lin(i) == newline .or. lin(i) == eos)then
         bump = 0
      endif
   elseif (pat(j) == ccl) then
      if (locate(lin(i), pat, j + 1) == yes)then
         bump = 1
      endif
   elseif (pat(j) == nccl) then
      if (lin(i) /= newline .and. locate(lin(i), pat, j + 1) == no)then
         bump = 1
      endif
   elseif (pat(j) == boss) then
      k = pat(j+1)
      bpos(k+1) = i
      bump = 0
   elseif (pat(j) == eoss) then
      k = pat(j+1)
      epos(k+1) = i
      bump = 0
   else
      call error("in omatch_: can't happen.")
   endif
   if (bump >= 0) then
      i = i + bump
      omatch_ = yes
   endif
end function omatch_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    amatch(3f) - [M_MATCH] look for pattern matching regular expression; returns its location
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    loc = amatch(line, from, pat, tagbeg, tagend)
!!
!!     character(len=*),intent(in) :: line
!!     integer,intent(in)          :: from
!!     character                   :: pat(MAXPAT)
!!     integer                     :: tagbeg(MAXTAGS), tagend(MAXTAGS)
!!     integer                     :: loc
!!##DESCRIPTION
!!    AMATCH scans LINE starting at location FROM, looking
!!    for a pattern which matches the regular expression coded
!!    in PAT. If the pattern is found, its starting location
!!    in LINE is returned. If the pattern is not found, AMATCH
!!    returns 0.
!!
!!    The regular expression in PAT must have been previously
!!    encoded by GETPAT(3f) or MAKPAT(3f). (For a complete description
!!    of regular expressions, see the manpage for M_match.)
!!
!!    AMATCH(3f) is a special-purpose version of MATCH(3f), which should
!!    be used in most cases.
!!##OPTIONS
!!    LINE           input line to scan
!!    FROM           beginning location to start scan from
!!    PAT            coded regular expression encoded by GETPAT(3f) or MAKPAT(3f)
!!    TAGBEG,TAGEND  element "i + 1" returns start or end, respectively, of "i"th tagged subpattern
!!##RETURNS
!!    LOC   returns location match was found or zero (0) if no match remains
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_amatch
!!     use :: M_match, only : getpat, amatch
!!     use :: M_match, only : MAXPAT, MAXARG, MAXLINE, MAXTAGS, YES, ERR
!!     implicit none
!!     ! find _ find patterns in text
!!     integer                      :: pat(MAXPAT)
!!     character(len=MAXARG-1)      :: argument
!!     integer                      :: stat
!!     integer                      :: ios
!!     integer                      :: len_arg
!!     integer                      :: loc
!!     integer                      :: ii
!!     character(len=MAXLINE-2)     :: line
!!     integer                      :: tagbeg(MAXTAGS),tagend(MAXTAGS)
!!     call get_command_argument(1, argument,status=stat,length=len_arg)
!!     if(stat.ne.0.or.argument.eq.'')then
!!        write(*,*)"usage: find pattern."
!!     elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
!!        write(*,*)"illegal pattern."
!!     else
!!        INFINITE: do
!!           read(*,'(a)',iostat=ios)line
!!           tagbeg=-9999;tagend=-9999
!!           if(ios.ne.0)exit
!!           loc = amatch(trim(line), 1, pat, tagbeg, tagend) ! returns location/0
!!           if(loc.gt.0)then ! matched; if no match, loc is returned as 0
!!              write(*,'(*(a))')trim(line)
!!              ! (element "i + 1" returns start or end, respectively, of "i"th tagged subpattern)
!!              write(*,'(*(i0,1x,i0,1x,i0,/))')(ii,tagbeg(ii),tagend(ii),ii=1,size(tagbeg))
!!           endif
!!        enddo INFINITE
!!     endif
!!     end program demo_amatch
!!
!!##SEE ALSO
!!    match, getpat, makpat
!!##DIAGNOSTICS
!!    None
!!##AUTHOR
!!    John S. Urban
!!##REFERENCE
!!    "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!    Public Domain
function amatch_(lin, from, pat)

! ident_12="@(#)M_match::amatch_  (non-recursive) look for match starting at lin(from)"

integer(kind=def) :: amatch_
integer(kind=chr)  :: lin(maxline), pat(maxpat)
integer(kind=def) :: from, i, j, offset, stack
   stack = 0
   offset = from      ! next unexamined input character
!-   for (j = 1; j <= maxsubs; j = j + 1) <     ! clear partial match results
   do j = 1, maxsubs     ! clear partial match results
      bpos(j) = offset
      epos(j) = offset
   enddo
!-   for (j = 1; pat(j) /= eos; j = j + patsiz(pat, j))
   j=1
   do while (pat(j) /= eos)
      if (pat(j) == closure) then   ! a closure entry
         stack = j
         j = j + closize      ! step over closure
         !- for (i = offset; lin(i) /= eos; )   ! match as many as possible
         i = offset
         do while ( lin(i) /= eos )                 ! match as many as
            if (omatch_(lin, i, pat, j) == no)then   ! possible
               exit
            endif
         enddo
         pat(stack+count) = i - offset
         pat(stack+start) = offset
         offset = i      ! character that made us fail
      elseif (omatch_(lin, offset, pat, j) == no) then   ! non-closure
!-         for ( ; stack > 0; stack = pat(stack+prevcl))
         do while (stack >0)
            if (pat(stack+count) > 0)then
               exit
            endif
            stack = pat(stack+prevcl)
         enddo
         if (stack <= 0) then      ! stack is empty
            amatch_ = 0            ! return failure
            return
         endif
         pat(stack+count) = pat(stack+count) - 1
         j = stack + closize
         offset = pat(stack+start) + pat(stack+count)
      endif
      j = j + patsiz(pat, j)
   enddo ! else omatch_ succeeded
   epos(1) = offset
   amatch_ = offset
   ! success
end function amatch_
!==================================================================================================================================!
function amatch__(lin_str, from, pat)

! ident_13="@(#)M_match::amatch"

character(len=*),intent(in) :: lin_str
integer,intent(in) :: from
integer(kind=def)  :: amatch__
integer(kind=chr)  :: pat(maxpat)
integer(kind=chr)  :: lin(maxline)
   lin=f2r(lin_str,size(lin))
   amatch__=amatch_(lin,from,pat)
end function amatch__
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    makpat(3f) - [M_MATCH] make pattern from arg(from), terminate on delim
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! integer function makpat(arg, from, delim, pat)
!!##DESCRIPTION
!!  make pattern from arg(from), terminate on delim
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!   John S. Urban
!!##REFERENCE
!!   "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!   Public Domain
function makpat_(arg, from, delim, pat)

! ident_14="@(#)M_match::makpat_ make pattern from arg(from), terminate at delim"

integer(kind=def) :: makpat_
integer(kind=chr)  :: arg(maxarg), delim, pat(maxpat)
integer(kind=def) :: from, i, j, junk, lastcl, lastj, lj, nsubs, sp, substk(maxsubs)

   j = 1      ! pat index
   lastj = 1
   lastcl = 0
   nsubs = 0  ! counts number of @(@) pairs
   sp = 0     ! stack pointer for substk
   !-   for (i = from; arg(i) /= delim .and. arg(i) /= eos; i = i + 1) <
   i=from
   do while ( arg(i) /= delim .and. arg(i) /= eos )
      lj = j
      if (arg(i) == any)then
         junk = addset(any, pat, j, maxpat)
      elseif (arg(i) == bol .and. i == from)then
         junk = addset(bol, pat, j, maxpat)
      elseif (arg(i) == eol .and. arg(i + 1) == delim)then
         junk = addset(eol, pat, j, maxpat)
      elseif (arg(i) == ccl) then
         if (getccl(arg, i, pat, j) == err)then
            exit
         endif
      elseif (arg(i) == closure .and. i > from) then
         lj = lastj
         !x!if(pat(lj)==bol .or. pat(lj)==eol .or. pat(lj)==closure .or. pat(lj-1) == boss .or. pat(lj-1) == eoss) then
         if(pat(lj)==bol .or. pat(lj)==eol .or. pat(lj)==closure .or. pat(lj) == boss .or. pat(lj) == eoss) then
            exit
         endif
         lastcl = stclos(pat, j, lastj, lastcl)
      elseif (arg(i) == escape .and. arg(i+1) == lparen) then
         nsubs = nsubs + 1
         if (nsubs >= maxsubs)then
            exit
         endif
         junk = addset(boss, pat, j, maxpat)
         junk = addset(nsubs, pat, j, maxpat)
         sp = sp + 1
         substk(sp) = nsubs
         i = i + 1
      elseif (arg(i) == escape .and. arg(i+1) == rparen) then
         if (sp <= 0)then
            exit
         endif
         junk = addset(eoss, pat, j, maxpat)
         junk = addset(substk(sp), pat, j, maxpat)
         sp = sp - 1
         i = i + 1
      else
         junk = addset(char, pat, j, maxpat)
         junk = addset(esc(arg, i), pat, j, maxpat)
      endif
      lastj = lj
      i=i+1
   enddo
   if (arg(i) /= delim .or. sp /= 0)then   ! terminated early
      makpat_ = err
   elseif (addset(eos, pat, j, maxpat) == no)then   ! no room
      makpat_ = err
   else
      makpat_ = i
   endif
end function makpat_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    error(3f) - [M_MATCH] print message and stop program execution
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine error(line)
!!##DESCRIPTION
!!    print message and stop program execution
!!##OPTIONS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##REFERENCE
!!    "Software Tools" by Kernighan and Plauger , 1976
!!##LICENSE
!!    Public Domain
subroutine error(message)

! ident_15="@(#)M_match::error(3f): print message and stop program execution"

character(len=*),intent(in) :: message
   write(stderr,'(a)')trim(message)
   stop
end subroutine error
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
end module M_match
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
