program demo_m_match
use M_match,   only : getpat, match, regex_pattern
use M_match,   only : YES, ERR
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
