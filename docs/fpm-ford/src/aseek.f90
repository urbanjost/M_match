program demo_m_match
use M_match,   only : getpat, regex_pattern, amatch, bpos, epos
use M_match,   only : ERR
implicit none
character(len=1024) :: line='', argument=''
type(regex_pattern) :: p
integer             :: ios
integer             :: loc
integer             :: from
integer             :: j
   call get_command_argument(1,argument)
   if(argument.eq.'')stop 'missing regular expression'
   write(*,*)'PATTERN=',trim(argument)
   if (getpat(trim(argument), p%pat) .eq. ERR) then
      stop '*M_match* Illegal pattern.'
   endif
   from=1
   INFINITE: do
      read(*,'(a)',iostat=ios)line
      if(ios.ne.0)exit INFINITE
      write(*,*)'LINE=',trim(line)
      loc= amatch(trim(line)//char(10),from, p%pat) 
      if(loc.ne.0)then
         write(*,*)'LOC=',loc,trim(line)
         write(*,'(i0,t6,i0)')(bpos(j),epos(j),j=1,size(epos))
      endif
   enddo INFINITE
end program demo_m_match
