      SUBROUTINE GETPAR(npar,prompt,type,value)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : another version of the compack original
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      integer npar
      character*(*) prompt(*)
      character*1 type
      real value
c
      integer mpar,ival,readi,readh,ipar
      real xval,readr
      logical lval
      integer length,maxline
      character*132 line,tprompt
c
      equivalence (xval,ival,lval)
c
      data maxline/132/
c
      integer iar,i,len,trulen
      character*1 cprompt(132)   !max 132 characters
c
      if (abs(npar).lt.1) return
c
c     zero prompt
c
      do i=1,132
        cprompt(i) = char(0)
        line(i:i) = char(0)
      enddo
c
c     loop over "npar"
c
      mpar = abs(npar)
      do ipar=1,mpar
        len = trulen(prompt(ipar))
        tprompt(1:len) = prompt(ipar)
c
        do i=1,len
          cprompt(i) = tprompt(i:i)
        enddo
c
        length = len
        call cgetpar(%ref(cprompt),%ref(line),
     &    %ref(length),%val(maxline))
        if (length.eq.0) return
c
c       now parse the line returned by cgetpar
c
        if (type.eq.'R') then
          value = readr(line,length)
        else if (type.eq.'L') then
          if (  line(1:2).eq.'.t'.or.
     &          line(1:2).eq.'.T'.or.
     &          line(1:1).eq.'t'.or.
     &          line(1:1).eq.'T'.or.
     &          line(1:1).eq.'y'.or.
     &          line(1:1).eq.'Y') then
            lval = .true.
          else if (line(1:2).eq.'.f'.or.
     &          line(1:2).eq.'.F'.or.
     &          line(1:1).eq.'f'.or.
     &          line(1:1).eq.'F'.or.
     &          line(1:1).eq.'n'.or.
     &          line(1:1).eq.'N') then
            lval = .false.
          else
            write(*,'('' ***** ILLEGAL LOGICAL *****'')')
            return
          endif
          value = xval 
        else if (type.eq.'I') then
          ival = readi(line,length)
          value = xval
        else if (type.eq.'H') then
          ival = readh(line,length)
          value = xval
        else if (type.eq.'C') then
C&IF VAXVMS
          call movstr(line,value)
C&ELSE
C&          call strmov(%ref(line),%val(length),%ref(value))
C&ENDIF
        else if (type.eq.'A') then
C&IF VAXVMS
          call movstr(line,value)
C&ELSE
C&          call strmov(%ref(line),%val(length),%ref(value))
C&ENDIF
        else if (type.eq.'U') then
          call str$upcase(line(1:length),line(1:length))
C&IF VAXVMS
          call movstr(line,value)
C&ELSE
C&          call strmov(%ref(line),%val(length),%ref(value))
C&ENDIF
        else
          write(*,'('' **** GETPAR SAYS: TYPE '''''',A1,
     &    '''''' NOT ALLOWED!!!'')') type
        endif
      enddo
c
c     that's all folks
c
      return
      end
