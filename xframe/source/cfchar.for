      SUBROUTINE CFCHAR(mode,ifile,file,len)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
c     converts file from int (passed from c) to character and
c     counts characters - note filename is less than 400 characters please
C-
C-   Inputs  : mode: 0=from file to ifile, 1=from ifile to file
C-   Outputs : depends on mode
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      integer mode,len
      byte ifile(*),b1
      character*1 c1
      character*(*) file
c
      equivalence (c1,b1)
c
      integer i,tlen
c
      if (mode.eq.1) goto 100
c
c     translate from c-pointer to character
c
      do i=1,len
        b1 = ifile(i)
        file(i:i) = c1
      enddo
      return
c
  100 continue
c
c     translate from character to c-pointer
c
      len = tlen(file)
      do i=1,len
        c1 = file(i:i)
        ifile(i) = b1
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgetstring(prompt,returned,length)
c
c     allows you to pop up a window, asking a question, getting an
c     answer
c
      implicit none
c
      character*(*) prompt,returned
      integer length
c
      integer status,i
      byte iret(512),b1
      character*1 c1
c
      equivalence (c1,b1)
c
      call fxgetchar(%ref(prompt),%ref(' '),iret,status)
c
      length = 512
      do i=1,512
C
C       32 is space, 126 is ~ and that should be the limits
C
        b1 = iret(i)
        if (b1.lt.32.or.b1.gt.126) then
          length = i-1
          return
        endif
        returned(i:i) = c1
      enddo
c
c     i hope we never get to this point!
c
      return
      end
