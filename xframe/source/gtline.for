      SUBROUTINE GTLINE(wrup,nr,first6,i,data,mode)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
c     utility for making the line for printout of data into char array
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'd0$inc:autof.inc'
c
      character*4 wrup(*)
      integer nr
      character*24 tline
      character*6 first6
      integer i,mode,data
      integer idat
      real rdat
c
      integer iline4,j,i1,i2,type,iauto_fmt
      character*4 cline4
c
      equivalence (iline4,cline4)
      equivalence (rdat,idat)
c
c     first stuff into tline, then put into wrup
c
      tline(1:6) = first6(1:6)
      write(tline(7:12),'(i5,'' '')') i
c
      idat = data
      if (mode.eq.0) then
c
c       auto format
c
        idata = i
        type = iauto_fmt()
        if (type.eq.1) then
          write(tline(13:24),'(2x,z8,'' X'')') idat
        else if (type.eq.2) then
          write(tline(13:24),'(i12)') idat
        else if (type.eq.3) then
          write(tline(13:24),'(g12.4)') rdat
        else if (type.eq.5) then
          iline4 = idat
          write(tline(13:24),'(8x,a4)') cline4
        else
          write(tline(13:24),'(i12)') idat
        endif
c
      else if (mode.eq.1) then
c
c       integer
c
        write(tline(13:24),'(i12)') idat
c
      else if (mode.eq.2) then
c
c       float
c
        write(tline(13:24),'(g12.4)') rdat
c
      else if (mode.eq.3) then
c
c       hex
c
        write(tline(13:24),'(2x,z8,'' X'')') idat
c
      else if (mode.eq.4) then
c
c       character*4
c
        iline4 = idat
        write(tline(13:24),'(8x,a4)') cline4
      endif
c
      i1 = 1
      i2 = 4
      do j=1,6
        wrup(nr) = tline(i1:i2)
        nr = nr + 1
        i1 = i1 + 4
        i2 = i2 + 4
      enddo
c
c     put in \n
c
      wrup(nr) = wrup(1)
      nr = nr + 1
c
      return
      end
