      SUBROUTINE CFCHAR(mode,ifile,file,len)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
c     converts file from int (passed from c) to character and
c     counts characters - note filename is less than 81 characters please
C-
C-   Inputs  : mode: 0=from file to ifile, 1=from ifile to file
C-   Outputs : depends on mode
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      integer mode,len,ifile(*)
      character*4 cfile(20)
      character*80 file
c
      integer i,tlen,tfile(20)
c
      equivalence (tfile(1),cfile(1))
c
      if (mode.eq.1) goto 100
c
c     translate from c-pointer to character
c
      do i=1,20
        tfile(i) = ifile(i)
        file(4*i-3:4*i) = cfile(i)
      enddo
c
      len = tlen(file)
      return
c
  100 continue
c
c     translate from character to c-pointer
c
      len = tlen(file)
      do i=1,20
        cfile(i) = file(4*i-3:4*i)
        ifile(i) = tfile(i)
      enddo
c
      return
      end
