      SUBROUTINE fEVOUT(tag,mode,length,ifile)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : opens or closes zebcom output file - uses lun=85
C-
C-   Inputs  : tag=0 close, 1 open, 
C-             mode=0 exchange, 1 native
C-             ifile is integer-->character
C-   Outputs :
C-   Controls:
C-
C-   Created  18-OCT-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,mode,ifile(*)
c
      character*80 filename,listname
      character*2 chopt/'OU'/
      logical ok
      integer status,i,length,ilen,nd,nuhead,iuhead(50),jrun,jev
c
      if (mode.eq.0) then
c
c       exchange mode
c
        chopt = 'XO'
c
      else if (mode.eq.1) then
c
c       native mode
c
        chopt = 'OU'
      endif
c
c     what to do?
c
      if (tag.eq.0) then
        call fzendo(85,'T')
        close(85)
        iouttype = 0
c
      else if (tag.eq.1) then
        call cfchar(0,ifile,filename,ilen)
        call d0open(85,filename(1:length),chopt,ok)
        call xzrecl(ilen,chopt)
        call fzfile(85,ilen,chopt)
c
      else if (tag.eq.2) then
c
c       write out LHEAD banks from THIS event
c
        ND = min(50,IQ(LHEAD-1))
        DO NUHEAD=1,ND
          IUHEAD(NUHEAD)=IQ(LHEAD+NUHEAD)
        enddo
c
c       write out events - use MZMARK to mark subset list of 
c       banks if appropriate
c
        if (nbdrop.gt.0) then
          if (dropkeep.eq.1) call mydrop('SAVE',nbdrop,cbdrop)
          if (dropkeep.eq.0) call mydrop('DROP',nbdrop,cbdrop)
          CALL FZOUT(85,IXMAIN,LHEAD,1,'M',2,ND,IUHEAD)
        else
          CALL FZOUT(85,IXMAIN,LHEAD,1,' ',2,ND,IUHEAD)
        endif
c
      else if (tag.eq.3) then
c
c       set flag for writing out ALL events
c
        iouttype = 1
c
      else if (tag.eq.4) then
        write(*,'('' List file name: '',$)')
        read(*,'(a)') listname
        call d0open(95,listname,'I',ok)
        if (.not.ok) then
            call xerrmsg('List file NOT opened (type-o perhaps?)!!!')
            write(*,'('' List file NOT opened (type-o perhaps?)!!!'')')
            return
        else
c
c           read them all in
c
            do while (ok)
              read(95,*,end=100) jrun,jev
              ntag = ntag + 1
              irtag(ntag) = jrun
              ietag(ntag) = jev
            enddo
  100       iouttype = 3
            write(listname,'('' List file opened - found '',i4,
     &        '' sets of run/event pairs'')') ntag
            call xerrmsg(listname)
            write(*,'(a)') listname
        endif 
c
      endif
c
      return
      end
