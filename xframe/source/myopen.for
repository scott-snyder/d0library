      SUBROUTINE MYOPEN(filnam,xopt,lun,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : opens files for me
C-
C-   Inputs  : the usual d0open-type inputs
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
c
      implicit none
c
      integer lun
      logical ok
      character*(*) filnam
      character*1 xopt
c
      CALL D0XEVOPIN(FILNAM,XOPT,LUN,OK)  ! OPEN INPUT FILE
      if (ok) then
      else
        call xerrmsg('D0OPEN CANNOT FIGURE OUT HOW TO OPEN THIS!')
      endif
c
      return
      end
      SUBROUTINE D0XEVOPIN(INPUT_FILE,XOPT,INUNIT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Open a ZEBRA input file
C-   Inputs  : 
C-     FILE_NAME= name of input file
C-     XOPT= ' ' native mode, 'X' exchange mode, 'G' Geant mode
C-   Outputs : 
C-     INUNIT = allocated unit number
C-     OK = true if ok
C-
C-   Created   7-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$XFRAME$SOURCE:D0MAP.INC'
      INTEGER IER,INUNIT,ILEN,IERR
      CHARACTER*(*) INPUT_FILE
      CHARACTER*(*) XOPT
      CHARACTER*3 CHOPT
      CHARACTER*1 CH
      INTEGER I,L,LEN
      LOGICAL OK,OPTX,OPTG,OPTT
C----------------------------------------------------------------------
C
      CALL GTUNIT(D0XUSERUNIT,INUNIT,IER)
C
C  Check for a d0dad file...
C
      CALL D0DAD_DFTYPE(INPUT_FILE,IER)
      IF( IER.EQ.0 ) THEN
        OK=.TRUE.
        CALL D0DAD_SYSOPEN(INPUT_FILE,INUNIT,IERR)
        IF( IERR.NE.0 ) OK=.FALSE.
        RETURN
      ENDIF
C
      CHOPT='IU'
      OPTX = .FALSE.
      OPTG = .FALSE.
      OPTT = .FALSE.
      L=LEN(XOPT)
      DO 1 I=1,L
        CALL UPCASE(XOPT(I:I),CH)
        IF(CH.EQ.'X')OPTX = .TRUE.
        IF(CH.EQ.'G')OPTG = .TRUE.
        IF(CH.EQ.'T')OPTT = .TRUE.
    1 CONTINUE
c      IF ( OPTT ) THEN
      IF ( .not.d0xdisk ) THEN
        CHOPT='TUI'
        IF(OPTX) CHOPT='TXI'
        IF(OPTG) CHOPT='TGI'
      ELSE
        IF(OPTX) CHOPT='XI'
        IF(OPTG) CHOPT='GI'
      ENDIF
      CALL D0OPEN(INUNIT,INPUT_FILE,CHOPT,OK)
      CALL XZRECL(ILEN,CHOPT)
      CALL FZFILE(INUNIT,ILEN,CHOPT)
C
  999 RETURN
      END
