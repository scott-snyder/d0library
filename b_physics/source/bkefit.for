      SUBROUTINE BKEFIT(LEFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank EFIT. For now use link -3 in CAPH
C-
C-   Inputs  : 
C-
C-   Outputs : LEFIT  EFIT bank address
C-   Controls: 
C-
C-   Created  12-JUL-1993 Andrzej Zieminski, Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCAPH
      INTEGER LEFIT
C----------------------------------------------------------------------
      INTEGER ND,IXIO
      INTEGER GZCAPH,IZEFIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
!      INCLUDE 'D0$LINKS:IZEFIT.LINK'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LEFIT = 0
      IZEFIT=IZJTCS
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('EFIT','2I 27F 2I -F',IXIO)       
      ENDIF
      LCAPH = GZCAPH()
      IF ( LCAPH .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      ND=50  ! number of data words
      CALL MZBOOK(IXMAIN,LEFIT,LCAPH,-IZEFIT,'EFIT',0,0,ND,IXIO,0)
C
      IQ(LEFIT+1) = 0  ! version number 
C
  999 RETURN
      END
