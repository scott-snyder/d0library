      SUBROUTINE BKCAEC(NCH,LCAEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Zebra CAEC bank without filling
C-
C-   Inputs  :
C-    NCH= number of channels
C-   Controls:
C-
C-   Created 10/10/89   Andrew P. White
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEC.LINK'
C
      INTEGER LCAEC,NCH
      INTEGER GZCAEC,GZCAEH
      INTEGER LCAEH
C
      INTEGER IOH
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
C        initialize
C
      LCAEC=0
      IF ( FIRST ) THEN
        CALL MZFORM('CAEC','3I/1I1F',IOH)
        FIRST=.FALSE.
      ENDIF
C
      LCAEH = GZCAEH()
      IF ( LCAEH.EQ.0 ) THEN     ! construct CAEH bank
        CALL ERRMSG('Cannot book CAEC','BKCAEC',
     &    'CAEH bank does not exist','W')
        GOTO 999
      ENDIF
      CALL MZBOOK(IXMAIN,LCAEC,LCAEH,-IZCAEC,
     &               'CAEC',1,1,2*NCH+3,IOH,-1)       
      IQ(LCAEC+1) = 1                   ! Bank version #
      IQ(LCAEC+2) = 2                   ! Repetition number
      IQ(LCAEC+3) = 0                   ! Number of channels used
  999 RETURN
      END
