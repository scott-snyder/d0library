      PROGRAM TEST_EZCONVERT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-JUN-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INTEGER LUN,LSUP,LCRCP,IZLINK,NCRCP,IFL,IER,LSRCP
      LOGICAL OK
      CHARACTER CFL*7,BKNAME*80
C----------------------------------------------------------------------
C
C ****  INIT 
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL CONSTP
C
C ****  STICK some stuff in STP
C
      CALL CAISTP ('CAL_STPFILE',IER)
      CALL DZSURV('SURVEY CAL_STPFILE',0,LC)
C
C ****  READ RCP FILE ****  MOVE TO STP (SRCP)
C
      CALL INRCP('TEST_RCP',IER)
      LSUP = LC(LSTPH-IZSCPH)
      IZLINK = 3
      BKNAME= 'TEST_RCP'
      CALL EZSHUNT (BKNAME,LSUP,IZLINK)
      CALL DZSURV('SURVEY STP',0,LC)
C
C ****  CONVERT ALL SRCP banks under STPH to CRCP
C
      CALL EZCONVERT (LSTPH,IER)
      CALL DZSURV('SURVEY STP',0,LC)
C
C ****  EZNAME ALL CRCP BANKS and EZDUMP
C
      END
