C DEC/CMS REPLACEMENT HISTORY, Element BKTFLT.FOR
C *1    18-MAY-1990 11:11:26 STEWART "TB90 L2 FILTER ZEBRA ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element BKTFLT.FOR
      SUBROUTINE BKTFLT(LTFLT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills bank TFLT for test beam
C-                         filter banks
C-
C-   Inputs  : None
C-   Outputs : LTFLT : Address of bank TFLT
C-
C-   Created  10-MAY-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTFLT.LINK'
      INTEGER  LTFLT, IOTFLT, LTB90, GZTB90 
      INTEGER  NDATA,NLNKS
      PARAMETER (NDATA=1)
      PARAMETER (NLNKS=5)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('TFLT','-I',IOTFLT)   ! format for TFLT
        FIRST=.FALSE.
      ENDIF
C
      LTB90=GZTB90()
      IF ( LTB90.LE.0 ) CALL BKTB90(LTB90)
      LTFLT=LQ(LTB90-IZTFLT)
      IF (LTFLT .NE. 0) GOTO 999        ! return if TFLT exists already
C
C
C **** Create TFLT bank
C
      CALL MZBOOK(IXMAIN,LTFLT,LTB90,-IZTFLT,
     +            'TFLT',NLNKS,NLNKS,NDATA,IOTFLT,0)
C
      IQ(LTFLT+1)=0             ! version number
C
  999 RETURN
      END
