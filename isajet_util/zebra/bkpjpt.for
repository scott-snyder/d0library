      SUBROUTINE BKPJPT(LPJET,NP,LPJPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJPT under PJET-
C-
C-   Inputs  : LPJET - PJET BANK POINTER
C-             NP - NUMBER OF PARTONS IN THIS PJET
C-   Outputs : LPJPT - Link of Booked PJPT Bank
C-   Controls: None
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPJPT.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C--   INTERNAL VARIABLES
      INTEGER LPJPT,LPJPTP
      INTEGER LPJET
      INTEGER IXIO
C
      INTEGER NP
C----------------------------------------------------------------------
C
C--   DO PRELIMINARIES ON FIRST CALL
      IF(FIRST)THEN
        CALL MZFORM('PJPT','-I',IXIO)
        FIRST=.FALSE.
      END IF
C
      IF(LPJET.LE.0) CALL GZPJET( )
      IF(LPJET.LE.0) CALL ERRMSG('PJET','BKPJPT',
     &  ' NO PJET BANK TO HANG PJPT FROM','W')
C
      IF ( LQ(LPJET-IZPJPT).GT.0 ) THEN
        CALL ERRMSG('PJET','BKPJPT',
     &  ' Already PJPT hanging under this PJET - not booked','W')
      ELSE
        CALL MZBOOK(IXMAIN,LPJPT,LPJET,-IZPJPT,'PJPT',NP+1,0,1,IXIO,0)
        IQ(LPJPT+1)=1         ! Version number
      ENDIF
C
  999 RETURN
      END
