      SUBROUTINE BKPPHO(LPARH,NDATA,LPPHO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PPHO
C-
C-   Inputs  : LPARH = Link of parent bank.
C-                      = 0, will find it for you.
C-             NDATA = Number of data words in Bank
C-   Outputs : Link of Booked PPHO Bank
C-   Controls: None
C-
C-   Created   6-APR-1990 11:48:48.44  Rajendran Raja
C-   Updated  28-FEB-1995   Meenakshi Narain  increase bank size by 
C-                          4 real words at the end (version 6 and greater)
C-   Updated  18-NOV-1995   Meenakshi Narain  increase bank to be compatible
C-                          with d0fix data optionaly 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPPHO
      INTEGER LPARH
      INTEGER IXIO
      INTEGER GZPARH
      INTEGER NDATA
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPPHO.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LPPHO = 0
      IF(FIRST)THEN
C
        IF (NDATA.EQ.35)  THEN
          CALL MZFORM('PPHO','2I 27F 2I 4F',IXIO)        ! Describe Bank format
        ELSE IF (NDATA.EQ.47) THEN
          CALL MZFORM('PPHO','2I 27F 2I 4F 1I 10F 1I',IXIO) ! Bank format
        ELSE
          CALL ERRMSG('WRONG BANK LENGTH','BKPPHO',
     &        ' INCOMPATIBLE NUMBER OF DATA WORDS ','F')
        ENDIF
C
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LPARH.EQ.0 ) THEN
        LPARH = GZPARH()
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LPPHO,LPARH,-IZPPHO,'PPHO',4,1,NDATA,IXIO,0)
C
  999 RETURN
      END
