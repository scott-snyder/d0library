      SUBROUTINE BKCASH(LCACL,NCELLS,LCASH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CASH
C-
C-   Inputs  : LCACL = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CASH Bank
C-   Controls: None
C-
C-   Created  25-FEB-1992 15:09:00.17  Norman A. Graf
C-   Updated  14-APR-1993   Norman A. Graf  removed call to GZCACL, 
C-                          since if LCACL is not provided one shouldn't 
C-                          hang it just anywhere.
C-   Updated  24-AUG-1995   R. J. Genik II  Added 5 Structural links in MZBOOK
C-                          call. Changed IQ(LCASH+1) to IQ(LCASH+2) for number
C-                          of cells. Changed Bank Version to 2.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCASH
      INTEGER LCACL
      INTEGER IXIO
      INTEGER NCELLS
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCASH.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C:::  INITIALIZE
C
      LCASH = 0
      IF(FIRST)THEN
        FIRST = .FALSE.
C
        CALL MZFORM('CASH','2I/1I1F',IXIO)        ! Describe Bank format
C
      ENDIF
C
C:::  FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCACL.EQ.0 ) THEN
         CALL ERRMSG('CAPHEL','BKCASH',
     &      'CANNOT BOOK CASH BANK WITHOUT CACL LINK','W')
         GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCASH,LCACL,-IZCASH,'CASH',5,5,2*NCELLS+2,IXIO,0)
C
C:::   FILL IN BANK VERSION NUMBER
C
      IQ(LCASH+1) = 2
      IQ(LCASH+2) = NCELLS
C
  999 RETURN
      END
