      SUBROUTINE BKPJET(LPJHD,LPJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJET -
C-      IF NO PJET BANKS EXIST THEN BOOK THE FIRST ONE
C-      ELSE BOOK NEW PJET BANK AT END OF LINEAR CHAIN OF PJET BANKS
C-
C-   Inputs  : LPJHD - PJET HEADER BANK POINTER
C-   Outputs : LPJET - Link of Booked PJET Bank
C-   Controls: None
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-   Modified 13-NOV-1992 Brent May, Andy Milder
C-     Version 3 - Added word 13 (I)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPJET.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C--   INTERNAL VARIABLES
      INTEGER LPJET,LPJETP
      INTEGER LPJHD
      INTEGER IXIO
      INTEGER GZPJHD
C
      INTEGER IDN
C----------------------------------------------------------------------
C
C--   DO PRELIMINARIES ON FIRST CALL
      IF(FIRST)THEN
        CALL MZFORM('PJET','1I 9F 3I ',IXIO)
        FIRST=.FALSE.
      END IF
C
      IF(LPJHD.LE.0) LPJHD = GZPJHD()
      IF(LPJHD.LE.0) CALL BKPJHD(LPJHD)
C
      LPJETP=LPJHD-IZPJET
      IDN=0
      IF(LQ(LPJETP).EQ.0) THEN
C
C ****  FIRST PJET BANK NEEDS TO BE BOOKED
C
        CALL MZBOOK(IXMAIN,LPJET,LPJHD,-IZPJET,'PJET',2,2,13,IXIO,0)
        IQ(LPJET-5) = 1
      ELSE
C
C ****  LOOK FOR LAST PJET BANK TO ADD SUBSEQUENT PJET BANKS
C
   10   CONTINUE
        IF(LQ(LPJETP).NE.0) THEN
          LPJETP=LQ(LPJETP)
          IDN=IQ(LPJETP-5)
          GOTO 10
        END IF
C
C--         BOOK THE PJET BANKS, SET THE ID NUMBER
        CALL MZBOOK(IXMAIN,LPJET,LPJETP,0,'PJET',2,2,13,IXIO,0)
        IQ(LPJET-5)=IDN+1
      ENDIF
      IQ(LPJET+1)=3         ! Version number
  999 RETURN
      END
