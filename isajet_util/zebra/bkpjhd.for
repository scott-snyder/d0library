      SUBROUTINE BKPJHD(LPJHD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PJHD
C-      IF NO PJHD BANKS EXIST THEN BOOK THE FIRST ONE
C-      ELSE BOOK NEW PJHD BANK AT END OF LINEAR CHAIN OF PJHD BANKS
C-
C-   Inputs  : NONE
C-   Outputs : LPJHD Link of Booked PJHD Bank
C-   Controls: None
C-
C-   Created   7-NOV-1989 17:57:58.00  Chip Stewart
C-   Updated  13-NOV-1992 Brent May, Andy Milder
C-      Version 3 - Added word 9
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPJHD,LPJHDP
      INTEGER LISAE
      INTEGER IXIO
      INTEGER GZISAE
      INTEGER IDN
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPJHD.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LPJHD = 0
      IF(FIRST)THEN
C
        FIRST = .FALSE.
        CALL MZFORM('PJHD','3I2F3I-F',IXIO)      ! Describe Bank format
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LISAE = GZISAE ()
C
      IDN=0

C
      LPJHDP = LISAE-IZPJHD
C
      IF(LQ(LPJHDP).EQ.0) THEN
C
C ****  FIRST PJHD BANK NEEDS TO BE BOOKED
C

        CALL MZBOOK(IXMAIN,LPJHD,LISAE,-IZPJHD,'PJHD',2,2,9,IXIO,0)
        IQ(LPJHD-5) = 0
      ELSE
C
C ****  LOOK FOR LAST PJHD BANK TO ADD SUBSEQUENT PJHD BANKS
C
   10   CONTINUE
        IF(LQ(LPJHDP).NE.0) THEN
          LPJHDP=LQ(LPJHDP)
          IDN=IQ(LPJHDP-5)
          GOTO 10
        END IF
C
C--         BOOK THE PJHD BANKS, SET THE ID NUMBER
        CALL MZBOOK(IXMAIN,LPJHD,LPJHDP,0,'PJHD',2,2,9,IXIO,0)
        IQ(LPJHD-5)=IDN+1
      ENDIF
      IQ(LPJHD+1)=3         ! Version number
C
  999 RETURN
      END
