      SUBROUTINE BKISRC(ND,LISRC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank ISRC
C-
C-   Inputs  :    ND    = Number of data words to book 
C-                LISAB = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked ISRC Bank
C-   Controls: None
C-
C-   Created  11-JAN-1990 16:49:35.86  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISRC
      INTEGER LISAB
      INTEGER IXIO
      INTEGER GZISAB
      INTEGER ND
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISRC.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAB.LINK/LIST'
C
C----------------------------------------------------------------------
C
      LISRC = 0
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LISAB.EQ.0 ) THEN
        LISAB = LQ(LHEAD-IZISAB)
      ENDIF
C
      IXIO = 1 ! MIXED DATA TYPE
      CALL MZBOOK
     &  (IXMAIN,LISRC,LISAB,-IZISRC,'ISRC',1,1,ND,IXIO,0)
C
  999 RETURN
      END
