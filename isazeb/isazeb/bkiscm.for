      SUBROUTINE BKISCM(ND,LISCM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank ISCM
C-
C-   Inputs  : ND    = NUMBER OF WORDS IN ISCM
C-   Outputs : LISCM  = Link of Booked ISCM Bank
C-   Controls: None
C-
C-   Created  13-DEC-1989 10:20:16.28  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LISCM
      INTEGER LISAB
      INTEGER IXIO
      INTEGER GZISAB
      INTEGER ND
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISCM.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISAB.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LISCM = 0
      IF(FIRST)THEN
C
        CALL MZFORM('ISCM','-H',IXIO)        ! Describe Bank format
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LISAB = LQ(LHEAD-IZISAB)
C
      CALL MZBOOK
     &  (IXMAIN,LISCM,LISAB,-IZISCM,'ISCM',1,1,ND,IXIO,0)
C
  999 RETURN
      END
