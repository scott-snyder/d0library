      SUBROUTINE BKHMTP(LPPHO,LHMTP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank HMTP
C-
C-   Inputs  : LPPHO = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked HMTP Bank
C-   Controls: None
C-
C-   Created  25-SEP-1990 12:23:38.33  Norman A. Graf
C-   Updated  24-AUG-1992   Rajendran Raja  INCREASED DIMENSIONS OF BANK TO 20 
C-   Updated  20-FEB-1994   Meenakshi Narain  
C-                          declare word 16 and 17 to be integers
C-                          until now 16-20 were real
C-   Updated  27-JAN-1995   Steven M. Glenn  declared word 18 integer 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LHMTP
      INTEGER LPPHO
      INTEGER IXIO
      INTEGER GZPPHO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZHMTP.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LHMTP = 0
      IF(FIRST)THEN
C
        CALL MZFORM('HMTP','4I 11F 3I 2F',IXIO)        ! Describe Bank format
C
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LPPHO.EQ.0 ) THEN
        LPPHO = GZPPHO()
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LHMTP,LPPHO,-IZHMTP,'HMTP',1,1,20,IXIO,0)
C
  999 RETURN
      END
