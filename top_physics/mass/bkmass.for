      SUBROUTINE BKMASS(LPROC,LMASS,NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank MASS.
C-
C-   Inputs  : LPROC  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-                        NDATA  = Number of datawords
C-   Outputs : LMASS  [I] Address of booked MASS bank.
C-   Controls: None
C-
C-   Created  22-JUN-1993 10:51:16.15  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPROC
      INTEGER LMASS
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZPROC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMASS.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
      INTEGER NDATA
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LMASS = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('MASS','1I 35F 1H 2F 1H 2F -F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LPROC .LE. 0 ) THEN
        LPROC = GZPROC()
      ENDIF
      IF ( LPROC .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 1
      NS = 1
      ND = NDATA
C
      CALL MZBOOK(IXMAIN,LMASS,LPROC,-IZMASS,'MASS',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
