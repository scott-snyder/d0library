      SUBROUTINE BKGLOB(LGLOB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank GLOB.
C-
C-   Inputs  : None
C-   Outputs : LGLOB  [I] Address of booked GLOB bank.
C-   Controls: None
C-
C-   Created  15-DEC-1992 Harrison B. Prosper
C-   Updated  14-JAN-1993   Harrison B. Prosper
C-      Add extra links; change format
C-   Updated  27-APR-1993   Darien Wood
C-      change format
C-   Updated  26-May-1993 Amber Boehnlein, added 8 words for level 1
C-                        andor terms.
C-   Updated  11-OCT-1994   Serban D. Protopopescu  increase size to 30
C-                          total negative energy and Et
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LGLOB
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZPROC
      INTEGER LPROC
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGLOB.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LGLOB = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('GLOB','4I 13F 1B 2I 8B -F',IXIO)  ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LPROC = GZPROC()
      IF ( LPROC .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 4
      NS = 1
      ND = 33
      CALL MZBOOK(IXMAIN,LGLOB,LPROC,-IZGLOB,'GLOB',NL,NS,ND,IXIO,0)
  999 RETURN
      END
