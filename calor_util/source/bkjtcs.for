      SUBROUTINE BKJTCS(LCAPH1,ND,LJTCS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank JTCS.
C-
C-   Inputs  : LCAPH  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-             ND     [I] Number of entries
C-
C-   Outputs : LJTCS  [I] Address of booked JTCS bank.
C-   Controls: None
C-
C-   Created  11-JAN-1993 15:55:57.02  Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:L2LINK.INC'           ! Add zebra link common
      INTEGER LCAPH, LCAPH1
      INTEGER LJTCS
      EQUIVALENCE( L2LINK(5), LCAPH )
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZCAPH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJTCS.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CALL MZLINT(IXCOM,'/L2LINK/',DUM,L2LINK(NLNK),DUM)
      LCAPH = LCAPH1
C
C--   INITIALIZE
C
      LJTCS = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('JTCS','3I -F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCAPH .LE. 0 ) THEN
        LCAPH = GZCAPH()
      ENDIF
      IF ( LCAPH .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = 2
      NS = 1
      CALL MZBOOK(IXMAIN,LJTCS,LCAPH,-IZJTCS,'JTCS',NL,NS,ND,IXIO,0)
C
      IQ(LJTCS+1) = 1    !BANK VERSION NUMBER
C
  999 DUM(1) = 0         ! DEACTIVATE LINK COMMON
      RETURN
      END
