      SUBROUTINE BKXXXX(LXPARENT,LXXXX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank XXXX.
C-
C-   Inputs  : LXPARENT  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-   Outputs : LXXXX  [I] Address of booked XXXX bank.
C-   Controls: None
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LXPARENT
      INTEGER LXXXX
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZXPARENT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZXXXX.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LXXXX = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('XXXX','XFORM',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LXPARENT .LE. 0 ) THEN
        LXPARENT = GZXPARENT()
      ENDIF
      IF ( LXPARENT .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
      NL = XNL
      NS = XNS
      ND = XND
      CALL MZBOOK(IXMAIN,LXXXX,LXPARENT,-IZXXXX,'XXXX',NL,NS,ND,IXIO,0)
C
C ****  Book a stand-alone bank
C
C      CALL MZBOOK(IXMAIN,LXXXX,0,2,'XXXX',NL,NS,ND,IXIO,0)
  999 RETURN
      END
