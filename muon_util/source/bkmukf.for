      SUBROUTINE BKMUKF(LMUON,NSEG,VERS,LMUKF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank MUKF.
C-
C-   Inputs  : LMUON  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-             VERS   [I] Bank version ID
C-             NSEG   [I] Number of fit segments to allocate
C-   Outputs : LMUKF  [I] Address of booked MUKF bank.
C-   Controls: None
C-
C-   Created   13-Jan-1995 Igor V. Mandrichenko
C-   Updated   24-Jan-1995 Igor V. Mandrichenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMUON
      INTEGER LMUKF,NSEG,VERS
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUKF.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF ( LMUON .LE. 0 ) GOTO 999
      LMUKF = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('MUKF','2I / 2I 30F',IXIO)    ! Describe Bank format
      ENDIF
C
      NL = 1
      NS = 1
      IF(NSEG.LT.0) THEN
        ND = MUKF_HDRLEN
      ELSE
        ND = MUKF_HDRLEN + NSEG*MUKF_SEGLEN
      ENDIF
      CALL MZBOOK(IXMAIN,LMUKF,LMUON,-IZMUKF,'MUKF',NL,NS,ND,IXIO,0)
C
C ****  Book a stand-alone bank
C
C      CALL MZBOOK(IXMAIN,LMUKF,0,2,'MUKF',NL,NS,ND,IXIO,0)
      IQ(LMUKF+1) = VERS
      IQ(LMUKF+2) = NSEG
  999 RETURN
      END
