C DEC/CMS REPLACEMENT HISTORY, Element GZJPAR.FOR
C *2     8-AUG-1991 14:03:12 LINNEMANN "rebuild release"
C *1    24-JUL-1991 21:57:34 ASTUR "' '"
C DEC/CMS REPLACEMENT HISTORY, Element GZJPAR.FOR
      FUNCTION GZJPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to L2JETS parameter bank. 
C-          15-MAR-1992    Since we
C-                         cant write to the STP bank in Level2, a copy
C-                         is held under FRES. Move it under SL2H first
C-                         chance we get. This is expected to be
C-                         a temporary measure.
C-
C-   Returned value  : Zebra link of bank: 0 if not found
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-JUL-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZJPAR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZJPAR.LINK'
      INTEGER GZSL2H,LSUP, GZFRES
      INTEGER LFRES, LJPAR1
      INTEGER NUM_PAR_SETS            ! Number of parameter sets
      INTEGER NDATA                   ! Number of data words in bank
C----------------------------------------------------------------------
      GZJPAR = 0
      LSUP = GZSL2H()
C: Look in normal place
      IF ( LSUP .GT. 0 ) GZJPAR = LC(LSUP-IZJPAR)
      IF ( GZJPAR .LE. 0 ) THEN
C
C: Check if it is under FRES. Copy it here if it is.
C
        LFRES = GZFRES()
        IF ( LFRES .LE. 0 ) RETURN    ! No FRES bank.
        LJPAR1 = LQ(LFRES-6)          ! Intentional hard code
        IF ( LJPAR1 .LE. 0) RETURN    ! No JPAR
        IF (LSUP .LE. 0) CALL BKSL2H( LSUP ) ! Make a SL2H if none there
        CALL MZCOPY( IXMAIN, LJPAR1, IDVSTP, LSUP, -IZJPAR, ' ') 
        GZJPAR = LC( LSUP - IZJPAR )  
      END IF
  999 RETURN
      END
