      SUBROUTINE BKCAID (NCH, LCAID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Zebra CAID bank without filling
C-
C-   Inputs  :  NCH   - number of channels to book
C-   Outputs :  LCAID - Link to new bank
C-   Controls:  none
C-
C-   Created   7-APR-1993   Marc Paterno
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAID.LINK'
C
      INTEGER LCAID, NCH
      INTEGER GZCAID, GZPROC, LZFIND
      INTEGER LPROC
C
      INTEGER IOH, NUMBNK, NVER
      LOGICAL FIRST
      DATA    FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Allow only one bank
C
      LCAID = GZCAID ()
      IF ( LCAID .GT. 0 ) RETURN        ! already done
      NVER = 1
C
C ****  initialize
C
      IF ( FIRST ) THEN
        CALL MZFORM ('CAID', '5I2F/1I1F', IOH)
        FIRST=.FALSE.
      ENDIF

      LPROC = GZPROC ()
      IF ( LPROC .LE. 0 ) THEN     ! construct PROC bank
        CALL BKPROC (LPROC)
      ENDIF

      CALL MZBOOK (IXMAIN, LCAID, LPROC, -IZCAID,
     &            'CAID', 1, 1, 2*NCH+7, IOH, -1)
      IQ (LCAID + 1) = NVER                ! Bank version number
      IQ (LCAID + 2) = 2                   ! Repetition number

      RETURN
      END
