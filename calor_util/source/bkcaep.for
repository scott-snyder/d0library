      SUBROUTINE BKCAEP(NCH,LCAEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Zebra CAEP bank without filling
C-
C-   Inputs  :  NCH   - number of channels to book
C-   Outputs :  LCAEP - Link to new bank
C-   Controls:
C-
C-   Created   9-FEB-1989   Alan M. Jonckheere
C-   Modified 25-APR-1989   S. D. Protopopescu
C-   Updated   8-AUG-1989   Alan M. Jonckheere  Add Entry to allow 2d bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
C
      INTEGER LCAEP,NCH
      INTEGER GZCAEP,GZCAHT,LZFIND
      INTEGER LCAHT
C
      INTEGER IOH,NUMBNK,NVER
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Allow only one bank
C
      LCAEP = GZCAEP()
      IF ( LCAEP.GT.0 ) GOTO 999   ! already done
      NVER = 0
      GOTO 100
C
      ENTRY BKCAEP_2(NCH,LCAEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allow booking of *multiple* CAEP banks. The
C-                              additional banks will be placed at the end of
C-                              the linear structure.
C-
C-   Inputs  : NCH   - number of channels
C-   Outputs : LCAEP - Link to the new bank
C-   Controls:
C-
C----------------------------------------------------------------------
C
C ****  allow only 2 banks
      LCAEP = GZCAEP()
      IF ( LCAEP.GT.0 ) THEN
        IF ( LQ(LCAEP).GT.0 ) THEN
          LCAEP = LQ(LCAEP)
          GOTO 999
        ENDIF
      ENDIF
      NVER = 1000
C
C ****  initialize
C
  100 IF ( FIRST ) THEN
        CALL MZFORM('CAEP','3I/1I1F',IOH)
        FIRST=.FALSE.
      ENDIF
C
      LCAHT = GZCAHT()
      IF ( LCAHT.EQ.0 ) THEN     ! construct CAHT bank
        CALL BKCAHT(LCAHT)
      ENDIF
      CALL MZBOOK(IXMAIN,LCAEP,LCAHT,-IZCAEP,
     &               'CAEP',1,1,2*NCH+3,IOH,-1)
      IQ(LCAEP+1) = 2 + NVER            ! Bank version #
      IQ(LCAEP+2) = 2                   ! Repetition number
      IQ(LCAEP+3) = 0                   ! Number of channels used
      NUMBNK = IQ(LCAEP-5)              ! Remember Bank number
      CALL ZSORTI(IXCOM,LCAEP,1)        ! Sort on version number
      LCAEP = LZFIND(IXCOM,LCAEP,NUMBNK,-5)     ! Refind bank
  999 RETURN
      END
