      SUBROUTINE DISPLAY_SHLB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DISPLAY SHLB AS A BANK
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-MAY-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INTEGER LSHLB
      EQUIVALENCE (LBANK,LSHLB)
C
      INTEGER IXIO,LDUM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('SHLB','-F',IXIO)        ! Describe Bank format
      ENDIF
C
      CALL MZBOOK(IXMAIN,LSHLB,LDUM,2,'SHLB',0,0,NDATA,IXIO,0)
      CALL UCOPY(SHLB,Q(LSHLB+1),NDATA)
      CALL DBANK
      CALL MZDROP(IXCOM,LSHLB,' ')
  999 RETURN
      END
