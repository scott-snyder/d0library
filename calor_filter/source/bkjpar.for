      SUBROUTINE BKJPAR(NUM_PARAM_SETS,LJPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book JPAR, L2JETS parameter storage bank
C-
C-   Inputs  : NUM_PARAM_SETS,LSUP = supporting link
C-   Outputs : LJPAR = returned link
C-   Controls: 
C-
C-   Created  22-JUL-1991   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$LINKS:IZJPAR.LINK'
      INCLUDE 'D0$INC:L2JETS_PAR.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NDATA,LSUP,LJPAR,NUM_PARAM_SETS
      INTEGER IOH,GZSL2H
      SAVE IOH
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      NDATA = NUM_PARAM_SETS*(NPAR_VAL_MAX + 1) + 3

      IF ( FIRST ) THEN
        CALL MZFORM('JPAR','3I-F',IOH)
        FIRST = .FALSE.
      END IF
C---Book it
      LSUP = GZSL2H()
      IF (LSUP .LE. 0) RETURN
      CALL MZBOOK(IXSTP,LJPAR,LSUP,-IZJPAR,'JPAR',0,0,NDATA,IOH,0)
  999 RETURN
      END

