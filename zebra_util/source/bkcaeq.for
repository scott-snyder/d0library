      SUBROUTINE BKCAEQ(LCAEQ,NUM_CELLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books CAEQ (compressed CAEP) under PROC
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-MAR-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEQ.LINK'
      INTEGER LCAEQ,NUM_CELLS,GZPROC,LPROC,IOH,NDATA,IVER,NR
      DATA IVER / 2 /
      DATA NR /6/
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C-
C---  Initialize
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('CAEQ','3I 3F -I',IOH)
      ENDIF
C
C--- Book it if not there, else just get pointer
C
      LCAEQ = 0
      CALL BKPROC( LPROC )
      IF ( LPROC .LE. 0 ) RETURN
C
      NDATA = NUM_CELLS + NR
      CALL MZBOOK(IXMAIN,LCAEQ,LPROC,-IZCAEQ,'CAEQ',1,1,NDATA,IOH,0)
      IQ(LCAEQ+1) = IVER
      IQ(LCAEQ+2) = NR
      IQ(LCAEQ+3) = NUM_CELLS
C
  999 RETURN
      END
