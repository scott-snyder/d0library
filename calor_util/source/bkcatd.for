      SUBROUTINE BKCATD(LCATD,NTOWR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Zebra CATD bank without filling
C-
C-   Inputs  :  NTOWR - number of towers(EM + HAD) to be booked
C-   Outputs :  LCATD - Link to new bank
C-   Controls:
C-
C-   Created   6-DEC-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATD.LINK'
C
      INTEGER LCATD,NTOWR
      INTEGER LPROC,GZPROC
      INTEGER NDATA,IOH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C-
C---  Initialize
C-
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('CATD','-I',IOH)
      ENDIF
C-
C--- Book the bank CATD to the bank PROC
C--- If PROC was not booked yet, construct it here...
C-
      LPROC = GZPROC()
      IF ( LPROC .LE. 0 ) THEN
        CALL BKPROC(LPROC)
      ENDIF
      NDATA = NTOWR + 10
      CALL MZBOOK(IXMAIN,LCATD,LPROC,-IZCATD,'CATD',1,1,NDATA,IOH,0)
C---
C-
  999 RETURN
      END
