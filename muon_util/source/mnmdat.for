      SUBROUTINE MNMDAT(NMOD,IFLG,NCEL,NSCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return scintillator status for a module
C-
C-   Inputs  : NMOD     Module ID
C-
C-   Outputs : IFLG   -1 if no scintillator
C-                     0 if CF cosmic scintillator
C-                     1 if EF pixel scintillator
C-                     2 if A-stub scintillator
C-             NCEL    number of readout cells
C-             NSCT    number of pieces of scintillator
C-   Controls: 
C-
C-   Created   3-MAY-1995   M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INTEGER NMOD,IFLG,NCEL,NSCT,JCEL(307)
C
C-----# of scint cells
      DATA JCEL/ 99*0, 27*0, 216, 64*0, 112, 7*0,
     1           5*16, 2*0, 16, 2*0, 5*16, 2*0, 16, 2*0,
     2           5*16, 2*0, 16, 2*0, 5*16, 2*0, 16, 2*0,
     3           5*16, 2*0, 16, 2*0, 50*0, 2*0, 112, 5*0/
C
      IF (NMOD.LT.0.OR.NMOD.GT.307) THEN
        NCEL = 0
      ELSE
        NCEL = JCEL(NMOD)
      END IF
C
      IF (NCEL.EQ.0) THEN
        IFLG = -1
        NSCT = 0
      ELSE IF (NCEL.LT.100) THEN
        IFLG = 0
        NSCT = NCEL/2
      ELSE IF (NCEL.LT.200) THEN
        IFLG = 1
        NCEL = NCEL - 100
        NSCT = NCEL
      ELSE
        IFLG = 2
        NCEL = NCEL - 200
        NSCT = NCEL
      END IF
C
      RETURN
      END
