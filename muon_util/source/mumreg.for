      SUBROUTINE MUMREG(IMOD,IREG,IOCT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine the trigger region for a module
C-
C-   Inputs :  IMOD  = module id
C-
C-   Outputs : IREG  = trigger region (CF=1, EF=2, OV=3, SA=4)
C-             IOCT  = trigger octant (CF=0-7, WN=10-13, WS=20-23, S=30,40)
C-
C-   Created  22-OCT-1993   M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IMOD,IREG,IOCT
      INTEGER ITHT,KREG(45),KOCT(45)
C
      DATA KREG/1,1,1,0,0,2,0,0,2,            ! WAMUS A-layer
     &        1,1,1,1,1,3,2,0,3,2,            ! WAMUS B-layer
     &        1,1,1,1,1,3,2,2,3,2,2,9*0,      ! WAMUS C-layer
     &        3,3,4,3,3,4/                    ! SAMUS
C
      DATA KOCT/1,1,1,0,0,2,0,0,3,            ! WAMUS A-layer
     &        1,1,1,1,1,2,2,0,3,3,            ! WAMUS B-layer
     &        1,1,1,1,1,2,2,2,3,3,3,9*0,      ! WAMUS C-layer
     &        4,4,4,5,5,5/                    ! SAMUS
C
      IREG = 0
      IOCT = 0
      ITHT = IMOD/10
      IF (ITHT.LE.0.OR.ITHT.GT.45) RETURN
      IREG = KREG(ITHT)
      IOCT = IMOD - ITHT*10
      IF (KOCT(ITHT).EQ.2.OR.KOCT(ITHT).EQ.3) IOCT=IOCT/2
      IF (KOCT(ITHT).EQ.4.OR.KOCT(ITHT).EQ.5) IOCT=0
      IOCT = IOCT + 10*(KOCT(ITHT)-1)
C
      RETURN
      END
