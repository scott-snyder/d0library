      SUBROUTINE TRD_ENERGY_MAX_CELLS 
     &  (RW,IW,ENERGY_MAX_CELLS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : computes energies of max cells only
C-
C-   Inputs  : RW       real(3,NWORD)        output of TRD_DST_COR.FOR
C-             IW       integer(3,NWORD)     output of TRD_DST_COR.FOR
C-
C-   Outputs :
C-             ENERGY_MAX_CELLS             real(5)          energies in MIP
C-                      ENERGY_MAX_CELLS(1) energy layer 1 max cell
C-                      ENERGY_MAX_CELLS(2) energy layer 2 max cell
C-                      ENERGY_MAX_CELLS(3) energy layer 3 max cell
C-                      ENERGY_MAX_CELLS(4) total energy max cells
C-                      ENERGY_MAX_CELLS(5) truncated energy max cells
C-
C-   Controls: none
C-
C-   Created  26-OCT-1994   Jean-Francois LEBRAT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      REAL RW(3,NWORD),PHIT,PHI(3),POINT(3),ENERGY_MAX_CELLS(5)
     &  ,ENMAX,ENMAX_PRE
      INTEGER IW(3,NWORD),LAYER,NCEL
C
      ENERGY_MAX_CELLS(1)=RW(1,51)
      ENERGY_MAX_CELLS(2)=RW(2,51)
      ENERGY_MAX_CELLS(3)=RW(3,51)
C
          DO LAYER=1,3
            ENMAX_PRE=0.
            DO NCEL=1,IW(LAYER,4)
              ENMAX=RW(LAYER,50+NCEL)
              IF (ENMAX.GT.ENMAX_PRE) THEN
                ENERGY_MAX_CELLS(LAYER)=ENMAX
                ENMAX_PRE=ENMAX
              END IF
            END DO
          END DO
C
      ENERGY_MAX_CELLS(4)=
     &        ENERGY_MAX_CELLS(1)+
     &        ENERGY_MAX_CELLS(2)+
     &        ENERGY_MAX_CELLS(3)
      ENERGY_MAX_CELLS(5)=
     &        ENERGY_MAX_CELLS(4)-
     &    MAX(ENERGY_MAX_CELLS(1),
     &        ENERGY_MAX_CELLS(2),
     &        ENERGY_MAX_CELLS(3))
C
      END          
