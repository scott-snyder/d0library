      SUBROUTINE ZSESET(ILAY,ICELL,ANODE,ENRG)
C----------------------------------------------------------------------
C-
C-             acording to it.
C-
C-   Inputs  : ILAY  - Number of the layer to get the energy from
C-             ICELL - Number of the cell to get energy from
C-             ANODE - Anodes=1, cathodes strip = 2
C-
C-   Output  : ENRG  - Energy of the wire
C-   Controls: 
C-
C-   Created   6-JUN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    ENRG, PTGEGT, SCALE, EMAX1, EMAX2
      INTEGER ILAY, ICELL, ANODE, ITRY2, MXCELL
C----------------------------------------------------------------------
      IF (ITRY2.EQ.0) THEN
        ITRY2 = 1
        CALL PTHELA(ILAY,1,MXCELL)         ! Getting wire num w/max enrg
        EMAX1 = PTGEGT(ILAY, MXCELL, 1)    ! Getting maximum energy anodes
        CALL PTHELA(ILAY,2,MXCELL)         ! Getting wire num w/max enrg
        EMAX2 = PTGEGT(ILAY, MXCELL,2)     ! Getting maximum energy cathodes
        SCALE = EMAX1/EMAX2
      ENDIF
      ENRG = PTGEGT(ILAY,ICELL,ANODE)
      IF (ANODE .EQ. 2) THEN   
        ENRG = ENRG * SCALE    ! Scaling energy for cathodes
      ENDIF
      CALL ZSECUT(ENRG)    ! Setting the color or line style
  999 RETURN
      END
