      SUBROUTINE MUUNRT(IQUAD,DCI,DCO,XYZGI,XYZGO,
     &  SLI1,SLNBI,SLO1,SLNBO,XYZLI,XYZLO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : un-rotate muon from local to global coordinates
C-
C-   Inputs  : IQUAD = quadrant
C-              DCI(3) = global direction cosines inside magnet
C-              DCO(3) = global direction cosines outside magnet
C-              XYZGI(3) = point inside (global coord)
C-              XYZGO(3) = point outside (global coord)
C-   Outputs : 
C-              SLI1 = bend view slope inside magnet
C-              SLNBI = nonbend view slope inside magnet
C-              SLO1 =  bend      "    "   outside " 
C-              SLNBO = nonbend      "    "   outside " 
C-              XYZLI(3) = local coordinates inside magnet (XI,YANB,ZI)
C-              XYZLO(3) = local coordinates outside magnet (X1,YNB,ZBEND)
C-   Controls: 
C-
C-   Created  27-JUL-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IQUAD
      REAL SLI1,SLNBI,SLO1,SLNBO,DCI(3),DCO(3),XYZGI(3),XYZGO(3)
      REAL XYZLO(3),XYZLI(3)
      INTEGER IX,IY,IZ
C----------------------------------------------------------------------
C
C
        IF(IQUAD.EQ.1) THEN         ! CENTRAL +X
          IX = 3
          IY = 2
          IZ = 1
        ELSE IF(IQUAD.EQ.2.OR.IQUAD.EQ.0) THEN         ! CENTRAL +Y
          IX = 3
          IY = 1
          IZ = 2
        ELSE IF(IQUAD.EQ.3) THEN        ! CENTRAL -X
          IX = 3
          IY = 2
          IZ = 1
        ELSE IF(IQUAD.EQ.4) THEN       ! CENTRAL -Y
          IX = 3
          IY = 1
          IZ = 2
        ELSE IF(IQUAD.EQ.5.OR.IQUAD.EQ.7) THEN   ! NORTH +-X
          IX = 1
          IY = 2
          IZ = 3
        ELSE IF(IQUAD.EQ.6.OR.IQUAD.EQ.8) THEN   ! NORTH +-Y
          IX = 2
          IY = 1
          IZ = 3
        ELSE IF(IQUAD.EQ.9.OR.IQUAD.EQ.11) THEN   ! SOUTH +-X
          IX = 1
          IY = 2
          IZ = 3
        ELSE IF(IQUAD.EQ.10.OR.IQUAD.EQ.12) THEN   ! SOUTH +-Y
          IX = 2
          IY = 1
          IZ = 3
        ENDIF
C
          SLNBI = 0.
          SLI1 = 0.
          SLNBO = 0.
          SLO1 = 0.
          IF (DCI(IZ).NE.0.) THEN
            SLNBI = DCI(IY)/DCI(IZ)
            SLI1 = DCI(IX)/DCI(IZ)
          ENDIF
          IF (DCO(IZ).NE.0.) THEN
            SLNBO = DCO(IY)/DCO(IZ)
            SLO1 = DCO(IX)/DCO(IZ)
          ENDIF
          XYZLO(1) = XYZGO(IX)
          XYZLO(2) = XYZGO(IY)
          XYZLO(3) = XYZGO(IZ)
          XYZLI(1) = XYZGI(IX)
          XYZLI(3) = XYZGI(IZ)
          XYZLI(2) = XYZGI(IY) - SLNBI*(XYZLI(3)-XYZLO(3))
C
  999 RETURN
      END
