      SUBROUTINE CPOPHF(X,Y,Z,IETA,IPHI,ILYR,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the physics variables of the cell 
C-                         containing a position.  Uses CPOSPH for a
C-                         first try and then searches neighboring
C-                         cells using CNBORS and CINCEL.
C-
C-   Inputs  : X,Y,Z           The given point
C-   Outputs : IETA,IPHI,ILYR  The physics variables of the cell 
C-   Controls: IOK             0 means OK, not 0 means point in no cell
C-
C-   Created  26-MAY-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL X,Y,Z
      INTEGER IETA,IPHI,ILYR,IOK
      LOGICAL CINCEL
      INTEGER IDP(3,27),I,NJ,JETA(4),JPHI(4),JLYR(4),INTOK,J
C
C ****  IDP is a list of the 27 neighbor directions with eta varying first
C
      DATA IDP /0,0,0   ,-1,0,0   ,1,0,0   ,0,-1,0   ,0,1,0
     &           ,0,0,-1  ,0,0,1    ,-1,-1,0 ,1,-1,0   ,-1,1,0
     &           ,1,1,0   ,-1,0,-1  ,1,0,-1  ,-1,0,1   ,1,0,1
     &           ,0,-1,-1 ,0,1,-1   ,0,-1,1  ,0,1,1    ,-1,-1,-1
     &           ,1,-1,-1 ,-1,1,-1  ,1,1,-1  ,-1,-1,1  ,1,-1,1
     &           ,-1,1,1  ,1,1,1/
      CALL CPOSPH(X,Y,Z,IETA,IPHI,ILYR,INTOK)
      IF(INTOK.NE.0) THEN
        IOK=1
        GO TO 999
      ENDIF
      DO 500 I=1,27
        CALL CNBORS(IETA,IPHI,ILYR,IDP(1,J),IDP(2,J),IDP(3,J),
     &    NJ,JETA,JPHI,JLYR,INTOK)
        IF(NJ.EQ.0.OR.INTOK.NE.0) GO TO 500
        DO 300  J=1,NJ
          IF(CINCEL(X,Y,Z,JETA(J),JPHI(J),JLYR(J))) GO TO 800
  300   CONTINUE
  500 CONTINUE
      IOK=2
      GO TO 999
  800 CONTINUE
      IOK=0
  999 RETURN
      END
