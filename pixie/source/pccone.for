      SUBROUTINE PCCONE(DR,LENGJ,ETAJ,PHIJ,EMFRAC,ZVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a cone of Jet in 3D view
C-
C-   Inputs  : DR     - Cone size R
C-             ETAJ   - Eta of Jet
C-             PHIJ   - Phi of Jet
C-             EMFRAC - Fraction of EM Et = EM_ET/TOTAL_ET
C-             ZVTX   - Z coord. of Primary Vertex
C-
C-   Controls: This should be called between JROPEN/JOPEN and JRCLOS/
C-             JCLOSE.
C-
C-   Created  25-FEB-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER I,J,ISP
      REAL    DR,LENGJ,ETAJ,PHIJ,EMFRAC,ZVTX
      REAL    ETAPNT(64),PHIPNT(64),THETAPNT(64)
      REAL    XPNT(64),YPNT(64),ZPNT(64)
      REAL    XPEM(64),YPEM(64),ZPEM(64)
      REAL    LENC,LT,PHIBIN,PHIRAD
      REAL    DTHETA,THETAJ
C----------------------------------------------------------------------
C-
      THETAJ = 2.*(ATAN(EXP( -ETAJ )))
      LENC   = LENGJ/COS(DTHETA)
      DTHETA = PI/3.2 * DR
      PHIBIN = DR/16.
      PHIRAD = PI/3.2 * DR/16.
C-
C--- Make points in ETA-PHI plane
C-
      ISP = 17
      DO I = 1,64
        IF (I .LE. 33) THEN
          ISP = ISP - 1
        ELSE
          ISP = ISP + 1
        ENDIF
        PHIPNT(I) = PHIJ + ISP*PHIRAD
      ENDDO
C--
      ISP = 17
      DO J = 1,64
        IF (J .LE. 33) THEN
          ISP = ISP - 1
          ETAPNT(J) = ETAJ + SQRT(DR**2 - (ISP*PHIBIN)**2)
        ELSE
          ISP = ISP + 1
          ETAPNT(J) = ETAJ - SQRT(DR**2 - (ISP*PHIBIN)**2)
        ENDIF
      ENDDO
C-
C--- Fill THETAPNT from ETAPNT
C-
      DO I = 1,64
        THETAPNT(I) = 2.*(ATAN(EXP( -ETAPNT(I) )))
      ENDDO
C-
C--- GET X, Y and Z Coordinates for TOTAL
C-
      DO J = 1,64
        LT      = LENC*SIN(THETAPNT(J))
        XPNT(J) = LT*COS(PHIPNT(J))
        YPNT(J) = LT*SIN(PHIPNT(J))
        ZPNT(J) = LENC*COS(THETAPNT(J)) + ZVTX
      ENDDO
C-
C--- GET X, Y and Z Coordinates for EM
C-
      DO J = 1,64
        LT      = LENC*SIN(THETAPNT(J)) * EMFRAC
        XPEM(J) = LT*COS(PHIPNT(J))
        YPEM(J) = LT*SIN(PHIPNT(J))
        ZPEM(J) = (LENC*COS(THETAPNT(J)) * EMFRAC) + ZVTX
      ENDDO
C---
      CALL PXCOLR('CYA')
      CALL J3PLGN(XPNT,YPNT,ZPNT,64)
      CALL J3MOVE(XPEM(1), YPEM(1), ZPEM(1))
      CALL JR3DRA(XPNT(1)-XPEM(1),YPNT(1)-YPEM(1),ZPNT(1)-ZPEM(1))
      CALL J3MOVE(XPEM(33), YPEM(33), ZPEM(33))
      CALL JR3DRA(XPNT(33)-XPEM(33),YPNT(33)-YPEM(33),ZPNT(33)-ZPEM(33))
      IF (EMFRAC .GT. 0.) THEN
        CALL PXCOLR('RED')
        CALL J3PLGN(XPEM,YPEM,ZPEM,64)
        CALL J3MOVE(0., 0., ZVTX)
        CALL JR3DRA(XPEM(1), YPEM(1), ZPEM(1)-ZVTX)
        CALL J3MOVE(0., 0., ZVTX)
        CALL JR3DRA(XPEM(33), YPEM(33), ZPEM(33)-ZVTX)
      ENDIF
C-
C---
C-
  999 RETURN
      END
