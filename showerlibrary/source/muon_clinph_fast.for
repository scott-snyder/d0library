      SUBROUTINE MUON_CLINPH_FAST(VTX,DIR,ETAM,PHIM,IETAM,IPHIM,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds eta and Phi in B-layer of Muon Chamber
C-
C-   Inputs  : VTX(3)         Point origin of the line
C-             DIR(3)         Direction cosines of the line
C-   Outputs : ETAM           Eta of track hit point in B LAYER
C-             PHIM           Phi of track hit point in B LAYER
C-             IETAM          eta KEY index of hit
C-             IPHIM          Phi KEY index of hit
C-             OK             Flag for arg errors
C-   Controls:
C-
C-   CREATED 9-Jul-1993     Jasbir Singh and Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    VTX(*),DIR(*)
      INTEGER IETAM,IPHIM,OK
      INTEGER I,IERR
      REAL    POINT_INT(3)
      REAL    ETAM,THETA,PHIM,ETAA,PHIA
      REAL    S_CENTOR(2),S_EFTOR,S
      INTEGER NS_CENTOR,NS_EFTOR
      REAL    BLAYER_RADIUS,BLAYER_Z
      PARAMETER ( BLAYER_RADIUS = 410.)  !x/y of the B-Layer of
      PARAMETER ( BLAYER_Z      = 550.)  !z of the B-Layer of
C----------------------------------------------------------------------
C
C ****  FIND INTERSECTION WITH BLAYER CYLINDER
C
      OK = 0
      CALL CLNRD(VTX,DIR,BLAYER_RADIUS,NS_CENTOR,S_CENTOR)
      IF(NS_CENTOR.EQ.0)THEN
        OK = 1
        GO TO 999
      ENDIF
C
C ****  Find intersection with B layer End Toroid
C
      IF(DIR(3).GT.0) THEN
        CALL CLNZD(VTX,DIR,BLAYER_Z,NS_EFTOR,S_EFTOR)
      ELSE
        CALL CLNZD(VTX,DIR,-BLAYER_Z,NS_EFTOR,S_EFTOR)
      END IF
C
C ****  Choose smaller S
C
      IF(S_CENTOR(1).LT.S_EFTOR) THEN
        S = S_CENTOR(1)
      ELSE
        S = S_EFTOR
      END IF
C
      DO 1, I= 1,3
    1 POINT_INT(I) = VTX(I) + DIR(I)*S
C
      THETA = ATAN2(SQRT(POINT_INT(1)*POINT_INT(1)+
     &  POINT_INT(2)*POINT_INT(2)),POINT_INT(3))
      ETAM = -ALOG(TAN(THETA/2.0))
      ETAA = ETAM+5.0
      IF(ETAA.GT.10.0) ETAA = 10.0
      IF(ETAA.LT.0.0)  ETAA = 0.0
      IETAM = (ETAA+1/3.1)*3.1
C
      PHIM = ATAN2(POINT_INT(2),POINT_INT(1))
      PHIA = PHIM - PI/8
      IF(PHIA.LT.0.0)PHIA = PHIA + TWOPI
      IF(PHIA.GT.TWOPI)PHIA = PHIA - TWOPI
      IPHIM = (PHIA+TWOPI/7)*(7/TWOPI)
C
  999 RETURN
      END
