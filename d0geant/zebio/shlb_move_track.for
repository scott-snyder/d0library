      SUBROUTINE SHLB_MOVE_TRACK(PS,LISP1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rotate Isajet Track to match position of Shower
C-              Library track chosen
C-
C- The rotation is done in two stages:
C-      coarse rotation of the reconstructerd hit position of the shower to 
C-              the phi bin of the ISAJET track
C-      fine rotation of the track to the shower hit position
C- If the reconstructed shower hit position does not match the recorded phi
C-      bin of the shower, no rotation will be performed, since there is little
C-      reason to believe that the reconstruction was correct.
C- The rotated track's incidence angle will vary slightly from the incidence
C-      angle of the Shower Library track.
C- Both charged and neutral tracks are rotated.
C-
C-   Inputs  :PS(4) = 4 Vector Of ISAJET Track
C-            LISP1 = Link to ISAJET Track
C-           /SHLDAT/, supplies IETA,IPHI of ISAJET and showerlib tracks
C-   Outputs : Modified ISP1 bank
C-   Controls: none
C-
C-   Created  30-AUG-1991   James T. Linnemann
C-   Updated  28-MAR-1992   W.G.D.Dharmaratna, few changes for the new version.
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PS(4)
      INTEGER LISP1
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:SHLDAT.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER I,IESTO,IPSTO             ! eta,phi indices of SHWLIB track
      REAL PHIFACE,RHOFACE,CALFACE(3)   ! where showerlib track hit calorimeter
      INTEGER POINT                     ! pointer to next stored hit
      REAL PHINEW,ETANEW,PNEW(3),PISA   ! rotated ISAJET hit
      INTEGER IETANEW,IPHINEW,ICC,ARGSOK,IERRP,IERRE
      REAL PHICEN,DELPHI,ETACEN,DELETA,EFAC,PFAC     ! cell boundaries
      REAL ISA_PHI
      PARAMETER( EFAC = 1.1 ) ! tolerance for rotated eta hit wrt cell
      PARAMETER( PFAC = .75 ) ! tolerance for rotated phi hit wrt cell
      LOGICAL OK
C
C----------------------------------------------------------------------
C
      IESTO = SHLB(10)                   ! ETA OF STORED TRACK
      IPSTO = SHLB(11)                  ! PHI OF STORED TRACK
c      CALL SHLB_CAL_XYZ(CALFACE)        ! where shower hit calorimeter
      CALL UCOPY(SHLB(16),CALFACE,3)      

      PHIFACE = ATAN2(CALFACE(2),CALFACE(1))    ! TAN(Y/X) = ATAN2(Y,X)
C
C...rotate the intersection point with the calorimeter face into the cell hit
C   by the Isajet track.  (do rotation same amount as shower rotated)
      PHIFACE = PHIFACE + TWOPI*FLOAT(IPHIC_PRIMARY-IPSTO)/64.0
      RHOFACE = SQRT(CALFACE(1)**2 + CALFACE(2)**2)
      CALFACE(1) = RHOFACE*COS(PHIFACE)
      CALFACE(2) = RHOFACE*SIN(PHIFACE)
      IF(CALFACE(3)*IETAC_PRIMARY.LT.0) CALFACE(3) = - CALFACE(3)
C
C... rotate ISAJET track so it hits at the same point as the showerlib track
      DO I = 1,3
        PNEW(I) = CALFACE(I) - VERTEX(I) ! unnorm vector from vtx to "hit"
      ENDDO
      DIST = SQRT(PNEW(1)**2+PNEW(2)**2+PNEW(3)**2)
      DO I = 1,3
        PNEW(I) = PNEW(I)/DIST        ! now a direction cosine
      ENDDO
C
C...check if rotation is small, i.e. takes track to nearly same cell
      CALL CLINPH_FAST(VERTEX,PNEW,ETANEW,PHINEW,IETANEW,IPHINEW,
     &    ICC,ARGSOK)
        OK = (IETANEW.EQ.IETAC_PRIMARY).AND.(IPHINEW.EQ.IPHIC_PRIMARY)
      IF (.NOT.OK) THEN                 ! allow hit slightly outside cell
        CALL CALPHI(IPHIC_PRIMARY,IETAC_PRIMARY,PHICEN,DELPHI,IERRP)
        CALL CALETA(IETAC_PRIMARY,ETACEN,DELETA,IERRE)
        OK = (IERRP.EQ.0).AND.(IERRE.EQ.0)
        OK = OK.AND.(ABS(PHINEW-PHICEN).LT.PFAC*DELPHI)
        OK = OK.AND.(ABS(ETANEW-ETACEN).LT.EFAC*DELETA)
        OK = OK.AND.(SHLB(20).EQ.0)     ! require single track
      ENDIF
      OK = OK.AND.(ARGSOK.EQ.0)
      IF (OK) THEN
C
C... new hit position near desired cell, so store rotated track
        PISA = SQRT(PS(1)**2+PS(2)**2+PS(3)**2) ! ISAJET momentum
        DO I = 1,3
          Q(LISP1+1+I) = PNEW(I)*PISA ! scale back to original momentum
        ENDDO
        ISA_PHI = ATAN2(PNEW(2),PNEW(1))       
        IF (ISA_PHI .LT. 0.0) ISA_PHI = ISA_PHI +TWOPI
        Q(LISP1+7) = ISA_PHI                      ! phi
        Q(LISP1+8) = ACOS(PNEW(3))                ! theta
        Q(LISP1+9) = -LOG(TAN(Q(LISP1+8)/2.))     ! eta
      ENDIF
  999 RETURN
      END
