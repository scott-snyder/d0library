      SUBROUTINE PLESUMTK(PTMIN,PID,DPT,LTRK,PXYZ,P,XV,XC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Auxiliary routine of PESUM_TRACK to draw
C-                         ESUM tracks
C-
C-   Inputs  : PTMIN   - minimum Pt to be display a track
C-             PID     - Particle Identity( e =12,muon=14,PJET=0..)
C-             DPT     - Pt step for color code (NOT USED NOW)
C-             LTRK    - track length for display
C-             PXYZ(3) - Px, Py, Pz of particle
C-             P       - momentum of particle
C-             XV(3)   - coordinate of begin point
C-             XC(3)   - coordinate of end   point, if there...
C-   Outputs : None
C-   COPIED FROM PLISTK.FOR
C-   Created 29-MAR-1993     Vipin Bhatnagar
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER KCOL, KC, KIN, KFIL, KSTY,PID
      REAL    PXYZ(3),P, XV(3),XC(3)
      REAL    PT,PTMIN,DPT,LTRK
C----------------------------------------------------------------------
      PT = SQRT(PXYZ(1)**2 + PXYZ(2)**2)
      IF (PT .LT. PTMIN) GO TO 999
C-
      KSTY = 0
C-
      IF ( PID .EQ. 10 ) THEN
        KCOL = 12    ! Dark Red
        KSTY =  2    ! + Dashed line-Photon
      ELSEIF ( PID .EQ. 12 ) THEN
        KCOL = 13    ! Red-ELECTRON
      ELSEIF ( PID .EQ. 14 ) THEN
        KCOL = 9     ! Green-MUON
      ELSEIF ( PID .EQ. 16 ) THEN
        KCOL = 6     ! Blue-TAU
      ELSEIF (PID  .EQ. 0  ) THEN
        KCOL = 7     ! Cyan-Parton jets
      ELSEIF (PID  .EQ.-1  ) THEN
        KCOL = 14    ! Magenta-Miss. Et
      ELSE
        KCOL = 17    ! Foreground
      ENDIF

      IF (PTMIN .LT. 0.)    KCOL = 17  ! Foreground
C-
      IF(XC(1).EQ.0. .AND. XC(2).EQ.0. .AND. XC(3).EQ.0.) THEN
        XC(1) = XV(1) + (LTRK*PXYZ(1)/P)
        XC(2) = XV(2) + (LTRK*PXYZ(2)/P)
        XC(3) = XV(3) + (LTRK*PXYZ(3)/P)
      ENDIF
      CALL JLSTYL(KSTY)
      CALL PXCOLN('CDC',KCOL,4,.TRUE.,KC,KIN,KFIL,KSTY)! Using colors
                                                       ! when in col dev using
                                                       ! line styles when not.
      CALL J3MOVE(XV(1), XV(2), XV(3))
      CALL JR3DRA(XC(1)-XV(1),XC(2)-XV(2),XC(3)-XV(3))
C-
      IF (KCOL .EQ. 17) THEN
        CALL JCMARK(2)
        CALL J3MARK(XC(1),XC(2),XC(3))
      ENDIF
C-
  999 RETURN
      END
