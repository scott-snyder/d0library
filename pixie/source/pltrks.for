      SUBROUTINE PLTRKS(PID,PNOR,PXYZ,P,XV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Auxiliary routine of PC3DST which draw
C-                         JET Axes, Muon and Electron tracks by different
C-                         color. Their length is proportional to energy
C-                         or momentum.
C-
C-   Inputs  : PID     - Particle/Jet ID
C-                       [Jet=0, Electron=12, Muon=14, Miss E=-1]
C-             PNOR    - P or E to be normalize a track length
C-             PXYZ(3) - Px, Py, Pz of particle/Jet
C-             P       - momentum or energy of particle/Jet
C-             XV(3)   - coordinate of begin point( primary )
C-   Outputs : None
C-
C-   Created  06-FEB-1991   Nobuaki Oshima
C-   Modified 22-FEB-1992   Nobuaki Oshima - Remove 1st argu. PTMIN.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PID,KCOL, KC, KIN, KFIL, KSTY
      REAL    PNOR,PXYZ(3),P, XV(3)
      REAL    LTRK,XC(3)
C----------------------------------------------------------------------
C-
      KSTY = 0
      IF (PID .EQ. 0 ) THEN
        KCOL = 7     ! Cyan-JET
      ELSEIF ( PID .EQ. 10 ) THEN
        KCOL = 12    ! Dark Red
        KSTY =  2    ! + Dashed line-Photon
      ELSEIF ( PID .EQ. 12 ) THEN
        KCOL = 13    ! Red-ELECTRON
      ELSEIF ( PID .EQ. 14 ) THEN
        KCOL = 9     ! Green-MUON
      ELSEIF ( PID .EQ. 16 ) THEN
        KCOL = 6     ! Blue-TAU
      ELSEIF ( PID .EQ. -1 ) THEN
        KCOL = 14    ! Magenta-MISS. E
      ELSE
        KCOL = 17   ! Forground-ETC...
      ENDIF
C-
      LTRK  = 400.*P/PNOR
      XC(1) = XV(1) + (LTRK*PXYZ(1)/P)
      XC(2) = XV(2) + (LTRK*PXYZ(2)/P)
      XC(3) = XV(3) + (LTRK*PXYZ(3)/P)
      IF (PID .EQ. -1)  XC(3) = XV(3)
C-
      CALL JLSTYL(KSTY)
      CALL PXCOLN('CDC',KCOL,4,.TRUE.,KC,KIN,KFIL,KSTY)! Using colors
                                                       ! when in col dev using line styles when not.
      CALL J3MOVE(XV(1), XV(2), XV(3))
      CALL JR3DRA(XC(1)-XV(1),XC(2)-XV(2),XC(3)-XV(3))
C-
      IF (KCOL .EQ. 17) THEN
        CALL JCMARK(2)
        CALL J3MARK(XC(1),XC(2),XC(3))
      ENDIF
C-
      RETURN
      END
