      SUBROUTINE PELCFL(DCLA,NZTRAKS,ETRANS,ECLUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank PELC
C-
C-   Inputs  :LZTRAK_ELECTRON Link to Ztrak of electron
C-            DCLA       Distance of closest approach of ZTRAK
C-            NZTRAKS    Number of ZTRAKs in road
C-            ETRANS     energy in EM cluster outside central tower
C-            ECLUS      Energy of EM cluster
C-
C-   Outputs : LPELC is output if on input LPELC =0
C-
C-   Controls:
C-
C-   Created   6-APR-1990 11:45:28.16  Rajendran Raja
C-   Added to  20-APR-1990  N.A. Graf
C-   Updated  28-SEP-1992   Norman A. Graf
C-
C-            ECLUS is now input to allow for corrections made to
C-            the cluster in CAPHEL. It is envisioned that the electron
C-            direction will ultimately come from tracking. Currently the
C-            cluster center from CACL is used.
C-            Note that this also allows for corrections made
C-            in CAPHEL. Et is calculated from the cluster energy and
C-            cluster center.
C-   Updated  17-DEC-1992   Meenakshi Narain   add switch to run from dst
C-   Updated  17-MAY-1993   Harrison B. Prosper Add full error matrix - V.4
C-   Updated   8-DEC-1993   Marcel Demarteau   Add CALWIN call from CACLFL
C-   Updated   6-MAR-1994   Meenakshi Narain : 
C-                      Add Paul Rubinov's mods -  added third isolation cone
C-   Updated  28-FEB-1995   Meenakshi Narain   Save isolation and emfraction in
C-                              bank version 6 and above
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
C
      LOGICAL FIRST
      INTEGER IER,GZCAEH,NREP,POINT,I,IVERS,IVERS_CACL
      INTEGER NCELL, CENTRAL_TOWER, IETA, IPHI, SIZE
      REAL THETA,PHI,ETA,HLFTHT,ECACL,ECLUS,CORE_RAD,ISO_RAD
      REAL ISO_ET1_RAD,ISO_ET2_RAD,ISO_ET3_RAD
      REAL CONE_RAD(5),E_CONE(5),ET_CONE(5),EM_CONE(5)
      REAL ETACNT,PHICNT
      REAL DCLA,NZTRAKS,ETRANS
      REAL    EMFRAC, FISOL
      LOGICAL USE_TRACK_DIR,RERUN_DST,DO_WINDOW
      REAL WINDOW_ETOT,WINDOW_ET
      DATA FIRST /.TRUE./
      CHARACTER*4 BANK
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('RADIUS_OF_CORE_CONE',CORE_RAD,IER)
        CALL EZGET('RADIUS_OF_ISOLATION_CONE',ISO_RAD,IER)
        CALL EZGET('PHYSICS_ISOLATION_CONE1',ISO_ET1_RAD,IER)
        CALL EZGET('PHYSICS_ISOLATION_CONE2',ISO_ET2_RAD,IER)
        CALL EZGET('PHYSICS_ISOLATION_CONE3',ISO_ET3_RAD,IER)
        CALL EZGET('DO_WINDOW',DO_WINDOW,IER)
        CALL EZGET('USE_TRACK_DIR',USE_TRACK_DIR,IER)
        CALL EZGET('RERUN_CAPHEL_FROM_DST',RERUN_DST,IER)
        CALL EZRSET
      ENDIF
C
      IQ(LPELC+1) = 6               ! Bank version
      IQ(LPELC+2) = 12              ! Particle ID
C
C FILL IN REFERENCE LINKS
C
      LQ(LPELC-2) = LCACL               ! POINTS TO CLUSTER
      LQ(LPELC-3) = LZTRAK_ELECTRON     ! REFERENCE LINK TO ZTRAK TRACK
      LQ(LCACL-6) = LZTRAK_ELECTRON     ! REFERENCE LINK TO ZTRAK TRACK
C
C      REFERENCE LINK FROM ZTRK TO PELC
C
      IF(LZTRAK_ELECTRON.GE.0) LQ(LZTRAK_ELECTRON-4) = LPELC
C
      LCASH = LQ(LCACL-2)
      IF (LCASH.NE.0) THEN
        CALL CASH_ETAPHIMX(LCASH,IETA,IPHI,IER)
      ELSE
        LCACH = LQ(LCACL-1)
        IF(LCACH.GE.0) THEN
          NCELL = IQ(LCACH+2)
          CENTRAL_TOWER = IQ(LCACH+NCELL+3)
          IETA = IQ(LCATE + (CENTRAL_TOWER-1)*14 + 12)
          IPHI = IQ(LCATE + (CENTRAL_TOWER-1)*14 + 13)
        ENDIF
      ENDIF
C
C
C ****  calculate energy in fixed (LBL) window and store in cacl
C
      IF(DO_WINDOW) THEN 
        CALL CALWIN(IETA,IPHI,WINDOW_ETOT,WINDOW_ET,SIZE)
        Q(LCACL+23) = WINDOW_ETOT
      ENDIF
C
C
C
C ****  fill in the rest of the bank here.
C
      ECACL = Q(LCACL+ 7)
      THETA = Q(LCACL+11)
      PHI   = Q(LCACL+12)
      ETA   = Q(LCACL+13)
C
C ****  Get ZTRAK global fit information from ZFIT bank
C
      IF(USE_TRACK_DIR) THEN
C
C ****  May want to impose some selection criteria here, i.e. ensure
C ****  that the track really should be associated with the cluster.
C ****  Until this is resolved the default will be to not use track
C ****  direction. The user can do so from the DST if he so wishes.
C
        IF(LZTRAK_ELECTRON.GT.0) LZFIT = LQ(LZTRAK_ELECTRON-IZZFIT)
        IF(LZFIT.GT.0) THEN
          PHI   = Q(LZFIT+10)         ! Use ZFIT phi
          THETA = Q(LZFIT+13)         ! Use ZFIT theta
          HLFTHT = THETA/2.0
          IF(SIN(HLFTHT)/COS(HLFTHT).GT.0.)THEN
            ETA = -ALOG(SIN(HLFTHT)/COS(HLFTHT))  ! pseudorap of cluster
          ELSEIF(SIN(HLFTHT).EQ.COS(HLFTHT))THEN
            ETA = 0.0
          ENDIF
        ENDIF
      ENDIF
C
      Q(LPELC+3)  = ECLUS*COS(PHI)*SIN(THETA)  ! Ex
      Q(LPELC+4)  = ECLUS*SIN(PHI)*SIN(THETA)  ! Ey
      Q(LPELC+5)  = ECLUS*COS(THETA)           ! Ez
      Q(LPELC+6)  = ECLUS                ! E
      Q(LPELC+7)  = ECLUS*SIN(THETA)     ! Et
      Q(LPELC+8)  = THETA                ! Theta
      Q(LPELC+9)  = ETA                  ! Eta
      Q(LPELC+10) = PHI                  ! Phi
C
      Q(LPELC+11) = 0.0                 ! sig**2(Ex)
      Q(LPELC+12) = 0.0                 ! sig**2(Ey)
      Q(LPELC+13) = 0.0                 ! sig(Et)
      Q(LPELC+26) = 0.0                 ! sig**2(Ez)
      Q(LPELC+27) = 0.0                 ! <dExdEy>
      Q(LPELC+28) = 0.0                 ! <dExdEz>
      Q(LPELC+29) = 0.0                 ! <dEydEz>
C
C ****  Add full error matrix
C
      LCACH = LQ(LCACL-1)
      IF ( LCACH.GT.0 ) THEN
C
        LCAEH = GZCAEH()
        IF ( LCAEH .GT. 0 ) THEN
C
          NCELL = IQ(LCACH+2)
          IVERS = IQ(LCAEH+1)
          NREP  = IQ(LCAEH+2)
C
          DO I =  1, NCELL
            POINT = LCAEH + NREP*(IQ(LCACH+2+I)-1)
            Q(LPELC+11) = Q(LPELC+11) + Q(POINT+9)    !sig(Ex)**2
            Q(LPELC+12) = Q(LPELC+12) + Q(POINT+10)   !sig(Ey)**2
            IF ( IVERS .GE. 3 ) THEN
              Q(LPELC+13) = Q(LPELC+13) + Q(POINT+16)   !sig(Et)**2
              Q(LPELC+26) = Q(LPELC+26) + Q(POINT+17)   !sig(Ez)**2
              Q(LPELC+27) = Q(LPELC+27) + Q(POINT+18)   !<dExdEy>
              Q(LPELC+28) = Q(LPELC+28) + Q(POINT+19)   !<dExdEz>
              Q(LPELC+29) = Q(LPELC+29) + Q(POINT+20)   !<dEydEz>
            ENDIF
          ENDDO
C
C ****  COMPUTE SigEt
C
          Q(LPELC+13) = SQRT(ABS(Q(LPELC+13)))
        ELSE
          CALL ERRMSG('NO_CAEH','PELCFL',' LCAEH = 0','W')
        ENDIF
      ENDIF
C
      Q(LPELC+14) = ETRANS
C
C ****  Compute isolation energies and
C ****  Save some things in CACL if we ever want to rerun from DST
C
      IF (.NOT.RERUN_DST) THEN
        ETACNT = FLOAT(IETA)/10.
        PHICNT = FLOAT(IPHI)/10.
C                                   
C        CALL CONISO(IETA,ETA,PHI,CORE_RAD,ISO_RAD,ECORE,EISOLATION)
C        CALL CONISO_ET(IETA,ETA,PHI,ISO_ET1_RAD,ISO_ET2_RAD,ET1,ET2)
C
C *** skip the old calls to coniso and coniso_et and call cone_iso
C *** just once instead. Also clean up the call. .--- March 6th'94
C
        CONE_RAD(5)= ISO_ET2_RAD    ! 0.7 cone 
        CONE_RAD(4)= ISO_ET3_RAD    ! 0.6 cone 
        CONE_RAD(3)= ISO_ET1_RAD    ! 0.4 cone 
        CONE_RAD(2)= ISO_RAD        ! 0.4 cone 
        CONE_RAD(1)= CORE_RAD       ! 0.2 cone 
        CALL CONE_ISO(IETA,ETA,PHI,CONE_RAD,E_CONE,ET_CONE,EM_CONE)
        Q(LCACL+24) = E_CONE(1)
        Q(LCACL+25) = E_CONE(2)
        Q(LCACL+26) = EM_CONE(1)
        Q(LCACL+27) = EM_CONE(2)
        Q(LCACL+28) = E_CONE(3)     ! 0.4 cone 
        Q(LCACL+29) = ET_CONE(3)               
        Q(LCACL+30) = E_CONE(5)     ! 0.7 cone 
        Q(LCACL+31) = ET_CONE(5)               
        Q(LCACL+32) = E_CONE(4)     ! 0.6 cone 
        Q(LCACL+33) = ET_CONE(4)
      ELSE
        IVERS_CACL=IQ(LCACL+1)
        IF(IVERS_CACL.GE.4)THEN
          E_CONE(1)      = Q(LCACL+24)
          E_CONE(2)      = Q(LCACL+25)
          EM_CONE(1)     = Q(LCACL+26)
          EM_CONE(2)     = Q(LCACL+27)
        ELSE
          E_CONE(1)      = 0.
          E_CONE(2)      = 0.
          EM_CONE(1)     = 0.
          EM_CONE(2)     = 0.
        ENDIF
      ENDIF
C
      Q(LPELC+15) = E_CONE(1)           ! Total Energy in core cone
      Q(LPELC+16) = E_CONE(2)       ! Total Energy in isolation cone
      Q(LPELC+17) = EM_CONE(1)           ! EM Energy in core cone
      Q(LPELC+18) = EM_CONE(2)      ! EM Energy in isolation cone
      Q(LPELC+19) = IETA                ! Calorimeter eta
C
      Q(LPELC+21) = NZTRAKS             ! Number of central tracks in
                                        ! cluster road
      Q(LPELC+22) = DCLA                ! Distance of closest approach
                                        ! of central track
      CALL UCOPY(SHOWER_CENTER,Q(LPELC+23),3)  !center used for roads
C
C ****  Catalogue energy corrections applied to this electron in VCOR
C
      IF((ECLUS-ECACL).NE. 0) THEN
        CALL CATALOGUE_EM_CORR(LPELC,BANK,ECACL,ECLUS,PHI,THETA)
      ENDIF
C
C ****  Add a few quantities which are used in the STANDARD definition
C
      Q(LPELC+32) = 0.0
      Q(LPELC+33) = 0.0
      IF(Q(LPELC+17).GT.0) THEN
        FISOL = (Q(LPELC+16) - Q(LPELC+17))/Q(LPELC+17) ! isolation fraction
        Q(LPELC+32) = FISOL
      ENDIF
      IF (Q(LCACL+17).GT.0) THEN
        EMFRAC =(Q(LCACL+7)-Q(LCACL+19))/Q(LCACL+17) ! EM fraction
        Q(LPELC+33) = EMFRAC
      ENDIF

C----------------------------------------------------------------------
  999 RETURN
      END
