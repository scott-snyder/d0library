      SUBROUTINE PPHOFL(ETRANS,ECLUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank PPHO
C-
C-   Inputs  :
C-            ETRANS     energy in EM cluster outside central tower
C-            ECLUS      Energy of EM cluster
C-   Outputs : LPPHO is output if on input LPPHO =0
C-
C-   Controls:
C-
C-   Created   6-APR-1990 11:48:48.44  Rajendran Raja
C-   Added to  10-MAY-1990 N.A. Graf
C-   Updated  28-SEP-1992   Norman A. Graf
C-            ECLUS is now input to allow for corrections made to
C-            the cluster in CAPHEL. The cluster center from CACL is
C-            used. Note that this also allows for corrections made
C-            in CAPHEL. Et is calculated from the cluster energy and
C-            cluster center.
C-   Updated  17-DEC-1992   Meenakshi Narain   add switch to run from dst
C-   Updated  17-MAY-1993   Harrison B. Prosper Add full error matrix - V.4
C-   Updated   8-DEC-1993   Marcel Demarteau   Add CALWIN call from CACLFL
C-   Updated   6-MAR-1994   Meenakshi Narain : incorporate 
C-                        Paul Rubinov's changes to add third isolation cone
C-   Updated  28-SEP-1994   Meenakshi Narain  Update bank version number to 5 
C-   Updated  28-FEB-1995   Meenakshi Narain   Save isolation and emfraction in
C-                              bank version 6 and above
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
C
      REAL THETA,PHI,ETA,ECACL,ECLUS,CORE_RAD,ISO_RAD
      REAL ISO_ET1_RAD,ISO_ET2_RAD,ISO_ET3_RAD
      REAL CONE_RAD(5),E_CONE(5),ET_CONE(5),EM_CONE(5)
      REAL ETACNT,PHICNT
      REAL ETRANS
      REAL WINDOW_ETOT,WINDOW_ET
      REAL    EMFRAC, FISOL
      INTEGER NCELL, CENTRAL_TOWER, IETA, IPHI,IVERS,IVERS_CACL
      INTEGER IER,GZCAEH,NREP,POINT,I,SIZE
      LOGICAL FIRST,RERUN_DST,DO_WINDOW 
      DATA FIRST/.TRUE./
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
        CALL EZGET_l('DO_WINDOW',DO_WINDOW,IER)
        CALL EZGET_l('RERUN_CAPHEL_FROM_DST',RERUN_DST,IER)
        CALL EZRSET
        BANK = 'PPHO'
      ENDIF
C
      IQ(LPPHO+1) = 6               ! Bank version
      IQ(LPPHO+2) = 10              ! Particle ID
C fill in the rest of the bank here.
C----------------------------------------------------------------------
C FILL IN REFERENCE LINKS
      LQ(LPPHO-2) = LCACL               ! POINTS TO CLUSTER
C
C ****  temporary handling of possible multi-vertex events
C
      LQ(LPPHO-3) = LZTRAK_ELECTRON     ! REFERENCE LINK TO ZTRAK TRACK
      LQ(LCACL-6) = LZTRAK_ELECTRON     ! REFERENCE LINK TO ZTRAK TRACK
C
C      REFERENCE LINK FROM ZTRK TO PELC
C
      IF(LZTRAK_ELECTRON.GT.0) LQ(LZTRAK_ELECTRON-4) = LPPHO
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
C fill in the rest of the bank here.
C
      ECACL = Q(LCACL+ 7)
      THETA = Q(LCACL+11)
      PHI   = Q(LCACL+12)
      ETA   = Q(LCACL+13)
      Q(LPPHO+3)  = ECLUS*COS(PHI)*SIN(THETA)  ! Ex
      Q(LPPHO+4)  = ECLUS*SIN(PHI)*SIN(THETA)  ! Ey
      Q(LPPHO+5)  = ECLUS*COS(THETA)           ! Ez
      Q(LPPHO+6)  = ECLUS                ! E
      Q(LPPHO+7)  = ECLUS*SIN(THETA)     ! Et
      Q(LPPHO+8)  = THETA                ! Theta
      Q(LPPHO+9)  = ETA                  ! Eta
      Q(LPPHO+10) = PHI                  ! Phi
C
      Q(LPPHO+11) = 0.0                 ! sig**2(Ex)
      Q(LPPHO+12) = 0.0                 ! sig**2(Ey)
      Q(LPPHO+13) = 0.0                 ! sig(Et)
      Q(LPPHO+26) = 0.0                 ! sig**2(Ez)
      Q(LPPHO+27) = 0.0                 ! <dExdEy>
      Q(LPPHO+28) = 0.0                 ! <dExdEz>
      Q(LPPHO+29) = 0.0                 ! <dEydEz>
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
          IVERS = IQ(LCAEH+1)   ! Get version number
          NREP  = IQ(LCAEH+2)
C
          DO I =  1, NCELL
            POINT = LCAEH + NREP*(IQ(LCACH+2+I)-1)
            Q(LPPHO+11) = Q(LPPHO+11) + Q(POINT+9)    !sig(Ex)**2
            Q(LPPHO+12) = Q(LPPHO+12) + Q(POINT+10)   !sig(Ey)**2
C
C ****  Check version number of CAEH
C
            IF ( IVERS .GE. 3 ) THEN
              Q(LPPHO+13) = Q(LPPHO+13) + Q(POINT+16)   !sig(Et)**2
              Q(LPPHO+26) = Q(LPPHO+26) + Q(POINT+17)   !sig(Ez)**2
              Q(LPPHO+27) = Q(LPPHO+27) + Q(POINT+18)   !<dExdEy>
              Q(LPPHO+28) = Q(LPPHO+28) + Q(POINT+19)   !<dExdEz>
              Q(LPPHO+29) = Q(LPPHO+29) + Q(POINT+20)   !<dEydEz>
            ENDIF
          ENDDO
C
C ****  COMPUTE SigEt
C
          Q(LPPHO+13) = SQRT(ABS(Q(LPPHO+13)))
        ELSE
          CALL ERRMSG('NO_CAEH','PPHOFL',' LCAEH = 0','W')
        ENDIF
      ENDIF
C
      Q(LPPHO+14) = ETRANS
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
C *** just once instead. Also clean up the call.--- March 6th'94
C
        CONE_RAD(5)= ISO_ET2_RAD      ! 0.7 cone 
        CONE_RAD(4)= ISO_ET3_RAD      ! 0.6 cone 
        CONE_RAD(3)= ISO_ET1_RAD      ! 0.4 cone 
        CONE_RAD(2)= ISO_RAD          ! 0.4 cone 
        CONE_RAD(1)= CORE_RAD         ! 0.2 cone 
        CALL CONE_ISO(IETA,ETA,PHI,CONE_RAD,E_CONE,ET_CONE,EM_CONE)
        Q(LCACL+24) = E_CONE(1)
        Q(LCACL+25) = E_CONE(2)
        Q(LCACL+26) = EM_CONE(1)
        Q(LCACL+27) = EM_CONE(2)
        Q(LCACL+28) = E_CONE(3)       ! 0.4 cone 
        Q(LCACL+29) = ET_CONE(3)                 
        Q(LCACL+30) = E_CONE(5)       ! 0.7 cone 
        Q(LCACL+31) = ET_CONE(5)                 
        Q(LCACL+32) = E_CONE(4)       ! 0.6 cone 
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
      Q(LPPHO+15) = E_CONE(1)            ! Energy in core cone
      Q(LPPHO+16) = E_CONE(2)            ! Energy in isolation cone
      Q(LPPHO+17) = EM_CONE(1)           ! EM Energy in core cone
      Q(LPPHO+18) = EM_CONE(2)          ! EM Energy in isolation cone
      Q(LPPHO+19) = IETA                ! Calorimeter eta
      CALL UCOPY(SHOWER_CENTER,Q(LPPHO+23),3) !Store used shower_center
C
C ****  Catalogue energy corrections applied to this electron in VCOR
C
      IF((ECLUS-ECACL) .NE. 0) THEN
        CALL CATALOGUE_EM_CORR(LPELC,BANK,ECACL,ECLUS,PHI,THETA)
      ENDIF
C
C ****  Add a few quantities which are used in the STANDARD definition
C
      Q(LPPHO+32) = 0.0
      Q(LPPHO+33) = 0.0
      IF(Q(LPPHO+17).GT.0) THEN
        FISOL = (Q(LPPHO+16) - Q(LPPHO+17))/Q(LPPHO+17) ! isolation fraction
        Q(LPPHO+32) = FISOL
      ENDIF
      IF (Q(LCACL+17).GT.0) THEN
        EMFRAC =(Q(LCACL+7)-Q(LCACL+19))/Q(LCACL+17) ! EM fraction
        Q(LPPHO+33) = EMFRAC
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
