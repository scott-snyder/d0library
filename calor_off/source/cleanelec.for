      SUBROUTINE CLEANELEC(LPELC,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a link to a PELC bank, this subroutine
C-                         chases down the links to all the associated
C-                         banks and returns a logical which identifies
C-                         the electron as being good or not
C-
C-  ****THIS ROUTINE IS CURRENTLY INTENDED FOR MC EVENTS ONLY****
C-
C-      THE CUTS ARE FAIRLY HARSH, THE INTENTION BEING TO GET
C-               THE CLEANEST SAMPLE POSSIBLE.
C-
C-   Inputs  :  LPELC      Link to PELC bank
C-   Outputs :  OK         true if electron passes quality cuts
C-   Controls:
C-
C-   Created  26-AUG-1991   NORMAN A. GRAF
C-   Updated  10-JAN-1992   NORMAN A. GRAF  Changed to CAPHEL RCP
C-                                          control with default
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LPELC,LCACL,LZTRK,LZFIT,LTRDT
C
      LOGICAL TRACK_INFO,TRD_INFO,LMIP
      REAL PHI_ELEC,THETA_ELEC
      REAL PHI_TRK,THETA_TRK,DIFFPHI,DIFFTHETA
      REAL ETOT_LIKE,ETOT_LEFF
      REAL CORELEC,ISOLEC
C
      REAL ECLUS
      REAL DCLA,NZTRAKS,MIP
C
      REAL CDCMIP_CUT,FDCMIP_CUT,ETOT_LIKE_CUT,NZTRAKS_CUT
      REAL DIFFPHI_CUT,DIFFTHETA_CUT,DCLA_CUT
      REAL CORELEC_CUT,ISOLEC_CUT
C
      REAL DIFF_PHI
C
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZERR(IER)
        IF(IER.NE.0) GOTO 200
C
        CALL EZGET('CDCMIP_CUT',CDCMIP_CUT,IER)
        CALL EZGET('FDCMIP_CUT',FDCMIP_CUT,IER)
        CALL EZGET('ETOT_LIKE_CUT',ETOT_LIKE_CUT,IER)
        CALL EZGET('NZTRAKS_CUT',NZTRAKS_CUT,IER)
        CALL EZGET('DIFFPHI_CUT',DIFFPHI_CUT,IER)
        CALL EZGET('DIFFTHETA_CUT',DIFFTHETA_CUT,IER)
        CALL EZGET('DCLA_CUT',DCLA_CUT,IER)
        CALL EZGET('CORELEC_CUT',CORELEC_CUT,IER)
        CALL EZGET('ISOLEC_CUT',ISOLEC_CUT,IER)
C
  200   IF(IER .NE. 0) THEN
          CALL ERRMSG(' NO_CAPHEL_RCP','CLEANELEC',
     &        ' USING DEFAULT PARAMETERS','W')
          CDCMIP_CUT    = 1000.
          FDCMIP_CUT    = 2.
          ETOT_LIKE_CUT = 0.
          NZTRAKS_CUT   = 1.
          DIFFPHI_CUT   = 0.03
          DIFFTHETA_CUT = 0.03
          DCLA_CUT      = 3.3
          CORELEC_CUT   = 1.0
          ISOLEC_CUT    = 2.5
        ENDIF
      ENDIF
  100 CONTINUE
      OK = .TRUE.
      LMIP = .TRUE.
      TRD_INFO = .FALSE.
      TRACK_INFO = .FALSE.
C
      LCACL = LQ(LPELC-2)             ! Link to associated CACL bank
      LZTRK = LQ(LPELC-3)             ! Link to associated ZTRAK bank
      IF(LZTRK .GT. 0) THEN
        TRACK_INFO = .TRUE.
        IF(LQ(LZTRK-7).NE.0) THEN       ! CDC
          MIP = Q(LQ(LZTRK-7)+20)       ! Ionization in chamber
          IF(MIP .GE. CDCMIP_CUT ) LMIP = .FALSE.
        ELSEIF(LQ(LZTRK-8).NE.0) THEN   ! FDC
          MIP = Q(LQ(LZTRK-8)+20)       ! Number of min. ionizing
                                        ! particles
          IF(MIP .GT. FDCMIP_CUT) LMIP = .FALSE.
        ENDIF
        LTRDT = LQ(LZTRK-9)           ! Link to associated TRD bank
        IF(LTRDT .GT. 0) THEN
          ETOT_LIKE = Q(LTRDT+6)        ! Likelihood based on total
                                        ! anode energy
          ETOT_LEFF = Q(LTRDT+16)       ! Efficiency derived from likelihood
          TRD_INFO = .TRUE.
        ENDIF
        LZFIT = LQ(LZTRK-1)           ! Link to global fit
        IF(LZFIT .GT. 0) THEN
          PHI_TRK   = Q(LZFIT+10)
          THETA_TRK = Q(LZFIT+13)
        ELSE
          IF(LQ(LZTRK-7).NE.0) THEN       ! CDC ONLY
            PHI_TRK   = Q(LQ(LZTRK-7)+6)
            THETA_TRK = Q(LQ(LZTRK-7)+9)
          ELSEIF(LQ(LZTRK-8).NE.0) THEN   ! FDC ONLY
            PHI_TRK   = Q(LQ(LZTRK-7)+6)
            THETA_TRK = Q(LQ(LZTRK-7)+22)
          ENDIF
        ENDIF
      ENDIF
C
      PHI_ELEC  = Q(LPELC+10)
      THETA_ELEC = Q(LPELC+8)
      NZTRAKS   = Q(LPELC+21)
      DCLA      = Q(LPELC+22)
      DIFFPHI   = DIFF_PHI(PHI_ELEC,PHI_TRK)
      DIFFTHETA = THETA_ELEC - THETA_TRK
C
      ECLUS = Q(LPELC+6)
      CORELEC = Q(LPELC+15) - Q(LPELC+6)
      ISOLEC  = Q(LPELC+16) - Q(LPELC+6)
C
C ****  Energy in cone (R=0.2) should be within 1.0 Gev of cluster energy
C
      IF(abs(corelec) .GT. CORELEC_CUT) OK = .FALSE.
C
C ****  ISOLATION REQUIREMENT: Energy in cone (R=0.4) around
C ****  electron is less than 2.5 GeV more than energy of cluster
C
      IF(ISOLEC .GT. ISOLEC_CUT .OR. isolec .LT. 0) OK = .FALSE.
C
C ****  TRACK MATCH REQUIREMENT
C
      IF(NZTRAKS .GT. NZTRAKS_CUT) OK = .FALSE. !One and only one track in road
      IF(.NOT. LMIP) OK = .FALSE.        !Should be minimum ionizing
      IF(TRACK_INFO .AND. DIFFPHI .GT. DIFFPHI_CUT) OK = .FALSE. !match in phi
      IF(TRACK_INFO .AND. DIFFTHETA .GT. DIFFTHETA_CUT) OK = .FALSE. !match in
                                                                     ! theta
      IF(DCLA .GT. DCLA_CUT) OK = .FALSE.           !distance of closest
                                                    ! approach
C
C ****  TRD INFORMATION
C
      IF(TRD_INFO .AND. ETOT_LIKE .LT. ETOT_LIKE_CUT) OK = .FALSE.
C
  999 RETURN
      END
