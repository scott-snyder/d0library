      SUBROUTINE EM_ETZV(LCLUS,NVERT,ET_NEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LPELC
C-   Outputs : nvert : number of vertices with track in road for the PELC
C-             et_new (1:nvert) : et w.r.t. nvert-th primary vertex
C-   Controls:
C-
C-   Created  15-JUL-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NVERT,NV,LCLUS,NZTRAKS,I,NEVOLD
      REAL    ET_NEW(10),TH,ECLUS,RHO,ZCLUS
      REAL    ZVTX_INFO(3, 10),ZV,DZ
      REAL    THETA_ROAD_FACT,MIN_THETA_ROAD,DEL_ZVERTEX
      REAL    PHI_CENTER,PHI_LO,PHI_HI,PHI_ROAD
      REAL    THETA_NEW,THETA_NLO,THETA_NHI
      REAL    PTDUM
      REAL    DELZ,SHOWER_CENTER(3)
      INTEGER IER,LRCP,LCACL 
      INTEGER NEWTRAKS,ZLINK_NEWTRAKS(450)

      LOGICAL first,OK
      SAVE first
      DATA first / .true. /

      LOGICAL OKZ
      SAVE    NEVOLD
C----------------------------------------------------------------------
      CALL VZERO(ET_NEW,10)
      IF( first ) THEN
        first = .false.
        CALL EZLOC('CAPHEL_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('CAPHEL_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('CAPHEL_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('GET_PPHO_TRACKS','GET_PPHO_TRACKS',
     &        ' CAPHEL_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('PHI_ROAD',PHI_ROAD,IER)
        CALL EZGET('THETA_ROAD_FACT',THETA_ROAD_FACT,IER)
        CALL EZGET('MIN_THETA_ROAD',MIN_THETA_ROAD,IER)
        CALL EZGET('MIN_VERTEX_Z_ERROR',DEL_ZVERTEX,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('GET_PPHO_TRACKS','GET_PPHO_TRACKS',
     &        'problem reading CAPHEL RCP parameters','F')
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  Get Zvertex
C
      if (nevold.ne.IQ(LHEAD+9)) then
        NEVOLD = IQ(LHEAD+9)
        CALL VERTEX_INFO(10,NV,ZVTX_INFO,OKZ)
        NVERT = MIN(10,NV)
      endif
      IF (.NOT.OKZ) GOTO 999
C **** Get Zclus,rho
      ZCLUS = Q(LCLUS+25)
      RHO   = SQRT(Q(LCLUS+23)**2+Q(LCLUS+24)**2)
      ECLUS= Q(LCLUS+6)
C
      DO I = 1, NVERT
        ZV = ZVTX_INFO(1,I)
        DZ = ZVTX_INFO(2,I)
C ****  get road size
        NZTRAKS = 0
        LCACL = LQ(LCLUS-2)
        PTDUM = Q(LCACL+8)   !  Et of shower
        CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)  ! shower Centroid
        RHO = SQRT(SHOWER_CENTER(2)**2+SHOWER_CENTER(1)**2)
        PHI_CENTER = ATAN2(SHOWER_CENTER(2),SHOWER_CENTER(1))
        IF(PHI_CENTER.LT.0.0)PHI_CENTER = PHI_CENTER + TWOPI
        PHI_LO = PHI_CENTER - PHI_ROAD
        PHI_HI = PHI_CENTER + PHI_ROAD
        IF(DZ.LT.DEL_ZVERTEX)DZ = DEL_ZVERTEX   ! Minimum value
        DELZ = DZ*THETA_ROAD_FACT
        THETA_NEW = ATAN2(RHO,SHOWER_CENTER(3)-ZV)
        THETA_NLO = ATAN2(RHO,SHOWER_CENTER(3)-ZV+DELZ)
        THETA_NHI  = ATAN2(RHO,SHOWER_CENTER(3)-ZV-DELZ)
        IF(ABS(THETA_NHI-THETA_NLO).LT. 2.*MIN_THETA_ROAD) THEN
          THETA_NLO = THETA_NEW - MIN_THETA_ROAD
          THETA_NHI = THETA_NEW + MIN_THETA_ROAD
        ENDIF
C ****  Call ZTRK_IN_ROAD to get the links to all tracks in road...
        CALL ZTRK_IN_ROAD(ZV,PHI_LO,PHI_HI,THETA_NLO,THETA_NHI,
     &        NEWTRAKS,ZLINK_NEWTRAKS)
        NZTRAKS = NEWTRAKS
        IF (NZTRAKS.GT.0) THEN
          TH  =  ATAN2(RHO,ZCLUS-ZV)
          ET_NEW(I) = ECLUS*SIN(TH)
        ENDIF
      ENDDO
  999 RETURN
      END
