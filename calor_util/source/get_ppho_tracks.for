      SUBROUTINE GET_PPHO_TRACKS(LCLUS,NZTRAKS,LZTRK,VERTEX_ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a PPHO bank, get the link to the
C-                        closest track in road with any primary vertex
C-
C-
C-   Inputs  : LCLUS : link to PPHO bank
C-   Outputs : NZTRAKS  : numbre of tracks in road
C-             LZTRK    : link to the closest track in road
C-                             with any primary vertex
C-             VERTEX_ID     : ID of the vertex with the closest track
C-   Controls:
C-
C-   Created  25-MAR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      REAL    THETA_ROAD_FACT,MIN_THETA_ROAD,DEL_ZVERTEX
      REAL    PHI_CENTER,PHI_LO,PHI_HI,PHI_ROAD
      REAL    THETA_NEW,THETA_NLO,THETA_NHI
      REAL    ZV(14),DZ(14),ZVTX_INFO(3,14),PTDUM
      REAL    RHO,DELZ,TRK_CENT(3),DIR_COS(3),DCL,DCLA,SHOWER_CENTER(3)
      REAL    TRKTHETA, TRKPHI
      INTEGER NVER,NV,NZTRAKS,VERTEX_ID,J,I,II,IER
      INTEGER LRCP,LCLUS,LZTRK,LCACL,LZFIT
      INTEGER NEWTRAKS,ZLINK_NEWTRAKS(450)

      LOGICAL first,OK,OKZ
      SAVE first
      DATA first / .true. /

C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
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
C ****  first get all vertices in the event
C
      DCLA = 999.
      NZTRAKS = 0
      LZTRK = 0
      VERTEX_ID = 0 

      CALL VZERO(ZV,14)
      CALL VZERO(DZ,14)
      CALL VERTEX_INFO(14,NVER,ZVTX_INFO,OKZ) ! Vertex from tracking
      IF ( OKZ ) THEN
        IF(NVER.EQ.0) THEN
          CALL ERRMSG('No Vertices','CAPHEL','z set to 0','W')
          goto 999
        ELSE
          NV = MIN(14,NVER)
          DO I = 1, NV
            ZV(I) = ZVTX_INFO(1,I)
            DZ(I) = ZVTX_INFO(2,I)
          ENDDO
        ENDIF
      ELSE
        CALL ERRMSG('Error getting Vertices','CAPHEL','z set to 0','W')
        goto 999
      ENDIF
C
C ****  get the Phi road limits
C
      LCACL = LQ(LCLUS-2)
      PTDUM = Q(LCACL+8)   !  Et of shower
      CALL UCOPY(Q(LCACL+14),SHOWER_CENTER,3)  ! shower Centroid
      RHO = SQRT(SHOWER_CENTER(2)**2+SHOWER_CENTER(1)**2)
      PHI_CENTER = ATAN2(SHOWER_CENTER(2),SHOWER_CENTER(1))
      IF(PHI_CENTER.LT.0.0)PHI_CENTER = PHI_CENTER + TWOPI
      PHI_LO = PHI_CENTER - PHI_ROAD
      PHI_HI = PHI_CENTER + PHI_ROAD
C
C ****  Loop over all vertices
C
      DO J = 2,NV
        IF(DZ(J).LT.DEL_ZVERTEX)DZ(J) = DEL_ZVERTEX   ! Minimum value
        DELZ = DZ(J)*THETA_ROAD_FACT
        THETA_NEW = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J))
        THETA_NLO = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J)+DELZ)
        THETA_NHI  = ATAN2(RHO,SHOWER_CENTER(3)-ZV(J)-DELZ)
C ****  Impose minimum road size...
        IF(ABS(THETA_NHI-THETA_NLO).LT. 2.*MIN_THETA_ROAD) THEN
          THETA_NLO = THETA_NEW - MIN_THETA_ROAD
          THETA_NHI = THETA_NEW + MIN_THETA_ROAD
        ENDIF
C ****  Call ZTRK_IN_ROAD to get the links to all tracks in road...
        CALL ZTRK_IN_ROAD(ZV(J),PHI_LO,PHI_HI,THETA_NLO,THETA_NHI,
     &        NEWTRAKS,ZLINK_NEWTRAKS)
        DO II = 1,NEWTRAKS
          NZTRAKS = NZTRAKS+1
          LZFIT = LQ(ZLINK_NEWTRAKS(II)-IZZFIT)
          trkphi    = Q(LZFIT+10)
          trktheta  = Q(LZFIT+13)
          DIR_COS(1) = SIN(TRKTHETA)*COS(TRKPHI)
          DIR_COS(2) = SIN(TRKTHETA)*SIN(TRKPHI)
          DIR_COS(3) = COS(TRKTHETA)
          TRK_CENT(1) = Q(LZFIT+11)
          TRK_CENT(2) = Q(LZFIT+12)
          TRK_CENT(3) = Q(LZFIT+15)
          CALL CLOSE_DIST(SHOWER_CENTER,TRK_CENT,DIR_COS,DCL)
          IF (DCL .LT. DCLA) THEN
            DCLA = DCL
            LZTRK = ZLINK_NEWTRAKS(II)
            VERTEX_ID = J
          ENDIF
        ENDDO
      ENDDO
C
  999 RETURN
      END
