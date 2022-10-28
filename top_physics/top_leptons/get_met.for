      SUBROUTINE GET_MET(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-NOV-1993   Balamurali V
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VERT.INC'
      INCLUDE 'D0$INC:MET.INC'
      REAL     TWOPI
      PARAMETER (TWOPI=6.28318530717958647693)
C
      INTEGER RVERSION,PASS
      INTEGER I,J,N,IER,LPNUT,GZPNUT
      REAL    EM_SCALE(3),MET_CORRECTION(3),MET,MET_PHI,MET_X
      REAL    MET_Y,ERRMET_X,ERRMET_Y,SCAL_MET,ERRMET,SIGMET
      REAL    MET_CORR,MET_PHI_CORR
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('TTEE_RCP')
        CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
        CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_SCALE,IER)
        CALL EZRSET
        CALL RECO_VERSION(RVERSION,PASS)
        FIRST=.FALSE.
      ENDIF
      CALL MISS_ET_CORR(RVERSION,ZVERT,EM_SCALE,MET_CORRECTION)
C     CALL GET_MET_CORRECTION(RVERSION,ZVERT,MET_CORRECTION)
C
C ** Initialize variables
C
      DO I = 1,MAX_TYPE
        DO J = 1,8
          META(J,I) = 0.0
        ENDDO
      ENDDO
C
C ** Now get Missing ET variables
C
      DO I = 1,3
        LPNUT = GZPNUT(I)
        IF(LPNUT.GT.0) THEN
          IF(I.GE.2)THEN
            MET = Q(LPNUT+7)
            MET_PHI = Q(LPNUT+10)
          ENDIF
          MET_X = Q(LPNUT+3)
          MET_Y = Q(LPNUT+4)
          MET_X = MET_X-MET_CORRECTION(1)
          MET_Y = MET_Y-MET_CORRECTION(2)
          ERRMET_X = Q(LPNUT+11)
          ERRMET_Y = Q(LPNUT+12)
          SCAL_MET = Q(LPNUT+14)
          ERRMET   = 0.
          IF (MET.GT.0) THEN
            ERRMET = (MET_X/MET)**2*ERRMET_X+(MET_Y/MET)**2*ERRMET_Y
          ENDIF
          IF(ERRMET.GT.0.) THEN
            ERRMET=SQRT(ERRMET)
            SIGMET = MET/ERRMET
          ELSE
            SIGMET = 0.
          ENDIF
          MET_CORR =  SQRT(MET_X**2+MET_Y**2)
          MET_PHI_CORR = ATAN2(MET_Y,MET_X)
          IF(MET_PHI_CORR.LT.0.)MET_PHI_CORR=MET_PHI_CORR+TWOPI
C
          META(1,I) = MET
          META(2,I) = MET_PHI
          META(3,I) = SCAL_MET
          META(4,I) = MET_CORR
          META(5,I) = MET_PHI_CORR
          META(6,I) = ERRMET_X
          META(7,I) = ERRMET_Y
          META(8,I) = SIGMET
C
        ENDIF
      ENDDO
C
  999 RETURN
      END
