      FUNCTION CACL_ESUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Fill ESUM bank with CACL bank info
C-   Returns :
C-     Always returns TRUE
C-   Created  27-JUN-1992   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'
      INTEGER LVERH,GZVERH,LVERT,LCAPH
      INTEGER LCACL,GZCACL,LESUM,GZESUM,GZCAPH,IFLAGS,IER
      REAL    ET,ETA,PHI,ETA_DET,THETA, Z
      REAL    EM_EFRAC, EM_ETFRAC, EM_ERATIO, EM_ETRATIO
      REAL    ETA_FROM_THETA, RTHETA
      LOGICAL CACL_ESUM, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C ****  Statement functions
C
      ETA_FROM_THETA(RTHETA) =  -ALOG(MAX(TAN((RTHETA)/2.),1.E-9))
C----------------------------------------------------------------------
      CACL_ESUM = .TRUE.
      LESUM=GZESUM('CACL')
      IF(LESUM.GT.0) THEN !  bank already exists
        GOTO 999
      ENDIF
C
C ****  read in RCP file
C
      IF (FIRST) THEN 
        CALL INRCP('CACL_ESUM_RCP',IER)                          
        IF(IER.NE.0) THEN                  ! read in RCP file 
          EM_EFRAC = 0.9                    ! failed           
          EM_ETFRAC = 0.9
          CALL ERRMSG('CACL_ESUM',' No RCP',
     &      'Error reading CACL_ESUM_RCP','W')
          CALL ERRMSG('CACL_ESUM','Set EMfrac',
     &      'Set both EM_ERATIO and EM_ETRATIO threshold = 0.9','W')
          GOTO 5
        END IF
        CALL EZPICK('CACL_ESUM_RCP')
        CALL EZGET('CLUSTER_EM_ERATIO_THRESHOLD',EM_EFRAC,IER)
        CALL EZGET('CLUSTER_EM_ETRATIO_THRESHOLD',EM_ETFRAC,IER)
        CALL EZRSET
        FIRST = .FALSE.
    5   CONTINUE
      END IF
      CALL SET_CAPH('ELECTRON',0,IER)
      IF (IER.LT.0) THEN
        CALL ERRMSG('CACL_ESUM','SET-CAPH path',
     &  'Error setting CAPH path to ELECTRON algorithm','W')
        GOTO 998
      END IF
C
C ****  get the event vertex
C
      LVERT=0
      LVERH=GZVERH() 
      IF(LVERH.GT.0) LVERT=LQ(LVERH-IZVERT)
      Z=0.
      IF(LVERT.NE.0) Z = Q(LVERT+5)
C
C ****  Fill Esum with CACL information
C
      LCACL=GZCACL()
      IF(LCACL.NE.0) THEN
C
C ****  Sort banks so they are in increasing order of Et
C
        CALL ZSORT(IXCOM,LCACL,8)
        LCACL=GZCACL()
        CALL ZTOPSY(IXCOM,LCACL)
        LCACL=GZCACL()
C
C ****  Loop over all cacl banks with FH/EM ratio gt EM_FRAC or EM_ETFRAC
C
        DO WHILE (LCACL.GT.0)
          EM_ERATIO  = 0.
          EM_ETRATIO = 0.
          IF (Q(LCACL+17).GT.0.) EM_ERATIO  = Q(LCACL+7)/Q(LCACL+17)
          IF (Q(LCACL+17).GT.0.) EM_ETRATIO = Q(LCACL+8)/Q(LCACL+18)
          IF (EM_ERATIO  .LT. EM_EFRAC .OR. 
     &        EM_ETRATIO .LT. EM_ETFRAC) GOTO 10
          IFLAGS  = 0
          ET      = Q(LCACL+8)
          THETA   = Q(LCACL+11)
          PHI     = Q(LCACL+12)
          ETA     = Q(LCACL+13)
          CALL DET_ETA(Z,THETA,ETA_DET)
C        fill ESUM bank
          CALL ESUMFL('CACL',ID_ELECTRON,ET,ETA,ETA_DET,PHI,IFLAGS)
   10     CONTINUE
          LCACL   = LQ(LCACL)          ! pointer to next CACL
        ENDDO
C
      ENDIF
      CALL RESET_CAPH
  998 CONTINUE
  999 RETURN
      END
