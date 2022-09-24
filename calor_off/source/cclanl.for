      SUBROUTINE CCLANL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills histograms for CAPHEL
C-                         package when used for H matrix development.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Renamed  24-Apr-1990  N.A. Graf (was CPHANL)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:DEADM.INC'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER GZCACL,I,J
      INTEGER IDEPTH,SSUNIT,DMPUNI
      REAL    YDUM,DELI,DELJ
      REAL    RAPC,PHID,PHID1,RAPC1
      REAL SC(3),RADCNT
C
      INCLUDE 'D0$INC:CTRAK.INC'
C
      LOGICAL HMREAD
      LOGICAL DO_CCLANL1,DO_CCLANL2,DO_HMATRIX
C
      REAL    HM_PHILIM(2)
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C BOOK HISTOGRAMS HERE
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('HMATRIX_PHI_LIMITS',HM_PHILIM,IER)
        CALL EZGET_l('DO_HMATRIX_ANALYSIS',DO_HMATRIX,IER)        
        CALL EZGET_l('DO_CCLANL1',DO_CCLANL1,IER)
        CALL EZGET_l('DO_CCLANL2',DO_CCLANL2,IER)
        CALL EZRSET
      ENDIF
C
      IF(.NOT. DO_CCLANL1 .AND. .NOT. DO_CCLANL2) RETURN
C
      CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY2',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CCLANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C      LCACL = GZCACL()                  ! TOP OF LINEAR STRUCTURE
      IF(LCACL.EQ.0)GO TO 999            ! NO CLUSTERS
      LCACH = LQ(LCACL-IZCACH)          ! CACH LINK SET UP.
C
C ****  NOW ANALYZE FIRST OVER CLUSTERS . AND THEN OVER INCOMING TRACKS
C
      CALL CISKIN(NPART,VERT,IDPART,PART)
C
      IF(NPART.LE.0.OR.NPART.GT.1)THEN
        CALL ERRMSG('CALORIMETER','CCLANL',
     &    'WRONG NUMBER OF PARTICLES ','W')
        GO TO 999
      ENDIF
C
C ****  WE NEED TO PICK UP PRIMARY ELECTRON PARAMETERS
C
      UVEC(1,1) = PART(1,1)/PART(4,1)
      UVEC(2,1) = PART(2,1)/PART(4,1)
      UVEC(3,1) = PART(3,1)/PART(4,1)     ! UNIT VECTOR ALONG
C                                        ! ISAJET TRAJECTORY
      PHI(1) = ATAN2(UVEC(2,1),UVEC(1,1))/RADIAN     ! IN DEGREES
      RAP(1) = ATAN2(SQRT(UVEC(2,1)**2+UVEC(1,1)**2),UVEC(3,1))
      RAP(1) = -ALOG(SIN(RAP(1)/2.)/COS(RAP(1)/2.))    ! RAPIDITY OF TRACK
C
      IF(PHI(1).LT.HM_PHILIM(1).OR.PHI(1).GT.HM_PHILIM(2))GO TO 999
C
      IF(DO_CCLANL1)CALL CCLANL1                    ! ANALYZE SHOWER PROFILES
C
      IF(DO_HMATRIX .AND. DO_CCLANL2)CALL CCLANL2   ! ANALYZE H MATRIX
C
      CALL DHDIR('CAPHEL_RCP','HBOOK_DIRECTORY',IER,' ')
  999 RETURN
      END
