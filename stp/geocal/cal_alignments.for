      SUBROUTINE CAL_ALIGNMENTS 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To provide supplementary corrections to the
C-                         Surveyed positions.  These corrections are
C-                         read in from the survey RCP files.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-DEC-1992   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      REAL    DXEM(3), DXFH(3), DXCH(3), DXSIH(3), DXSMH(3), DXSOH(3)
      REAL    DXSEM(3), DXNEM(3), DXNIH(3), DXNMH(3), DXNOH(3), DX(3)
      INTEGER IERR, IDEN
      CHARACTER*48 REMARK
      LOGICAL DEB
      DATA DEB/.TRUE./
C
C----------------------------------------------------------------------
C
      CALL EZPICK('CC_SURVEY_RCP')
      CALL EZGET('CC_EM_TRANSL_CORRECTION', DXEM, IERR)
      IF( IERR .NE. 0) DXEM(1) = -999.
      CALL EZGET('CC_FH_TRANSL_CORRECTION', DXFH, IERR)
      IF( IERR .NE. 0) DXFH(1) = -999.
      CALL EZGET('CC_CH_TRANSL_CORRECTION', DXCH, IERR)
      IF( IERR .NE. 0) DXCH(1) = -999.
      CALL EZRSET
C
      CALL EZPICK('ECN_SURVEY_RCP')
      CALL EZGET('ECN_EM_TRANSL_CORRECTION', DXNEM, IERR)
      IF( IERR .NE. 0) DXNEM(1) = -999.
      CALL EZGET('ECN_IH_TRANSL_CORRECTION', DXNIH, IERR)
      IF( IERR .NE. 0) DXNIH(1) = -999.
      CALL EZGET('ECN_MH_TRANSL_CORRECTION', DXNMH, IERR)
      IF( IERR .NE. 0) DXNMH(1) = -999.
      CALL EZGET('ECN_OH_TRANSL_CORRECTION', DXNOH, IERR)
      IF( IERR .NE. 0) DXNOH(1) = -999.
      CALL EZRSET
C
      CALL EZPICK('ECS_SURVEY_RCP')
      CALL EZGET('ECS_EM_TRANSL_CORRECTION', DXSEM, IERR)
      IF( IERR .NE. 0) DXSEM(1) = -999.
      CALL EZGET('ECS_IH_TRANSL_CORRECTION', DXSIH, IERR)
      IF( IERR .NE. 0) DXSIH(1) = -999.
      CALL EZGET('ECS_MH_TRANSL_CORRECTION', DXSMH, IERR)
      IF( IERR .NE. 0) DXSMH(1) = -999.
      CALL EZGET('ECS_OH_TRANSL_CORRECTION', DXSOH, IERR)
      IF( IERR .NE. 0) DXSOH(1) = -999.
      CALL EZRSET
C
      LQCLIN = LC( LCGEH - IZCLIN)
      DO WHILE ( LQCLIN .NE. 0)
        IDEN = IC(LQCLIN + IGMDID)/100 * 100
        IF( IDEN .EQ. ICCEMA) THEN    ! CCEM
          IF( DXEM(1) .EQ. -999.) GO TO 100
          REMARK = ' CCEM TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXEM, DX, 3)
        ELSEIF ( IDEN .EQ. ICCFHA) THEN
          IF( DXFH(1) .EQ. -999.) GO TO 100
          REMARK = ' CCFH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXFH, DX, 3)
        ELSEIF ( IDEN .EQ. ICCCHA) THEN
          IF( DXCH(1) .EQ. -999.) GO TO 100
          REMARK = ' CCCH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXCH, DX, 3)
C
        ELSEIF (ABS(IDEN).EQ.ICEEMA.AND.(IC(LQCLIN+1).EQ.2)) THEN
          IF( DXNEM(1) .EQ. -999.) GO TO 100
          REMARK = ' NECEM TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXNEM, DX, 3)
        ELSEIF ((ABS(IDEN) .EQ.  ICIFHA .OR. ABS(IDEN) .EQ. ICICHA)
     +    .AND.(IC(LQCLIN+1).EQ.2) ) THEN
          IF( DXNIH(1) .EQ. -999.) GO TO 100
          REMARK = ' NECIH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXNIH, DX, 3)
        ELSEIF ((ABS(IDEN) .EQ.  ICMFHA .OR. ABS(IDEN) .EQ. ICMCHA)
     +    .AND.(IC(LQCLIN+1).EQ.2) ) THEN
          IF( DXNMH(1) .EQ. -999.) GO TO 100
          REMARK = ' NECMH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXNMH, DX, 3)
        ELSEIF (( ABS(IDEN) .EQ. ICOCHA).AND.(IC(LQCLIN+1).EQ.2))THEN
          IF( DXNOH(1) .EQ. -999.) GO TO 100
          REMARK = ' NECOH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXNOH, DX, 3)
C
        ELSEIF (ABS(IDEN).EQ.ICEEMA.AND.(IC(LQCLIN+1).EQ.3)) THEN
          IF( DXSEM(1) .EQ. -999.) GO TO 100
          REMARK = ' PECEM TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXSEM, DX, 3)
        ELSEIF ((ABS(IDEN) .EQ.  ICIFHA .OR. ABS(IDEN) .EQ. ICICHA)
     +    .AND. (IC(LQCLIN+1).EQ.3)) THEN
          IF( DXSIH(1) .EQ. -999.) GO TO 100
          REMARK = ' PECIH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXSIH, DX, 3)
        ELSEIF ((ABS(IDEN) .EQ.  ICMFHA .OR. ABS(IDEN) .EQ. ICMCHA)
     +    .AND.(IC(LQCLIN+1).EQ.3) ) THEN
          IF( DXSMH(1) .EQ. -999.) GO TO 100
          REMARK = ' PECMH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXSMH, DX, 3)
        ELSEIF (( ABS(IDEN) .EQ. ICOCHA).AND.(IC(LQCLIN+1).EQ.3))THEN
          IF( DXSOH(1) .EQ. -999.) GO TO 100
          REMARK = ' PECOH TRANSLATION FACTOR APPLIED'
          CALL UCOPY( DXSOH, DX, 3)
        ELSE
          GO TO 100
        END IF
        IF ( (DEB) ) THEN
          WRITE(6,1) REMARK
        ENDIF
    1   FORMAT(1X,A)
        C( LQCLIN + IGDTX) = C(LQCLIN + IGDTX) - DX(1)
        C( LQCLIN + IGDTY) = C(LQCLIN + IGDTY) - DX(2)
        C( LQCLIN + IGDTZ) = C(LQCLIN + IGDTZ) - DX(3)
        IC( LQCLIN + IGATYP) = 2
  100   CONTINUE
        LQCLIN = LC(LQCLIN)
      END DO
  999 RETURN
      END
