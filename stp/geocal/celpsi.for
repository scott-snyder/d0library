      FUNCTION CELPSI( IETA, IDEPTH, IGROUP, IPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To return the angle theta to describe
C-      a trapizoid shape for the central calorimeter cells
C-
C-   Inputs  :     IETA      physics variable eta
C-                 IDEPTH    physics variable layer
C-                 IGROUP    identification of "sub layer"
C-                 IPHI      physics variable phi
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-DEC-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:LCLYR.INC'
      INTEGER  IETA, IDEPTH, IGROUP, IPHI
      REAL CELPSI, PSI, PHI, DPHI
C
      CELPSI = 999.                    ! unrealistic number for error return
      IF( JREGN .NE. ICCAL) RETURN     ! central calorimeter only
C
      DPHI = C(LQCLAY + ILDPHI)        ! subtended angle of layer pad 
      IF (IDEPTH .EQ. MNLYEM .OR. IDEPTH .EQ. MNLYEM+1 .OR. 
     +    IDEPTH .EQ. MXLYEM ) THEN    ! floor 1, 2 or 4
        IF ( MOD( IPHI, 2) .EQ. 1) THEN
          CELPSI = -DPHI/2.             ! odd iphi
        ELSE
          CELPSI = DPHI/2.            ! even iphi
        END IF
      ELSE IF (IDEPTH .EQ. LYEM3A .OR. IDEPTH .EQ. LYEM3C) THEN
        IF (MOD( IPHI, 2) .EQ. 1) THEN 
          CELPSI = -DPHI/2.             ! odd iphi
        ELSE
          CELPSI = 3*DPHI/2.          ! even iphi
        END IF
      ELSE IF (IDEPTH .EQ. LYEM3B .OR. IDEPTH .EQ. LYEM3D) THEN
        IF (MOD( IPHI, 2) .EQ. 1) THEN
          CELPSI = -3*DPHI/2.           ! odd iphi
        ELSE
          CELPSI = DPHI/2.            ! even iphi
        END IF
      ELSE IF (IDEPTH .GE. MNLYFH .AND. IDEPTH .LE. MXLYFH) THEN
        IF (MOD(IPHI,4) .EQ. 1) THEN
          CELPSI = 3*DPHI/2.             ! IPHI = 1
        ELSE IF (MOD(IPHI,4) .EQ. 2) THEN
          CELPSI = -3*DPHI/2.            ! IPHI = 2
        ELSE IF (MOD(IPHI,4) .EQ. 3) THEN
          CELPSI = -DPHI/2.          ! IPHI = 3
        ELSE IF (MOD(IPHI,4) .EQ. 0) THEN
          CELPSI = DPHI/2.           ! IPHI = 4 (wrap around)
        END IF
      ELSE IF (IDEPTH .EQ. MNLYCH) THEN
        IF (MOD(IPHI,4) .EQ. 1) THEN
          CELPSI = -DPHI/2.         ! IPHI = 1
        ELSE IF (MOD(IPHI,4) .EQ. 2) THEN
          CELPSI = DPHI/2.          ! IPHI = 2 (wrap around)
        ELSE IF (MOD(IPHI,4) .EQ. 3) THEN
          CELPSI = 3*DPHI/2.             ! IPHI = 3
        ELSE IF (MOD(IPHI,4) .EQ. 0) THEN
          CELPSI = -3*DPHI/2.            ! IPHI = 4
        END IF
      END IF
C
C----------------------------------------------------------------------
  999 RETURN
      END
