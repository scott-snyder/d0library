      SUBROUTINE CL2_SHRINK_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to drop unused arrays in SRCP_ECAL & SRCP_UCAL
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
C
      INTEGER I, IER_CL2, IER, LENGTH
      INTEGER NWORDS
      CHARACTER NAME*32
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP('CAL_SHRINK_GEOM_RCP',IER)    ! Read in RCP file
        CALL EZPICK('CAL_SHRINK_GEOM_RCP')       
        DO I = 1, 1000
          CALL EZGETS('ECAL_DELETE', I, NAME, LENGTH, IER_CL2)
          IF (IER_CL2 .EQ. 0 .AND. LENGTH .NE. 0) THEN
            CALL EZPICK ('SRCP_ECAL')
            CALL EZDELETE (NAME, IER)
            CALL EZRSET 
          ELSE
            GO TO 20      !no more names to find
          ENDIF
        ENDDO
   20   CONTINUE
        CALL EZRSET   !cal_shrink_geom_rcp
        CALL EZPICK ('SRCP_ECAL')
        CALL EZSQUEEZE(NWORDS)
        CALL EZRSET
        CALL EZPICK('CAL_SHRINK_GEOM_RCP')
        DO I = 1, 1000
          CALL EZGETS('UCAL_DELETE', I, NAME, LENGTH, IER_CL2)
          IF (IER_CL2 .EQ. 0 .AND. LENGTH .NE. 0) THEN
            CALL EZPICK ('SRCP_UCAL')
            CALL EZDELETE (NAME, IER)
            CALL EZRSET
          ELSE
            GO TO 40  !no more names to find
          ENDIF
        ENDDO
   40   CONTINUE
        CALL EZPICK ('SRCP_UCAL')
        CALL EZSQUEEZE(NWORDS)
        CALL EZRSET
        CALL EZRSET   !from CAL_SHRINK_GEOM
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
