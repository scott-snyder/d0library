      SUBROUTINE SCNRCP( LCSCN, ICD_ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtain SRCP position data for CSCN banks
C-
C-   Inputs  :     LCSCN     pointer to CSCN bank
C-                 ICD_ETA   title to SRCP array
C-   Outputs :     in zebra banks
C-   Controls:     
C-   Zebra Banks Altered:    CSCN
C-
C-   Created   2-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
C
      CHARACTER ICD_ETA*(*)
      INTEGER IMICD, LCSCN, I, INDX, IER, ICD
C
      CALL EZGET(ICD_ETA,IVAL,IER)  ! get icd arrays
      IF (IER .NE. 0) THEN
        WRITE (6,*) ' ICD ARRAY NOT FOUND: ', ICD_ETA
        STOP 556
      END IF
C
      IC( LCSCN + IGMATE) = IVAL(3)    ! material type
      IC( LCSCN + IGSHAP) = IVAL(2)    ! shape name
      IC( LCSCN + IGNAM) = IVAL(1)     ! volume name
      IC( LCSCN + IGCOOR) = 123        ! Cartesian coordinates
      IC( LCSCN + IGPERP) = 3          ! perpendicular direction is Z
      C( LCSCN + IGXCEN) = RVAL(8)     ! X
      C( LCSCN + IGYCEN) = RVAL(9)     ! Y
      C( LCSCN + IGZCEN) = RVAL(10)    ! Z
      C( LCSCN + IGDPHI) = TWOPI       ! TWOPI extent for ring
      READ(ICD_ETA,'(8X,I2,1X)') ICD   ! get eta number
      C( LCSCN + IGETA0) = 0.1*ICD + 0.05   ! nominal central eta value
C
C----------------------------------------------------------------------
  999 RETURN
      END
