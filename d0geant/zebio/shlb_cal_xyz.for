      SUBROUTINE SHLB_CAL_XYZ(CALFACE) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find where stored shower hit calorimeter
C-      The position reported is a kinetic-energy weighted mean of
C-      the GCAH hits.
C-
C-   Inputs  : SHLB, in /SHLDAT/
C-   Outputs : CALFACE(3), the x,y,z position of the hit
C-   Controls: none
C-
C-   Created  30-AUG-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL CALFACE(3)                   ! where showerlib track hit calorimeter
      INCLUDE 'D0$INC:SHLDAT.INC'
      INTEGER I
      INTEGER POINT                     ! pointer to next stored hit
      REAL ESUM,FACESUM(3)              ! overall position from individual hits
      INTEGER JTYPE,ITRTYP,NWUSER       ! output params from GEANT mass routine
      INTEGER NPRTYP
      PARAMETER( NPRTYP = 48 )
      REAL KE,MASS(NPRTYP),CHARGE(NPRTYP),TLIFE,USER(100),NAME(100)

      LOGICAL FIRST
      SAVE FIRST,MASS,CHARGE
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        DO I = 1,NPRTYP     ! build mass table for KE calculation
          CALL GFPART(I,NAME,ITRTYP,MASS(I),CHARGE(I),TLIFE,USER,NWUSER)
        ENDDO
        FIRST = .FALSE.
      ENDIF
C
C----------------------------------------------------------------------
C
      POINT = SHLB(1)
      IF (SHLB(POINT).LE.0) THEN           ! only one sub-track
        DO I = 1,3
          CALFACE(I) =  SHLB(POINT+I)
        ENDDO
      ELSE                              ! energy weighted position
        ESUM = 0
        DO I = 1,3
          FACESUM(I) = 0.
        ENDDO
        DO WHILE (POINT.NE.0)     ! loop over sub-tracks stored
          JTYPE = SHLB(POINT+5)         ! GEANT particle type
          KE = SHLB(POINT+4) - MASS(JTYPE)      ! Kinetic Energy ordering
          IF (KE.LE.0) KE = 1.E-9
          ESUM = ESUM + KE
          DO I = 1,3
            FACESUM(I) = FACESUM(I) + KE*SHLB(POINT+I)
          ENDDO
          POINT = SHLB(POINT)
        ENDDO
        DO I = 1,3
          CALFACE(I) = FACESUM(I)/ESUM  ! position from average of subtracks
        ENDDO
      ENDIF
  999 RETURN
      END
