      FUNCTION ESUM_CON_VERS_2_TO_3()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert ESUM banks from version 2 to 3
C-          1) Change version number to 3
C-          2) Copy old eta (detector eta) to detector eta spot
C-          3) Calculate physics eta
C-
C-   Inputs  :
C-   Outputs : always returns true
C-   Controls:
C-
C-   Created  19-JUN-1992   Richard V. Astur
C-   Modified 26-JUN-1992   Amber Boehnlein
C-   Updated  27-JUN-1992   James T. Linnemann  leave vertex object alone
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS/LIST'
      LOGICAL ESUM_CON_VERS_2_TO_3
      INTEGER NSTYP
      PARAMETER( NSTYP=1 )
      INTEGER NFIX
      PARAMETER( NFIX = 30 )  !fixed header
      INTEGER NR
      PARAMETER (NR = 6 ) !per object
      CHARACTER*4 STYP(NSTYP)
      INTEGER LESUM, ISTYP, GZESUM
      INTEGER NOBJ,IOBJ,THIS_OBJ
      REAL ETA_PHYS, L2_VERT, THETA, THETA_FROM_ETA, ETA_FROM_THETA
      REAL RETA, RTHETA, CAL_TH, ZVERT
      DATA STYP /'FILT'/
C----------------------------------------------------------------------
C: Statement functions
      THETA_FROM_ETA(RETA ) = 2*ATAN(EXP(-(RETA)))
      ETA_FROM_THETA(RTHETA) =  -ALOG(MAX(TAN((RTHETA)/2.),1.E-9))
C----------------------------------------------------------------------
      ESUM_CON_VERS_2_TO_3 = .TRUE.
C: Loop over different kinds of ESUM banks
C
      ZVERT = 0.0
      DO ISTYP = 1, NSTYP
        LESUM = GZESUM( STYP( ISTYP ) )
        IF ( LESUM .GT. 0 ) THEN
          IF ( IQ( LESUM + 1 ) .EQ. 2 ) THEN
            IQ( LESUM + 1 ) = 3              ! Change version number
            NOBJ = IQ(LESUM + 4)
            DO  IOBJ = 0 , NOBJ - 1
              THIS_OBJ = LESUM + NFIX + IOBJ*NR
              IF (IQ(THIS_OBJ + JESUM_ID).NE.ID_VERTEX) THEN
              ! Get old theta (det)
                THETA = Q( THIS_OBJ + JESUM_ETA_DET )
                Q(THIS_OBJ + JESUM_ETA_DET) = Q(THIS_OBJ + JESUM_ETA)
                THETA = CAL_TH( THETA, ZVERT )      ! Get Physics theta
                Q( THIS_OBJ + JESUM_ETA ) = ETA_FROM_THETA( THETA )
              ENDIF
            ENDDO
          END IF
        END IF
      END DO
  999 RETURN
      END
