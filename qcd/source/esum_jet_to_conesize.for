      FUNCTION ESUM_JET_TO_CONESIZE( STYP, IESUM_OBJECT, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the corresponding cone size for this
C-                         ESUM object.
C-
C-   Returned value  : Cone size of jet in radians
C-   Inputs  :         STYP= type of ESUM bank, 'FILT','RECO', 'RECO','ISAE'
C-                     IESUM_OBJECT = object number of jet desired
C-
C-   Outputs : IER returned error flag
C-                 = -1 Invalid object type for jets
C-                 = -2 No corresponding cone size for this object
C-   Controls:
C-
C-   Created  15-MAR-1992   Richard V. Astur
C-   Modified 15-DEC-1992   R. Astur "Version 4 ESUM packs conesize different"
C-   Modified  1-MAR-1994   R. Astur "bug fix for STYP=RECO and handle
C-                          problems caused by L2JETS using version 4 ESUM
C-                          before ESUM bank was labeled version 4.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      LOGICAL VERSION4
      REAL ESUM_JET_TO_CONESIZE
      INTEGER IER, IESUM_OBJECT, GZJPAR, LJPAR, NUM_SETS, NR, I,J
      INTEGER LESUM, GZESUM, CONE_FLAG, IBIT
      CHARACTER*4 STYP
C----------------------------------------------------------------------
      ESUM_JET_TO_CONESIZE = 0.0            ! Init to 0.
      IER                  = -2             ! Not found
C
C: Check that object id is valid for jets.
C
      IF ( IESUM_OBJECT .LT. ID_JET_1 .OR. IESUM_OBJECT .GT. ID_JET_1 +
     &  NUM_JET_OBJECTS - 1 ) THEN
        IER = -1
        GOTO 999
      END IF
C
C: Cone size returned depends on STYP.
C
      IF ( STYP .EQ. 'FILT' ) THEN
        LJPAR = GZJPAR()         ! Try to get cone size from L2JETS
        IF ( LJPAR .GT. 0 ) THEN
          NUM_SETS = IC( LJPAR + 3 )
          NR       = IC( LJPAR + 2 ) + 1
          DO I = 1, NUM_SETS
            IF ( INT( C( LJPAR + 12 + (I-1)*NR )) .EQ. IESUM_OBJECT - (
     &        ID_JET_1 - 1 ) ) THEN
              IER = 0
              ESUM_JET_TO_CONESIZE = C( LJPAR + 7 + (I-1)*NR )
              RETURN
            END IF
          END DO
        ENDIF
      ELSEIF ( STYP .EQ. 'RECO' ) THEN
        IF ( IESUM_OBJECT .EQ. ID_JET ) ESUM_JET_TO_CONESIZE = .7
        IF ( IESUM_OBJECT .EQ. ID_JET_1 ) ESUM_JET_TO_CONESIZE = .5
        IF ( IESUM_OBJECT .EQ. ID_JET_2 ) ESUM_JET_TO_CONESIZE = .3
        IF ( IESUM_OBJECT .EQ. ID_JET_3 ) ESUM_JET_TO_CONESIZE = .1
        IER = 0
        RETURN
      ENDIF


C
C: In this case Level2 information is not available to them. We can only
C: Check the ESUM bank and decode the flag word if their object type
C: is present.
C
      LESUM = GZESUM( STYP )
      IF ( LESUM .LE. 0 ) RETURN
      NUM_SETS = IQ( LESUM + 4 )
      NR       = IQ( LESUM + 3 )
      DO I = 1, NUM_SETS                ! Loop over objects
        IF ( IQ( LESUM + NR*(I-1) + 31 ) .EQ. IESUM_OBJECT   ) THEN
          CONE_FLAG = IQ( LESUM + NR*(I-1) + 32 )
          VERSION4  = .FALSE.
C
C: If version 4 or greater, than one bit will be set
C
          IBIT = 0
          DO J = 0, 20
            IF ( BTEST( CONE_FLAG, J ) ) IBIT = IBIT + 1
          ENDDO
          VERSION4  = ( IBIT .EQ. 1 .AND. IQ( LESUM + 1 ) .GE. 3)
          IF ( .NOT. VERSION4 ) THEN    ! Pre-version 4
            ESUM_JET_TO_CONESIZE = CONE_FLAG/10.
            IER = 0
          ELSE
            IER = -2
            DO J = 1,15
              IF ( BTEST(CONE_FLAG, J ) ) THEN
                IER = 0
                ESUM_JET_TO_CONESIZE = J*.1
                RETURN
              ENDIF
            ENDDO
          ENDIF
          RETURN
        ENDIF
      ENDDO

  999 RETURN
      END
