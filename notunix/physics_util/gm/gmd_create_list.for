      SUBROUTINE GMD_CREATE_LIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create list of objects.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   5-MAY-1993   Harrison B. Prosper
C-   Updated   8-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Correct FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:GM_4VECT.INC'
      INCLUDE 'D0$PARAMS:GM_PARAMS.DEF'
C----------------------------------------------------------------------
      INTEGER I, J, N, LP, II
      LOGICAL PARTON_OBJECT
      REAL    X
C
      EXTERNAL LABEL    ! ISAJET FUNCTION
      CHARACTER*2 DELIMITER
      CHARACTER*8 LABEL
      CHARACTER*8 GMD_LABEL
      CHARACTER*8 ORIGIN_PARTICLE, PARENT_PARTICLE, PARTICLE
      CHARACTER*32 PARAM, BLANK
C----------------------------------------------------------------------
      BLANK = ' '
C
C ****  Create list of particle labels
C
      DO II = 1, NOBJECT
C
C ****  Get particle label
C
        ORIGIN_PARTICLE = ' '
        PARENT_PARTICLE = ' '
        PARTON_OBJECT   = II .LE. NPART
C
C ****  LABEL ACCORDING TO WHETHER THIS IS A PARTON OR
C ****  A RECO OBJECT
C
        IF ( PARTON_OBJECT ) THEN
          DELIMITER = '> '
          IF ( PARENT(II) .NE. 0 ) THEN
            ORIGIN_PARTICLE = LABEL(ORIGIN(II))
            PARENT_PARTICLE = LABEL(PARENT(II))
          ENDIF
          PARTICLE = LABEL(PARTID(II))
        ELSE
          DELIMITER = '  '
          PARTICLE = GMD_LABEL(II)
        ENDIF
C
        CALL WORD(PARTICLE, I, J, LP)
        PARTICLE = PARTICLE(I:J)
C
C ****  Compute pt
C
        X = SQRT(P(1, II)*P(1, II)+P(2, II)*P(2, II))
        WRITE(UNIT=PARAM, FMT='(F5.1)', ERR=900) X
  900   CONTINUE
C
        CALL WORD(PARAM, I, J, N)
        PARAM = PARAM(I:J)//' Gev'
        N = N + 4
        LIST(II) =
     &    ORIGIN_PARTICLE//DELIMITER//
     &    PARENT_PARTICLE//DELIMITER//
     &    PARTICLE//
     &    BLANK(1:11-N)//PARAM(1:N)
      ENDDO
      RETURN
      END
