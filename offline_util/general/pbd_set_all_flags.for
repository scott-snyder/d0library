      SUBROUTINE PBD_SET_ALL_FLAGS(FLAG,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set ALL PBD flags to the value VAL except
C-   the given FLAG which is to be left unchanged. 
C-   Use PBD_RESET_ALL_FLAGS to reset the flags to their original values.
C-
C-   Inputs  : FLAG     [C*]    Flag name (= package name)
C-             VAL      [L]     Value of flag
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-APR-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) FLAG
      LOGICAL VAL
C
      INTEGER I,J,L
      INTEGER MXFLAG
      PARAMETER( MXFLAG = 100 )
      LOGICAL BOOLE(MXFLAG)
      LOGICAL ACTIVE,TRUTH
      LOGICAL PBD_GET_NEXT_FLAG
      CHARACTER*32 NAME
      SAVE BOOLE
C----------------------------------------------------------------------
C
C ****  Set all PBD flags to value VAL, but leave out FLAG.
C
      L = LEN(FLAG)
      J = 1
      I = 0
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        I = I + 1
        IF ( I .LE. MXFLAG ) THEN
          IF ( PBD_GET_NEXT_FLAG(NAME,BOOLE(I),J) ) THEN
            IF ( NAME(1:L) .NE. FLAG(1:L) ) THEN
              CALL PBD_SET_FLAG(NAME,VAL)
            ENDIF
          ELSE
            ACTIVE = .FALSE.
          ENDIF
        ELSE
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
      RETURN
C
      ENTRY PBD_RESET_ALL_FLAGS
C
C ****  Reset all flags to original values
C
      J = 1
      I = 0
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        I = I + 1
        IF ( I .LE. MXFLAG ) THEN
          IF ( PBD_GET_NEXT_FLAG(NAME,TRUTH,J) ) THEN
            CALL PBD_SET_FLAG(NAME,BOOLE(I))
          ELSE
            ACTIVE = .FALSE.
          ENDIF
        ELSE
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
  999 RETURN
      END
