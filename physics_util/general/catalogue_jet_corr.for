      SUBROUTINE CATALOGUE_JET_CORR( LJETS, LVCOR, OLD_E, NEW_E )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Record the difference between uncorrected and
C-                         corrected values of this JETS bank in a
C-                         VCOR bank.
C-
C-   Inputs  :        LJETS     [I]   - Pointer to JETS bank
C-                    LVCOR     [I]   - Pointer to VCOR bank to fill
C-                    OLD_E(5)  [R]   - old Ex,Ey,Ez,E,Et
C-                    NEW_E(5)  [R]   - new Ex,Ey,Ez,E,Et
C-
C-   Outputs :        none
C-   Controls:
C-
C-   Created  23-OCT-1993   Richard V. Astur
C-   Updated  19-OCT-1995   Dhiman Chakraborty   
C-                          Use DCTOH before storing 'JETS' in IQ(LVCOR+2)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJETS, LVCOR, HJETS
      REAL OLD_E(5), NEW_E(5)
      INTEGER LDROP, I
C----------------------------------------------------------------------
C
C: The user should give us a valid JETS and VCOR bank pointer. If not,
C: stop the program!
C
      IF ( LJETS .LE. 0 .OR. LVCOR .LE. 0 ) THEN
        CALL ERRMSG('No VCOR/JETS','CATALOGUE_JET_CORR',
     &    'No JETS or VCOR bank!','F')
        RETURN
      ENDIF
C
C: If this JETS bank already had a VCOR associated with it and it is
C: not this VCOR, we will drop it.
C
      IF ( LQ( LJETS - 7 ) .NE. 0 .AND. LQ( LJETS - 7 ) .NE. LVCOR )
     &  THEN
        CALL ERRMSG('VCOR exists','CATALOGUE_JET_CORR',
     &    'VCOR already there, will drop it','W')
        LDROP = LQ( LJETS - 7 )
      ELSE
        LDROP = 0
      ENDIF
C
C: Fill reference links
      LQ( LJETS - 7 ) = LVCOR
      LQ( LVCOR - 2 ) = LJETS
C
C: Fill VCOR bank
C
      CALL DCTOH(4,'JETS',HJETS)
      IQ( LVCOR + 2 ) = HJETS
C
      DO I = 1, 4
        Q( LVCOR + 2 + I ) = NEW_E(I) - OLD_E(I)
      ENDDO
C
      Q( LVCOR + 11 ) = NEW_E(5) - OLD_E(5)
C
C: Drop the old VCOR if there was one
C
      IF ( LDROP .GT. 0  ) CALL MZDROP(IXCOM, LDROP , ' ')

  999 RETURN
      END
