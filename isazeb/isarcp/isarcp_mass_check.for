      SUBROUTINE ISARCP_MASS_CHECK (MASS_OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare the input mass of the parton (and
C-   anti-parton) with the mass given in the ISAJ bank. (The input mass
C-   is stored in /QLMASS/ AMLEP(6).)  Return TRUE if the values
C-   agree within an amount specified in the bank ISARCP_RCP. The parton
C-   whose mass is to be checked should be specified in the bank ISARCP_RCP.
C-
C-   Inputs  : None
C-   Outputs : MASS_OK  [L]     TRUE if mass in ISAJ is ok
C-   Controls: None
C-
C-   Created  22-DEC-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MASS_OK
C----------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAJ.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QLMASS.INC'
C----------------------------------------------------------------------
      INTEGER PARTON_ID
      REAL    MASS_TOLERANCE
C
      INTEGER MAXARR
      PARAMETER( MAXARR = 20 )
      REAL    RARRAY(MAXARR)
      INTEGER IARRAY(MAXARR)
      EQUIVALENCE(RARRAY(1),IARRAY(1))
C
      INTEGER LISAE,LISAJ,IER
      LOGICAL EZERROR,DO_MASS_CHECK
      LOGICAL FIRST,SKIP
      SAVE FIRST,SKIP
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      MASS_OK = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('ISARCP_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ISARCP','ISARCP_MASS_CHECK',
     &      'No ISARCP_RCP bank','F')
        ENDIF
C
        CALL EZGET('DO_MASS_CHECK',DO_MASS_CHECK,IER)
        CALL EZGETA('MASS_CHECK',1,2,1,IARRAY,IER)
        SKIP = (.NOT. DO_MASS_CHECK) .OR. (IER .NE. 0)
        PARTON_ID = IARRAY(1)
        MASS_TOLERANCE = RARRAY(2)
        CALL EZRSET
      ENDIF
C
C ****  Skip if parton id is zero
C
      IF ( SKIP ) GOTO 999
C
C ****  Get pointer to bank ISAE
C
      LISAE = LQ(LHEAD-IZISAE)
      IF ( LISAE .LE. 0 ) THEN
        CALL ERRMSG('ISARCP','ISARCP_MASS_CHECK',
     &    'No ISAE bank','W')
        GOTO 999
      ENDIF
C
C ****  Get pointer to bank ISAJ
C
      LISAJ = LQ(LISAE-IZISAJ)
      IF ( LISAJ .LE. 0 ) THEN
        CALL ERRMSG('ISARCP','ISARCP_MASS_CHECK',
     &    'No ISAJ bank','W')
        GOTO 999
      ENDIF
C
C ****  Loop over ISAJ
C
      DO WHILE ( LISAJ .GT. 0 )
C
C ****  Check parton id
C
        IF ( IABS(PARTON_ID) .EQ. IABS(IQ(LISAJ+1)) ) THEN
C
C ****  Check parton mass
C
          IF ( ABS(AMLEP(6)-Q(LISAJ+6)) .GT. MASS_TOLERANCE  ) THEN
            MASS_OK = .FALSE.
            GOTO 999                    ! Exit loop
          ENDIF
        ENDIF
        LISAJ = LQ(LISAJ)
      ENDDO
  999 CONTINUE
      RETURN
      END
