      SUBROUTINE ISARCP_MASS_DISTRIBUTION
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate a random distribution of initial
C-   parton masses by altering ALMAP(6) in /QLMASS/. The parameters of
C-   the mass distribution are read from ISARCP_RCP.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-DEC-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QLMASS.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      REAL    RANF
      INTEGER MAXDISPAR
      PARAMETER( MAXDISPAR = 5 )
      REAL    DISPAR(MAXDISPAR),VAL
      INTEGER NDISPAR,LVAL,ITYPE,IER,PTR
      CHARACTER*32 CVAL
C
      LOGICAL EZERROR,UNIFORM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('ISARCP_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ISARCP','ISARCP_MASS_DISTRIBUTION',
     &      'Unable to access bank ISARCP_RCP','F')
        ELSE
C
C ****  Decode MASS_DISTRIBUTION array
C
          UNIFORM = .FALSE.
          NDISPAR = 0
          PTR = 1
          IER = 0
          DO WHILE ( IER .EQ. 0 )
            CALL EZGET_NEXT_VALUE_TYPE
     &      ('MASS_DISTRIBUTION',VAL,CVAL,ITYPE,LVAL,IER,PTR)
            IF ( ITYPE .GT. VTCHR ) THEN        ! Check for character
              UNIFORM = CVAL(1:1) .EQ. 'U'
            ELSE
              NDISPAR = NDISPAR + 1     ! Distribution parameters
              DISPAR(NDISPAR) = VAL
            ENDIF
          ENDDO
          CALL EZRSET
        ENDIF
      ENDIF
C
C ****  Distribute mass
C
      IF ( UNIFORM ) THEN
        AMLEP(6) = DISPAR(1) + (DISPAR(2)-DISPAR(1))*RANF(0)
      ENDIF
C
  999 RETURN
      END
