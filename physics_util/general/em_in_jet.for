      FUNCTION EM_IN_JET( LBANK, IDEM, CTYPE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine whether this PELC/PPHO bank is
C-          contained in any JETS bank. WARNING: This routine assumes
C-          that SET_CAPH has been called to set the CAPH path!!
C-
C-   Returned value  : TRUE if some JETS object contains this PELC/PPHO
C-   Inputs  :  LBANK   [I]   Pointer to PELC or PPHO
C-   Outputs :  IDEM    [I]   If LBANK=0, IDJET specifies which member
C-                            in the linear chain to check
C-              CTYPE   [C*4] If LBANK=0, CTYPE='PELC' or 'PPHO'
C-   Controls:
C-
C-   Created   5-JAN-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL EM_IN_JET
      INTEGER LJETS, GZJETS, I, LBANK, LBANK1, ICOUNT, IDEM
      INTEGER  GZPELC, GZPPHO
      EXTERNAL GZJETS, GZPELC, GZPPHO
      CHARACTER*4 CTYPE
C----------------------------------------------------------------------
      EM_IN_JET = .FALSE.
      IF ( LBANK .LE. 0 ) THEN
        IF ( IDEM .LE. 0 ) RETURN
        LBANK1 = GZPELC()
        IF ( CTYPE .EQ. 'PPHO' ) LBANK1 = GZPPHO()
        ICOUNT = IDEM - 1
        DO WHILE ( LBANK1 .NE. 0 .AND. ICOUNT .GT. 0 )
          ICOUNT = ICOUNT - 1
          LBANK1 = LQ( LBANK1 )
        ENDDO
      ELSE
        LBANK1 = LBANK
      ENDIF
C
      IF ( LBANK1 .LE. 0 ) RETURN
C

      LJETS     = GZJETS()          ! Algorithm path set by SET_CAPH
      DO WHILE ( LJETS .GT. 0 )
        DO I = 1, 4
          IF ( LBANK1 .EQ. LQ( LJETS - I - 2) ) THEN
            EM_IN_JET = .TRUE.
            RETURN
          ENDIF
        ENDDO
        LJETS = LQ( LJETS )
      ENDDO
  999 RETURN
      END
