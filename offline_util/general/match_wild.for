      FUNCTION MATCH_WILD (STRING,PATTERN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : See description of STR$MATCH_WILD.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-NOV-1989   Harrison B. Prosper
C-   Updated  23-SEP-1992   Harrison B. Prosper   
C-      Use SWORDS instead of WORD
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      CHARACTER*(*) PATTERN
C----------------------------------------------------------------------
      LOGICAL MATCH_WILD
      LOGICAL ACTIVE,AMPERSAND,ASTERISK
      INTEGER LENSTR,LENPAT,I,J,K,N,II,JJ,LL,III
      INTEGER JS,JE,IS,IE
C----------------------------------------------------------------------
      CALL SWORDS(STRING,JS,JE,LENSTR)
      CALL SWORDS(PATTERN,IS,IE,LENPAT)
C
C ****  Check first character for either & or *
C
      II  = IS
      JJ  = 1
      III = JS
      AMPERSAND = PATTERN(IS:IS) .EQ. '&'
      ASTERISK  = PATTERN(IS:IS) .EQ. '*'
      IF ( AMPERSAND .OR. ASTERISK ) THEN
        II = II + 1                     ! Skip first & or *
      ENDIF

      MATCH_WILD = .TRUE.
      ACTIVE     = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Extract next sub-pattern to be searched for
C
        I = INDEX(PATTERN(II:LENPAT),'&')       ! Note I
        J = INDEX(PATTERN(II:LENPAT),'*')
        K = MIN(I,J)
        IF ( K .LE. 0 ) K = MAX(I,J)
        IF ( K .LE. 0 ) THEN
          LL = LENPAT - II + 1          ! No & or * found in sub-pattern
        ELSE
          LL = K - 1                    ! Get length of sub-pattern
        ENDIF
C
C ****  Check length of sub-pattern
C
        IF ( LL .LE. 0 ) THEN
          MATCH_WILD = .FALSE.
          GOTO 999
        ENDIF

        IF ( AMPERSAND ) THEN            ! Here if last was Ampersand
          IF ( PATTERN(II:II+LL-1) .EQ. STRING(III:III+LL-1) ) THEN
            III = III + LL
            II  = II + LL + 1            ! Start of next sub-pattern
          ELSE
            ACTIVE = .FALSE.
            MATCH_WILD = .FALSE.
          ENDIF
        ELSE                            ! Here if last was asterisk
          JJ = INDEX(STRING(III:LENSTR),PATTERN(II:II+LL-1))
          IF ( JJ .GT. 0 ) THEN
            III = III + LL + JJ - 1
            II  = II + LL + 1            ! Start of next sub-pattern
          ELSE
            ACTIVE = .FALSE.
            MATCH_WILD = .FALSE.
          ENDIF
        ENDIF

        AMPERSAND = I .GT. 0
        IF ( II  .GT. LENPAT .OR. III .GT. LENSTR ) ACTIVE = .FALSE.
      ENDDO

  999 RETURN
      END
