      FUNCTION QCD_FILT_STRING( NOTWANTED1, SEARCH_STRING1, NOTWANTED2,
     &  SEARCH_STRING2 )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Look at all the QCD filters that were on for
C-                         this event. Return TRUE if ANY QCD filter meets
C-                         this requirement:
C-                           Contains the string SEARCH_STRING1 if NOTWANTED1
C-                           is FALSE/ doesnt contain that string if NOTWANTED1
C-                           is TRUE.
C-                           Contains the string SEARCH_STRING2 if NOTWANTED2
C-                           is FALSE/ doesnt contain that string if NOTWANTED2
C-                           is TRUE.
C-
C-   Returned value  : TRUE if a QCD bit which was on matched these conditions
C-                     and FALSE otherwise.
C-
C-   Inputs  :  SEARCH_STRING1   [C*(*)]   - First string
C-              NOTWANTED1       [L]       -
C-              SEARCH_STRING2   [C*(*)]   - Second string
C-              NOTWANTED2       [L]       -
C-   Outputs :
C-   Controls:
C-
C-   Created  17-FEB-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_BIT_NAMES.INC'
      INCLUDE 'D0$INC:QCD_BIT.INC'
      LOGICAL    QCD_FILT_STRING, QCDJETS, NOTWANTED1, NOTWANTED2
      EXTERNAL   QCDJETS
      CHARACTER*(*) SEARCH_STRING1, SEARCH_STRING2
      INTEGER ILEN1, ILEN2, MASK1, MASK2, IBIT, IMASK, ITEST
      LOGICAL GOOD, NOTFOUND1, NOTFOUND2
C----------------------------------------------------------------------
      QCD_FILT_STRING = .FALSE.           ! Initialize as FALSE
C
C: Make sure at least one QCD bit is on, else return
C
      IF ( .NOT. QCDJETS() )  RETURN      ! No QCD bits on
C
C: Look at string
C
      ILEN1  = LEN( SEARCH_STRING1 )
      ILEN2  = LEN( SEARCH_STRING2 )
      IF ( ILEN1 .LE. 0 .OR. ILEN2 .LE. 0 ) GOTO 999
C Cant search for 0 length string

C
C: Loop over all QCD bits set
C
      CALL  QCD_GET_MASK( MASK1 )
      CALL  QCD_GET_MASK2( MASK2 )
      MASK1 = IBCLR( MASK1, 14 )
      MASK2 = IBCLR( MASK2, 14 )
      DO  IBIT = 0, N_QCD_BITS - 1
        ITEST = IBIT
        IMASK = MASK1
        IF ( ITEST .GE. 32 ) THEN
          ITEST = IBIT - 32
          IMASK = MASK2
        ENDIF
        IF ( BTEST( IMASK, ITEST ) ) THEN
          NOTFOUND1  = ( INDEX( QCD_BIT_NAME(IBIT), SEARCH_STRING1(1:
     &      ILEN1)) .EQ. 0 )
          NOTFOUND2  = ( INDEX( QCD_BIT_NAME(IBIT), SEARCH_STRING2(1:
     &      ILEN2)) .EQ. 0 )
          GOOD   =( ( ( NOTFOUND1) .AND. (NOTWANTED1) ) .OR.
     &              ( (.NOT. NOTFOUND1) .AND. (.NOT. NOTWANTED1 ))) 
     &             .AND.
     &              ( ( ( NOTFOUND2) .AND. (NOTWANTED2 ) ) .OR.
     &              ( (.NOT. NOTFOUND2) .AND. (.NOT. NOTWANTED2 )))
C
C: Pass this event if this filter fulfilled the requirements
C
          IF ( GOOD ) QCD_FILT_STRING = .TRUE.
        ENDIF
      ENDDO

  999 RETURN
      END
