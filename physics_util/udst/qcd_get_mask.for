      SUBROUTINE QCD_GET_MASK( QCD_MASK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert the mask of which filter bits passed
C-                         into the QCD mask of bits on.
C-
C-    ENTRY QCD_GET_MASK2 ( QCD_MASK2) - Return second part of mask
C-   Inputs  :
C-   Outputs :        [I]   QCD_MASK: mask of QCD bits on
C-   Controls:
C-
C-   Created  11-DEC-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_BIT_NAMES.INC'          ! QCD bit names
      INTEGER QCD_MASK, I, QCDBIT, GZJUTL, LJUTL, QCD_MASK2
      LOGICAL L2BIT_PASSED
      CHARACTER*4 PATH
C----------------------------------------------------------------------
      QCD_MASK = 0                                ! No bits set
      QCD_MASK = IBSET( QCD_MASK, 14 )            ! Always set
C
      CALL PATHGT( PATH )                         ! What is the path?

      IF ( PATH .EQ. 'MDST' ) THEN
        LJUTL = GZJUTL()
        IF ( LJUTL .GT. 0 ) THEN
          QCD_MASK = IQ( LJUTL + 2 )
          IF ( IQ( LJUTL + 1 ) .LT. 4 ) QCD_MASK = IBSET( QCD_MASK, 14 )
        ELSE
          CALL ERRMSG('No JUTL bank','QCD_GET_MASK',
     &      'Path set to MDST but no JUTL bank?', 'W')
        ENDIF
      ELSE
        CALL QCD_FILTER_BIT_INIT
        DO I = 0, 127
          IF ( L2BIT_PASSED(I) ) THEN
            CALL QCD_BIT_FROM_D0( I, QCDBIT )
            IF ( QCDBIT .GE. 0 .AND. QCDBIT .LE. 31 ) QCD_MASK = IBSET(
     &        QCD_MASK, QCDBIT )
          ENDIF
        ENDDO
      ENDIF
      RETURN
C********************************************************************
C ENTRY QCD_GET_MASK2
C   - Return 2nd part of mask
C*******************************************************************

      ENTRY QCD_GET_MASK2( QCD_MASK2 )
      QCD_MASK2 = 0                                ! No bits set
      QCD_MASK2 = IBSET( QCD_MASK2, 14 )           ! Always set
C
      CALL PATHGT( PATH )                          ! What is the path?

      IF ( PATH .EQ. 'MDST' ) THEN
        LJUTL = GZJUTL()
        IF ( LJUTL .GT. 0 ) THEN
          IF ( IQ(LJUTL + 1 ) .LT. 4 ) THEN
C
          ELSE
          QCD_MASK2 = IQ( LJUTL + 3 )
          ENDIF
        ELSE
          CALL ERRMSG('No JUTL bank','QCD_GET_MASK',
     &      'Path set to MDST but no JUTL bank?', 'W')
        ENDIF
      ELSE
        CALL QCD_FILTER_BIT_INIT
        DO I = 0, 127
          IF ( L2BIT_PASSED(I) ) THEN
            CALL QCD_BIT_FROM_D0( I, QCDBIT )
            IF ( QCDBIT .GE. 32 .AND. QCDBIT .LE. 63 ) QCD_MASK2 =
     &        IBSET(QCD_MASK2, (QCDBIT-32) )
          ENDIF
        ENDDO
      ENDIF
      RETURN

      END
