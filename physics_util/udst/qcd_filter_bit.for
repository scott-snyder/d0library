      SUBROUTINE QCD_FILTER_BIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Map the 128 filter bits used by the experiment,
C-                         to a uniquely defined set of bits defined by
C-                         the QCD group.
C-
C-
C-   Created   5-DEC-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_BIT_NAMES.INC'
      INCLUDE 'D0$INC:QCD_BIT.INC'
      INTEGER QCDBIT, AQCDBIT, I, AI, IER, RUNNO, OLD_RUNNO, J
      INTEGER QCDBIT1, D0BIT1, QCDBIT2, D0BIT2, L, L2, L3
      CHARACTER*(*) NAME, ANAME
C:
C----------------------------------------------------------------------
      RETURN
C
C: Map D0 bit to QCD bit
C
      ENTRY QCD_BIT_FROM_D0( D0BIT1, QCDBIT1 )
      CALL QCD_FILTER_BIT_INIT
      IF ( D0BIT1 .GE. 0 .AND. D0BIT1 .LE. 127 ) THEN
        QCDBIT1 = D0_TO_QCD( D0BIT1 )
      ELSE
        QCDBIT1 = -1
      ENDIF
      RETURN
C
C: Map QCD bit to D0 bit
C
      ENTRY QCD_BIT_TO_D0( QCDBIT2, D0BIT2 )
      CALL QCD_FILTER_BIT_INIT
      IF ( QCDBIT2 .GE. 0 .AND. QCDBIT2 .LE. N_QCD_BITS-1 ) THEN
        D0BIT2 = QCD_TO_D0( QCDBIT2 )
      ELSE
        QCDBIT2 = -1
      ENDIF
      RETURN
C
C: Find the QCD bit name to match NAME. If none, return -1 (never set!)
C
      ENTRY QCD_NAME_TO_BIT( NAME, QCDBIT )
C      CALL QCD_FILTER_BIT_INIT
      QCDBIT = -1
      L = LEN(NAME)
C      NAME = NAME(1:L)              ! Get full string
      L = INDEX(NAME,'-') - 1       ! Look for strings like -L20
      IF ( L .LE. 0 ) L = LEN(NAME) ! Position L at end of string
   21 CONTINUE                      ! Find last alphanumeric character
      IF (ICHAR(NAME(L:L)) .LT. ICHAR('0') .OR. ICHAR(NAME(L:L)) .GT.
     &  ICHAR('Z')) THEN
        L = L - 1
        IF ( L .GT. 1 ) GOTO 21
      ENDIF
      L3 = 1
   23 CONTINUE                      ! Find first alphanumeric character
      IF (ICHAR(NAME(L3:L3)) .LT. ICHAR('0') .OR. ICHAR(NAME(L3:L3))
     &  .GT. ICHAR('Z')) THEN
        IF ( L3 .LT. L ) THEN
          L3 = L3 + 1
          GOTO 23
        ENDIF
      ENDIF
      DO I = 0, N_QCD_BITS-1
        L2 = INDEX( QCD_BIT_NAME(I), ' ') - 1
        IF ( L2 .LE. 0 ) L2 = LEN( QCD_BIT_NAME(I) )
        IF ( NAME(L3:L) .EQ. QCD_BIT_NAME(I)(1:L2) ) THEN
          QCDBIT = I
          RETURN
        ENDIF
      ENDDO
      RETURN
C
C: Find QCD name to match bit.
C
      ENTRY QCD_BIT_TO_NAME( AQCDBIT, ANAME )
C     CALL QCD_FILTER_BIT_INIT
      IF (AQCDBIT .GE. N_QCD_BITS .OR. AQCDBIT .LT. 0 ) THEN
        ANAME = 'ERROR '
      ELSE
        L2 = INDEX(QCD_BIT_NAME(AQCDBIT),' ')
        IF ( L2 .LE. 0 ) L2 = LEN( QCD_BIT_NAME(AQCDBIT) )
        L = MIN( LEN(ANAME), L2 )
        ANAME = QCD_BIT_NAME( AQCDBIT )(1:L)
      ENDIF
  999 RETURN
      END
