      SUBROUTINE EZDBUG (LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out names and addresses of RCP banks.
C-
C-   Inputs  : LUN         Logical unit number of output stream
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Updated   3-JUL-1991   Harrison B. Prosper
C-      Use INTMSG if LUN = 6
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER       I,J,K,L,LSUPP,II,JJ,KK
      CHARACTER*10  SUPP
      CHARACTER*78 RECORD
C----------------------------------------------------------------------
      IF ( ISRCP .GT. 0 ) THEN
        LSRCP = KSRCP(ISRCP)
      ELSE
        LSRCP = 0
      ENDIF
C
      RECORD = ' Directory of RCP banks'
      CALL SWORDS(RECORD,I,L,K)
      IF ( LUN .EQ. 6 ) THEN
        CALL INTMSG('  ')
        CALL INTMSG(RECORD(1:L))
        CALL INTMSG('  ')
      ELSE
        WRITE(LUN,'(A)') '  '
        WRITE(LUN,'(A)') RECORD(1:L)
        WRITE(LUN,'(A)') '  '
      ENDIF
C
      RECORD = ' '
      IF ( ISRCP .LE. 0 ) THEN
        WRITE(RECORD,FMT='(1X,17X,''NO BANK SELECTED'')')
      ELSE
        WRITE(RECORD,FMT='(1X,17X,''SELECTED BANK: '',A32)')
     &    MSRCP(ISRCP)
      ENDIF
      CALL SWORDS(RECORD,I,L,K)
      IF ( LUN .EQ. 6 ) THEN
        CALL INTMSG(RECORD(1:L))
        CALL INTMSG('  ')
      ELSE
        WRITE(LUN,'(A)') RECORD(1:L)
        WRITE(LUN,'(A)') '  '
      ENDIF

      RECORD = ' '
      WRITE(RECORD,FMT='(1X,17X,''NUMBER OF BANKS: '',I10)') NSRCP
      CALL SWORDS(RECORD,I,L,K)
      IF ( LUN .EQ. 6 ) THEN
        CALL INTMSG(RECORD(1:L))
        CALL INTMSG('  ')
      ELSE
        WRITE(LUN,'(A)') RECORD(1:L)
        WRITE(LUN,'(A)') '  '
      ENDIF
C
      RECORD = ' '
      WRITE(RECORD,FMT='(1X,3X,''ID'',1X,''SRCP BANK NAME'',18X,
     & 4X,''ADDRESS'',4X,''SUPPORT'',4X,''ADDRESS'')')
      CALL SWORDS(RECORD,I,L,K)
      IF ( LUN .EQ. 6 ) THEN
        CALL INTMSG(RECORD(1:L))
      ELSE
        WRITE(LUN,'(A)') RECORD(1:L)
      ENDIF
C
      I = 0
      J = 0
      DO WHILE ( (I .LT. MXSRCP) .AND. (J .LT. NSRCP) )
        I = I + 1
        IF ( KSRCP(I) .GT. 0 ) THEN     ! Check for live banks
          J = J + 1
          LSUPP = LC(KSRCP(I)+1)
          IF ( LSUPP .GT. 0 ) THEN
            SUPP = ' '
            CALL UHTOC (IC(LSUPP-4),4,SUPP(7:10),4)
          ELSE
            SUPP = 'Standalone'
          ENDIF
C
          RECORD = ' '
          WRITE(RECORD,FMT='(1X,I5,1X,A32,1X,I10,1X,A10,1X,I10)')
     &        I,MSRCP(I),KSRCP(I),SUPP,LC(KSRCP(I)+1)
          CALL SWORDS(RECORD,II,JJ,KK)
          IF ( LUN .EQ. 6 ) THEN
            CALL INTMSG(RECORD(1:JJ))
          ELSE
            WRITE(LUN,'(A)') RECORD(1:JJ)
          ENDIF
        ENDIF
      ENDDO
C
      IF ( LUN .EQ. 6 ) THEN
        CALL INTMSG('  ')
      ELSE
        WRITE(LUN,'(A)') '  '
      ENDIF
  999 RETURN
      END
