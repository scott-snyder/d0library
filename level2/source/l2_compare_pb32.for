      SUBROUTINE L2_COMPARE_PB32(LUN,FIRST,NUM,STRINGS,TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C
C     display bits on from 32 bit words - utility for L2_COMPARE
C-
C-   Inputs  : LUN        [I]     unit number
C-             STRINGS    [C*32]  string of markers for bits
C-             FIRST,NUM  [I]     which markers to print
C-             TITLE      [C*128] header for display
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
C
      INTEGER LUN,FIRST,NUM,NBITS
      CHARACTER*32 STRINGS(*)
      CHARACTER*128 TITLE
      CHARACTER*80 FRMAT
      CHARACTER*1 WH(2)
C
      INTEGER I,J,LAST,I1,I2,LEN,SIZE,NRUN,NEV
C
      DATA NBITS/32/,WH/'S','D'/
C----------------------------------------------------------------------
C
C     look for '$' to mark end of string (I'm still in dark ages!)
C
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (NUM.LT.1) RETURN
      SIZE = LEN(TITLE)
      DO LAST=1,NBITS
        IF (TITLE(LAST:LAST).EQ.'$') GOTO 10
      ENDDO
   10 CONTINUE
      LAST = LAST - 1
C
C     ok, do it - this code is unreadable because we can't use things
C     like <NBITS>X in format statements!!!
C
      WRITE(FRMAT,'(''(2X,'',I3,''(''''_''''))'')') NBITS
      WRITE(LUN,FRMAT)
      WRITE(FRMAT,'(
     &    ''(1X,''''| Run/Event '''',2I7,7X,''''|'''',1X,A'',
     &    I3.3,'')'''')'')') LAST
      WRITE(LUN,FRMAT) NRUN,NEV,TITLE(1:LAST)
      WRITE(LUN,'(
     &    1X,''|'',3(I1,9X),''3'',1X,''|'',/,
     &    1X,''|'',3(''0123456789''),''01|'')')
     &    (MOD(J,10),J=0,2)
      DO I=FIRST,NUM
        WRITE(FRMAT,'(
     &    ''(A1,''''|'''',A'',I3.3,'',''''|'''')'')') NBITS
        WRITE(LUN,FRMAT) WH(I),STRINGS(I)
      ENDDO
      WRITE(FRMAT,'(
     &  ''(1X,''''|'''''',I3,''(''''_''''),''''|'''',/)'')') NBITS
      WRITE(LUN,FRMAT)
C
      RETURN
      END
