      SUBROUTINE L2_COMPARE_PB128(LUN,FIRST,NUM,STRINGS,TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C       display bits on from 128 bit words - utility for L2_COMPARE
C-
C-   Inputs  : LUN        [I]     unit number
C-             STRINGS    [C*128]  string of markers for bits
C-             FIRST,NUM  [I]     which markers to print
C-             TITLE      [C*128] header for display
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-APR-1992   Drew Baden
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
C
      INTEGER LUN,FIRST,NUM,NBITS
      CHARACTER*128 STRINGS(2),TITLE
      CHARACTER*80 FRMAT
      CHARACTER*1 WH(2)
      INTEGER I,J,LAST,I1,I2,LEN,SIZE,NRUN,NEV
C
      DATA NBITS/128/,WH/'S','D'/
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
C     ok, do it
C
      WRITE(FRMAT,'(''(2X,'',I3,''(''''_''''))'')') NBITS
      WRITE(LUN,FRMAT)
      WRITE(FRMAT,'(
     &    ''(1X,''''| '''',A'',I3.3,'',''''  Run/Event '''',2I7,'',
     &    I3,''X,''''|'''')'')') LAST,NBITS-27-LAST
      WRITE(LUN,FRMAT) TITLE(1:LAST),NRUN,NEV
      WRITE(LUN,'(
     &    1X,''|'',100('' ''),2(''1         ''),''1       |'',/,
     &    1X,''|'',12(I1,9X),''2'',7X,''|'',/,
     &    1X,''|'',12(''0123456789''),''01234567|'')')
     &    (MOD(J,10),J=0,11)
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
