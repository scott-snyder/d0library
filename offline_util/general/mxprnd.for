      SUBROUTINE MXPRND(PRU,TITLE,A,IDIM1,IDIM2,IROW,ICOL,NCOL,FORMAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a DOUBLE PRECISION matrix neatly
C-
C-   Inputs  : PRU - Print unit
C-             TITLE  - TITLE at head of print
C-             A      - Matrix (Real*4) to be printed
C-             IDIM1,IDIM2 - A is dimensioned to A(IDIM1,IDIM2)
C-                           in calling program
C-             IROW - Number of rows of A to be printed
C-             ICOL - Number of columns of A to be printed
C-             NCOL - Number of columns across the page.
C-             FORMAT - Character variable containing Format of individual
C-                      element - e.g. (F8.3).
C-                      MUST HAVE OPEN AND CLOSE BRACKETS
C-   Outputs : to PRUNIT
C-   Controls:
C-
C-   Created  30-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRU,IROW,ICOL,NCOL,IDIM1,IDIM2
      DOUBLE PRECISION A(IDIM1,IDIM2)
      CHARACTER*(*) FORMAT,TITLE
      INTEGER IR,IC,LEN3,IC1
      INTEGER MXLEN,IPOWR,IPOWC
      PARAMETER( MXLEN = 132 )
      CHARACTER*(MXLEN) LINSEG,LINSEG1,LINSEG2,LINE
      CHARACTER*2 FRM1,FRM2
      CHARACTER*3 FRM0,FRM3,FRM4
      DATA FRM0/''','''/
      DATA FRM3/'''('''/
      DATA FRM4/''')'''/
      CHARACTER*30 FRM
C----------------------------------------------------------------------
      IF(IDIM1.EQ.0.OR.IDIM2.EQ.0)RETURN        ! NULL MATRIX
      WRITE(PRU,1)TITLE
    1 FORMAT(/,A,/)
      LINE = ' '
      LINSEG1 = ' '
      LINSEG2 = ' '
      DO 10 IR = 1,IROW
        IC1 = 0
        IPOWR = ALOG10(FLOAT(IR))+ 1.0  ! FORMAT PURPOSES
        DO 20 IC = 1,ICOL
          IPOWC = ALOG10(FLOAT(IC))+ 1.0  ! FORMAT PURPOSES
          WRITE(FRM1,21)IPOWR
   21     FORMAT('I',I1)
          WRITE(FRM2,21)IPOWC
          FRM = '('//'1X'//FRM3//FRM1//FRM0//FRM2//FRM4//')'
          WRITE(LINSEG,FRM)IR,IC
          WRITE(LINSEG1,FORMAT)A(IR,IC)
          CALL ADDSTR(LINSEG,LINSEG1,LINSEG2,LEN3)
          CALL ADDSTR(LINE,LINSEG2,LINSEG1,LEN3)
          LINE = LINSEG1(1:LEN3)
          IC1 = IC1 + 1
          IF(IC1.EQ.NCOL.OR.IC.EQ.ICOL)THEN
C PRINT
            IC1 = 0
            WRITE(PRU,22)LINE
   22       FORMAT(A)
            LINE = ' '
          ENDIF
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
