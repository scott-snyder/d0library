C----------------------------------------------------------------------
      SUBROUTINE MSGO (CHOP,CSUB,MSG,INUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will print a message (default is unit 6),
C-   in the format '-x- CSUB: MSG nnn' (x and nnn depend on CHOP).
C-   To change the output unit: 
C-   include COMMON /MSGOC/, set MSGO_UN = 9000 + NewUnit
C-
C-   Inputs  : CHOP   I  information (default) (MSGO_ER set to 0)
C-                    W  warning               (MSGO_ER set to 1)
C-                    E  error                 (MSGO_ER set to 2)
C-                    N  display integer
C-                    X  display hexadecimal
C-             CSUB   String to display
C-             MSG    String to display
C-             INUM   Number to display (if chop include a N or a X)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-SEP-1992   Lars O. Rasmussen
C-   Updated  18-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICFIND,ICFNBL
C
      CHARACTER*(*)  CHOP, CSUB, MSG
      INTEGER        INUM, IC0, NC, LC1, LC2
      INTEGER *2     IC1,IC2
      CHARACTER      MSG1*78, MSG2*78, CHLP*8, CBHL*12, C3*3
      INTEGER        INUT
      DATA INUT /6/
C
      INTEGER MSGO_ER,MSGO_UN
      COMMON/MSGOC/ MSGO_ER,MSGO_UN
C----------------------------------------------------------------------
      IF (MSGO_UN/1000 .EQ. 9) THEN
         MSGO_UN = MSGO_UN - 9000
         INUT = MSGO_UN
      END IF
      CBHL= ' '
      CHLP = CHOP
      CALL STR$UPCASE(CHLP,CHLP)
      LC1 = ICFIND('E',CHLP,1,LEN(CHLP))
      LC2 = ICFIND('W',CHLP,1,LEN(CHLP))
      IF (LC1 .GT. 0 .AND. LC1 .LE. LEN(CHLP)) THEN
         C3 = '-e-'
         MSGO_ER = 2 
      ELSE IF (LC2 .GT. 0 .AND. LC2 .LE. LEN(CHLP)) THEN
         C3 = '-w-'
         MSGO_ER = 1
      ELSE
         C3 = '-i-'
         MSGO_ER = 0
      ENDIF
C
      MSG1 = CSUB
      MSG2 = MSG
      CALL STR$TRIM(MSG1,MSG1,IC1)
      CALL STR$TRIM(MSG2,MSG2,IC2)
C
      LC1 = ICFIND('N',CHLP,1,LEN(CHLP))
      LC2 = ICFIND('X',CHLP,1,LEN(CHLP))
      IF (LC1 .GT. 0 .AND. LC1 .LE. LEN(CHLP)) THEN
         WRITE(CBHL,'(I10)',ERR=9) INUM
      ELSE IF (LC2 .GT. 0 .AND. LC2 .LE. LEN(CHLP)) THEN
         WRITE(CBHL,'(Z8,A1)',ERR=9) INUM,'x'
      ELSE
         WRITE(INUT,801,ERR=999) C3,MSG1(1:IC1),MSG2(1:IC2)
         RETURN
      END IF
      IC0 = ICFNBL(CBHL,1,LEN(CBHL))
      GOTO 10
9     IC0 = 10
      CBHL = '         ***'
10    NC = 13-IC0
      WRITE(INUT,802,ERR=999) C3,MSG1(1:IC1),MSG2(1:IC2),CBHL(IC0:)
C
  801 FORMAT (' ',A3,' ',A,': ',A)
C&IF LINUX
C&  802 FORMAT (' ',A3,' ',A,': ',A,' ',A)
C&ELSE
  802 FORMAT (' ',A3,' ',A,': ',A,' ',A<NC>)
C&ENDIF
  999 RETURN
      END
