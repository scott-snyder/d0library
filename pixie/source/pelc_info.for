      SUBROUTINE PELC_INFO(LPELC,CTAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a PELC Bank Information
C-
C-   Inputs  : LPELC - Address of PELC Bank
C-             CTAG  - PELC(E)/PPHO(P) tag
C-
C-   Modified 30-MAR-1993   S. Chopra - Use GET_EM_DISPLAY_QUANS.
C-   Created  24-MAR-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL XMAX,XMIN,YMAX,YMIN
      REAL YTOP,YBOT,X0,Y0
      INTEGER LPELC,STATUS,NVAR
      REAL EX,EY,EZ,E,ET
      REAL STEPX,STEPY,XSIZ,YSIZ,YPT
      CHARACTER*17 STRING
      CHARACTER*8 CQUAN_NAMES(50),TQUAN_NAMES(50),NAMQUANS(50)
      INTEGER NQUANS,I
      REAL CQUANS(50),TQUANS(50),QUANS(50)
      CHARACTER*1 CTAG
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBCOM.INC'
C--------------------------------------------------------------------------
      CALL J4RGET(1,XMIN,XMAX,YMIN,YMAX)
C
      CALL JOPEN
      XSIZ = (XMAX-XMIN)*0.0575
      YSIZ = (XMAX-XMIN)*0.0215
      YTOP = YMAX - YMAX*0.55
      YPT = YTOP - (YMAX - YMIN)*0.04
      CALL JMOVE(XMIN,YTOP)
      CALL JDRAW(XMAX,YTOP)
      CALL JFONT(5)
      CALL JSIZE(XSIZ,YSIZ)
      CALL JJUST(1,1)
      X0 = XMIN+(XMAX-XMIN)*.035
      Y0 = YTOP-YSIZ*1.5
      CALL JMOVE(X0,Y0)
      IF (CTAG .EQ. 'E') CALL J1STRG('  PELC INFO : ')
      IF (CTAG .EQ. 'P') CALL J1STRG('  PPHO INFO : ')
C-
C.... CALL GET_EM_DISPLAY_QUANS(LPELC,QUANS,NAMQUANS,NQUANS)
      CALL CLEANEM(LPELC,1,OK,STATUS)
      CALL CLEANEM_CQUANS(NVAR,CQUANS)
      CALL CLEANEM_CQUAN_NAMES(NVAR,CQUAN_NAMES)
      CALL CLEANEM_TQUANS(NVAR,TQUANS)
      CALL CLEANEM_TQUAN_NAMES(NVAR,TQUAN_NAMES)
      CALL DISPLAY_QUANS(LPELC,CQUANS,CQUAN_NAMES,TQUANS
     &  ,TQUAN_NAMES,QUANS,NAMQUANS,NQUANS)
C-
      IF (NQUANS .GT. 15)NQUANS = 15
      DO 10 I = 1,NQUANS
        YPT = YPT - YSIZ*2
        Y0  = YPT
        CALL JMOVE(X0,Y0)
        IF (QUANS(I) .LT. 99999.9) THEN
          WRITE(STRING,101)NAMQUANS(I)(1:8)//'=',QUANS(I)
        ELSE
          WRITE(STRING,102)NAMQUANS(I)(1:8)//'='//'*******'
        ENDIF
        CALL J1STRG(STRING)
   10 CONTINUE
      CALL JCLOSE
  101 FORMAT(A9,F8.2)
  102 FORMAT(A17)
C
  999 RETURN
      END

