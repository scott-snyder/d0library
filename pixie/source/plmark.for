      SUBROUTINE PLMARK(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put marks on LEGO PLOT
C-
C-   Inputs  : IMARK - Kind of mark to make .GT.0 Missing ET
C-             TXMIN - Min of y lego plot
C-             TXMAX - Max of x lego plot
C-             TYMIN - Min of y lego plot
C-             TYMAX - Max of y lego plot
C-             ZDIV  - Scale factor
C-
C-   Created   3-MAY-1989   Sharon Hagopian
C-   Updated  19-DEC-1989   Lupe Rosas Set up to use color table
C-   Updated  20-FEB-1992   Nobuaki Oshima - Change PNUT handling.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C ARGUMENTS:
      INTEGER IMARK  ! FLAG to tell which kind of mark to put
                     ! IMARK.GT.0 for Missing Et
      REAL TXMIN,TXMAX ! Min and max of lego plot x axis
      REAL TYMIN,TYMAX ! Min and max of lego plot y axis
      REAL ZDIV !Z SCALE FACTOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C------------------------------------------------------------------------
      INTEGER LPNUT,GZPNUT,IPASS
      INTEGER IPHI,IETA
      REAL ETA,ET,ETERR
      REAL TX,TY,Z1,Z2,Z3,DELZ
      REAL XSIZ,YSIZ
      CHARACTER*3 COL1
      CHARACTER*8 TITLE
      DATA TITLE/'Miss ET '/
C------------------------------------------------------------------------
      IF(IMARK.LE.0)GO TO 999
C-
C--- Missing ET
C-
      DO 10 IPASS = 5,1,-1
        LPNUT = GZPNUT(IPASS)
        IF( LPNUT .GT. 0)   GO TO 20
   10 CONTINUE
      GO TO 100
C-
   20 IPHI=(Q(LPNUT+10)/TWOPI)*64+1
      ETA=Q(LPNUT+9)
      IETA=10.*ETA
      IF(ETA.LT.0)THEN
        IETA=IETA+NETAL+1
      ELSE
        IETA=IETA+NETAL+2
      ENDIF
      ET=Q(LPNUT+7)
      ETERR=Q(LPNUT+13)
      TX=IPHI
C Set missing Et at ETA=0.
      TY=37.
      IF(TX.LT.TXMIN.OR.TX.GT.TXMAX)GO TO 100
      Z2=ET/ZDIV
      CALL JLSTYL(1)
      CALL JPINTR(0) ! No fill
      CALL PXCOLR('MAG') ! Filling with magenta
C DRAW VBAR
      CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z2,0)
C label line
      XSIZ=1.0
      YSIZ=.67
      CALL JSIZE(XSIZ,YSIZ)
      DELZ=1.
      CALL J3MOVE(TX,TY,Z2+DELZ)
      CALL JJUST(2,2)
      CALL J1STRG(TITLE)
  100 CONTINUE
C-
  999 RETURN
      END
