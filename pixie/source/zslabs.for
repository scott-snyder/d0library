      SUBROUTINE ZSLABS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the energy labels of the TRD Z-strip display
C-
C-   Created  14-JUN-1989   LUPE ROSAS
C-   Updated  10-JAN-1990   LUPE HOWELL
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      INDX(4), ITYP, KCOLOR, KINTEN, KFILL,
     X             KSTYL, I
      REAL         X, Y, Z, ENGMIN(4), ENGMAX(4)
      CHARACTER*3  IDET
      CHARACTER*7  MESS1
      CHARACTER*4  MESS2, BOUND1, RCX1
      CHARACTER*5  BOUND2, RCX2
      CHARACTER*22 LINE
C----------------------------------------------------------------------
      DATA MESS1  /'>ENRG<='/
      DATA MESS2  /' TO '/
      DATA ENGMIN /0.,35.,70.,105./
      DATA ENGMAX /35.,70.,105.,9999./
      DATA INDX  /6, 9, 14, 7/  ! blue, green, magenta, cyan
C----------------------------------------------------------------------
      X = 80.
      Y = -3.
      Z = 0.
C- Printing Labels
      DO 20 I=1, 4
        CALL PXCOLN('CDC',INDX(I),4,.FALSE.,KCOLOR,KINTEN,
     &              KFILL,KSTYL)
        CALL PUFTOC(ENGMIN(I),'F4.0',RCX1,BOUND1)
        CALL PUFTOC(ENGMAX(I),'F5.0',RCX2,BOUND2)
        LINE = BOUND1//MESS1//BOUND2 
        CALL PUOPEN
        CALL JJUST(2,2)
        CALL JSIZE(3.,6.0)
        IF (KCOLOR.EQ.0)  THEN
          CALL JLSTYL(KSTYL)
        ELSE
          CALL JPINTR(1)
        ENDIF
        CALL J3MOVE(X,Y,Z)
        CALL J1STRG(LINE)
        CALL JCOLOR(KCOLOR)
        CALL J3MOVE((X+18.),Y,Z)
        CALL J3DRAW((X+33.),Y,Z)
        Y = Y - 6.
        CALL JRCLOS
   20 CONTINUE
  999 RETURN
      END
