      SUBROUTINE PFDCDR(IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the FDC main Theta and Phi Volumes for
C-                         scale
C-
C-   Inputs  : IVIEW = 1 for X-Z, 2 for Y-Z view, 3 for R-Z view
C-   Outputs : none
C-
C-   Created  24-JUL-1989   Jeffrey Bantly
C-   Updated  10-JAN-1990   Lupe Howell  Changing the parameter type
C-                          for calls to PFRECT (Col Table)
C-   Updated   4-JUN-1990   Jeffrey Bantly
C-   Updated  30-APR-1991   Jeffrey Bantly  add labels
C-   Updated   7-MAY-1991   Lupe Howell  Write the labels only if FDC ONLY is
C-                          true
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves,
C-    add EZPICK call
C-   Updated  30-MAR-1992   Robert E. Avery  Change colors. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER HALF,MODULE,LAYER,IVIEW
      INTEGER IER
C
      REAL    DIMENS(6),DIR,YC,DY,ZC,DZ,ANG
C
      CHARACTER*4 ICOLOR,TITCLR
      CHARACTER*10 TEXT
C
      LOGICAL EZERROR
      LOGICAL FDCONLY
C
C----------------------------------------------------------------------
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DSGDR','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC ONLY', FDCONLY)
      CALL PUGETA( 'FDC COLR LABELS', TITCLR )
      CALL PUGETA( 'FDC COLR BOUNDARY',ICOLOR)
      CALL EZRSET
C
      DO 10 HALF=0,1
        DO 20 LAYER=0,2
          MODULE=HALF*3+LAYER
          CALL GTFWAL(MODULE,DIMENS)
          IF(DIMENS(1).LT. 0.0) GOTO 20
          YC = (DIMENS(1) + DIMENS(2))/2.       ! Cut away view
          DY = (DIMENS(2) - DIMENS(1))/2.       ! Cut away view
          ZC = DIMENS(6)
          DZ = DIMENS(3)
          ANG = 0.
          CALL PFRECT(ICOLOR,ZC,YC,ZC,DZ,DY,ANG)
          CALL PFRECT(ICOLOR,ZC,(-1.)*YC,ZC,DZ,DY,ANG)
   20   CONTINUE
C
C ****  Write label only if FDC ONLY
C
        IF ( FDCONLY ) THEN
          IF(HALF.EQ.0) THEN
            WRITE(TEXT,30)
          ELSE
            WRITE(TEXT,31)
          ENDIF
          CALL JJUST(2,2)
          CALL PXCOLR(TITCLR)
          CALL PUVSTR(ZC,YC*2.,1.5,2.5,TEXT)
        ENDIF
   10 CONTINUE
   30 FORMAT('North FDC ')
   31 FORMAT(' South FDC')
C----------------------------------------------------------------------
  999 RETURN
      END
