      SUBROUTINE KMARK(X,Y,Z)
C
C   This routine displays a marker at point X,Y,Z.
C
      IMPLICIT NONE
      REAL X, Y, Z
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      REAL DEFACX, DEFACY, XSIZE, YSIZE
      CHARACTER*4 PRIMI
      CHARACTER*5 MRKRS
      INTEGER IMRK
      EXTERNAL ERRHND
      DATA MRKRS/'++*OX'/

      CALL KPRIM(PRIMI)
      IMRK = MOD(CMARKR,5)
      IF (IMRK .EQ. 0) IMRK = 5
      DEFACX = (UWIND(2) - UWIND(1)) / (UVIEW(2) - UVIEW(1))
      DEFACY = (UWIND(4) - UWIND(3)) / (UVIEW(4) - UVIEW(3))
      IF (IMRK .EQ. 1) THEN
         XSIZE = DEFACX * 0.02
         YSIZE = DEFACY * 0.02
      ELSE
         XSIZE = DEFACX * 0.04
         YSIZE = DEFACY * 0.04
      ENDIF
      CALL PBEGS(PRIMI,ERRHND)
      CALL PSECOL('"',HUECOL(CURCOL+1,1),SATCOL(CURCOL+1,1),'"',ERRHND)
      CALL PCHSCA('"', XSIZE, YSIZE, '"', ERRHND)
      CALL PCHS('"', X - XSIZE / 2.0, Y - YSIZE / 2.0, Z, 0.0, 0.0,
     +          MRKRS(IMRK:IMRK), ERRHND)
      CALL PENDS(ERRHND)
      CALL PINCL(PRIMI, INST, ERRHND)
      RETURN
      END
