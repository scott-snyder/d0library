      SUBROUTINE PF3DTH(HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one FDC Theta sector in 3-D using PFBOX
C-
C-   Inputs  : HALF,QUAD,SECTOR
C-   Outputs : Display on screen
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated  16-MAY-1991   Susan K. Blessing  Fix EZRSET error. 
C-   Updated  30-MAR-1992   Robert E. Avery  Change  color name.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HALF,QUAD,SECTOR,LAYER,MODULE
      REAL DIMENS(6),XC,YC,ZC,X,Y,Z,DIR,QUANG(0:7),ANG,RC
      CHARACTER*4 SECCLR
      CHARACTER*4 CVAL, REM
      INTEGER TYP,IVAL,IER
      LOGICAL EZERROR
      DATA QUANG/45.,45.,225.,225.,0.,0.,180.,180./
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DTH','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some FDC constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','FDC COLR SECTOR',1,IVAL,
     &       SECCLR,TYP,REM,IER)
      LAYER=0
      IF(QUAD.GT.3) LAYER=1
      DIR=-1.
      IF(HALF.EQ.1) DIR=1.
      MODULE=HALF*3+LAYER
      CALL GTFWTX(HALF,QUAD,SECTOR,DIMENS)
      XC=0.
      YC=0.
      ZC=0.
      RC=SQRT(DIMENS(4)**2 + DIMENS(5)**2)
      IF(QUAD.EQ.0 .OR. QUAD.EQ.2 .OR. QUAD.EQ.4 .OR. QUAD.EQ.6) THEN
        X=XC+RC
        Y=YC
        Z=ZC+DIMENS(6)
      ELSE
        X=XC
        Y=YC+RC
        Z=ZC+DIMENS(6)
      ENDIF
      ANG=QUANG(QUAD)
      CALL PXCOLR(SECCLR)
      CALL PFBOX(X,Y,Z,DIMENS(1),DIMENS(2),DIMENS(3),ANG)
C----------------------------------------------------------------------
C
C ****  Reset RCP bank
C
  900 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
