      SUBROUTINE VEE_MC(MOM,PHI,THE,DELMOM,DELPHI,DELTHE,OK)
C------------------------------------------------------------------
C  Compare reconstructed vee parameters with Isajet values
C
C  Daria Zieminska 26-OCT-1991
C  Updated   1-SEP-1993   Oscar Ramírez  Now  look for lambda and anti-lambda
C
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZISAE.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP2.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISV1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK/LIST'
      INTEGER LZLOC,LISAE,LOC,LPRIM
      INTEGER LISV2I,NISV2,IFL
      INTEGER NV,K,IORV,IORP,LISV2,LISP2,IS
      CHARACTER*8 NAME,LABEL
      INTEGER ICALL,IER
      REAL PS(2),RATIO,MOMISA,PHIISAMIN,THEISAMIN,ZPRIM
      REAL MOM,PHI,THE,DELMOM,DELPHI,DELTHE,DELPHIMIN,PHIISA,THEISA
      REAL XYZ(3),EXYZ(3),DX,DY,DZ,MASS
      INTEGER GZISV1,PRUNIT,USUNIT,IPRINT
      LOGICAL OK
      SAVE ICALL
      DATA ICALL/0/
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VEES_RCP')
        CALL EZGET('IPRINT',IPRINT,IER)
        PRUNIT=USUNIT()
        ICALL=1
        CALL EZRSET
      END IF
      LISAE = LQ(LHEAD-IZISAE)
      LISV2 = LQ(LISAE-IZISV2)
      LPRIM=GZISV1()
      ZPRIM=Q(LPRIM+9)
      OK=.FALSE.
      PHIISAMIN=999.
      THEISAMIN=999.
      DELPHIMIN=9.99
      DELPHI=9.99
      DELTHE=9.99
      DELMOM=9.99
    1 IF ( LISV2.GT.0 ) THEN
        IORV = LQ(LISV2-2)
        IF ( IORV.GT.0) IORV = IQ(IORV-5)
        IORP = LQ(LISV2-3)
        IF ( IORP.GT.0) IORP = IQ(IORP-5)
        NV = IQ(LISV2-5)
        NAME = LABEL(IQ(LISV2+1))
        IF (NAME.EQ.'KS'.OR.NAME.EQ.'L'.OR.NAME.EQ.'AL') THEN
          LOC=LQ(LISV2-3)
          LOC=LQ(LOC+1)
          MASS=Q(LOC+6)
C          IF (MASS.GT.5.1.AND.MASS.LT.5.2) THEN
          PHIISA=ATAN2(Q(LISV2+3),Q(LISV2+2))
          IF (PHIISA.LT.0.) PHIISA=PHIISA+TWOPI
          DELPHI=PHI-PHIISA
          IF (DELPHI.GT.TWOPI) DELPHI=DELPHI-TWOPI
          IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
          IF (DELPHI.LT.-PI) DELPHI=TWOPI+DELPHI
          IF (DELPHI.GT.PI) DELPHI=TWOPI-DELPHI
          IF (ABS(DELPHI).LT.ABS(DELPHIMIN)) THEN
            DELPHIMIN=DELPHI
            PHIISAMIN=PHIISA
            THEISAMIN=
     &        ATAN2(SQRT(Q(LISV2+2)**2+Q(LISV2+3)**2),Q(LISV2+4))
            DELTHE=THE-THEISAMIN
            MOMISA=Q(LISV2+5)
            DELMOM=MOM-MOMISA
          END IF
C          END IF
        ENDIF
        LISV2 = LQ(LISV2)
        GO TO 1
      END IF
      DELPHI=DELPHIMIN
      PHIISA=PHIISAMIN
      THEISA=THEISAMIN
      IF (ABS(DELPHI).LT.3.1.AND.ABS(DELTHE).LT.3.1) THEN
        OK=.TRUE.
      END IF
C      IPRINT=0
      IF (OK.AND.IPRINT.GT.0) THEN
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) MOMISA,DELMOM,DELPHI,DELTHE,PHI,PHIISA,
     1                    THE,THEISA
      END IF
  101 FORMAT(/1X,' Ks momentum(Isa)   delta_p  delta_phi   delta_theta',
     1       '   phi       phiisa    the        theisa  ')
  102 FORMAT(5X,8F11.4)
 1000 RETURN
      END
