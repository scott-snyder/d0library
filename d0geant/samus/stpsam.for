      LOGICAL FUNCTION STPSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle stepping through SAMUS drift tubes
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  29-SEP-1990   A.Kiryunin
C-   Updated  18-OCT-1990   A.Kiryunin  New set-up of SAMUS geometry
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER SDRF
      INTEGER NST,NSE,NTU,NR, I,J,K, IHIT
      REAL    HITSM(5),SQ,VECLOC(6),CELSIZ(3)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      STPSAM = .TRUE.
      IF ( DSAM .LT. 2 ) GOTO 999
C
      IF (FIRST) THEN
        CALL UCTOH ('SDRF',SDRF,4,4)
        FIRST = .FALSE.
      ENDIF
C
C ****  Check if SAMUS drift volume and if charge particle
C
      IF (IHSET.NE.SDRF.OR.INWVOL.NE.1.OR.CHARGE.EQ.0.) GOTO 999
C     -- Skip GEANTino
      IF(IPART.EQ.48) GOTO 999
C
C ****  Define station, section and tube numbers as hits
C
      NR=NUMBER(NLEVEL)
      I=NR/1200
      J=NR-1200*I
      K=J/200
      NTU=J-200*K
      NST=I+1
      NSE=K+1
      HITSM(1)=NST
      HITSM(2)=NSE
      HITSM(3)=NTU
      IF (SSAM(1).NE.0.0) THEN
        IF (ISTAK.EQ.0) THEN
          HITSM(5)=0
        ELSE
          HITSM(5)=IPART
        ENDIF
      ENDIF
C
C ****  Convert to local coordinate system
C
      CALL LOCGEO (VECT,VECLOC,CELSIZ,3)
C
C ****  Calculate drift length
C
      SQ=SQRT(VECLOC(4)**2+VECLOC(5)**2)
      IF (SQ.EQ.0.0) THEN
        HITSM(4)=SQRT(VECLOC(1)**2+VECLOC(2)**2)
      ELSE
        HITSM(4)=ABS(VECLOC(5)*VECLOC(1)-VECLOC(4)*VECLOC(2))/SQ
      ENDIF
C
C ****  Store hits
C
      CALL GSAHIT (ISET,IDET,ITRA,NUMBV,HITSM,IHIT)
C
  999 RETURN
      END
