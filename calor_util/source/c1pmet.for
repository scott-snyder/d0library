      SUBROUTINE C1PMET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    First pass at calculating Missing ET (no corrections)
C-
C-   Created  18-JAN-1989   Serban D. Protopopescu
C-   Updated  10-JAN-1990   Harrison B. Prosper
C-      Added ET scaler sum
C-   Modified  6-Jul-1992   Ulrich Heintz: fixed scalar Et sum
C-   Modified 12-Dec-1992   S. Krzywdzinski: corrected sigEt=Q(LPNUT+13)
C-   Modified  6-Apr-1993   S. Krzywdzinski: cell contributions of
C-     sigEt**2 are taken from CAEH bank if its version > 1
C-   Updated  16-APR-1993   Stan M. Krzywdzinski: set negative variances
C-                          to zero, instead of 1.0E5 before.
C-                          Corrected code to avoid sqrt of negative.
C-   Updated  27-APR-1993   Stan M. Krzywdzinski: protection against
C-                          CAEH not existing.
C-   Updated  17-MAY-1993   Stan M. Krzywdzinski: error matrix contributions
C-                          are taken from CAEH bank if its and PNUT
C-                          bank versions are > 2.
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C----------------------------------------------------------------------
      REAL    PHI,THETA,ETA,EZOE,ETSQ,ETOT,EX,EY,EZ,SIG2EX,SIG2EY,ET
      INTEGER GZCAEH,LCAEH,NCH,POINTR,I,NR,LPNUT,GZPNUT,IETA,IPHI,LAYER
      CHARACTER*80 MSG
      INTEGER NVERSN
      REAL SIG2ET,EXYZMAT(3,3)
      LOGICAL GETS2ET,GETMAT
C----------------------------------------------------------------------
C
      LPNUT=GZPNUT(1)
      IF(LPNUT.GT.0) GOTO 999
      LCAEH=GZCAEH()
      IF (LCAEH .LE. 0) THEN
        CALL ERRMSG('NO_CAEH','C1PMET',
     &    'No CAEH bank - will not book PNUT(1)','W')
        GOTO 999
      ENDIF
      CALL BKPNUT(1)     ! Book PNUT bank (1st pass)
C
      NVERSN=IQ(LCAEH+1)
      NR=IQ(LCAEH+2)
      GETS2ET=(NVERSN.GE.2).AND.(NR.GE.13)
      GETMAT =(NVERSN.GE.3).AND.(NR.GE.17)
      NCH=IQ(LCAEH+3)
      POINTR=LCAEH
      EX=0
      EY=0
      EZ=0
      ET=0
      SIG2EX=0
      SIG2EY=0
      SIG2ET=0
      CALL VZERO(EXYZMAT,9)
C
C         loop over channels
C
      DO 1 I=1,NCH
        IETA =IQ(POINTR+12)
        IPHI =IQ(POINTR+13)
        LAYER=IQ(POINTR+14)
        IF ( PTCAEP(IETA,IPHI,LAYER) .GT. 0 ) THEN
C         skip massless gaps or ICD
          IF(LAYER.LT.8.OR.LAYER.GT.10) THEN
            EX=EX+Q(POINTR+4)
            EY=EY+Q(POINTR+5)
            EZ=EZ+Q(POINTR+6)
            ET=ET+Q(POINTR+8)
            SIG2EX=SIG2EX+Q(POINTR+9)
            SIG2EY=SIG2EY+Q(POINTR+10)
            IF(GETS2ET) SIG2ET=SIG2ET+Q(POINTR+16)
            IF(GETMAT) THEN
              EXYZMAT(1,1)=EXYZMAT(1,1)+Q(POINTR+ 9)
              EXYZMAT(2,2)=EXYZMAT(2,2)+Q(POINTR+10)
              EXYZMAT(3,3)=EXYZMAT(3,3)+Q(POINTR+17)
              EXYZMAT(1,2)=EXYZMAT(1,2)+Q(POINTR+18)
              EXYZMAT(1,3)=EXYZMAT(1,3)+Q(POINTR+19)
              EXYZMAT(2,3)=EXYZMAT(2,3)+Q(POINTR+20)
            ENDIF
          ENDIF
        ENDIF
        POINTR=POINTR+NR
    1 CONTINUE
      IF  (SIG2EX.LT.0.) THEN
        WRITE(MSG,'('' SIGMA EX**2 = '',E12.2)') SIG2EX
        CALL ERRMSG('SIGMA_PNUT1_MISTAKE','C1PMET',MSG,'W')
        SIG2EX=0.
        IF (GETMAT) EXYZMAT(1,1)=SIG2EX
      ENDIF
      IF (SIG2EY.LT.0.) THEN
        WRITE(MSG,'('' SIGMA EY**2 = '',E12.2)') SIG2EY
        CALL ERRMSG('SIGMA_PNUT1_MISTAKE','C1PMET',MSG,'W')
        SIG2EY=0.
        IF (GETMAT) EXYZMAT(2,2)=SIG2EY
      ENDIF
      IF      (SIG2ET.LT.0.) THEN
        WRITE(MSG,'('' SIGMA ET**2 = '',E12.2)') SIG2ET
        CALL ERRMSG('SIGMA_PNUT1_MISTAKE','C1PMET',MSG,'W')
        SIG2ET=0.
      ENDIF
      IF (GETMAT) THEN
        IF (EXYZMAT(3,3).LT.0.) THEN
          WRITE(MSG,'('' SIGMA EZ**2 = '',E12.2)') EXYZMAT(3,3)
          CALL ERRMSG('SIGMA_PNUT1_MISTAKE','C1PMET',MSG,'W')
          EXYZMAT(3,3)=0.
        ENDIF
        EXYZMAT(2,1)=EXYZMAT(1,2)
        EXYZMAT(3,1)=EXYZMAT(1,3)
        EXYZMAT(3,2)=EXYZMAT(2,3)
      ENDIF
C
C      fill PNUT bank
C
      LPNUT=GZPNUT(1)
      GETMAT=GETMAT.AND.(IQ(LPNUT+1).GE.3)   ! check PNUT bank version
      Q(LPNUT+3)=-EX
      Q(LPNUT+4)=-EY
      Q(LPNUT+5)=-EZ
      ETSQ=EX**2+EY**2+.0001     ! add small amount to avoid sqrt(0)
      ETOT=SQRT(ETSQ+EZ**2)
      Q(LPNUT+6)=ETOT
      Q(LPNUT+7)=SQRT(ETSQ)
C
C   calculate PHI, THETA and ETA
      PHI=ATAN2(-EY,-EX+.001)
      IF(PHI.LT.0) PHI=PHI+TWOPI
      EZOE=-EZ/ETOT
      THETA=ACOS(EZOE)
      ETA=10.*SIGN(1.,EZOE)
      IF(ABS(EZOE).LT.0.999) ETA=-ALOG(TAN(THETA/2.))
      Q(LPNUT+8)=THETA
      Q(LPNUT+9)=ETA
      Q(LPNUT+10)=PHI
      Q(LPNUT+11)=SIG2EX
      Q(LPNUT+12)=SIG2EY
      IF (GETS2ET) THEN
        Q(LPNUT+13)=SQRT(SIG2ET)
      ELSE
        Q(LPNUT+13) = SQRT(SIG2EX*EX**2+SIG2EY*EY**2)/Q(LPNUT+7)
      ENDIF
      IF(GETMAT) THEN
        Q(LPNUT+15)=EXYZMAT(3,3)
        Q(LPNUT+16)=EXYZMAT(1,2)
        Q(LPNUT+17)=EXYZMAT(1,3)
        Q(LPNUT+18)=EXYZMAT(2,3)
      ENDIF
      Q(LPNUT+14)=ET
C
  999 RETURN
      END
