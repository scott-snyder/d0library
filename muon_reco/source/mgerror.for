C VAX/DEC CMS REPLACEMENT HISTORY, Element GERROR.FOR
C *2     5-FEB-1992 01:21:12 ABACHI "Protection against crash"
C *1     6-JAN-1992 17:40:16 HEDIN " "
C VAX/DEC CMS REPLACEMENT HISTORY, Element GERROR.FOR
      SUBROUTINE MGERROR(NFITA,NADDA,NHITS,PIN,PLOW,TRAD_CAL,VARD,
     &  VARW,ECD,ECW,XPF,VARPD,VARPW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CONSTRUCT WEIGHTS FOR POLYNOMIAL GLOBAL FIT
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-JAN-1992   A.Klatchko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NADDA,NHITS,I,NFITA
      REAL PIN,TRAD_CAL,VARD,VARW,ECD,ECW,VARPD(*),VARPW(*),PLOW
      REAL XPF(*),VARADD(15),P,RADM,THETA_CMS
      CHARACTER*40 MSG
      DATA RADM/0.1/
C----------------------------------------------------------------------
      CALL VZERO(VARPD(1),NHITS)
      CALL VZERO(VARPW(1),NHITS)
      CALL VZERO(VARADD(NADDA),NHITS)
      P = ABS(PIN)
      IF(P .LT. PLOW)THEN
        THETA_CMS = (0.0136/P)*SQRT(RADM)*
     &    (1.+0.088*LOG10(RADM))
        DO  I = NADDA+2,NFITA
          VARADD(I) = ABS((XPF(I) - XPF(NADDA+1))*THETA_CMS)
        ENDDO
C
        DO  I = NFITA+2,NHITS
          VARADD(I) = ABS((XPF(I) - XPF(NFITA+1))*THETA_CMS)
        ENDDO
        GOTO 999
      ENDIF
C
      IF(TRAD_CAL .GT. 0)THEN
        THETA_CMS = (0.0136/P)*SQRT(TRAD_CAL)*
     &    (1.+0.088*LOG10(TRAD_CAL))
      ELSE
        WRITE(MSG,'(''Negative or zero RADL in CAL '',F8.1)')TRAD_CAL
        CALL ERRMSG('Warning ','MGERROR',MSG,'W')
        THETA_CMS = (0.0136/P)*SQRT(1000.*RADM)
      ENDIF
      VARADD(1) = ABS((XPF(2) - XPF(1))*THETA_CMS)
      VARPD(1) = VARADD(1) + ECD
      VARPD(1) = VARPD(1)*VARPD(1)
      VARPD(1) = 1./VARPD(1)
      VARPW(1) = VARADD(1) + ECW
      VARPW(1) = VARPW(1)*VARPW(1)
      VARPW(1) = 1./VARPW(1)
C
      DO I =NADDA+1,NHITS
        VARPD(I) = VARADD(I) + VARD
        VARPD(I) = VARPD(I)*VARPD(I)
        VARPD(I) = 1./VARPD(I)
        VARPW(I) = VARADD(I) + VARW
        VARPW(I) = VARPW(I)*VARPW(I)
        VARPW(I) = 1./VARPW(I)
      ENDDO
  999 RETURN
      END
