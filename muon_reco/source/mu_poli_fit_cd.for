      SUBROUTINE MU_POLI_FIT_CD(PORD,PORW,NFITDA,NFITDBC,NFITWA,
     &       NFITWBC,VARD,VARW,ORENT,X_1,X_2,XPDF,XDF,XPWF,XWF,
     &       ORDERW,ORDERD,DDA,DDB,DWA,DWB,COEFD,COEFW,SLDA,SLDBC,
     &       SLWA,SLWBC,POLY_CHISQD,POLY_CHISQW,VTXH,MUPFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fit a (up to) 5 order polinom to
C-   the torodial section
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JUN-1991   AKl
C-   Updated  24-OCT-1991   A.Klatchko  RESTRUCTURE 
C-   Updated   3-JAN-1992   A.Klatchko  SPECIALIZE FOR GLOBAL FIT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MUPFIT,FIRST
      INTEGER ORDERW,ORDERD,NFITDA,NFITDBC,NFITWA,NFITWBC,ORENT
      INTEGER NFD,NFITD,NFITW,L1,PORD,PORW,NFW,NVER,IERZ
      REAL VTXH(3),VTXE(3),ERP,VARD(*),CHI_MIN_CD
      REAL DDA,DDB,DWA,DWB,SLDA,SLDBC,SLWA,SLWBC
      REAL XPDF(*),XDF(*),XPWF(*),XWF(*),POLY_CHISQD,POLY_CHISQW
      REAL COEFD(*),COEFW(*),X_1,X_2,XD(15),XP(15),XW(15)
      REAL VARW(*),COERW(5),COERD(5),POLI_DER,POLI_ERR
      DATA FIRST/.TRUE./ 
C----------------------------------------------------------------------
      IF(FIRST)THEN
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('CHI_MIN_CD',CHI_MIN_CD,IERZ)
      ENDIF
      MUPFIT = .TRUE.
C------------------------------DRIFT--------------------------------C
      NFITD = NFITDA + NFITDBC
C
      DO L1 = 1,NFITD
        XD(L1) = XDF(L1)
        XP(L1) = XPDF(L1)
        XW(L1) = XWF(L1)
      ENDDO
C
      CALL POLI_FIT(PORD,VARD,X_1,X_2,XP,XD,NFITD,
     &      NFITDA,NFITDBC,COEFD,POLY_CHISQD,ORDERD,COERD)
C
C-----------------------------------------------------------------C
C------------------------------WIRE--------------------------------C
C-----------------------------------------------------------------C
      NFITW = NFITWA + NFITWBC
C
C
      CALL POLI_FIT(PORW,VARW,X_1,X_2,XP,XW,NFITW,
     &      NFITWA,NFITWBC,COEFW,POLY_CHISQW,ORDERW,COERW)
C
      IF(POLY_CHISQD+POLY_CHISQW .GT. CHI_MIN_CD)MUPFIT = .FALSE.
C-----------------------------------------------------------------C
C    slopes and errors
C-----------------------------------------------------------------C
      SLDA = POLI_DER(1,ORDERD,COEFD,X_1)
      DDA = ABS(POLI_ERR(ORDERD,COERD,X_1)/SLDA)
      SLDBC = POLI_DER(1,ORDERD,COEFD,X_2)
      DDB = ABS(POLI_ERR(ORDERD,COERD,X_2)/SLDBC)
      SLWA = POLI_DER(1,ORDERW,COEFW,X_1)
      DWA = ABS(POLI_ERR(ORDERW,COERW,X_1)/SLWA)
      SLWBC = POLI_DER(1,ORDERW,COEFW,X_2)
      DWB = ABS(POLI_ERR(ORDERW,COERW,X_2)/SLWBC)
C
      IF(ABS(SLDA - SLDBC) .LT. 0.00001)THEN
        MUPFIT = .FALSE.
      ENDIF
C_________________________________________________________________C
C-----------------------------------------------------------------C
C
  999 RETURN
      END
