      SUBROUTINE TOP_LEPTONS_MUON_MTCA_DUMP(LPMUO,LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Supplemental Muon Dump Utility for MTCA
C-                         Package Information
C-
C-   Inputs  :
C-              PMUO Bank Version 5 and Higher (RECO 12.11 onwards)
C-              LPMUO - current bank pointer
C-              LUN   - Lun for printout
C-   Outputs :
C-              Printout on unit LUN
C-   Controls:
C-
C-   Created  11-MAY-1994   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LUN,LPMUO,LMUON,LMTCA,NS
      INTEGER LYRMU,IVTX,IMTC_MAX,IMTC_HMAX,IMTC_GHMAX
C
      REAL TRES,TRES_V,FRACT,HFRACT,GHFRACT,ECHI,EN3,EFRACT_1,ECHI2
C
C *** Look to see if MTCA Bank is available
C
      LMTCA=-1
      NS=IQ(LPMUO-2)
      LMUON=LQ(LPMUO-2-NS)
      IF(LMUON.GT.0) THEN
        LMTCA=LQ(LMUON-9)
      ENDIF
C
C *** Check PMUO Bank Version to see that Information Exists
C
C     IF(IQ(LPMUO+1).LT.5) RETURN
C
C *** Kluge for Version No. Error in RECO 12.11
C *** .... needed as of 5/17/94
C
      IF(IQ(LPMUO+1).LT.5) THEN
        IF(Q(LPMUO+97).LT.0.1) RETURN
      ENDIF
C
C *** MTCA Track Fit Results (normalized residuals)
C
      WRITE(LUN,1000)
      TRES=Q(LPMUO+91)
      TRES_V=Q(LPMUO+92)
      IF(LMTCA.GT.0) THEN
        IVTX=IQ(LMTCA+2)
        WRITE(LUN,1010) IVTX,TRES,TRES_V
      ELSE
        WRITE(LUN,1011) TRES,TRES_V
      ENDIF
C
C *** MTCA Used Calorimeter Layers (total,hadronic,gap/icd)
C
      FRACT=Q(LPMUO+93)
      HFRACT=Q(LPMUO+94)
      GHFRACT=Q(LPMUO+95)
      IF(LMTCA.GT.0) THEN
        IMTC_MAX=IQ(LMTCA+3)
        IMTC_HMAX=IQ(LMTCA+4)
        IMTC_GHMAX=IQ(LMTCA+5)
        WRITE(LUN,1020) FRACT,IMTC_MAX,HFRACT,IMTC_HMAX,GHFRACT,
     1    IMTC_GHMAX
      ELSE
        WRITE(LUN,1021) FRACT,HFRACT,GHFRACT
      ENDIF
C
C *** MTCA Energy Information
C ***       Chisqaure ('MIP-like-ness')
C ***       Energy in 3x3 tower
C ***       Fraction of Energy in Last layer (of OH)
C ***       Layer no. at which muon appears isolated
C ***       Chisquare for layers after muon appears isolated
C
      ECHI=Q(LPMUO+96)
      EN3=Q(LPMUO+97)
      EFRACT_1=Q(LPMUO+98)
      LYRMU=IQ(LPMUO+99)
      ECHI2=Q(LPMUO+100)
      WRITE(LUN,1030) ECHI,LYRMU,ECHI2,EN3,EFRACT_1
C
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(5X,' MTCA Information : ')
 1010 FORMAT(5X,' Vertex No. = ',I3,
     1 ' Norm Track Fit Residuals with/without vertex = ',F8.2,
     2 ' / ',F8.2)
 1011 FORMAT(5X,' Norm Track Fit Residuals with/without vertex = ',F8.2,
     1 ' / ',F8.2)
 1020 FORMAT(5X,' Calorimeter Layers (Used Fraction : No. Avialable) : '
     1 F5.2,' :',I2,' / ',F4.2,' :',I2,' / ',F4.2,' :',I2,
     2 ' (Total/Hadronic/MG+ICD) ')
 1021 FORMAT(5X,' Fraction of Used Calorimeter Layers = ',F4.2,' / ',
     1 F4.2,' / ',F4.2,' (Total/Hadronic/MG+ICD) ')
 1030 FORMAT(5X,' Energy Chisq = ',F10.2,' Layer for Isolation = ',I6,
     1 ' Isolated Energy Chisq = ',F8.2,
     2 /5X,' Energy in 3x3 tower array = ',F6.2,
     3 ' Fraction of Energy in Last Layer = ',F4.2)
      END
