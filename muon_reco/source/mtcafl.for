      SUBROUTINE MTCAFL(NTRK,IVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MTCA bank (results of Muon Tracking in
C-   Calorimeter)
C-
C-   Input :  LMUON    : Address of MUON bank
C-            contents of MTC.INC
C-
C-   Created    9-FEB-1994   Daria Zieminska   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTRK,IVTX,GZMUON,LMUON,LMTCA 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZMTCA.LINK/LIST'
      INCLUDE 'D0$INC:MTC.INC/LIST'
      LOGICAL FIRST 
      DATA FIRST /.TRUE./
C
      IF(FIRST) THEN
        FIRST = .FALSE.
      ENDIF
      LMUON=GZMUON(NTRK)
      LMTCA=LQ(LMUON-IZMTCA)
      IF (LMTCA.EQ.0) CALL BKMTCA(LMUON,LMTCA)
      iQ(LMTCA+2)=IVTX
      IQ(LMTCA+3)=IMTC_MAX
      IQ(LMTCA+4)=IMTC_HMAX
      IQ(LMTCA+5)=IMTC_GHMAX
      IQ(LMTCA+6)=IMTC_LYRMU 
      CALL UCOPY2(XMTC_DIRCOS,Q(LMTCA+7),3)
      CALL UCOPY2(XMTC_POINT,Q(LMTCA+10),3)
      Q(LMTCA+13)=XMTC_TRES
      Q(LMTCA+14)=XMTC_FRACT 
      Q(LMTCA+15)=XMTC_HFRACT 
      Q(LMTCA+16)=XMTC_GHFRACT 
      Q(LMTCA+17)=XMTC_ECHI 
      CALL UCOPY2(XMTC_DIRCOS_V,Q(LMTCA+18),3)
      CALL UCOPY2(XMTC_POINT_V,Q(LMTCA+21),3)
      Q(LMTCA+24)=XMTC_TRES_V
      Q(LMTCA+25)=XMTC_EN3 
      Q(LMTCA+26)=XMTC_EN5 
      CALL UCOPY2(XMTC_EFRACT_H,Q(LMTCA+27),3)
      Q(LMTCA+30)=XMTC_ECHI_33 
      Q(LMTCA+31)=XMTC_FRACT_33 
      Q(LMTCA+32)=XMTC_ECHI_55 
      Q(LMTCA+33)=XMTC_FRACT_55 
      Q(LMTCA+34)=XMTC_ECHI2 
C- EG added 13-APR-94
      Q(LMTCA+35)=XMTC_ETRACK
      Q(LMTCA+36)=0.  ! words 36-40 to be filled by MUFIX
      Q(LMTCA+37)=0.
      Q(LMTCA+38)=0.
      Q(LMTCA+39)=0. 
      Q(LMTCA+40)=0. 
C-
  999 RETURN
      END
