      SUBROUTINE MFIT_FILL(IMTRK,ITRAK,HITA,HITB,HITC,NFITA,NFITBC,
     &   QUAD,MAG,DIC,DOC,VMU,PFIT,DPFIT,DE_T,DE_C,PX,PY,PZ,ITER,
     &   CHI_TRACK_TOR,B_MEAN,AMISS,IFAIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILL (PARTIALLY/TEMPORARILY) MUON BANK
C-
C-   Inputs  :ALL PARAMETERS
C-   Outputs :NONE
C-   Controls:
C-
C-   Created  21-OCT-1991   A.Klatchko
C-   Updated  28-OCT-1991   A.Klatchko  Add eta theta phi load to bank
C-   Updated   8-NOV-1991   A.Klatchko  Change format
C-   Updated  18-NOV-1991   A.Klatchko
C-   Updated  02-DEC-1991   S. ABACHI  Links correctly protected
C-   Updated  09-DEC-1991   S. ABACHI  muon track counter now correct
C-   DH 1/92 remove structural link assignment
C-   Updated  24-JAN-1992   S. ABACHI  momentum errors modified
C-   Updated  04-FEB-1992   S. ABACHI  A new kludge for errors on px,py,pz
C-   Updated  21-FEB-1992   S. ABACHI  ITRAK added to argument,
C-                                     MUOT added.
C-   Updated  31-JAN-1992   A.Klatchko  ERROR ON MOMENTUM CHECKED
C-   Updated  24-FEB-1992   A.Klatchko  ADD AMISS
C-   Updated  04-MAR-1992   S. ABACHI  Flag (item 6) modified
C-   Updated  06-JAN-1993   S. ABACHI  verxyz2 was used instead of verxyz
C    DH 4/93 minor problem in VERXYZ call
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZTRLNK.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER LMFIT,GZMUON,LMUON,LMUOT,GZMUOT,IMFIT,IMUON
      INTEGER NFITA,NFITBC,IFAIL
      INTEGER IMTRK,ITRAK,HITA,HITB,HITC,QUAD,ITER,NS,CHARGE,FC,FM
      REAL MAG(3),DIC(3),DOC(3),PFIT,DPFIT,DE_T,PX,PY,PZ
      REAL CHI_TRACK_TOR,PT,VMU(3),DE_C,THETA,PHI,ETA,SQRT3
      REAL B_MEAN,SQRT2,VERTEX(3),PP,SUMP,SUMP2,EPS
      INTEGER IVER,IV,I,AMISS
      DATA SQRT2,SQRT3,EPS/1.414213562,1.732050808,0.000001/
C----------------------------------------------------------------------
C
C-------- BOOK & FILL MFIT BANK
C
      IFAIL = 0
      LMUON = GZMUON(IMTRK)
      IF(LMUON .LE. 0) THEN
        CALL INTMSG('MFIT_FILL : NO MUON BANK EXIST')
        IFAIL = 1
        GOTO 999
      ENDIF
      CALL GRLINK('MFIT_FILL', IMUON)
      LRLINK(IMUON) = LMUON
C
      CALL BKMFIT(IMUON,LMFIT)
      CALL GRLINK('MFIT_FILL', IMFIT)
      LRLINK(IMFIT) = LMFIT
      NS = IQ(LMFIT - 2)
      LMUOT = GZMUOT(ITRAK)
      LQ(LMFIT - NS - 1) = LMUOT
C
      LMUON = LRLINK(IMUON)
C
      IF(ITER .EQ.0)THEN
        FC = 2
        FM = 0
      ELSEIF(ITER .EQ. 100 .OR. ITER .EQ. 1)THEN
        FC = 2
        FM = 2
      ELSEIF(ITER .EQ. 2)THEN
        FC = 1
        FM = 1
      ELSEIF(ITER .GE. 999)THEN
        FM = 3
        IF(HITA .GT. 0) THEN
          FC = 2
        ELSE
          FC = 1
          IF(ITER .GT. 999) FC = 3
        ENDIF
        IF(FC .EQ. 1 .OR. FC .EQ. 3) THEN
          IVER=-1
          CALL VERXYZ2(IVER,VERTEX,IV)
          DO I=1,3
            VMU(I) = VERTEX(I)
          ENDDO
        ENDIF
      ENDIF
C
      CHARGE = INT(PFIT/ABS(PFIT))
C
      IQ(LMFIT+1) = 1
      IQ(LMFIT+2) = 10*NFITA + NFITBC
      IQ(LMFIT+3) = 0
      IQ(LMFIT+4) = QUAD
      IQ(LMFIT+5) = 2
      IQ(LMFIT+6) = FC
      IQ(LMFIT+7) = FM
      IQ(LMFIT+8) = 0
      IQ(LMFIT+9) = 0
      IQ(LMFIT+10) = CHARGE
C
      Q(LMFIT+11)= VMU(1)
      Q(LMFIT+12)= VMU(2)
      Q(LMFIT+13)= VMU(3)
C
      Q(LMFIT+14) = MAG(1)
      Q(LMFIT+15) = MAG(2)
      Q(LMFIT+16) = MAG(3)
C
      Q(LMFIT+17)= DOC(1)
      Q(LMFIT+18)= DOC(2)
      Q(LMFIT+19)= DOC(3)
C
      Q(LMFIT+20)= PX
      Q(LMFIT+21)= PY
      Q(LMFIT+22)= PZ
      PP = ABS(PFIT) !NOTE! MOMENTUM IS NOT SIGNED
      Q(LMFIT+23)= PP
      PT = SQRT(PX*PX + PY*PY)
      Q(LMFIT+24)= PT
      SUMP = ABS(PX) + ABS(PY) + ABS(PZ)
      SUMP2 = ABS(PT) + ABS(PZ)
      Q(LMFIT+25)= (DPFIT * PX )**2
      Q(LMFIT+26)= (DPFIT * PY )**2
      Q(LMFIT+27)= (DPFIT * PZ )**2
      Q(LMFIT+28)= (DPFIT * PP )**2
      Q(LMFIT+29)= (DPFIT * PT )**2
      Q(LMFIT+30)= CHI_TRACK_TOR
      Q(LMFIT+31)= B_MEAN
      Q(LMFIT+32)= DE_C
      Q(LMFIT+33)= DE_T
C
      IF(ITER .GE. 999) THEN
        IQ(LMFIT+5) = 2
        IQ(LMFIT+2) = HITB
        IQ(LMFIT+3) = HITC
      ENDIF
C
      IF(AMISS .EQ. 1 .OR. AMISS .EQ. 4)IQ(LMFIT+6)=1
      IF(ABS(VMU(1)) .LE. EPS .AND. ABS(VMU(2)) .LE. EPS)
     &        IQ(LMFIT+6)=1
C
      CALL RRLINK('MFIT_FILL', IMUON)
      CALL RRLINK('MFIT_FILL', IMFIT)
C
  999 RETURN
      END
