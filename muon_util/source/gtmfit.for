      SUBROUTINE GTMFIT(ITRAK,NWHITS,NSHITS,QUAD,FDIM,ORIGT,FIMT,QUF,   
     &        CHARGE,MUOR,MUMAG,DIRCOS,P,ERRP,CHIND,BMEAN,ELOSC,ELOSM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : UNPACK MFIT BANK
C-
C-   Inputs  : ITRAK
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-DEC-1991   A.Klatchko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LMFIT,GZMFIT
      INTEGER ITRAK,NWHITS,NSHITS,QUAD,FDIM,ORIGT,FIMT,QUF,CHARGE
      REAL MUOR(3),MUMAG(3),DIRCOS(3),P(5),ERRP(5),CHIND,BMEAN,         
     &   ELOSC,ELOSM
C----------------------------------------------------------------------
      LMFIT = GZMFIT(ITRAK)
      NWHITS = IQ(LMFIT+2)
      NSHITS = IQ(LMFIT+3)
      QUAD = IQ(LMFIT+4)
      FDIM = IQ(LMFIT+5)
      ORIGT = IQ(LMFIT+6)
      FIMT = IQ(LMFIT+7)
      QUF  = IQ(LMFIT+8)
      CHARGE = IQ(LMFIT+10)
      MUOR(1) = Q(LMFIT+11)
      MUOR(2) = Q(LMFIT+12)
      MUOR(3) = Q(LMFIT+13)
      MUMAG(1) = Q(LMFIT+14)
      MUMAG(2) = Q(LMFIT+15)
      MUMAG(3) = Q(LMFIT+16)
      DIRCOS(1) = Q(LMFIT+17)
      DIRCOS(2) = Q(LMFIT+18)
      DIRCOS(3) = Q(LMFIT+19)
      P(1) = Q(LMFIT+20)
      P(2) = Q(LMFIT+21)
      P(3) = Q(LMFIT+22)
      P(4) = Q(LMFIT+23)
      P(5) = Q(LMFIT+24)
      ERRP(1) = Q(LMFIT+25)
      ERRP(2) = Q(LMFIT+26)
      ERRP(3) = Q(LMFIT+27)
      ERRP(4) = Q(LMFIT+28)
      ERRP(5) = Q(LMFIT+29)
      CHIND =  Q(LMFIT+30)
      BMEAN =  Q(LMFIT+31)
      ELOSC =  Q(LMFIT+32)
      ELOSM =  Q(LMFIT+33)
  999 RETURN
      END
