      SUBROUTINE TOP_LEPTONS_NTUPLE_NTPFIL(LEP,JET,PHO,NUT,HT,
     1  SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH,EVTYPE,PAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to combine LEP,JET,NUT into vector PAR
C-                         PAR is one event of NTUPLE
C-
C-   Inputs  : LEP,JET,NUT,EVTYPE,HT
C-   Outputs : PAR
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 14-Nov-1992   HT variable added - JT
C-   Modified 17-Mar-1993   routine name changed for library compatibility
C-   Modified 26-Mar-1993   added event shape parameters - jt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER EVTYPE,I
      REAL LEP(3,14),JET(0:5,8),NUT(3,8),PHO(0:3,8),PAR(150)
      REAL HT,SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH
C----------------------------------------------------------------------

      PAR(1)=FLOAT(EVTYPE)
C                           ----- put top 3 leps into ntup
      DO I=2,15
        PAR(I)=LEP(1,I-1)
        PAR(I+14)=LEP(2,I-1)
        PAR(I+28)=LEP(3,I-1)
      ENDDO
C
C                           ----- top 5 jets, nuts, & phos in ntup
      DO I=2,9
        PAR(I+42)=JET(1,I-1)
        PAR(I+50)=JET(2,I-1)
        PAR(I+58)=JET(3,I-1)
        PAR(I+66)=JET(4,I-1)
        PAR(I+74)=JET(5,I-1)
        PAR(I+82)=NUT(1,I-1)
        PAR(I+90)=NUT(2,I-1)
        PAR(I+98)=NUT(3,I-1)
        PAR(I+106)=PHO(1,I-1)
        PAR(I+114)=PHO(2,I-1)
        PAR(I+122)=PHO(3,I-1)
      ENDDO
      PAR(132)=HT
      PAR(133)=SPHER
      PAR(134)=PLAN
      PAR(135)=GSPHER
      PAR(136)=GAPLAN
      PAR(137)=GY
      PAR(138)=EMAXSH
      PAR(139)=ETMAXSH
      PAR(140)=EFOURSH
C
  999 RETURN
      END
