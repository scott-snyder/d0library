      SUBROUTINE TOP_LEPTONS_NTUPLE_LEPAN(NPAR,PAR,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To construct vector PAR (one event in NTUPLE)
C-
C-   Inputs  : None
C-   Outputs : vector PAR  and its length NPAR
C-   Controls: 
C-
C-   created  2-AUG-1991   Jim Cochran
C-   modified 1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Updated  20-SEP-1992   Meenakshi Narain   modfiied output params
C-   Modified 18-Sep-1992   Call to Jetfin modified - electron array
C-                          removed. SJW
C-   Modified 16-Nov-1992   HT Variable added - JT
C-   Modified  4-Dec-1992   Bug in Jetfin call fixed
C-   Modified 17-Mar-1993   Routine name changes for library compatibility
C-   Modified 26-Aug-1993   Added event shape parameters - jt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER EVTYPE,I,J,NPAR
      REAL  LEP(3,14),JET(0:5,8),NUT(3,8),ELE(0:3,14)
      REAL  MUO(0:3,14),PHO(0:3,8),PAR(150),MET_VEC(3)
      REAL  HT,SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH
C----------------------------------------------------------------------
C
C *** Parton loading routines 
C ***  a.) electrons
C
      CALL TOP_LEPTONS_NTUPLE_ELEFIN(ELE)
C
C ***  b.) jets
C
      CALL TOP_LEPTONS_NTUPLE_JETFIN(JET,HT,SPHER,PLAN,GSPHER,GAPLAN,GY,
     1                               EMAXSH,ETMAXSH,EFOURSH)
C
C ***  c.) photons
C
      CALL TOP_LEPTONS_NTUPLE_PHOFIN(PHO)
C
C ***  d.) missing et
C
      CALL TOP_LEPTONS_NTUPLE_NUTFIN(NUT,MET_VEC)
C
C ***  e.) muons
C
      CALL TOP_LEPTONS_NTUPLE_MUOFIN(MUO)
C
      DO I=1,3                              ! Diagnostic
        DO J=1,14
          LEP(I,J) = -9.                
        ENDDO
      ENDDO
C
      EVTYPE = 0 
C
C *** Classify event type
C
      CALL TOP_LEPTONS_NTUPLE_LTAG(ELE,MUO,LEP,EVTYPE) 
C                                           ! 1 => ee, 2 => mumu, 3 => emu
C                                           ! & tag vectors accordingly
C *** fill ntuple
C
      CALL TOP_LEPTONS_NTUPLE_NTPFIL(LEP,JET,PHO,NUT,HT,SPHER,PLAN,
     &  GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH,EVTYPE,PAR)  
C
      NPAR = 140
      RETURN
C
      END
