      SUBROUTINE ELFIT_SAVE(LPELC,POINT,EPOINT,CLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Save electron trajectory fit results
C-   in banks EFIT. For now use link -3 in CAPH  
C-
C-   Inputs  : LPELC - bank location
C-   Outputs : CLIST - fit parameters and error matrix
C-   Controls: 
C-
C-   Created  12-JUL-1993   Andrzej Zieminski, Daria Zieminska
C-
C=======================================================================
C
C  Bank Name : EFIT
C  Author    : Andrzej Zieminski
C  Date      : 12-JUL-1993
C  Tree description : 
C
C  Bank description : electron global fit results 
C
C     LQ     Q/IQ
C-----------------------------------------------------------------------
C      0          Next   link to EFIT
C     +1          Up     link to CAPH
C     +2          Origin link to CAPH
C.......................................................................
C             -5         Bank number
C             -4         Bank name, 'EFIT'
C             -3         NL = 0
C             -2         NS = 0
C             -1         ND = 50
C              0         Status
C             +1     I       bank version no.(=0)
C              2     I       id(=12)
C              3     F       Ex
C              4     F       Ey
C              5     F       Ez
C              6     F       E
C              7     F       Et
C              8     F       theta
C              9     F       eta
C             10     F       phi
C             11     F       sigE
C             12     F       sig_theta 
C             13     F       sig_phi 
C             14     F       number of degrees of freedom 
C             15     F       chi squared 
C             16     F       Total energy in isolation cone
C             17     F       EM energy in core cone
C             18     F       EM energy in isolation cone
C             19     F       Calorimeter eta of cluster (IETA)
C             20     F       spare
C             21     F       Number of central tracks in cluster road
C             22     F       Distance of closest approach of central track
C             23     F       X of shower center from CLEANEM 
C             24     F       Y of shower center from CLEANEM 
C             25     F       Z of shower center from CLEANEM 
C             26     F       sig(X) of shower center 
C             27     F       sig(Y) of shower center 
C             28     F       sig(Z) of shower center 
C             29     F       spare
C             30     I       spare 
C             31     I       spare
C             32     F       spare
C             33     F       spare
C             34     F       spare
C             35     F       spare
C             36     F       spare
C             37     F       fit residuum: x(y) vertex
C             38     F       fit residuum: z    vertex
C             39     F       fit residuum: x(y)          VTX
C             40     F       fit residuum: dy/dx (dx/dy) VTX
C             41     F       fit residuum: z             VTX
C             42     F       fit residuum: dz/dx (dz/dy) VTX
C             43     F       fit residuum: MSC angle (x-y plane)
C             44     F       fit residuum: MSC angle (z plane) 
C             45     F       fit residuum: x(y)          CDC
C             46     F       fit residuum: dy/dx (dx/dy) CDC
C             47     F       fit residuum: z             CDC
C             48     F       fit residuum: dz/dx (dz/dy) CDC
C             49     F       fit residuum: x(y)          CAL
C             50     F       fit residuum: z             CAL
C
C- above definition of parameters applies to the central region (|zcal|<150),
C- the two cases are x<y (x>y). 
C=======================================================================
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPELC,LEFIT
      REAL POINT(3),EPOINT(3),CLIST(100)
      REAL ECAL,PHI,THE
C----------------------------------------------------------------------
      CALL BKEFIT(LEFIT)
      CALL UCOPY(Q(LPELC+1),Q(LEFIT+1),22)   ! PELC contents to be
                                             ! partially overwritten
      ECAL=Q(LPELC+6)
      THE = CLIST(65)
      PHI = CLIST(66)
      Q(LEFIT+3)=ECAL*SIN(THE)*COS(PHI) 
      Q(LEFIT+4)=ECAL*SIN(THE)*SIN(PHI) 
      Q(LEFIT+5)=ECAL*COS(THE) 
C
      Q(LEFIT+7)=ECAL*SIN(THE) 
      Q(LEFIT+8)=CLIST(65)   
      IF(THE.GT.0) Q(LEFIT+9)= -ALOG(TAN(THE/2.))
      Q(LEFIT+10)=CLIST(66)
C
C-    Errors not yet available
C
      Q(LEFIT+14)=CLIST(2)   
      Q(LEFIT+15)=CLIST(3)
      CALL UCOPY(POINT(1), Q(LEFIT+23),3)     ! calorimeter point 
      CALL UCOPY(EPOINT(1),Q(LEFIT+26),3)    ! cal point errors 
      CALL UCOPY(CLIST(51),Q(LEFIT+37),14)   ! fit residuals
  999 RETURN
      END
