      FUNCTION TRDFMY(E,MAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-          CALCULATION OF ABSORPTION LENGTH FOR AN X RAY OF ENERGY E
C-          IN MATERIAL 'MAT'
C-
C-   Inputs  :COMPILATION OF X-RAY CROSSSECTION , MCMASTER ET AL
C-            UNI OF CALIF , LIVERMORE  ,  UC-34...
C
C-   Outputs :
C-   Controls:
C-
C-   Created                UNKNOWN BENEFACTOR
C-   Updated  13-SEP-1988   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      CHARACTER * 3 MAT,FOA,RO3,AON
      REAL  XXEN(29),XKRY(33),XARG(33),XOXY(25),XHYD(29)
      REAL XMYL(25),XLIT(53),XBER(13),XCAR(29),XBOR(53)
      REAL XAIR(13),XALU(25),XHEL(53),XRO3(37),XNIT(53),S1,S2
      REAL TRDFMY,TRABSL,E,RXEN,RKRY,RARG,ROXY,RHYD,RAIR,RHEL
      REAL RCO2,RCH4,RISO,RMYL,RALU,RLIT,RBER,RBOR,RCAR,RPAE,RFOA,RRO3
      REAL RNIT
C
C  DENSITIES FOR ALL THE MATERIALS
      DATA RXEN  ,  RKRY    ,   RARG   , RNIT
     +   / .00590  , .003740  , .001784  ,0.00125    /
      DATA ROXY  ,  RHYD    , RAIR
     +   / .001429 , 8.986E-5 , .0012928           /
      DATA RHEL  ,RCO2      ,RCH4   ,RISO
     +   / .000178 ,1.977E-03 ,7.17E-4,2.67E-03  /
      DATA RMYL  ,RALU      ,RLIT
     +   /    1.38 , 2.702    ,.534            /
      DATA RBER  , RBOR
     +   /     1.85 , 2.51                   /
      DATA RCAR  , RPAE
     +   / 1.85  , .92                     /
      DATA RFOA  ,RRO3
     +   /0.0252 ,0.031                  /
C
      DATA FOA,RO3/'FOA','RO3'       /
      DATA AON/'NON'                /
C
      DATA XNIT /52,1.,3395.,1.5,1122.,1.5,1122.,2.,494.1,2.,494.1,
     +          3.,149.1,3.,149.1,4.,62.2,4.,62.2,5.,31.25,5.,31.25,6.,
     +            17.74,6.,17.74,8.,7.258,8.,7.258,10.,3.669,10.,3.669,
     +            15.,1.154,15.,1.154,20.,.5804,20.,.5804,30.,.2958,30.,
     +            .2958,40.,.2246,40.,.2246,50.,.1961 /
      DATA XXEN / 28,1.,11240.,1.142,8124.,1.143,8511.,4.781,260.3,
     1  4.781,739.6,5.1,626.6,5.1,881.,5.452,741.5,5.452,859.,34.582,
     2  5.886,34.582,32.15,100.,1.978,100.,1.978,200.,.3668 /
      DATA XKRY / 32,1.,2854.,1.675,894.4,1.675,3711.,1.726,3447.,
     1  1.726,4851.,1.920,3711.,1.920,4299.,6.,215.5,6.,215.5,
     2  14.322,19.69,14.322,130.6,40.,8.405,40.,8.405,100.,.7083,
     3  100.,.7083,200.,.1862 /
      DATA XARG /32.,1.,3302.,1.5,1124.,1.5,1124.,3.2,141.7,3.2,1384.,
     1  10.,64.23,10.,64.23,30.,2.594,30.,2.594,50.,.6717,50.,.6717,
     2  80.,.2686,80.,.2686,100.,.2008,100.,.2008,200.,.2001 /
      DATA XCAR/28,1.,2199.,2.,307.2,2.,307.2,5.,18.55,5.,18.55,
     1  10.,2.192,10.,2.192,20.,.414,20.,.414,50.,.1858,50.,.1858,
     2  80.,.1609,80.,.1609,200.,.123/
      DATA XOXY /24,1.,4599.,2.,706.2,2.,706.2,8.,11.21,8.,11.21,20.,
     1  .8145,20.,.8145,50.,.211,50.,.211,100.,.1552,100.,.1552,200.,
     2  .123 /
      DATA  XHYD / 28,1.,7.318,2.,1.061,2.,1.061,4.,.464,4.,.464,
     1  6.,.404,6.,.404,10.,.3854,10.,.3854,30.,.357,30.,.357,
     2  80.,.301,80.,.301,200.,.2428 /
      DATA  XALU /24,1.,1178.,1.56,356.4,1.56,4242.,15.,7.89,15.,7.89,
     1  30.,1.11,30.,1.11,60.,.28,60.,.28,100.,.172,100.,.172,200.,.123/
      DATA  XAIR/12,.5,19500.,25.,.35,25.,.35,60.,.14,60.,.14,200.,.14/
      DATA XMYL /24,1.,8200.,3.,175.,3.,175.,5.9,16.51,5.9,16.51,
     +           14.,.95,14.,.95,24.,.38,24.,.38,50.,.22,50.,.22,
     +            200.,.19/
      DATA XLIT / 52,1.,202.7,1.5,62.49,1.5,62.49,2.,26.11,2.,26.11,3.,
     +            7.38,3.,7.38,4.,3.021,4.,3.021,5.,1.556,5.,1.556,6.,
     +            .9419,6.,.9419,8.,.4819,8.,.4819,10.,.327,10.,.327,
     +            15.,.2147,15.,.2147,20.,.185,20.,.185,30.,.1647,30.,
     +            .1647,40.,.1555,40.,.1555,50.,.1492 /
      DATA XHEL /52,1.,65.15,1.5,17.15,1.5,17.15,2.,6.677,2.,6.677,3.,
     +            1.888,3.,1.888,4.,.8696,4.,.8696,5.,.5359,5.,.5359,6.,
     +            .3949,6.,.3949,8.,.2847,8.,.2847,10.,.2439,10.,.2439,
     +            15.,.2089,15.,.2089,20.,.1964,20.,.1964,30.,.1842,30.,
     +            .1842,40.,.1766,40.,.1766,50.,.1705 /
      DATA XBER /12,.5,4068.,13.,0.45,13.,0.45,55.,0.13,55.,0.13,
     +            200.,0.13 /
      DATA XBOR/52,1.,1169.,1.5,372.2,1.5,372.2,2.,159.3,2.,159.3,3.,
     +              46.17,3.,46.17,4.,18.76,4.,18.76,5.,9.285,5.,9.285,
     +               6.,5.238,6.,5.238,8.,2.174,8.,2.174,10.,1.150,10.,
     +              1.150,15.,.4467,15.,.4467,20.,.2856,20.,.2856,30.,
     +              .2017,30.,.2017,40.,.1776,40.,.1776,50.,.1657/
      DATA  XRO3/36,1.,43700.,2.,5300.,2.,5300.,3.,1530.,3.,1530.,
     +             5.,322.,5.,322.,8.,76.,8.,76.,15.5,10.,15.5,10.,
     +             20.,4.7,20.,4.7,30.,1.5,30.,1.5,50.,0.7,50.,0.7,
     +             200.,.5/
C
C
      TRDFMY=0.
      IF (MAT.EQ.'AIR')  THEN
        TRDFMY = TRABSL(E,XAIR,12) * RAIR
      ELSE IF (MAT.EQ.'MYL')  THEN
        TRDFMY=TRABSL(E,XMYL,24)*RMYL
      ELSE IF (MAT.EQ.'ALU') THEN
        TRDFMY = TRABSL(E,XALU,24) * RALU
      ELSE IF (MAT.EQ.'PAE')THEN
        S1 = 6.0 / 7.0 * TRABSL(E,XCAR,28)
        S2 = 1.0 / 7.0 * TRABSL(E,XHYD,28)
        TRDFMY = ( S1 + S2 ) * RPAE
      ELSE IF(MAT.EQ.'FOA')THEN
        S1=(6./7.)*TRABSL(E,XCAR,28)
        S2=(1./7.)*TRABSL(E,XHYD,28)
        TRDFMY=(S1+S2)*RFOA
      ELSE IF(MAT.EQ.'RO3')THEN
        TRDFMY=TRABSL(E,XRO3,36)*RRO3
      ELSE IF (MAT.EQ.'AON')THEN
        TRDFMY = 1.0E36
      ELSE IF (MAT.EQ.'XEN')THEN
        TRDFMY = TRABSL(E,XXEN,28) * RXEN
      ELSE IF(MAT.EQ.'KRY')THEN
        TRDFMY = TRABSL(E,XKRY,32) * RKRY
      ELSE IF (MAT.EQ.'ARG')THEN
        TRDFMY = TRABSL(E,XARG,32) * RARG
      ELSE IF(MAT.EQ.'CAR')THEN
        TRDFMY = TRABSL(E,XCAR,28) * RCAR
      ELSE IF (MAT.EQ.'BER')THEN
        TRDFMY = TRABSL(E,XBER,12) * RBER
      ELSE IF(MAT.EQ.'LIT')THEN
        TRDFMY = TRABSL(E,XLIT,52) * RLIT
      ELSE IF(MAT.EQ.'NIN'.OR.MAT .EQ.'NO ')THEN
        TRDFMY = 0.
      ELSE IF(MAT.EQ.'BOR')THEN
        S1=43./55.*TRABSL(E,XBOR,52)
        S2=12./55.*TRABSL(E,XCAR,28)
        TRDFMY=(S1+S2)*RBOR
      ELSE IF(MAT.EQ.'HEL')THEN
        TRDFMY=TRABSL(E,XHEL,52)*RHEL
      ELSE IF(MAT.EQ.'NIT')THEN
        TRDFMY=TRABSL(E,XNIT,52)*RNIT
C  'EXOTIC' GASES
      ELSE IF(MAT.EQ.'ISO')THEN
        S1=48./58.*TRABSL(E,XCAR,28)
        S2=10./58.*TRABSL(E,XHYD,28)
        TRDFMY=(S1+S2)*RISO
      ELSE IF(MAT.EQ.'CH4')THEN
        S1=12./16.*TRABSL(E,XCAR,28)
        S2=4./16.*TRABSL(E,XHYD,28)
        TRDFMY=(S1+S2)*RCH4
      ELSE IF(MAT.EQ.'CO2')THEN
        S1=12./44.*TRABSL(E,XCAR,28)
        S2=32./44.*TRABSL(E,XOXY,24)
        TRDFMY=(S1+S2)*RCO2
      ELSE
        WRITE(LOUT,*)' IN TRDFMY BAD MAT ',MAT, ' E',E
        CALL EXIT(1)
      ENDIF
C      PRINT *,'TRDFMY',TRDFMY
      END
