      BLOCK DATA TRDBLK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BLOCK DATA FOR THE TRD
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-DEC-1986   A. ZYLBERSTEJN
C-   Updated  19-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN   
C-   Updated  18-JUN-1993   J.P. Cussonneau  : New gains for chambers (Uranium) 
C-                                             Change DELOE in a way
C-                                             to keep old DE/E value.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FADCCN.INC/LIST'
      INCLUDE 'D0$INC:FADDIG.INC/LIST'
      INCLUDE 'D0$INC:INFO.INC/LIST'
      INCLUDE 'D0$INC:NORTRD.INC/LIST'
      INCLUDE 'D0$INC:PROBDE.INC/LIST'
      INCLUDE 'D0$INC:RADDSC.INC/LIST'
      INCLUDE 'D0$INC:TECDSC.INC/LIST'
      INCLUDE 'D0$INC:WINDSC.INC/LIST'
      INCLUDE 'D0$INC:RADDCC.INC/LIST'
      INCLUDE 'D0$INC:TECDCC.INC/LIST'
      INCLUDE 'D0$INC:WINDCC.INC/LIST'
      INCLUDE 'D0$INC:XRAY.INC/LIST'
C
      DATA DELINF,DELSUP/1,37/
      DATA NBIT,OFSFAD,TCLOCK/8,100.,10./
      DATA NDATW/4/
      DATA NOPUN/1/
      DATA NSTEP,XSTEP,EDOWN/200,.15,1./
      DATA ECLMIN /.01/ !CLUSTER MINIMAL ENERGY (IN KEV)
      DATA PED/5./,NOISE/1./
C   TRD PHYSICAL DESCRIPTION
      DATA NFOIL,FOIL,XFOIL  , GAP ,XGAP,DXFOIL,DXGAP
     +/382,'PAE',18.E-04,'NIT',180.E-04 ,0.   ,0.             /
      DATA SKIN ,XSKIN  ,SAND ,XSAND, MET ,XMETAL, STUFF,XSTUFF
     +/   'MYL',50.E-04,'CO2',0.2   ,'ALU', 0.6E-04,'NO ',0.   /
      DATA   GAS ,XGAS,PERC
     +/     'XEN',2.3 ,.90 /
      DATA XDER,XAMP/1.5,.8/
C  INTEGRATED DISTRIBUTION FUNCTIONS FOR DELTA RAYS (FROM ERMILOVA ET AL)
      DATA FPS/1.E05,9.E04,8.E04,7.E04,6.E04,5.E04,4.E04,3.E04,2.E04,
     +1.E04,9.E03,8.E03,7.E03,6.E03,5.E03,4.E03,3.E03,2.E03,
     +1.E03,9.E02,8.E02,7.E02,6.E02,5.E02,4.E02,3.E02,2.E02,
     +1.E02,9.E01,8.E01,7.E01,6.E01,5.E01,4.E01,3.E01,2.E01,
     +11.25                                                /
C  PROBABILITE ON THE FERMI PLATEAU
      DATA PROBF/3.760E-03,3.859E-03,4.628E-03,5.509E-03,6.487E-03,
     +8.632E-03,1.179E-02,1.610E-02,2.439E-02,5.600E-02,
     +6.715E-02,7.848E-02,9.660E-02,1.252E-01,1.802E-01,
     +2.105E-01,2.801E-01,4.709E-01,1.515,1.771,2.296,2.826,
     +3.218    ,3.570    ,3.960    ,4.628,6.487,12.10,16.52,
     +19.31,   21.98     ,22.56    ,22.56,22.80,25.69,34.19,
     +65.32                                                /
C  PROB. FOR MIN. IONISING PARTICLES
      DATA PROBM/4.24E-03,4.89E-03,5.36E-03,6.34E-03,7.45E-03,8.75E-03,
     +1.11E-02,1.54E-02,2.27E-02,4.64E-02,5.22E-02,6.31E-02,
     +7.55E-02,8.80E-02,1.12E-01,1.43E-01,2.05E-01,3.26E-01,
     +8.80E-01,1.05    ,1.25    ,1.69    ,1.87    ,2.01    ,
     +2.31    ,2.74    ,3.98    ,10.0    ,11.96   ,12.92   ,
     +13.95   ,14.68   ,15.06   ,15.45   ,16.68   ,23.8    ,
     +48.13                                                 /
C
      DATA XA10,XD10/23.E-04,20.E-04/
C
      DATA ATTACH/.05/
C -- DELTA(E)/E FOR THE REFERENCE ENERGY
      DATA DELOE/.0428/  
C -- SURCAL (sum of fadc counts for Ur X ray (29. kev)) 
      DATA SURCAL/765.3,1177.5,961.1/ 
C -- EREF (kev)
      DATA EREF/29./
C
      DATA FACT,ORIG/100.,10./,NWORDF/4/,NBITF/10/
C
      END
