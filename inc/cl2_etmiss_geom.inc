C----------------------------------------------------------------------
C-    geoETMISSry for level 2 missing Et filling
C-
C-   Created  24-OCT-1992   James T. Linnemann
C-   Updated  07-JAN-1993   Amber Boehnlein, added total e tables
C----------------------------------------------------------------------
      REAL SN(NPHIL,2),CS(NPHIL,2)          
      REAL    ET_CORR(-NETAL:NETAL)         
      REAL    ET_TO_E(-NETAL:NETAL)         
      COMMON /CL2_ETMISS_GEOM/SN,CS,ET_CORR,ET_TO_E
