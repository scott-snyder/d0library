C   -------COMMON,ABSOR:TABLES OF 1./ABSORBTION LENGTHS FOR ALL MATERIALS
C           FOR PHOTONS WITH ENERGIES GIVEN IN EABS
      INTEGER LENGAB
      PARAMETER (LENGAB=201) 
      COMMON/ABSOR/ NABS,EABS(LENGAB)  ,ABSKI(LENGAB),ABMET(LENGAB),
     +                   ABSTUF(LENGAB),ABSAN(LENGAB),ABGAS(LENGAB),
     +                   ABFOIL(LENGAB),ABGAP(LENGAB),ABAIR(LENGAB)
      INTEGER       NABS
      REAL               EABS           ,ABSKI       ,ABMET        ,
     +                   ABSTUF         ,ABSAN       ,ABGAS        ,
     +                   ABFOIL         ,ABGAP       ,ABAIR
