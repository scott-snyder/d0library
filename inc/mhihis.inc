      INTEGER NHMAX
      PARAMETER (NHMAX=200)
C
      INTEGER NMIHIS         
      INTEGER MHMUD1(NHMAX)  
                             
      INTEGER IDMHIS(NHMAX)  ! ID's of interactively defined histograms
      INTEGER MHWADD(NHMAX)
      CHARACTER*24 TIMHIS(NHMAX)
                             ! TITLE's of interactively defined histograms


C
      COMMON / MHIHIS / NMIHIS,MHMUD1,IDMHIS,MHWADD,TIMHIS
C
 
