C===============================================================
C
C	Common MATTER describes the various types of matter present
c	in the D0 detector.  The values are filled in BLOCK DATA
C	MATTER.  
C
C	S. KAHN		28-FEB-1985
C
C===============================================================
      INTEGER MAXMAT
      PARAMETER (MAXMAT = 35)
      REAL
     1     RADLEN(MAXMAT),	
     2     COLLEN(MAXMAT),	
     3     INTLEN(MAXMAT),	
     4     DEDX(MAXMAT),	
     5     DENSIT(MAXMAT),	
     6     ATMNUM(MAXMAT),	
     7     ATMASS(MAXMAT),      
     8     POTION(MAXMAT),      
     9     CRITEN(MAXMAT),      
     X     RADMOL(MAXMAT)       
c
      COMMON /MATTER/ RADLEN,COLLEN,INTLEN,DEDX,DENSIT,ATMNUM
     1,    ATMASS,POTION,CRITEN,RADMOL
      REAL   ABSLEN(MAXMAT)	
      EQUIVALENCE (ABSLEN(1),INTLEN(1))
C
      COMMON /MATNAM/ MNAME(MAXMAT)
      CHARACTER*20 MNAME
C===============================================================
