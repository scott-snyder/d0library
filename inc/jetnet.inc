C----------------------------------------------------------------------
C-
C-   Created  25-JAN-1991   Harrison B. Prosper
C-   Modified  5-Feb-1991   Pushpa Bhat
C-   Updated  15-JUN-1992   Harrison B. Prosper
C-    Added NTUPLE_ID
C-   Updated  21-AUG-1992   Harrison B. Prosper
C-    Change dimensions
C-   Updated   8-FEB-1993   Pushpa C. Bhat
C-   Updated  31-DEC-1993   Chip Stewart
C-   Updated   7-MAR-1995   Harrison B. Prosper
C-    Implement event weighting
C----------------------------------------------------------------------
      INTEGER NINPUTS,NOUTPUTS,NETID,MAXFIL,MAXIN,MAXOUT,MAXPAT,IDNEXT
      INTEGER NPATTERNS,NTUPLE_ID,IUNIT,NLABI,NLABO,NTAG, NFIELDS,OUNIT
      REAL NN_OUTPUT
C----------------------------------------------------------------------
      PARAMETER( NETID  = 77)
      PARAMETER( MAXFIL = 20)
      PARAMETER( MAXIN  = 50)           
      PARAMETER( MAXOUT = 10)           
      PARAMETER( MAXPAT = 20000)        
C----------------------------------------------------------------------
      INTEGER  NRUN(MAXPAT),NEVENT(MAXPAT),PATTERN_SELECT(MAXIN)
      INTEGER PATTERN_INDEX(MAXPAT,MAXOUT)
      REAL    PATTERN_IN(MAXIN,MAXPAT),PATTERN_OUT(MAXOUT,MAXPAT)
      REAL    PATTERN_WT(MAXPAT), PATTERN_JNOUT(MAXPAT,MAXOUT)
C----------------------------------------------------------------------
      COMMON /JETNET/ PATTERN_IN,PATTERN_OUT,NINPUTS,NOUTPUTS,NPATTERNS,
     &  NTUPLE_ID,NN_OUTPUT,NRUN,NEVENT,IUNIT,NLABI,NTAG,NLABO,NFIELDS,
     &  PATTERN_WT,PATTERN_SELECT,OUNIT,IDNEXT,
     &  PATTERN_INDEX,PATTERN_JNOUT
C
      CHARACTER LABI(MAXIN)*80,TAGS(MAXIN)*80,LABO(MAXOUT)*80,
     &  FIELDS(MAXIN)*80, TOPDIR, HDIR_SAVE
      COMMON /JETNETC/ LABI,TAGS,LABO,FIELDS,TOPDIR, HDIR_SAVE
