      SUBROUTINE OBJECT_GLOBAL(IDX,NMAX,ARRAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return quantities for object GLOBAL. (Contents
C-   of GLOB bank.)
C-
C-   Inputs  : IDX      [I]   Object Number (Unused)
C-             NMAX     [I]   Maximum number of quantities to return
C-   Outputs : ARRAY(*) [R]   Array of quantities
C-
C-----------------------------------------------------------------------
C                   TYPE KEYWORD    DESCRIPTION
C-----------------------------------------------------------------------
C        ARRAY( 1)  I    VERSION    Bank Version Number (=2)
C               2   I    QUALITY    Bit-string indicating event quality
C               3   I    TOTRACK    Total number of tracks in CD
C               4   I    NCEMFH     Number of CC cells E>0.3 EM+FH
C-
C               5   F    CAL_SUMET  Total scalar Et in Calorimeter
C               6   F    EC_SUMET   Total scalar Et in EC
C               7   F    CC_SUMET   Total scalar Et in CC
C               8   F    CAL_SUME   Total energy in Calorimeter
C               9   F    ECS_SUME   Total energy in South EC
C               10  F    CCUP_SUME  Total energy in Upper CC
C               11  F    CCDN_SUME  Total energy in Lower CC
C               12  F    ECN_SUME   Total energy in North EC
C               13  F    MR_SUME    Total energy along Main Ring
C               14  F    MR_SUMET   Total scalar Et along Main Ring
C               15  F    HOT_SUME   Total energy of HOT cells
C               16  F    HOT_VSUMET Total vectorial sum of Et of HOT cells
C               17  F    SPARE1
C               18  F    SPARE2
C               19  F    SPARE3
C               20  F    SPARE4
C-----------------------------------------------------------------------
C-   Controls:
C-
C-   Created  29-APR-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX, NMAX
      REAL    ARRAY(*)
      INTEGER NOBJS, NSIZE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 50 )
      INTEGER IBUFFER(MAXBUF), NN, LGLOB, GZGLOB
      REAL    BUFFER(MAXBUF)
      EQUIVALENCE (IBUFFER(1),BUFFER(1))
C----------------------------------------------------------------------
      INTEGER NTOTAL
      PARAMETER( NTOTAL = 20 )
C----------------------------------------------------------------------
C
C ****  OBJECT: GLOBAL
C
      NN = MIN(NMAX,NTOTAL)
      CALL VZERO(BUFFER,NN)
C
      LGLOB = GZGLOB()
      IF ( LGLOB .GT. 0 ) THEN
        CALL UCOPY(Q(LGLOB+1),  BUFFER(1),NTOTAL)
        CALL UCOPY(BUFFER,ARRAY,NN)
      ENDIF
      RETURN
C
      ENTRY NOBJ_GLOBALS(NOBJS,NSIZE)
C
      LGLOB = GZGLOB()
      IF ( LGLOB .GT. 0 ) THEN
        NOBJS = 1
      ELSE
        NOBJS = 0
      ENDIF
      NSIZE = NTOTAL
C
      RETURN
      END
