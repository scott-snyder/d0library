      SUBROUTINE DETSAM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize SAMUS drift tubes for hits and
C-                         further digitization. The tubes of one type
C-                         are the separate detector
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  29-SEP-1990   A.Kiryunin
C-   Updated  18-OCT-1990   A.Kiryunin  New set-up of SAMUS detectors
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISET,IDET
C
      INTEGER NBITSV
      CHARACTER*4 NAMESV
C
      INTEGER NBITSH(5)
      CHARACTER*4 NAMESH(5)
      REAL    ORIG(5),FACT(5)
C
      INTEGER NTY,NTYPES
      CHARACTER*4 TUBE_NAME,VOLU_NAME
C
      DATA NBITSV/13/
      DATA NBITSH/3,3,8,32,6/,
     &NAMESH/'SSTA','SSEC','STUB','SDRL','SCPR'/
      DATA ORIG/5*0./, FACT/3*1.,10000.,1./
C----------------------------------------------------------------------
C
      CALL SAGTYP (NTYPES)              ! Define number of tubes' types
C
C ****  Loop over tubes' types ( = detectors )
C
      DO 30 NTY = 1,NTYPES
        CALL SANAME (NTY, TUBE_NAME,VOLU_NAME)
        NAMESV=VOLU_NAME
        CALL GSDET ('SDRF',VOLU_NAME,1,NAMESV,NBITSV,NTY,100,100,
     &  ISET,IDET)
        CALL GSDETH('SDRF',VOLU_NAME,5,NAMESH,NBITSH,ORIG,FACT)
   30 CONTINUE
C
  999 RETURN
      END
