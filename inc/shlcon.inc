C----------------------------------------------------------------------
C-   SHLCON.INC    Constants for Shower Library
C-
C-   Created  21-FEB-1989   John Womersley
C-   Updated  29-DEC-1989   Rajendran Raja   . ADDED NUMBERS OF KEYS.
C-   Updated  23-JULY-1992   W.G.D.Dharmaratna, cleaned the common block  
C----------------------------------------------------------------------

      INTEGER SPACE
      PARAMETER( SPACE = 100 )          
      INTEGER NKEY
      PARAMETER (NKEY=5)
C       KEY(1) = VERTEX
C       KEY(2) = RAPIDITY
C       KEY(3) = MOMENTUM
C       KEY(4) = PARTICLE ID
C       KEY(5) = PHI TYPE
C
      INTEGER NMOM,NPART_ID,NPHI_ID,NETA,NVRT,NDATA_CYCLES
C
      PARAMETER( NVRT = 6 )
      PARAMETER( NETA = 37 )
      PARAMETER( NMOM = 7 )
      PARAMETER( NPART_ID = 3 )         
      PARAMETER( NPHI_ID = 2 )          
      PARAMETER( NDATA_CYCLES = NVRT*NETA*NMOM*NPART_ID*NPHI_ID )
      INTEGER NCYCLE_MAX
      PARAMETER( NCYCLE_MAX =999999  )  
C
      COMMON /SHLCON/ ISUNIT,ISUNIT_AUX,IUSER,ISEED,
     &  MOMBIN(SPACE),NMBIN,
     &  NEBIN,VTXBIN(SPACE),NVBIN,KEY(NKEY),
     &  CYCLES(NDATA_CYCLES),
     &  NCYCLES,NCYCLES_CUR,NFILE
C
      INTEGER KEY
      INTEGER ISUNIT,ISUNIT_AUX,IUSER
      INTEGER ISEED                     
      INTEGER NMBIN,NEBIN,NVBIN
      INTEGER CYCLES
      INTEGER NCYCLES,NCYCLES_CUR
      INTEGER NFILE
C
      REAL    MOMBIN,VTXBIN
C
C
      CHARACTER CHTAG(NKEY)*3
      DATA CHTAG/'VTX','ETA','MOM','PRT','PHI'/
      CHARACTER PARTAG(NPART_ID)*3      
      DATA PARTAG/'E.M','MUO','HAD'/
