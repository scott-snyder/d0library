C
      INTEGER NWTMAX
      PARAMETER (NWTMAX=3)
      INTEGER NDISMX
      PARAMETER (NDISMX=20)
      INTEGER NWEDMX
      PARAMETER (NWEDMX=24)
      INTEGER  NFWGMX,NHWGMX,NFDSMX,NHDSMX,NPOSMX
      PARAMETER ( NFDSMX = 12, NFWGMX = 12 )
      PARAMETER ( NHDSMX =  8, NHWGMX = 24 )
      PARAMETER ( NPOSMX = NFDSMX*NFWGMX + NHDSMX*NHWGMX )
      REAL     SDACT(NWTMAX),SDPIT(NWTMAX),ACTWED(4*NWTMAX)
      REAL     SDSTE(NWTMAX)
      REAL     SHWXC(NHDSMX*NHWGMX),SHWYC(NHDSMX*NHWGMX)
      REAL     SHWZC(NHDSMX*NHWGMX),ZPOSHD(NHDSMX)
      REAL     SFWXC(NFDSMX*NFWGMX),SFWYC(NFDSMX*NFWGMX)
      REAL     SFWZC(NFDSMX*NFWGMX),ZPOSFD(NFDSMX)
      REAL     SFLTHK,SHLTHK
      INTEGER  IGEO(5),IELH(NWTMAX),IPSIGN(NWEDMX),IHOFFS
      COMMON /SDPAR / IHOFFS,IGEO,SDACT,SDPIT,ACTWED
     +,               SFLTHK,SHLTHK
C






