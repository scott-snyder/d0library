C---------------------------------------------------------------
C      COMMON FOR USE WITH GEOTRK  ---   S. KAHN  12 SEPT 1985
C---------------------------------------------------------------
      PARAMETER (NSPACE=10000)
      INTEGER LATTICE,CSYS,NOBJECTS,LATT(NSPACE),LATTCNT,ORIENT
      REAL*8 OBJECTID(NSPACE),DEFAULTID
      LOGICAL IPRINT
      REAL BOX(6,NSPACE),ROT(3,3),DISPLACE(3)
      COMMON/GEOTRK/LATTICE,CSYS,NOBJECTS,LATT,OBJECTID,DEFAULT,BOX
     X   ,LATTCNT,ROT,DISPLACE
      COMMON/PRTFLG/LPRINT
C---------------------------------------------------------------
