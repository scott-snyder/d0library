C
      PARAMETER (MAXDET=300)
      COMMON/DETNAM/DNAME(MAXDET)
C
      CHARACTER*6 DNAME
C
C    DNAME(I) contains the parameter (from GEOMETRY.INC)
C    that is assigned the number I.
C    For instance, if ICTCC = 3, the DNAME(3) = 'ICTCC '
C
C  DNAME is filled by a BLOCK DATA C$UTI:DNAMES.FOR
C
