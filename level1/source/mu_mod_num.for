       SUBROUTINE MU_MOD_NUM(NOCT,KODAT,KODIR)
C-----------------------------------------------------------------------
C  Return a list of muon modules associated with a single trigger octant
C  Created 11-90  M. Fortner
C  Module directions corrected for Central region    7-20-91 K. Bazizi
C  Module directions corrected for EF                9-15-91 K. Bazizi
C  New format with OTC order                        11-19-91 M. Fortner
C  Add SAMUS-WAMUS modules                           5-12-92 K. Bazizi
C
C  NOCT is the loctation of the octant, this is the CCT# - 900 + 1
C  KOCT(18) is the list of 18 modules (6 A, 6 B, 6 C) in the octant
C  LDIR(18) is the readout ID for the modules (negative=reversed)
C
C  NOCT:        LOCATION:
C   1-8         Central Octant 0-7
C   9-10        Spare
C  11,13,15,17  North End Octant 0,2,4,6
C  12,14,16,18  South End Octant 0,2,4,6
C  19-20        SAMUS North and South Modules
C  21,23,25,27  SAMUS-WAMUS North End Octant 0,2,4,6
C  22,24,26,28  SAMUS_WAMUS South End Octant 0,2,4,6
C  29-30        Spare
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NOCT,NOCTMX,KODAT(18),KODIR(18)
      INTEGER MODAT(18,30),MODIR(18,30)
      INTEGER I
C
C               Module List
      DATA NOCTMX/30/
      DATA MODAT/
     1      10,20,30,3*0,100,110,120,130,140,0,200,210,220,230,240,0,
     2      11,21,31,3*0,101,111,121,131,141,0,201,211,221,231,241,0,
     3      12,22,32,3*0,102,112,122,132,142,0,202,212,222,232,242,0,
     4      13,23,33,3*0,103,113,123,133,143,0,203,213,223,233,243,0,
     5      13,23,33,3*0,104,114,124,134,144,0,204,214,224,234,244,0,
     6      15,25,35,3*0,105,115,  0,135,145,0,205,215,  0,235,245,0,
     7      16,26,36,3*0,106,116,  0,136,146,0,206,216,  0,236,246,0,
     8      10,20,30,3*0,107,117,127,137,147,0,207,217,227,237,247,0,
     9      18*0,
     &      18*0,
     1 920,67,61,62,64,65,0,150,167,160,161,166,260,261,0,270,271,0,
     2 921,97,91,92,94,95,0,180,197,190,191,196,290,291,0,300,301,0,
     3 922,64,62,61,67,65,0,153,164,163,162,165,263,262,0,273,272,0,
     4 923,94,92,91,97,95,0,183,194,193,192,195,293,292,0,303,302,0,
     5 924,64,65,67,61,62,0,153,163,164,165,162,264,265,0,274,275,0,
     6 925,94,95,97,91,92,0,183,193,194,195,192,294,295,0,304,305,0,
     7 926,67,65,64,62,61,0,150,160,167,166,161,267,266,0,277,276,0,
     8 927,97,95,94,92,91,0,180,190,197,196,191,297,296,0,307,306,0,
     9 404,400,406,402,405,401,410,414,416,412,417,413,
     9                         424,420,426,422,425,421,
     & 430,434,436,432,437,433,444,440,446,442,445,441,
     &                         450,454,456,452,457,453,
C<<
     1 930,       5*0,          932,150,     4*0,  260,261,   4*0,
     2 931,       5*0,          933,180,     4*0,  290,291,   4*0,
     3 930,       5*0,          932,153,     4*0,  263,262,   4*0,
     4 931,       5*0,          933,183,     4*0,  293,292,   4*0,
C<<
     5 930,       5*0,          932,153,165, 3*0,  264,265,   4*0,
     6 931,       5*0,          933,183,195, 3*0,  294,295,   4*0,
     7 930,       5*0,          932,150,166, 3*0,  267,266,   4*0,
     8 931,       5*0,          933,180,196, 3*0,  297,296,   4*0,
C<<
     9      18*0,
     &      18*0/
C<<
C
C               Data for chamber direction and id
      DATA MODIR/
     1        1, 2, 3, 0, 0, 0,-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5, 0,
     2        1, 2, 3, 0, 0, 0,-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5, 0,
     3       -3,-2,-1, 0, 0, 0, 1, 2, 3, 4, 5, 0,-5,-4,-3,-2,-1, 0,
     4       -3,-2,-1, 0, 0, 0, 1, 2, 3, 4, 5, 0,-5,-4,-3,-2,-1, 0,
     5       -3,-2,-1, 0, 0, 0,-5,-4,-3,-2,-1, 0, 1, 2, 3, 4, 5, 0,
     6        1, 2, 3, 0, 0, 0, 4,-3, 0,-2, 1, 0, 1, 2, 0, 3, 4, 0,
     7       -3,-2,-1, 0, 0, 0,-1, 2, 0, 3,-4, 0,-4,-3, 0,-2,-1, 0,
     8        1, 2, 3, 0, 0, 0, 1, 2, 3, 4, 5, 0,-5,-4,-3,-2,-1, 0,
     9        18*0,
     &        18*0,
     1        0,-5, 1, 2, 3, 4, 0, 1, 2,-3, 5, 4, 1,-4, 0,-2, 3, 0,
     2        0, 5,-1, 2, 3, 4, 0,-2, 1, 3,-4, 5,-2, 3, 0, 1,-4, 0,
     3        0, 3,-2, 1, 5, 4, 0,-2, 1, 3,-4, 5,-2, 3, 0, 1,-4, 0,
     4        0,-3, 2, 1, 5, 4, 0, 1, 2,-3, 5, 4, 1,-4, 0,-2, 3, 0,
     5        0, 3,-4, 5, 1, 2, 0,-2, 3,-1, 5, 4, 1,-4, 0,-2,-3, 0,
     6        0,-3, 4, 5, 1, 2, 0, 1, 3, 2,-4, 5,-2, 3, 0, 1, 4, 0,
     7        0,-5,-4, 3, 2, 1, 0, 1, 3, 2,-4, 5,-2, 3, 0, 1, 4, 0,
     8        0, 5, 4, 3, 2, 1, 0,-2, 3,-1, 5, 4, 1,-4, 0,-2,-3, 0,
     9        1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
     &        1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
     1        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0,
     2        0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0,-1, 1, 0, 0, 0, 0,
     3        0, 0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0,-1, 1, 0, 0, 0, 0,
     4        0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1,-1, 0, 0, 0, 0,
     5        0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0, 0, 1,-1, 0, 0, 0, 0,
     6        0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0,-1, 1, 0, 0, 0, 0,
     7        0, 0, 0, 0, 0, 0, 0, 1,-1, 0, 0, 0,-1, 1, 0, 0, 0, 0,
     8        0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 0, 0, 1,-1, 0, 0, 0, 0,
     9        18*0,
     &        18*0/
C
      DO I=1,18
        KODAT(I)=0
        KODIR(I)=0
      ENDDO
      IF (NOCT.GT.0.AND.NOCT.LE.NOCTMX) THEN
        DO I=1,18
          KODAT(I)=MODAT(I,NOCT)
          KODIR(I)=MODIR(I,NOCT)
        ENDDO
      ENDIF
      RETURN
      END