      SUBROUTINE MNMODT( IMOD, ISCN, ICAT, SNUM, SMOD, SSCN )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get adjacent scintillator address
C-
C-   Inputs  : IMOD  : PDT module ID
C-             ISCN  : scintillator ID
C_             ICAT  : Category to scan for adjacent scinti.
C-                     Bit 0 : +Z
C_                     Bit 1 : -Z
C-                     Bit 2 : + in Y' for oct=0 same as Y 
C_                     Bit 3 : - in Y' 
C-   Outputs : SNUM : number of adjacent scinti
C-             SMOD : PDT module ID
C-             SSCN : scinti ID
C-   Controls: 
C-
C-   Created  14-FEB-1994   Atsushi Taketani
C-   Modified  7-NOV-1994   Tim McMahon,  added Oct. 4&7
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IMOD, ISCN, ICAT, SNUM, SMOD(*), SSCN(*)
C
      INTEGER SMODL(24,14) ! MOD_ID*10 + Scint_ID, (Z,other)
      INTEGER ITH, IOCT, MSCN
      INTEGER LMOD, L1, L2, L1SE(8), L2SE(8),L1SW(8),L2SW(8)
      INTEGER L1T, L2T
      LOGICAL FIRST
      INTEGER LOOK(2,3,8), LOOKCONV(0:10), ID, K1
C
      DATA    FIRST/.TRUE./
      DATA    L1SE/0,0,1,1,2,2,3,3/
      DATA    L2SE/0,1,0,1,0,1,0,1/
      DATA    L1SW/3,3,2,2,1,1,0,0/
      DATA    L2SW/1,0,1,0,1,0,1,0/
C
      DATA    LOOKCONV/0,1,2,0,3,5,6,0,4,7,8/
      DATA    LOOK /+1, 0, 0, 0, 0, 0,    ! (+Z,0)
     2              -1, 0, 0, 0, 0, 0,    ! (-Z,0)
     3               0,+1, 0, 0, 0, 0,    ! (0,+Y)
     4               0,-1, 0, 0, 0, 0,    ! (0,-Y)
     5              +1, 0, 0,+1,+1,+1,    ! (+Z,0),(0,+Y),(+Z,+Y)
     6              -1, 0, 0,+1,-1,+1,    ! (-Z,0),(0,+Y),(-Z,+Y) 
     7              +1, 0, 0,-1,+1,-1,    ! (+Z,0),(0,-Y),(+Z,-Y)
     8              -1, 0, 0,-1,-1,-1/    ! (-Z,0),(0,-Y),(-Z,-Y)
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C        CALL VZERO( SMODL, 24*14)
        DO L1=1,24
        DO L2=1,14
          SMODL(L1,L2) = -9999
        END DO
        END DO
C        
        DO ITH=0,4
          L1 = ITH*4 + 2
          IF ( ITH.EQ.4 ) THEN
            L1 = L1 + 2
          ELSE IF ( ITH.GE.1 ) THEN
            L1 = L1 + 1
          END IF
        DO IOCT=0,7
	  IF (IOCT.EQ.5 .OR. IOCT.EQ.6) GOTO 101
          LMOD = 200 + ITH*10 + IOCT
          IF (IOCT.EQ.7) THEN
             L2 = 2
          ELSE
             L2 = IOCT*2 + 4
          ENDIF
        DO MSCN=1,8
          IF (IOCT.EQ.4 .OR. IOCT.EQ.0 .OR. IOCT.EQ.1) THEN
            L1T = L1 + L1SE(MSCN) 
            L2T = L2 + L2SE(MSCN) 
          ELSE
            L1T = L1 + L1SW(MSCN) 
            L2T = L2 + L2SW(MSCN) 
          END IF
          SMODL( L1T,L2T ) = LMOD*10 + MSCN
        END DO
  101   CONTINUE
        END DO
        END DO
      END IF
C
C  same hit
C
      SNUM = 1
      SMOD(SNUM) = IMOD
      SSCN(SNUM) = ISCN
C
C valid category?
C
      ID = LOOKCONV(ICAT)
      IF ( ID.EQ.0 ) GOTO 999
C
C scan 1st address
C
      ITH = (IMOD-200)/10
      IOCT = MOD(IMOD,10)
C
      L1 = ITH*4 + 2
      IF ( ITH.EQ.4 ) THEN
        L1 = L1 + 2
      ELSE IF ( ITH.GE.1 ) THEN
        L1 = L1 + 1
      END IF
      IF (IOCT.EQ.7) THEN
         L2 = 2
      ELSE
         L2 = IOCT*2 + 4
      ENDIF
      IF ( IOCT.EQ.4 .OR. IOCT.EQ.0 .OR. IOCT.EQ.1 ) THEN
        L1T = L1 + L1SE(ISCN) 
        L2T = L2 + L2SE(ISCN) 
      ELSE
        L1T = L1 + L1SW(ISCN) 
        L2T = L2 + L2SW(ISCN) 
      END IF
C
C sacn adjacent scintillator
C
      K1 = 1
C
  200 CONTINUE
C
      L1 = L1T + LOOK(1,K1,ID)
      L2 = L2T + LOOK(2,K1,ID)
      IF ( SMODL(L1,L2).NE.-9999 ) THEN
        SNUM = SNUM + 1
        SMOD(SNUM) = SMODL(L1,L2)/10
        SSCN(SNUM) = MOD(SMODL(L1,L2),10)
      END IF
C
      K1 = K1 + 1
      IF ( K1.GE.4 ) GOTO 999
      IF ( LOOK(1,K1,ID).NE.0.OR.LOOK(2,K1,ID).NE.0 ) GOTO 200
C
  999 RETURN
      END
