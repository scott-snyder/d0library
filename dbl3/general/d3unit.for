C----------------------------------------------------------------------
      INTEGER FUNCTION D3UNIT(CHOP,TOPD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Will set or get the unit number for a given
C-    top directory (= dbl3 file identifier)
C-
C-   Returned value  : Unit number, or error code :
C-                      0  - top directory does not exist (option='O' or 'R')
C-                     -1  - top directory already assigned (option='N')
C-                     -2  - no more units in the bag (option='N')
C-                     -3  - top directory name invalid
C-                     -4  - some other error
C-
C-   Inputs  : CHOP   'N'  return a unit number for a new top directory 
C-                         (new file to open)
C-                    'R'  release that unit
C-                         (old file to be closed)
C-                    else return the unit number for old dirctory
C-                         (old, already open file)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-OCT-1992   Lars Rasmussen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INTEGER ICFIND,D3UTOP
C
      CHARACTER *(*) CHOP,TOPD
      INTEGER I,LD,IRET,IU,IUP
      CHARACTER*8 CHLP,DHLP
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
         ND3_TOPD = 0
         FIRST = .FALSE.
      END IF
      D3UNIT = 0
      CHLP = CHOP
      CALL UPCASE(CHLP,CHLP)
C
      LD = D3UTOP(TOPD,DHLP)    ! get top directory, in case TOPD is a path
      CALL UPCASE (DHLP,DHLP)
      CALL DBSBLC(DHLP,DHLP,LD)
      IF (LD .LE. 0) THEN
         D3UNIT = -3
         RETURN
      END IF
C
C- Does passed top directory already exist ?
C
      IU = 0
      DO I = 1,ND3_TOPD
         IF (DHLP .EQ. D3_TOPD(I)) THEN
           IU = D3_DUNI(I)
           IUP = I
         END IF
      END DO
C
C- Get a new unit number 
C
      IF (ICFIND('N',CHLP,1,LEN(CHLP)) .LE. LEN(CHLP)) THEN
C
         IF (ND3_TOPD .GE. MXTOPD) THEN
            D3UNIT = -1
            RETURN
         ELSE IF (IU .GT. 0) THEN
            D3UNIT = -2
            RETURN
         END IF
         CALL GTUNIT (983,IU,IRET)
         IF (IRET .NE. 0) THEN
            D3UNIT = -4
         ELSE 
            ND3_TOPD = ND3_TOPD + 1
            D3_TOPD(ND3_TOPD) = DHLP
            D3_DUNI(ND3_TOPD) = IU
            D3UNIT = IU
         END IF
C
C- Release that unit number
C
      ELSE IF (ICFIND('R',CHLP,1,LEN(CHLP)) .LE. LEN(CHLP)) THEN
C
         IF (IU .LE. 0) THEN
            D3UNIT = 0
            RETURN
         END IF
         CALL RLUNIT (983,IU,IRET)
         D3UNIT = IU
         D3_DUNI(IUP) = 0
         D3_TOPD(IUP) = ' '
         DO I = IUP,ND3_TOPD-1
            D3_DUNI(I) = D3_DUNI(I+1)
            D3_TOPD(I) = D3_TOPD(I+1)
         END DO
         ND3_TOPD = ND3_TOPD - 1
C
C- Return unit number
C
      ELSE 
C
         IF (IU .GT. 0) THEN
            D3UNIT = IU
         ELSE 
            D3UNIT = 0
         END IF
C
      END IF
C
  999 RETURN
      END
