      SUBROUTINE GTVHIT(IHIT,WORDS,LAYER,SECTOR,WIRE,JHIT,IENDS,
     &                  ONTRK,ISIDE,IZTRK,XPOS,YPOS,ZPOS,AREA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get information from the compressed hits bank 
C-                         VHIT for IHITth hit
C-
C-   Inputs  : IHIT: hit number (0, -1 are controls, see WORDS description)
C-   Outputs : WORDS: array of 2 words containing the compressed 
C-                    information for this hit
C-                    (if VHIT bank does not exist, WORDS(1)=-1)
C-                    (if IHIT=0, WORDS(1)=total number of hits in VHIT
C-                     and rest of the output arguments have no meaning)
C-                    (if IHIT=-1, WORDS are used as input arguments)
C-             LAYER, SECTOR, WIRE: layer, sector and wire # for this hit
C-             JHIT:  the hit number for this wire 
C-             IENDS: [1:2:3] means hit found on end [0:1:both].
C-             ONTRK: 1: if the hit is on a track VTRK; 0: not
C-             ISIDE: the hit is on which side of the wire 
C-                    (the ISIDE is meaningless if ONTRK=0)
C-             IZTRK: ZTRK id if the hit is on a ZTRK
C-             XPOS(1:2), YPOS(1:2), ZPOS: X, Y and Z positions for this hit.
C-       If a hit is on an track, the hit position is:(xpos(1),ypos(1),zpos).
C-       If a hit is not on an track, the hit and its mirror hit position
C-       are: (xpos(1),ypos(1),zpos) and (xpos(2),ypos(2),zpos).
C-             AREA: pulse area for this hit.
C-
C-   Created  16-JUL-1992  Tom Trippe (patterned after 1-NOV-1991 GTDHIT
C-                                     by Qizhong Li-Demarteau)
C-   Modified 23-SEP_1992  Tom Trippe - fix sign error
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IHIT, WORDS(3)
      INTEGER LAYER, SECTOR, WIRE, JHIT, IENDS, ONTRK, ISIDE, IZTRK
      INTEGER LVHIT, GZVHIT, PLVALS, GZVALS
      INTEGER PLHIT, IPAL
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    DIST_PLUS, DIST_MINUS, YR
C----------------------------------------------------------------------
C
      LVHIT = GZVHIT()
      IF (LVHIT .GT. 5) THEN    
        IF (IHIT .GT. 0) THEN
          PLHIT = LVHIT + 4 + IQ(LVHIT+3) * (IHIT-1) 
          CALL UCOPY(IQ(PLHIT),WORDS,3)
        ELSE
          IF (IHIT .EQ. 0) THEN
            WORDS(1) = IQ(LVHIT+2)
            GOTO 999
          ENDIF
        ENDIF
      ELSE
        WORDS(1) = -1
        GOTO 999
      ENDIF
C
      CALL VFILL(XPOS,2,999.9)
      CALL VFILL(YPOS,2,999.9)
      CALL VHIT_UNPACK(WORDS,LAYER,SECTOR,WIRE,JHIT,IENDS,
     &  ONTRK,ISIDE,IZTRK,DIST_PLUS,DIST_MINUS,ZPOS,AREA)
      PLVALS = GZVALS(LAYER,SECTOR)
      IF (PLVALS .LE. 0) GOTO 999
      IPAL = PLVALS + 6 + IC(PLVALS+6) * WIRE
      IF (ONTRK .EQ. 1) THEN
        IF (ISIDE .EQ. 0) THEN
          YR = DIST_PLUS
        ELSE
          YR = DIST_MINUS
        ENDIF
        XPOS(1) = C(IPAL+1) + YR * C(PLVALS+3)
        YPOS(1) = C(IPAL+2) + YR * C(PLVALS+4)
      ELSE
        XPOS(1) = C(IPAL+1) + DIST_PLUS * C(PLVALS+3)
        YPOS(1) = C(IPAL+2) + DIST_PLUS * C(PLVALS+4)
        XPOS(2) = C(IPAL+1) + DIST_MINUS * C(PLVALS+3)
        YPOS(2) = C(IPAL+2) + DIST_MINUS * C(PLVALS+4)
      ENDIF
C
  999 RETURN
      END
