      SUBROUTINE GTDHIT(IHIT,WORDS,LAYER,SECTOR,WIRE,JHIT,ONTRK,ISIDE,
     &                  IZTRK,XPOS,YPOS,ZPOS,AREA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get information from the compressed hits bank 
C-                         DHIT for IHITth hit
C-
C-   Inputs  : IHIT: hit number 
C-   Outputs : WORDS: array of 2 words containing the compressed 
C-                    information for this hit
C-                    (if DHIT bank does not exist, WORDS(1)=-1)
C-                    (if IHIT=0, WORDS(1)=total number of hits in DHIT
C-                     and rest of the output arguments have no meaning)
C-                    (if IHIT=-1, WORDS are used as input arguments)
C-             LAYER, SECTOR, WIRE: layer, sector and wire # for this hit
C-             JHIT:  the hit number for this wire 
C-             ONTRK: 1: if the hit is on a track DTRK; 0: not
C-             ISIDE: the hit is on which side of the wire 
C-                    (the ISIDE is meaningless if ONTRK=0)
C-             IZTRK: ZTRK id if the hit is on a ZTRK
C-             XPOS(1:2), YPOS(1:2), ZPOS: X, Y and Z positions for this hit
C-                   (if the hit is on an inner wire, ZPOS = 999.9)
C-       if a hit is on an track, the hit position is:(xpos(1),ypos(1),zpos)
C-       if a hit is not on an track, the hit and its mirror hit position 
C-       are: (xpos(1),ypos(1),zpos) and (xpos(2),ypos(2),zpos)
C-             AREA: pulse area for this hit (if the hit is on a outer wire,
C-                   the AREA=0.0)
C-
C-   Created   1-NOV-1991   Qizhong Li-Demarteau
C-   Modified 12-APR-1994   C. Klopfenstein - fetch area for outer sense
C-                          wire hit if available (new DHIT version).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER IHIT, WORDS(3)
      INTEGER LAYER, SECTOR, WIRE, JHIT, ISIDE, ONTRK, IZTRK
      INTEGER LDHIT, GZDHIT, PLDTMW, GZDTMW, PLDALS, GZDALS
      INTEGER PLHIT, IPTM, IPAL
      REAL    XPOS(2), YPOS(2), ZPOS, AREA
      REAL    TIME, YR
      integer words_per_hit
C----------------------------------------------------------------------
C
      LDHIT = GZDHIT()
      IF (LDHIT .GT. 5) THEN
        words_per_hit = IQ(LDHIT + 3)
        IF (IHIT .GT. 0) THEN
          PLHIT = LDHIT + 4 + IQ(LDHIT+3) * (IHIT-1) 
          CALL UCOPY(IQ(PLHIT),WORDS,2)
        ELSE
          IF (IHIT .EQ. 0) THEN
            WORDS(1) = IQ(LDHIT+2)
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
      AREA = 0.0
      CALL DHIT_UNPACK(WORDS,LAYER,SECTOR,WIRE,JHIT,ONTRK,ISIDE,
     &  IZTRK,TIME,ZPOS,AREA,words_per_hit)
      PLDTMW = GZDTMW(LAYER)
      PLDALS = GZDALS(LAYER,SECTOR)
      IF (PLDALS .LE. 0 .OR. PLDTMW .LE. 0) GOTO 999
      IPTM = PLDTMW + (SECTOR*IC(PLDTMW+4)+WIRE)*IC(PLDTMW+3) + 4
      IPAL = PLDALS + 6 + IC(PLDALS+6) * WIRE
      IF (ONTRK .EQ. 1) THEN
        YR = C(IPTM+2) * TIME * (-1)**ISIDE
        XPOS(1) = C(IPAL+1) + YR * C(PLDALS+3)
        YPOS(1) = C(IPAL+2) + YR * C(PLDALS+4)
      ELSE
        YR = C(IPTM+2) * TIME 
        XPOS(1) = C(IPAL+1) + YR * C(PLDALS+3)
        YPOS(1) = C(IPAL+2) + YR * C(PLDALS+4)
        XPOS(2) = C(IPAL+1) - YR * C(PLDALS+3)
        YPOS(2) = C(IPAL+2) - YR * C(PLDALS+4)
      ENDIF
C
  999 RETURN
      END
