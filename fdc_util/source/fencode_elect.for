      SUBROUTINE FENCODE_ELECT( HALF,UNIT,QUAD,SECTOR,WIRE,
     &                          CRATE,CARD,CHANNEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given HALF,UNIT,QUAD,SECTOR,WIRE, return
C-                      FADC crate number and electronics channel.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE
C-   Outputs : CRATE,CARD,CHANNEL
C-
C-   Created  19-SEP-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Input:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
C  Output:
      INTEGER CRATE,CARD,CHANNEL
C  Local:
      INTEGER CRATE_PHI(0:3,0:1)
      INTEGER CRATE_THETA(0:7,0:1) 

      INTEGER CARD_PHI(0:35)
      INTEGER CARD_THETA(0:3,0:7,0:1) 
      INTEGER CHANNEL_THETA(0:7,0:5) 
      INTEGER CHANNEL_THETA_DL(0:7,8:9)

      DATA CRATE_PHI    / 55, 35, 25, 45,
     &                   105, 85, 95,115 / 
      DATA CRATE_THETA  / 15,  5,  5, 15, 15,  5,  5, 15,
     &                    75, 65, 65, 75, 75, 75, 65, 65 /
      DATA CARD_PHI     / 15, 14, 13, 12, 11, 10,  9,  8,  7,
     &                     7,  8,  9, 10, 11, 12, 13, 14, 15, 
     &                     7,  8,  9, 10, 11, 12, 13, 14, 15, 
     &                    15, 14, 13, 12, 11, 10,  9,  8,  7/
      DATA CARD_THETA   /  0, 1, 2, 15, 2, 1, 0, 15,
     &                     5, 4, 3, 14, 3, 4, 5, 14,
     &                     8, 7, 6, 13, 6, 7, 8, 13,
     &                     9,10,11, 12,11,10, 9, 12,
     &                     2, 1, 0, 15, 0, 1, 2, 15,
     &                     3, 4, 5, 14, 5, 4, 3, 14,
     &                     9,10,11, 12, 6, 7, 8, 13,
     &                     8, 7, 6, 13,11,10, 9, 12 /
      DATA CHANNEL_THETA / 15,14,13,12,11,10, 9, 8, 
     &                      0, 1, 2, 3, 4, 5, 6, 7,
     &                     15,14,13,12,11,10, 9, 8,  
     &                      0, 1, 7, 2, 6, 3, 5, 4,
     &                      8, 9,15,10,14,11,13,12,
     &                      0, 1, 7, 2, 6, 3, 5, 4/
      DATA CHANNEL_THETA_DL / 6, 5, 4, 3, 2, 1, 0, 7,
     &                        9,10,11,12,13,14,15, 8 /
C----------------------------------------------------------------------
      IF ( UNIT .EQ. 0  ) THEN
        CRATE = CRATE_THETA(QUAD,HALF)
      ELSE
        CRATE = CRATE_PHI(SECTOR/9,HALF)
      ENDIF
      IF ( UNIT.EQ.0 ) THEN
        IF ( WIRE.LE.7 ) THEN
          CARD = CARD_THETA(SECTOR/2,QUAD,HALF)
          CHANNEL = CHANNEL_THETA(WIRE,SECTOR)
        ELSE
          CARD = CARD_THETA(3,QUAD,HALF)
          CHANNEL = CHANNEL_THETA_DL(SECTOR,WIRE)
        ENDIF
      ELSE
        CARD = CARD_PHI(SECTOR)
        CHANNEL = WIRE
      ENDIF
  999 RETURN
      END
