      SUBROUTINE SASLB1 (NSLAB,MAGNET_NAME,SIZE,HOLE,
     &                           SLAB_NAME,PAR ,COOR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Using SAMUS magnet geometry construct geometry
C-                         of the slab
C-  *-----------*
C-  |     3     |       ^Y
C-  |---*---*---|       |
C-  | 4 |   | 2 |       |
C-  |---*---*---|  <----*
C-  |     1     |  X
C-  *-----------*
C-
C-   Inputs  : NSLAB - slab number
C-             MAGNET_NAME - name of the magnet
C-             SIZE - half sizes of the magnet
C-             HOLE - half sizes of the magnet hole
C-   Outputs : SLAB_NAME - name of the slab
C-             PAR - half sizes of the slab
C-             COOR - coordinates of slab in the system of the magnet
C-   Controls:
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NSLAB,MAGNET_NAME,SLAB_NAME
      REAL    SIZE(3),HOLE(3), PAR(3),COOR(3)
      CHARACTER*80 MSGSTR               ! Error message
      CHARACTER*4 NAME,BL(4)
      DATA BL/'1','2','3','4'/
C----------------------------------------------------------------------
C
C ****  Define slab name
C
      CALL UHTOC (MAGNET_NAME,4,NAME,4)
      NAME(4:4)=BL(NSLAB)
      CALL UCTOH (NAME,SLAB_NAME,4,4)
C
C ****  Define parameters and corrdinates of the slab
C
      IF (MOD(NSLAB,2).EQ.0) THEN
        PAR(1)=(SIZE(1)-HOLE(1))/2.
        PAR(2)=HOLE(2)
        PAR(3)=SIZE(3)
        IF (NSLAB.EQ.2) THEN
          COOR(1)=-(SIZE(1)+HOLE(1))/2.
        ELSE
          COOR(1)=+(SIZE(1)+HOLE(1))/2.
        ENDIF
        COOR(2)=0.0
        COOR(3)=0.0
      ELSE
        PAR(1)=SIZE(1)
        PAR(2)=(SIZE(2)-HOLE(2))/2.
        PAR(3)=SIZE(3)
        COOR(1)=0.0
        IF (NSLAB.EQ.1) THEN
          COOR(2)=-(SIZE(2)+HOLE(2))/2.
        ELSE
          COOR(2)=+(SIZE(2)+HOLE(2))/2.
        ENDIF
        COOR(3)=0.0
      ENDIF
C
  999 RETURN
      END
