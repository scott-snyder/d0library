      SUBROUTINE BKGSAT ( ITRACK, LADDR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book bank GSAT fot track #ITRACK
C-
C-   Inputs  : ITRACK - track number in D0GEANT
C-   Outputs : LADDR - address of the bank GSAT
C-   Controls: 
C-
C-   Created   4-APR-1991   Andrei Kiryunin
C-   Modified  15-JUL-1994  Chip Stewart GZGMUH(0)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGSAT.LINK'
      INTEGER ITRACK,LADDR,LGMUH,MMBK(5)
      INTEGER GZGMUH,LZLAST
C
      INTEGER IPART,NVERT
      REAL    VERT(3),PVERT(4)
      REAL    PARTI
      INTEGER NWBUF
      REAL    UBUF(10)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      LADDR = 0
C
C ****  Initialization
C
      IF ( FIRST ) THEN
        CALL UCTOH ('GSAT',MMBK(1),4,4) ! IDH (bank name)
        MMBK(2)=0                       ! NL (total number of links)
        MMBK(3)=0                       ! NS (number of structural links)
        MMBK(4)=370                     ! ND (number of data worlds)
        MMBK(5)=1                       ! IO descriptor
        FIRST=.FALSE.
      ENDIF
C
C ****  Get address of supporting bank GMUH
C
      LGMUH = GZGMUH(0)
      IF ( LGMUH.EQ.0 ) THEN
        CALL BKGMUH(0,LGMUH)
      ENDIF
C
C ****  Create bank GSAT
C
      LADDR=LZLAST(IXCOM,LQ(LGMUH-IZGSAT))
      IF(LADDR.EQ.0) THEN
         CALL MZLIFT(IXMAIN,LADDR,LGMUH,-IZGSAT,MMBK,0)
C        -- reset bank number to 1...
         IQ(LADDR-5)=1
      ELSE
         CALL MZLIFT(IXMAIN,LADDR,LADDR,0,MMBK,0)
      ENDIF
C
C ****  Pack information about track in first 9 words
C
      IQ(LADDR+1)=ITRACK                ! track number
      CALL GFKINE (ITRACK,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
      IQ(LADDR+2)=IPART                 ! particle type
      CALL UCOPY (VERT(1),Q(LADDR+3),3) ! vector origin of the track
      CALL UCOPY (PVERT(1),Q(LADDR+6),4)        ! 4 momentum components
C                                       ! at the track origin
      IQ(LADDR+10)=0                    ! initial number of hits per track
C
  999 RETURN
      END
