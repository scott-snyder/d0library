      SUBROUTINE PRCLAY(LPRINT, LCLAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT 'CLAY' BANK
C-
C-   Inputs  :    LPRINT    PRINT UNIT NUMBER
C-                LCLAY     POINTER TO CLAY BANK
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-FEB-1989   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLAY.DEF'
      INTEGER NLINKS, NDATA, I, J, NETPH
      INTEGER LPRINT, LCLAY
C
      NLINKS = IC(LCLAY-3)     ! number of links
      NDATA = IC(LCLAY-1)      ! number of data words
      WRITE (LPRINT, 100) LCLAY, (LC(LCLAY-I),I=1,NLINKS)
  100 FORMAT('0',/,' PRINT OF CLAY BANK -- LQCLAY and links: ',/,
     +   5I10)
      WRITE (LPRINT, 110) IC(LCLAY)
  110 FORMAT(/,' STATUS WORD : ',Z10)
      WRITE (LPRINT, 120) (IC(LCLAY+I), I=1,6)
  120 FORMAT(/,' IDENT, MATER ,#ZONES, #PLATES, ICOOR, PERP_COOR: ',
     + /,6I7)
      WRITE (LPRINT, 130) (C(LCLAY+I), I=7,15)
  130 FORMAT(/,'    RCEN,    PHIC,    ZCEN,    THTE,    PHIE,    OMGE, 
     +  DELR,   #RADL,   #ABSL :',/,9F8.3)
      NETPH = MIN(IC(LCLAY+ILETPH), 4)
      DO 150 J = 1, NETPH
      WRITE (LPRINT, 140) (IC(LCLAY+I+(J-1)*NWZONE+ILNETA-1),I=1,2),
     +   (C(LCLAY+I+(J-1)*NWZONE+ILNETA-1),I=3,6)
  140 FORMAT(/,' NETA, NPHI, DEL_ETA,   ETA0,   DEL_PHI,   PHI0 :',/,
     + 2I5,4F8.3)
  150 CONTINUE
      WRITE (LPRINT, 160) 
  160 FORMAT(/,' R/Z BOUNDARIES : ')
      NDATA = NDATA-NWZONE*(NETPH-1)
      WRITE (LPRINT, 170) (C(LCLAY+I+(NETPH-1)*NWZONE),I=ILZPD0,NDATA)
  170 FORMAT(2X,8F10.3)
C----------------------------------------------------------------------
  999 RETURN
      END
