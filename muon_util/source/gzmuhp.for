C VAX/DEC CMS REPLACEMENT HISTORY, Element GZMUHP.FOR
C *2     5-NOV-1993 14:35:54 FORTNER "use links release"
C *1    15-SEP-1993 17:25:00 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element GZMUHP.FOR
      INTEGER FUNCTION GZMUHP(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :     Find pointer to MUHP
C-
C-   Created  27-AUG-1993   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUHP.LINK'
      INTEGER I,LMUHT,GZMUHT
C
      GZMUHP=0
      LMUHT=GZMUHT(0)                   
      IF(LMUHT.NE.0) GZMUHP=LQ(LMUHT-IZMUHP)
C
      RETURN
      END
