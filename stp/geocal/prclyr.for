      SUBROUTINE PRCLYR(LPRINT, LCLYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRINT 'CLYR' BANK
C-
C-   Inputs  :    LPRINT    PRINT UNIT NUMBER
C-                LCLYR     POINTER TO CLYR BANK
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-FEB-1989   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INTEGER NLINKS, NDATA, I, J, NPAR
      INTEGER LPRINT, LCLYR, JBYT, IETA, IPHI, IDEPTH
C
      NLINKS = IC(LCLYR-3)     ! number of links
      NDATA = IC(LCLYR-1)      ! number of data words
      WRITE (LPRINT, 100) LCLYR, (LC(LCLYR-I),I=1,NLINKS)
  100 FORMAT('0',/,' PRINT OF CLYR BANK -- LQCLYR and links: ',/,
     +   5I10)
      WRITE (LPRINT, 110) IC(LCLYR)
  110 FORMAT(/,' STATUS WORD : ',Z10)
      IETA = JBYT(IC(LCLYR+1),JBIETA,NBIETA)
      IPHI = JBYT(IC(LCLYR+1),JBIPHI,NBIPHI)
      IDEPTH = JBYT(IC(LCLYR+1),JBIDEP,NBIDEP)
      WRITE (LPRINT, 120) IC(LCLYR+1),IETA, IPHI, IDEPTH
  120 FORMAT(/,' CELL IDENT, ETA, PHI, LAYER : ',Z10,3I7)
      WRITE (LPRINT, 130) (C(LCLYR+I), I=2,7)
  130 FORMAT(/,' X,   Y,   Z,   THETA, PHI, OMEGA ',
     + /,6F10.3)
      WRITE (LPRINT, 140) (IC(LCLYR+I), I=8,9)
  140 FORMAT(/,' SHAPE, NPAR : ', A5, I10)
      NPAR = IC(LCLYR+9)
      NPAR = MIN(NPAR, 20)
      WRITE (LPRINT, 150) (C(LCLYR+J+9), J=1,NPAR)
  150 FORMAT(/,' PARAMETERS : ' ,/, (10F10.3,/))
  160 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
