      SUBROUTINE MUMTRG(IGO,IMOD,IREG,ITRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check trigger bank MTRG to see if a 
C-       particular module was in a trigger, also fills trigger info
C-       into MUHM bank.
C-
C-   Inputs :  IGO   = Input trigger condition (
C-             IMOD  = module id
C-
C-   Outputs : IREG  = trigger region (CF=1, EF=2, OV=3, SA=4)
C-             ITRG  = trigger level found
C-                     0 = none
C-                     1 = level 1
C-                     2 = level 1.5 low pt
C-                     3 = level 1.5 high pt
C-
C-   Created : 22-OCT-1993   M. Fortner
C-
C-   Modified: 2/94 MF - add trigger tag
C-             7/94 MF - require level 1 with level 1.5 triggers
C-             5/95 MF - Add IGO, IREG parameters, use bit 1 for CF
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IGO,IMOD,IREG,ITRG
      INTEGER IOCT,ITRIG,MCRS,MFINE(4)
      INTEGER GZMUHM
      EXTERNAL GZMUHM
C
C         Get trigger region
C
      CALL MUMREG(IMOD,IREG,IOCT)
C
C         Get trigger tag
C
      IF (IGO.LT.0) THEN
        IF (GZMUHM(IMOD).EQ.0) RETURN
        CALL MOTMOD(5,IMOD,ITRIG)
        CALL MUHMFL(6,IMOD,ITRIG,MCRS,MFINE)
      ELSE
        ITRIG = IGO
      END IF
C
C         Set trigger level
C
      ITRG = 0
      IF (BTEST(ITRIG,0)) ITRG=1
      IF (BTEST(ITRIG,1).AND.ITRG.GT.0.AND.IREG.EQ.1) ITRG=2
      IF (BTEST(ITRIG,2).AND.ITRG.GT.0) ITRG=2
      IF (BTEST(ITRIG,3).AND.ITRG.GT.0) ITRG=3
C
      RETURN
      END
