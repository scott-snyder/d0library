      LOGICAL FUNCTION MU1_B()
C----------------------------------------------------------------------
C-   Purpose and Methods : Filter for uDST-D0DAD B1M virtual stream
C-   Returned value  : 
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-   Created   5-OCT-1995   Arthur Maciel
C-  Reviewed : 3-SEP-1996   A.M. i) scint requirement removed
C-                              ii) Glob.chi**2 and Etrack "
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'

      INTEGER NPMUOS,LPMUO,GZPMUO,RUNNO
      INTEGER IFW1,IFW2,IFW4,IQUAD,IRGN
      REAL    HFRAC,PTMU
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      IF (FIRST) THEN
        FIRST=.FALSE.
        PRINT *, ' '
        PRINT *,'***** MU1_B() HAS BEEN ACCESSED *****'
        PRINT *, ' '
      ENDIF

C.. Init.
      MU1_B = .FALSE.

C***************
C.. SELECT MUONS
C***************
      NPMUOS=0
      LPMUO=GZPMUO(0)
      DOWHILE (LPMUO.GT.0)
      NPMUOS=NPMUOS+1
      IF(NPMUOS.GT.32) GOTO 999

C *** A-stub rejection
      IFW1=IQ(LQ(LPMUO-2)+4)
      IF(IFW1.EQ.5) GOTO 888

C *** Scintillator
      IFW2 = IQ(LPMUO+44)
***   IF(.NOT.BTEST(IFW2,16)) GOTO 888      ! require scint coverage

C ******* REGION-DEPENDENT STUFF ************
C *** determine which region this track is in
      IRGN = 0
      IQUAD=IQ(LPMUO+7)
      IF(IQUAD.LT.5) THEN
C *** CF
        IRGN=1
      ELSEIF(IQUAD.LT.13) THEN
C *** EF + EF/SAMUS
        IRGN=2
      ELSE
C *** Samus only (reject)
        GOTO 888
      ENDIF

C *** IRGN is set, get quantities of interest

      IFW4  = IQ(LPMUO+9)
      HFRAC=  Q(LPMUO+94)
      PTMU =  Q(LPMUO+14)
CC    CHISQ=  Q(LPMUO+23)
CC    ETRACK= Q(LPMUO+90)
CC    ECAL1=  Q(LPMUO+84)
CC    ETAMU=  ABS(Q(LPMUO+16))

C=-=-=-=-=-=-=
C  ** CF **
C=-=-=-=-=-=-=
      IF(IRGN.EQ.1) THEN
        IF(IFW4.GT.1)        GOTO 888
        IF(PTMU.LT.3.0)      GOTO 888
        IF(HFRAC.LT.0.60)    GOTO 888
***     IF(CHISQ.LT.0)       GOTO 888
        MU1_B = .TRUE.
        RETURN
      ENDIF

C=-=-=-=-=-=-=
C  ** EF **
C=-=-=-=-=-=-=
      IF(IRGN.EQ.2) THEN
        IF(RUNNO().LT.89223)  GOTO 888
        IF(IFW4.GT.1)         GOTO 888
        IF(IFW1.GT.10)        GOTO 888
        IF(PTMU.LT.3.0)       GOTO 888
        IF(HFRAC.LT.0.78)     GOTO 888
        MU1_B = .TRUE.
        RETURN
      ENDIF

  888 LPMUO=LQ(LPMUO)  ! go to next PMUO entry
      ENDDO
  999 RETURN
      END
