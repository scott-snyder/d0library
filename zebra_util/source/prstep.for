      SUBROUTINE PRSTEP(PRUNIT,LSTEPI,NSTEP,CFL,IFL)
C--------------------------------------------------------------
C-
C-  Print out for STEP Bank; Step description in ZEBWRK store
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LSTEP = bank address 
C-  NSTEP = bank number
C-  CFL   = flag to control printout
C-        = 'ONE' for one bank only
C-        = 'ALL' for all banks
C-  IFL   = flag to control printout
C-        = 0 only header
C-        > 0 list of expected channels to be activated
C-
C--------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INCLUDE 'D0$LINKS:IZSTEP.LINK'
      INTEGER PRUNIT,LSTEPI,NSTEP,IFL,I,NHEAD
      INTEGER LZLOC,NDATA
      CHARACTER CFL*(*)
C                                                          
      DATA NHEAD/20/
C
      LSTEP=LSTEPI
      IF(CFL.EQ.'ONE')THEN
        IF(LSTEP.EQ.0)THEN
          IF(NSTEP.EQ.0) GOTO 99  ! Error exit
          LSTEP=LZLOC(IDVWRK,'STEP',NSTEP)
        ENDIF
      ENDIF
C
      IF(CFL.EQ.'ALL') THEN
        LSTEP=LW(LWRKH-IZSTEP)
      ENDIF
C
 1    IF(LSTEP.GT.0) THEN
        WRITE(PRUNIT,101) LSTEP
        IF(IW(LSTEP+1).EQ.1 .OR. IFL.LE.0) THEN
           NDATA=NHEAD
        ELSE
           NDATA=NHEAD+IW(LSTEP+3)
        ENDIF
C 
        WRITE(PRUNIT,102) (IW(LSTEP+I),I=1,NHEAD)
        IF(NDATA.GT.NHEAD) THEN
          WRITE(PRUNIT,103)
          WRITE(PRUNIT,104) (IW(LSTEP+I),I=NHEAD+1,NDATA)
        ENDIF
C
        IF(CFL.NE.'ONE') THEN
          LSTEP=LW(LSTEP)
          GOTO 1
        ENDIF   
      ENDIF
      RETURN
C
  99  WRITE(PRUNIT,100) LSTEP,NSTEP
      RETURN
C
 100  FORMAT(/,' Wrong Address for a STEP bank: LSTEP =',I8
     $,' NSTEP =',I8/)
 101  FORMAT(/,
     $' ================================================='/
     $'       STEP: Summary bank in ZEBWRK area'/
     $'       LSTEP =',I8/
     $' ================================================='/)
 102  FORMAT(
     $' TASKNO      = ',I8,'    ! Task=1 for Pedestals '/
     $' NSTEP       = ',I8,'    ! Step number'/   
     $' MAX ADCS/STP= ',I8/
     $' NCHA        = ',I8,'    ! Number of good ADC channels '/
     $' NBAD        = ',I8,'    ! Number of bad ADC channels  '/
     $' ACT_RUN_NUM = ',I8/
     $' DATE        = ',I8/
     $' TIME        = ',I8/
     $' Crate (oct) = ',O8/
     $' Spare       = ',I8/
     $' Controller  = ',Z8/
     $' PED_SUB     = ',I8/
     $' RENORMAL    = ',I8/
     $' ZER_SUP     = ',I8/
     $' DAT_MOD     = ',I8/
     $' MAX_ADCS    = ',I8/
     $' Pulser Ampl = ',I8/
     $' Ampl sigma  = ',I8/
     $' CONFID      = ',I8,'    ! Laser configuration ID      '/
     $' NADCS       = ',I8,'    ! Number of activated channels'/)
 103  FORMAT(//' List of channels:'//)
 104  FORMAT(10I12)
C
      END
