      SUBROUTINE CSCN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates Banks to describe the ICD 
C-       Ring Geometry.  It is patterned after Z. Wolf's design
C-       of the geometry.  Variables are picked up from SRCP
C-       geometry arrays.
C-
C-   Inputs  :       None
C-   Outputs :       None
C-   Controls: 
C-   Zebra Banks Created:   CSCN, CSHA, CLYR(temp)
C-
C-   Created   2-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:SCPARR.INC'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCSCN.LINK'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
C
      INTEGER  MCSCN(5), MCSHA(5), LZFIND, ICDMIN, ICDMAX, ICD
      INTEGER  IZCLYR, MCLYR(5), MCLAY(5), MCLNK(5),IOCLYR, IMICD, IER
      INTEGER IOCSCN
      CHARACTER ICD_ETA*11, CD*2
C
      CHARACTER*4 CHAR41,CHAR42,CHAR43,CHAR44,CHAR45
      EQUIVALENCE (CHAR41,MCSCN(1))
      EQUIVALENCE (CHAR42,MCSHA(1)) 
      EQUIVALENCE (CHAR43,MCLYR(1))
      EQUIVALENCE (CHAR44,MCLAY(1))  
      EQUIVALENCE (CHAR45,MCLNK(1))
      DATA MCSCN / 0, 6, 1, 19, 9/
      DATA MCSHA / 0, 0, 0, 7, 9/
      DATA MCLYR / 0, 2, 1, 16, 9/
      DATA MCLAY / 0, 4, 0, 1, 9/
      DATA MCLNK / 0, 1, 0, 4, 2/
      DATA CHAR41/ 'CSCN'/
      DATA CHAR42/ 'CSHA'/
      DATA CHAR43/ 'CLYR'/
      DATA CHAR44/ 'CLAY'/
      DATA CHAR45/ 'CLNK'/
      DATA ICDMIN, ICDMAX / 9, 14 /
      DATA ICD_ETA /'ICD_ETA_00+'/
C
      MCSCN(5) = IOCLGA           ! form address
      MCSHA(5) = IOCSHA
      IZCLYR = IZCLAY             ! use CLAY location to temperarily
                                  ! store CLYR bank.
      CALL MZFORM('CLYR','1B6F1H1I-F',IOCLYR)    ! form address for CLYR
      CALL MZFORM('CSCN','5I2H-F',IOCSCN)
      MCSCN(5) = IOCSCN
      MCLYR(5) = IOCLYR
C
      CALL EZPICK('SRCP_ECAL')         ! select SRCP storage
      CALL EZGET('ICD_MIXTURES',IVAL,IER)
      IF(IER .NE. 0) THEN
        WRITE( 6, *) ' ICD MIXTURE CODE NOT FOUND '
        STOP 555
      END IF
      IMICD = IVAL(5)
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IMICD,1)
      LQCREG = LZFIND(IDVSTP,LC(LCGEH-IZCREG),ISICD,IGREGN)
C
      DO 100 ICD = ICDMIN, ICDMAX      ! loop on ICD rings
        CALL MZLIFT(IDVSTP, LQCLNK, LQCREG, -IZCLNK, MCLNK, 0)
        IC(LQCLNK + IGIDEN) = ICSCIN + (ICD-ICDMIN)*ICSINC  ! same code as CSCN
        IC(LQCLNK + IGJPHI) = 1        ! dummy since no surveys are currently
        IC(LQCLNK + IGN1) = 0          ! used
        IC(LQCLNK + IGNSEG) = 1
        WRITE (CD,'(I2.2)') ICD        ! encode ICD
        ICD_ETA(9:10) = CD             ! put into array name
        CALL MZLIFT(IDVSTP, LQCSCN, LQCREG, -IZCSCN, MCSCN, 0)
C                                      ! Lift CSCN bank
        IC(LQCSCN + IGIDEN) = ICSCIN + (ICD-ICDMIN)*ICSINC ! region code
        CALL SCNRCP(LQCSCN,ICD_ETA)    ! fill CSCN bank
        CALL MZLIFT(IDVSTP, LQCSHA, LCGEH, -IZCSHA, MCSHA, 0)
C                                      ! Lift CSHA bank
        CALL SHPSRC(LQCSHA, ICD_ETA)   ! fill CSHA bank
        CALL MZLIFT(IDVSTP, LQCLAY, LQCSCN, -IZCLAY, MCLAY, 0)
C                            ! CLAY bank is created without data words
C                            ! so that CLYR bank can be hanged from it
C                            ! It can also handle the CMAT bank
        IC(LQCLAY + ILIDEN) = IC(LQCSCN + IGIDEN)
        CALL MZLIFT(IDVSTP, LQCLYR, 0, 2, MCLYR, 0)
        LC(LQCLAY - IZCLYR) = LQCLYR    ! reference link to CLYR
C                            ! CLYR bank is being temperarily created
C                            ! here as a standalone bank.  It is by a
C                            ! reference link from CLAY in the link 1 position.
C                            ! It will be dropped later and the CLYR
C                            ! will be moved to its proper place
        CALL CLRRCP(LQCLYR, LQCSCN, ICD_ETA)     ! fill CLYR bank
C
        LC(LQCSCN - IXCMAT) = LQCMAT
        LC(LQCSCN - IXCSHA) = LQCSHA
        LC(LQCLAY - IZLMAT) = LQCMAT
        LC(LQCLAY - IZLSHA) = LQCSHA
        LC(LQCLAY - IZLLGA) = LQCSCN
  100 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
