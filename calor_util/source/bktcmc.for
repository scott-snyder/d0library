      SUBROUTINE BKTCMC(NCH,LTCMC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books bank TCMC for CAMAC information
C-
C-   Inputs  : None
C-   Outputs : LTCMC = Address to created bank TCMC
C-
C-   Created  2-APR-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTCMC.LINK'
      INTEGER NCH,NALOC
      INTEGER LTCMC, IOTCMC, LTRUN, GZTRUN 
      INTEGER NLNKS
      PARAMETER (NLNKS=1)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('TCMC','-I',IOTCMC)   ! format for TCMC
        FIRST=.FALSE.
      ENDIF
C
      LTRUN=GZTRUN()
      IF ( LTRUN.LE.0 ) CALL BKTRUN(LTRUN)
      LTCMC=LQ(LTRUN-IZTCMC)
      IF (LTCMC .NE. 0) GOTO 999        ! return if TCMC exists already
C
C
C
C **** Create TCMC bank
C
      NALOC=NCH+2
      CALL MZBOOK(IXMAIN,LTCMC,LTRUN,-IZTCMC,
     +            'TCMC',NLNKS,NLNKS,NALOC,IOTCMC,-1)
C
      IQ(LTCMC+1) = 0           ! version number
      IQ(LTCMC+2) = NCH         ! number of channels 
C
  999 RETURN
      END
