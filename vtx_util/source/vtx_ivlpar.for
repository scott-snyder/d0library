      SUBROUTINE VTX_IVLPAR(PAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in I vs. LUM parametrizations for each HV supply
C-               from RCP file
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-DEC-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C I/O:
      REAL PAR(3,192)
C Locals:
      INTEGER CHR,FLT,I,NELS,ERR,LEN,CHN
      CHARACTER*10 NAME
C----------------------------------------------------------------------
      CALL EZPICK('VTRAKS_RCP')
      CALL EZGETA('I_VS_L_PARAMS',0,0,0,NELS,ERR)
      CHR = 1
      FLT = 1
      DO WHILE(FLT .LT. NELS)
        CALL EZGETS('I_VS_L_PARAMS',CHR,NAME,LEN,ERR)
        FLT = FLT + (LEN+3)/4
        READ(NAME(8:10),'(I3)') CHN
        CALL EZGETA('I_VS_L_PARAMS',FLT,FLT+2,1,PAR(1,CHN),ERR)
c        write(*,'(A,1X,F6.3,1X,F7.4,1X,F8.5,1X,F6.3)') 
c     &    name,(par(i,chn),i=1,3)
        CHR = CHR + 1
        FLT = FLT + 3
      ENDDO
      CALL EZRSET
  999 RETURN
      END
