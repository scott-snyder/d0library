      SUBROUTINE BLSGMC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build A LINEAR STRUCTURE  which builds
C-   the description of the Overall mother volumes Volumes defined
C-                         in D0GEANT
C-
C-   Inputs  :
C-   Outputs : 
C-   Controls:
C-
C-   Created   2-AUG-1988   Ghita Rahal-Callot
C-   Updated  19-JUL-1989   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSGMC.LINK'
C
      INTEGER IER,I,K,NDATA
      INTEGER LSGEN, LSGMC, NDAT, NFORM ,LSTPC,IAR(100)
      INTEGER NVOL
      PARAMETER( NVOL = 4 )
      INTEGER NBDAT(NVOL)
C
      CHARACTER*32 VOLUMES(NVOL)
      DATA VOLUMES/'OVERALL_MOTHER_VOLUME',
     &             'CALORIMETER_MOTHER_VOLUME',
     &             'CENTRAL_DETECTOR_MOTHER_VOLUME',
     &             'MUON_MOTHER_VOLUME'/
C
      DATA NBDAT/3*14,33/               ! amount of data per volume
C----------------------------------------------------------------------
C
      DO K = 1,NVOL
        CALL EZGET(VOLUMES(K),IAR,IER)
        IF(IER.NE.0)GO TO 998
        NDATA = NBDAT(K)+1
        CALL BKSGMC(LSGMC,NDATA)
        DO I = 1 , NBDAT(K)
          IC(LSGMC+1+I) = IAR(I)
        ENDDO
      ENDDO
      RETURN
  998 CONTINUE
      CALL ERRMSG('GNWSTP','BLSGMC',
     &  'ERROR IN GETTING SRCP CONSTANTS ','F')
  999 RETURN
      END
