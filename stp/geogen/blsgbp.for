      SUBROUTINE BLSGBP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the LINEAR bank structure SGBP
C-   which contains the description
C-   of the Beam Pipes for the Tevatron and the beam flanges
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-AUG-1988   Ghita Rahal-Callot
C-   Updated  18-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSGEN.LINK'
      INCLUDE 'D0$LINKS:IZSGBP.LINK'
C
      INTEGER IER,I,K,NDATA
      INTEGER LSGEN, LSGBP, NDAT, NFORM ,LSTPC,IAR(100)
      INTEGER NVOL
      PARAMETER( NVOL = 4 )
      INTEGER NBDAT(NVOL)
C
      CHARACTER*32 VOLUMES(NVOL)
      DATA VOLUMES/'TEVATRON_BEAM_PIPE',
     &             'VACUUM_BEAM_PIPE',
     &             'BEAM_FLANGE+Z',
     &             'BEAM_FLANGE-Z'/
C
      DATA NBDAT/4*14/               ! amount of data per volume
C----------------------------------------------------------------------
C
      DO K = 1,NVOL
        CALL EZGET(VOLUMES(K),IAR,IER)
        IF(IER.NE.0)GO TO 998
        NDATA = NBDAT(K)+1
        CALL BKSGBP(LSGBP,NDATA)
        DO I = 1 , NBDAT(K)
          IC(LSGBP+1+I) = IAR(I)
        ENDDO
      ENDDO
      RETURN
  998 CONTINUE
      CALL ERRMSG('GNWSTP','BLSGBP',
     &  'ERROR IN GETTING SRCP CONSTANTS ','F')
  999 RETURN
      END
