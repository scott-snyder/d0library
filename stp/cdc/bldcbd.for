      SUBROUTINE BLDCBD(LAYER,PLDTMD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build DCBD bank which contains delay line
C-                         calibration from harware measurements
C-   Inputs  : data file containing hardware measurements for each module
C-             LAYER: layer number
C-             LDTMD: address of the support bank
C-   Outputs : DCBD bank is filled
C-
C-   Created  12-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LOWRUN, HIGRUN, NWDELY, NWORDS, MPDCBD(5)
      PARAMETER( NWDELY = 2 )
      PARAMETER( NWORDS = 19 )
      INTEGER LDCBD(0:3), PLDTMD, PLDCBD, LAYER
      DATA    MPDCBD / 4HDCBD, 0, 0, 0, 0 /
C----------------------------------------------------------------------
      CALL MZFORM( 'DCBD', '5I -F', MPDCBD(5) )
      MPDCBD(4) = 5 + 32*NWDELY*NWORDS
      CALL MZLIFT( IDVSTP, LDCBD(LAYER), PLDTMD, -1, MPDCBD, -1 )
      PLDCBD = LDCBD(LAYER)
      IC( PLDCBD-5 ) = LAYER 
      IC( PLDCBD+1 ) = 0 
      IC( PLDCBD+2 ) = LOWRUN
      IC( PLDCBD+3 ) = HIGRUN
      IC( PLDCBD+4 ) = NWORDS 
      IC( PLDCBD+5 ) = NWDELY
  999 RETURN
      END
