      REAL FUNCTION CORR_HIT_CELL_TYPE(RUN_NUM,HALF,ITYPE,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate the proper correction factor for
C-                         FDC Pulse Areas, based on fitted formula
C-                         ( an exponential ) and parameters from 1B 
C-                         CD_EXAMINE histograms.
C-              [ Note ] : the correction factors are relative to
C-                         some early run number for 1B, that is,
C-                         we pick run 74050 as unity for reference.
C-
C-   Returned value  :     CORR_HIT_CELL_TYPE
C-   Inputs  :             RUN_NUM, HALF, ITYPE, S
C-
C-   Created  19-JUL-1995   Yi-Cheng Liu
C-   Updated  12-SEP-1995   Yi-Cheng Liu, make the correction parameters
C-                          RCP controlled, and passed in the S array. 
C-   Updated  03-Oct-1995   Yi-Cheng Liu, bug fix on correction factors.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER RUN_NUM, HALF, ITYPE
C* Remember : NFDC : Half=0, SFDC : Half=1
      REAL S(0:1,0:6)
C      REAL C(0:1,0:6), S(0:1,0:6)  ! C : multiplicative constants 
C                                   ! of PA fits, not used for now.
C-                 N      S
C      DATA C / 5.649, 5.882, 
C     &         6.328, 6.062,
C     &         6.040, 6.125,
C     &         5.473, 5.679,
C     &         5.536, 5.846,
C     &         6.030, 6.350,
C     &         5.309, 5.293 /
C-                 N      S
C      DATA S / -0.000009558, -0.00001161 ,
C     &         -0.00001243 , -0.00001038 ,
C     &         -0.000007029, -0.000009161,
C     &         -0.000002920, -0.000006026,
C     &         -0.000004780, -0.000008778,
C     &         -0.00001237 , -0.00001690 ,
C     &          0.000003866,  0.000004125 /
C
      CORR_HIT_CELL_TYPE = EXP(S(HALF,ITYPE)*(74050. - RUN_NUM))
C
  999 RETURN
      END
