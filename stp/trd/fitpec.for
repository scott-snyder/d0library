      SUBROUTINE FITPEC (LT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the bank TPEC: 
C-                    Header for table of likelihood  of Total deposited 
C                     energy/nb. of clusters for electrons  and 5 Gev pions.
C                     To be integrated to give the electron "probability"
C                     Corresponds to PRLIKECLx10 and PRlIKECLx20 (50 and 90
C                      degrees resp.) x=A,B,C according to the treshold for
C                      the cluster definition
C-
C-   Inputs  : LT= Address of the bank
C-   Outputs :
C-   Controls:
C-
C-   Created  30-AUG-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LT
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      IC(LT+1) = 0     !   Bank Type
      IC(LT+2) = 0     !   Status
      IC(LT+3) = 0     !   Quality factor
      IC(LT+4) = 0     !   Lowest Valid Run Number
      IC(LT+5) = 1000  !   Highest Valid Run Number
      IC(LT+6) = 0     !   Run Number Used to Generate
      IC(LT+7) = 10789 !   Date Generated
      IC(LT+8) =  0    !   Time Generated
      IC(LT+9) =  1    !   Type of Run Generated for
       C(LT+10)= 0.0   !   Version Number
C----------------------------------------------------------------------
  999 RETURN
      END
