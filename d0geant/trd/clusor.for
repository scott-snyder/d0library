      SUBROUTINE CLUSOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C- SORT THE TRD CLUSTERS IN ASCENDING ORDER W.R.T. TIME              -
C- RE-ORDERING OF THE VARIABLES IN THE COMMON BLOCK "CLUSM"          -
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created                A. ZYLBERSTEJN
C-   Updated  12-OCT-1988   J.R. HUBBARD     Include Cathodes
C-
C---------------------------------------------------------------------
C
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:CLUSTR.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC/LIST'
C
      INTEGER INDEX(300),MODE,NWAY,NSORT,IFOIS
      INTEGER I,J
C
      DATA MODE,NWAY,NSORT/1,0,0/,IFOIS/0/
C
      IFOIS=IFOIS+1
      IF(NSMEAR.LE.0  .OR. NSMEAR.GT.300)
     + WRITE(LOUT,*) ' PROBLEM TRD IN CLUSOR: NSMEAR',NSMEAR,
     + 'IFOIS',IFOIS
      CALL UCOPY(XCLES(1)   ,WS(         1),NSMEAR)
      CALL UCOPY(ECLES(1)   ,WS(  NSMEAR+1),NSMEAR)
      CALL UCOPY(CLUDIS(1,1),WS(2*NSMEAR+1),NSMEAR)
      CALL UCOPY(CLUDIS(1,2),WS(3*NSMEAR+1),NSMEAR)
      CALL VFLOAT(IWIRE(1)  ,WS(4*NSMEAR+1),NSMEAR)
      CALL UCOPY(TIMEC(1)   ,WS(5*NSMEAR+1),NSMEAR)
      CALL UCOPY_i(IESCAP(1) ,IWS(6*NSMEAR+1),NSMEAR)
      CALL UCOPY(ZCLES(1)   ,WS(7*NSMEAR+1),NSMEAR)
      CALL UCOPY_i(ISTRIP(1) ,IWS(8*NSMEAR+1),NSMEAR)
      CALL UCOPY(DSTRIP(1)  ,WS(9*NSMEAR+1),NSMEAR)
C
C  COMBINE WIRE AND TIME IN ONE VARIABLE
C
      CALL
     &  VLINE(TIMEC(1),1.,WS(4*NSMEAR+1),10000.,TIMEC,NSMEAR)
      CALL SORTZV(TIMEC,INDEX,NSMEAR,MODE,NWAY,NSORT)
      DO 20 I = 1,NSMEAR
        J = INDEX(I)
        XCLES(I)    = WS(         J)
        ECLES(I)    = WS(  NSMEAR+J)
        CLUDIS(I,1) = WS(2*NSMEAR+J)
        CLUDIS(I,2) = WS(3*NSMEAR+J)
        IWIRE(I)    = WS(4*NSMEAR+J)
        TIMEC(I)    = WS(5*NSMEAR+J)
        IESCAP(I)   = IWS(6*NSMEAR+J)
        ZCLES(I)    = WS(7*NSMEAR+J)
        ISTRIP(I)   = IWS(8*NSMEAR+J)
        DSTRIP(I)   = WS(9*NSMEAR+J)
   20 CONTINUE
      END
