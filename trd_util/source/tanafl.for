      SUBROUTINE TANAFL(LTDST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank tana.
C-
C-   Inputs  :  Ltdst
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JUN-1994   A. Zylberstejn
C-   Updated  15-OCT-1994   A. Zylberstejn
C-   Updated  15-JUN-1995   A. Zylberstejn: put Z intersection of tracks  in
C-   anode plane in q(ltana+50)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER J,LTANA,LAYER,GZTDST,LTDST,NW
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      INCLUDE 'D0$INC:WORD_IN_TPRL.INC'
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER GZTANA
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF
C
      DO LAYER=1,3
        IF(I_IN_TPRL(LAYER,4).GE.1) THEN! request at leat one hit cell
          CALL BKTANA(LTDST,LAYER,LTANA)
          IF(LTANA.LE.0)THEN
            CALL ERRMSG('Cant book TANA','TANAFL',' ','W')
            GO TO 999
          END IF
C          R_IN_TPRL(LAYER,50)=z_trd(layer)
C          IF(IQ(LTANA+2*NWORD+1).LE.1)THEN
C            J=I_IN_TPRL(LAYER,4)+I_IN_TPRL(LAYER,5)+
C     &        5*(I_IN_TPRL(LAYER,6)+I_IN_TPRL(LAYER,7))
          J=NWORD-4
          R_IN_TPRL(LAYER,J+1)=PHI_TRD(LAYER)
          R_IN_TPRL(LAYER,J+2)=R_TRD(LAYER)
          R_IN_TPRL(LAYER,J+3)=Z_TRD(LAYER)
C          END IF
          DO NW=1,NWORD
            Q(LTANA+NW)=R_IN_TPRL(LAYER,NW)
            IQ(LTANA+NWORD+NW)=I_IN_TPRL(LAYER,NW)
          END DO
C fill in the rest of the bank here.
          IQ(LTANA+2*NWORD+1)=2  ! version nb.
        END IF
      END DO
  999 RETURN
      END
