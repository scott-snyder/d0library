      SUBROUTINE SAM_MA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read geometric constants for the SAMUS magnets
C-                         and construct GEANT geometry of the magnets as
C-                         consisting of four slabs (each is BOX-shaped).
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  23-APR-1991   Andrei Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:SRCPR.INC'
      INCLUDE 'D0$LINKS:IZSMAG.LINK'
      INTEGER LSMAG,GZSMAG,NMAG, I,J,K, ROT_FLAG
      REAL    ROTS(3,3), COOR(3)
      REAL    TH1,PH1,TH2,PH2,TH3,PH3
      CHARACTER*80 MSGSTR               ! Error message
C----------------------------------------------------------------------
C
      DO 50 NMAG=1,2
        LSMAG=GZSMAG(NMAG)
        IF (LSMAG.NE.0) THEN
C
C ****  Define parameters for GEANT volume description
C
          CALL UCTOH ('BOX ',SHAPE,4,4)
          NUMED=IC(LSMAG+4)
          MOTHER=IC(LSMAG+5)
          CALL UCTOH ('POS ',NPSTYP,4,4)
          ROT_FLAG=IC(LSMAG+25)
          IF (ROT_FLAG.EQ.0) THEN
            IROT=10000
          ELSE
            IROT=IC(LSMAG+15)
            K=15
            DO 30 J=1,3
              DO 20 I=1,3
                K=K+1
                ROTS(I,J)=C(LSMAG+K)
   20         CONTINUE
   30       CONTINUE
            CALL ROTGEA (ROTS(1,1),TH1,PH1,TH2,PH2,TH3,PH3)
            CALL GSROTM (IROT,     TH1,PH1,TH2,PH2,TH3,PH3)
          ENDIF
          ICOPY=1
          NPAR=3
C
C ****  Get parameters of slabs and construct its geometry
C
          DO 40 I=1,4
            CALL SASLB1 (I,IC(LSMAG+3),C(LSMAG+6),C(LSMAG+9),
     &               NAME,PAR(1),COOR(1))
            CALL SASLB2 (COOR(1),C(LSMAG+12),ROTS(1,1),ROT_FLAG, X,Y,Z)
            CALL VOLPOS1
   40     CONTINUE
C
        ENDIF
   50 CONTINUE
C
  999 RETURN
      END
