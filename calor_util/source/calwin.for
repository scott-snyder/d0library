      SUBROUTINE CALWIN(IETA_MAX,IPHI_MAX,ETOT,ET,SIZE)

C---------------------------------------------------------
C-
C-      Purpose and Methods : Calculate total and transverse energies
C-                            in eta-dependent window.
C-
C-      Inputs  : IETA_MAX (I*4)= Eta index of hottest cell (or tower)
C-                                in a cluster.
C-                IPHI_MAX (I*4)= Phi index of hottest cell (or tower)
C-                                in a cluster.
C-                CAEH BANK.
C-
C-      Outputs : ETOT (R*4)= Total enegy (EM+FH) in an eta-dependent window.
C-                            Cell energies are taken from CAEH bank.
C-                ET   (R*4)= Transverse energy (EM+FH) in an eta-dependent
C-                            window. ET=SQRT(VECTOR SUM OF CELLS**2)
C-                SIZE (I*4)= A window size common to eta and phi directions.
C-                            Unit is eta/phi of 0.1.
C-                            This is read in at the first event and is returned
C-                            to a caller as an output.
C-                            Size should be an ODD number.
C-
C-      Controls:
C-
C-      Created 17-JUN-1992     Hiro Aihara
C-
C-
C-------------------------------------------------------------------------------


      IMPLICIT        NONE

      INCLUDE         'D0$INC:ZEBCOM.INC'
      INCLUDE         'D0$INC:ZLINKC.INC'
      INCLUDE         'D0$PARAMS:CAL_OFFLINE.PARAMS'


      INTEGER         IETA_MAX, IPHI_MAX

      REAL            ETOT, ET
      INTEGER         SIZE

      LOGICAL         FIRST
      DATA            FIRST/.TRUE./

      INTEGER         IER

      INTEGER         WINDOW(35)
      DATA            WINDOW/13*3,    ! CC
     &                        10*5,     ! EC=14-23
     &                        12*7/     ! EC=24-35

      INTEGER         LIMIT(35)
      DATA            LIMIT/13*1,10*2,12*3/

      INTEGER         IETA, IPHI, ILYR

      INTEGER         IETA_LOW, IETA_HIGH
      INTEGER         IPHI_LOW, IPHI_HIGH

      REAL            EX, EY, EZ, E, ET1, SEX, SEY, CW
      INTEGER         ST

      REAL            EX_VEC, EY_VEC
      INTEGER         IETA1, IPHI1


!-------------------------------------------------------------


! Get window sizes such as 3x3, 5x5,....

      IF(FIRST) THEN

        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('WINDOW_SIZE',WINDOW,IER)
        IF(IER.NE.0)  CALL ERRMSG('FAIL TO READ WINDOW SIZE',
     &                   'CALWIN','USE DEFAULT','W')
        CALL EZRSET


        DO IETA = 1, 35
          IF(WINDOW(IETA).LT.1) WINDOW(IETA)=1
          LIMIT(IETA) = (WINDOW(IETA)-1)/2
        END DO

      END IF


! Initialization.

      ETOT = 0.
      ET   = 0.
      SIZE = 0

      EX_VEC = 0.
      EY_VEC = 0.


! Get CAEH.


      IETA_LOW  = IETA_MAX - LIMIT(ABS(IETA_MAX))
      IETA_HIGH = IETA_MAX + LIMIT(ABS(IETA_MAX))

      IPHI_LOW  = IPHI_MAX - LIMIT(ABS(IETA_MAX))
      IPHI_HIGH = IPHI_MAX + LIMIT(ABS(IETA_MAX))

      DO IETA = IETA_LOW, IETA_HIGH
        DO IPHI = IPHI_LOW, IPHI_HIGH

          IF(ABS(IETA).GT.NETAL) THEN
            IETA1 = SIGN(NETAL,IETA)
          ELSE
            IETA1 = IETA
          END IF
          IF(IPHI.GT.NPHIL) THEN
            IPHI1 = MOD(IPHI,NPHIL)
          ELSE IF(IPHI.LT.1) THEN
            IPHI1 = IPHI + NPHIL
          ELSE
            IPHI1 = IPHI
          END IF

          DO ILYR = MNLYEM,   MXLYEM

            CALL GTCAEH_ADDR(IETA1,IPHI1,ILYR,EX,EY,EZ,E,ET1,
     &                        SEX,SEY,CW,ST,IER)

            IF(IER.EQ.0.AND.E.NE.0.) THEN
              ETOT = ETOT + E
              EX_VEC = EX_VEC + EX
              EY_VEC = EY_VEC + EY
            END IF

          END DO

          ILYR = MNLYFH

          CALL GTCAEH_ADDR(IETA1,IPHI1,ILYR,EX,EY,EZ,E,ET1,
     &                        SEX,SEY,CW,ST,IER)

          IF(IER.EQ.0.AND.E.NE.0.) THEN
            ETOT = ETOT + E
            EX_VEC = EX_VEC + EX
            EY_VEC = EY_VEC + EY
          END IF


        END DO
      END DO


      ET = SQRT(EX_VEC**2+EY_VEC**2)

      SIZE = WINDOW(ABS(IETA_MAX))

      RETURN

      END
