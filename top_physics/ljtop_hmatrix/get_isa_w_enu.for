      SUBROUTINE GET_ISA_W_ENU(NDECAY_MAX,NDECAY,P4_ELEC,P4_NEUT,
     & IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET THE 4 VECTORS OF ELECTRON AND NEUTRINO
C-   COMING FROM W DECAYS IN AN EVENT.
C-
C-   Inputs  :NDECAY_MAX = MAXIMUM NUMBER OF W DECAYS TO FIND
C-   Outputs : NDECAY = Number of W decays found
C-             P4_ELEC = 4 vector of electron
C-             P4_NEUT = 4 vector of neutrino
C-             IER = error if non zero
C-   Controls:
C-
C-   Created   2-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NDECAY,NDECAY_MAX,IER
      REAL    P4_ELEC(4,*),P4_NEUT(4,*)
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LISAL,LISAL1,LISAJ1,GZISAL,LISAJ,IPART_PARENT,IPART
      INTEGER IPART1
      EQUIVALENCE (LISAL,LBANK)
      EQUIVALENCE (CSTLNK(LNKMX-1),LISAJ)
      EQUIVALENCE (CSTLNK(LNKMX-2),LISAJ1)
C----------------------------------------------------------------------
      IER = 1                           ! no electron
      LISAL = GZISAL()
      NDECAY = 0
      DO WHILE (LISAL.NE.0.AND.NDECAY.LT.NDECAY_MAX)
        IPART = IQ(LISAL+1)
        IF(IABS(IPART).EQ.12)THEN          ! ELECTRON OR POSITRON
          LISAJ = LQ(LISAL-3)           ! REFERENCE LINK TO ISAJ BANK
          IF(LISAJ.EQ.0)GO TO 123
          IPART_PARENT = IQ(LISAJ+1)
          IF(IABS(IPART_PARENT).EQ.80)THEN
C
C ****  W FOUND. NOW TO STORE LINK AND FIND NEUTRINO
C
            LISAJ1 = LISAJ

            LISAL1 = GZISAL()
            DO WHILE (LISAL1.NE.0)
              IPART1 = IQ(LISAL1+1)
              IF(IABS(IPART1).EQ.11)THEN        ! NEUTRINO
                LISAJ = LQ(LISAL1-3)
                IF(LISAJ.EQ.LISAJ1)THEN
C
C ****  FOUND MATCHING NEUTRINO.
C
                  NDECAY = NDECAY + 1
                  CALL UCOPY(Q(LISAL+2),P4_ELEC(1,NDECAY),4)
                  CALL UCOPY(Q(LISAL1+2),P4_NEUT(1,NDECAY),4)
                  IER = 0
                  GO TO 123
                ENDIF
              ENDIF
              LISAL1 = LQ(LISAL1)
            ENDDO
            IER = 2                     ! NO MATCH
            RETURN
          ENDIF
        ENDIF
  123   CONTINUE
        LISAL = LQ(LISAL)
      ENDDO
C
  999 RETURN
      END
