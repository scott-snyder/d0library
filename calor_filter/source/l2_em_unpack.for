      SUBROUTINE L2_EM_UNPACK(IETA,IPHI,ILYR,EFLOOR,ET_CAND,SUMEM_E,
     &  EM3,E3,E5,E7)
C----------------------------------------------------------------------
C-
C-   Purpose And Methods : get energy of candidate into local arrays
C-   Inputs  : CAndidAte IETA,IPHI,ILYR the peak EM3 cell
C-              DAtA from CAlorimeter
C-   Outputs : EFLOOR(8) sum in EM1,2,3,4, FH1, FH2+3, CH, and ICD/MG
C-             ET_CAND the ET sum of floors 1-4
C-             SUMEM_E  the E sum of floors 1-4
C-             EM3(-2:2,-2:2)  local array in EM3 space around candiate
C-             E3,E5,E7 sums in 3x3, 5x5 and 7x7 in the EM3 layer
C-                WARNING: the size switches from EM3 space to offline
C-                readout tower space for IABS(IETA) > 25 ("FORWARD")
C-                In tower 26, the neighbors no longer have EM3 subdivision
C-   Controls: ET_IN_CAEP = .TRUE. means EFLOOR,EM3 was in Et
C-
C-   CreAted 15-SEP-1990   Yi  XiA
C-
C-   Updated  17-DEC-1991   James T. Linnemann  merged EC and CC arrays
C-                                separate unpacking from cutting
C-   Updated  28-JAN-1992   James T. Linnemann  rewrite using ring routines
C-   Updated   8-FEB-1992   James T. Linnemann   debugging by Scott Snyder
C-   Updated   6-NOV-1993   James T. Linnemann   allow for minimum cell Et(nom)
C-   Updated   7-NOV-1993   James T. Linnemann   add any offset to floor 1
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! zeBrA mAin store /ZEBCOM/
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:CL2_RINGS.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
C
      INTEGER GZCAEP,IETA,IPHI,ILYR
      INTEGER ETA,PHI,LYR,I,NR
      REAL EM3(-2:2,-2:2)
      REAL EFLOOR(8),OFFSET
      REAL SUMEM,SUMEM_E,ET_CAND
      REAL E,E3,E5,E7,CL2_SNTH
      INTEGER ETALO,ETAHI,PHILO(2),PHIHI(2),NPHI,IBOUND,IPOINT
      INTEGER L2CAEP
      INTEGER IFL,IFLOOR(NLYRL) !1-4=EM  5=FH1  6=FH2,3   7=CH   8=ICD,MG
      INTEGER ETA3,PHI3,IE3,IP3,JE3,JP3   !coordinates and offsets in EM3 space
      LOGICAL RETURN_FLAG,OK
      DATA IFLOOR/1,2,4*3,4,3*8,5,3*6,3*7/  !LYR to FLOOR map
C----------------------------------------------------------------------
      RETURN_FLAG = .TRUE.
C
C...get boundaries of a ring around the candidate for calculating floor sums
C...  ring is size = 1 i.e. 3x3 Readout Towers.  Unpacking guaranteed in EM3_MAX
      CALL CL2_RING22(IETA,IPHI,1,ETALO,ETAHI,PHILO,PHIHI,NPHI)
      CALL VZERO(EFLOOR,8)    ! CLEAN ARRAY
      CALL VZERO(EM3,25)
      E3 = 0
      E5 = 0
      E7 = 0
C
C...calculate coordinates in EM3 space to fill 5x5 (-2:2) EM3 array
      ETA3 = 2*L2_JETA(IETA) + DETA3(ILYR) !eta in EM3 space of peak
      PHI3 = 2*IPHI  + DPHI3(ILYR)
      L2CAEP = GZCAEP()
      NR = IQ(L2CAEP+2)
      DO ETA = ETALO,ETAHI
        IE3 = 2*L2_JETA(ETA) - ETA3 !relative coord in em3 space before em3 corr
        DO IBOUND = 1,NPHI
          DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
            IP3 = 2*PHI - PHI3      !relative coord in em3 space before em3 corr
            DO LYR = 1,NLYRL
              IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*NR
              IF (IPOINT.GE.0) THEN
                E = Q(L2CAEP+IPOINT+5)
                IF (E.GE.ETMIN_CELL) THEN
                  IFL = IFLOOR(LYR)
                  EFLOOR(IFL) = EFLOOR(IFL) + E
                  IF (IFL.EQ.3) THEN  !em 3 sums only; for transverse shape
                    E3 = E3 + E   ! 3x3 in offline space, in case at large eta
                    JE3 = IE3 + DETA3(LYR)  !relative coordinate in EM3
                    IF (IABS(JE3).LE.2) THEN
                      JP3 = L3_DPHI(IP3 + DPHI3(LYR)) !relative coord in EM3
                      IF (IABS(JP3).LE.2) THEN
                        EM3(JE3,JP3) = E
                        E5 = E5 + E !5 x 5 in EM3 space
                      ENDIF
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
      IF (IABS(IETA).LE.25) THEN  !standard transverse algorithm
C...redefine E3 as 3x3 in EM3 space, not offline space
        E3=EM3(-1,-1)+EM3(0,-1)+EM3(1,-1)+EM3(-1,0)+EM3(0,0)+EM3(1,0)
     &      + EM3(-1,1) +EM3(0,1) +EM3(1,1)  !central 3x3 in EM3 space
      ELSE
C...redefine E5 as in offline space
C
C...get boundaries of a ring of size 2 for transverse sums
        CALL CL2_RING22(IETA,IPHI,2,ETALO,ETAHI,PHILO,PHIHI,NPHI)
C
C...be sure this region is unpacked
        CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(1),PHIHI(1))
        IF (NPHI.GT.1) CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(2),
     &        PHIHI(2))
        E5 = 0
        DO ETA = ETALO,ETAHI
          DO IBOUND = 1,NPHI
            DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
              DO LYR = LYEM3A,LYEM3D
                IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*NR
                IF (IPOINT.GE.0) THEN
                  E = Q(L2CAEP+IPOINT+5)
                  IF (E.GE.ETMIN_CELL) E5 = E5 + E !5 x 5 in offline space
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF (IABS(IETA).GE.31.AND.IABS(IETA).LE.33) THEN
C
C...get boundaries of a ring of size 3 for transverse sums
          CALL CL2_RING22(IETA,IPHI,3,ETALO,ETAHI,PHILO,PHIHI,NPHI)
C
C...be sure this region is unpacked
          CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(1),PHIHI(1))
          IF (NPHI.GT.1) CALL CL2_ROTOW_ETNOM(ETALO,ETAHI,PHILO(2),
     &        PHIHI(2))
          E7 = 0  !7 x 7 towers; since neighbors coarsen, a larger eta range
          DO ETA = ETALO,ETAHI
            DO IBOUND = 1,NPHI
              DO PHI = PHILO(IBOUND),PHIHI(IBOUND)
                DO LYR = LYEM3A,LYEM3D
                  IPOINT = (PTCAEP2(LYR,PHI,ETA)-1)*NR
                  IF (IPOINT.GE.0) THEN
                    E = Q(L2CAEP+IPOINT+5)
                    IF (E.GE.ETMIN_CELL) E7 = E7 + E !7 x 7 in offline space
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
  999 CONTINUE
      SUMEM = 0.
      DO I = 1,4
        SUMEM = SUMEM + EFLOOR(I)
      ENDDO
      IF (SUMEM.GT.0.) THEN
        CALL L2_EM_GET_OFFSET(IETA,IPHI,ILYR,OFFSET) !Zero, or offset
        EFLOOR(1) = EFLOOR(1) + OFFSET !insert offset into floor 1
        SUMEM = SUMEM + OFFSET
      ENDIF
      IF (ET_IN_CAEP) THEN
        ET_CAND = SUMEM
        SUMEM_E = SUMEM/CL2_SNTH(IETA,IPHI,ILYR,0.0)
      ELSE
        ET_CAND = SUMEM*CL2_SNTH(IETA,IPHI,ILYR,0.0)
        SUMEM_E = SUMEM
      ENDIF
      RETURN
      END
