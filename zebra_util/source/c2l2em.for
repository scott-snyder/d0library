      FUNCTION C2L2EM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Rebuild L2EM banks from C2EM banks.  Complexity
C-                         of this routine is necessary to restore original
C-                         L2EM ordering from C2EM (see diagram below).
C-
C-           Original L2EM:
C-
C- (L1 cand.) *   1     2     3     1     3     1     2     3
C-                ------------------------------------------- ...
C-                |     |     |     |     |     |     |     |
C- (parset #)     1     1     1     2     2     3     3     3
C-            
C- ------------>
C-
C-           C2EM repetition:
C-           
C-  L1 cand.----> 3           2           1
C-                ------------------------- ...
C-  p  |          |           |           |
C-  a  |          - 1         - 1         - 1
C-  r  |          |           |           |
C-  s  |          - 2         - 3         - 2
C-  e  |          |                       |
C-  t  |          - 3                     - 3
C-     | 
C-  #  V 
C-       
C-
C-   Returned value  : TRUE
C-   Inputs  : none
C-   Outputs : none
C-   Controls: no control !
C-
C-   Created   2-AUG-1993   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL C2L2EM
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C   my own personal zebra link area since ZLINKA could be too small!
      INTEGER JTM_NSLINK,JTM_NRLINK,JTM_LSLINK,JTM_LRLINK
      PARAMETER (JTM_NSLINK=129)        ! 129 = max L1 candidates + 1
      PARAMETER (JTM_NRLINK=129)        ! just to be safe
      COMMON/JTM_ZLINKA/JTM_LSLINK(JTM_NSLINK),JTM_LRLINK(JTM_NRLINK)
C
      INTEGER NFIX,NR,NP,PS(JTM_NRLINK)
      INTEGER I,J,NC2EM,NL2EM,NPS,NC2,NBANK
      INTEGER LL2EM,LC2EM,GZL2EM,GZC2EM,GZFRES,LFRES
      LOGICAL FIRST,FOUND
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      C2L2EM = .TRUE.
C
C BKL2EM will book FRES if necessary, so don't need to check it
C
      LL2EM = GZL2EM()
      IF(LL2EM.GT.0) GOTO 999                 ! L2EM banks exist, don't rebuild
      LC2EM = GZC2EM()
      IF(LC2EM.LE.0) GOTO 999                 ! no C2EM banks, cannot rebuild
C
      IF(FIRST) THEN                          ! initialize my zebra link area
        CALL MZLINK(IXCOM,'/JTM_ZLINKA/',JTM_LSLINK,JTM_LRLINK,
     &    JTM_LRLINK(JTM_NRLINK))
        FIRST=.FALSE.
      ENDIF
C
C want to restore original L2EM ordering, so must do some fancy footwork
C
      NC2EM = 0                               ! initialize C2EM bank counter
      NL2EM = 0                               ! initialize L2EM bank counter
      NPS = 0                                 ! parameter set index
C
      DOWHILE(LC2EM.GT.0)
        NC2EM = NC2EM + 1                     ! count C2EM banks, (L1 cands.) 
        JTM_LRLINK(NC2EM) = LC2EM             ! reserve C2EM links
        NFIX = IQ(LC2EM+2)
        NR = IQ(LC2EM+3)
        NP = IQ(LC2EM+4)
        DO I = 0,(NP-1)
          DO J = 1,NPS                        ! loop over all found parsets
            IF(PS(J).EQ.IQ(LC2EM+NFIX+I*NR+1)) GOTO 1
          ENDDO
          PS(J) = IQ(LC2EM+NFIX+I*NR+1)       ! found a new parset if got here
          NPS = J                             ! (J = NPS + 1 after DO loop)
    1     NL2EM = NL2EM + 1                   ! count L2EM banks to rebuild
        ENDDO
        LC2EM = LQ(LC2EM)                     ! next C2EM bank
      ENDDO
C
      NBANK = 0                               ! initialize new L2EM bank counter
      NC2 = NC2EM                             ! initialize array indices
      NPS = 1                                 ! for reserved links and parsets
C
      DOWHILE(NBANK.LT.NL2EM)                 ! loop until all L2EM banks made
        DOWHILE(NC2.GT.0)                     ! loop over C2EM chain starting
          LC2EM = JTM_LRLINK(NC2)             ! at end and working back
          NFIX = IQ(LC2EM+2)
          NR = IQ(LC2EM+3)
          NP = IQ(LC2EM+4)
          DO I = 0,(NP-1)                     ! loop over all parsets this bank
            IF(IQ(LC2EM+NFIX+I*NR+1).EQ.PS(NPS))THEN  ! find correct parset
              CALL BKL2EM(1,LL2EM)
              IF(LL2EM.LE.0)THEN
                CALL ERRMSG('C2L2EM','L2EM BANK PROBLEM',
     &            'Problem booking L2EM bank','W')
                GOTO 999
              ENDIF
C
C copy C2EM fixed bank to L2EM bank
C
              IQ(LL2EM+4)  = IQ(LC2EM+5)      ! TETA
              IQ(LL2EM+5)  = IQ(LC2EM+6)      ! TPHI
              IQ(LL2EM+6)  = IQ(LC2EM+7)      ! IETA
              IQ(LL2EM+7)  = IQ(LC2EM+8)      ! IPHI
              IQ(LL2EM+8)  = IQ(LC2EM+9)      ! LYR
               Q(LL2EM+30) = Q(LC2EM+10)      ! AETA
               Q(LL2EM+37) = Q(LC2EM+11)      ! ZETA
               Q(LL2EM+31) = Q(LC2EM+12)      ! APHI
               Q(LL2EM+32) = Q(LC2EM+13)      ! XCLUS
               Q(LL2EM+33) = Q(LC2EM+14)      ! YCLUS
               Q(LL2EM+34) = Q(LC2EM+15)      ! ZCLUS
               Q(LL2EM+9)  = Q(LC2EM+16)      ! ET
               Q(LL2EM+35) = Q(LC2EM+17)      ! ET_ZCORR
               Q(LL2EM+10) = Q(LC2EM+18)      ! SUMEM
               Q(LL2EM+11) = Q(LC2EM+19)      ! EM1/SUMEM
               Q(LL2EM+12) = Q(LC2EM+20)      ! (EM1+EM2)/SUMEM
               Q(LL2EM+13) = Q(LC2EM+21)      ! EM3/SUMEM
               Q(LL2EM+14) = Q(LC2EM+22)      ! EM4/SUMEM
               Q(LL2EM+15) = Q(LC2EM+23)      ! FH1/SUMEM
               Q(LL2EM+16) = Q(LC2EM+24)      ! SIGMA3
               Q(LL2EM+17) = Q(LC2EM+25)      ! SIGMA5
               Q(LL2EM+18) = Q(LC2EM+26)      ! SIG3+MID
               Q(LL2EM+19) = Q(LC2EM+27)      ! SH13
               Q(LL2EM+20) = Q(LC2EM+28)      ! SH24
               Q(LL2EM+21) = Q(LC2EM+29)      ! SH35
               Q(LL2EM+22) = Q(LC2EM+30)      ! SH57
C
C copy appropriate C2EM repetition bank to L2EM bank
C
              IQ(LL2EM+29) = IQ(LC2EM + NFIX + I*NR + 1)  ! PARSET
              IQ(LL2EM+27) = IQ(LC2EM + NFIX + I*NR + 2)  ! NTRAK
              IQ(LL2EM+28) = IQ(LC2EM + NFIX + I*NR + 3)  ! IFAILED
              IQ(LL2EM+36) = IQ(LC2EM + NFIX + I*NR + 4)  ! CUTBITS
               Q(LL2EM+23) =  Q(LC2EM + NFIX + I*NR + 5)  ! CONE_R
               Q(LL2EM+24) =  Q(LC2EM + NFIX + I*NR + 6)  ! F_CONE_ET
               Q(LL2EM+25) =  Q(LC2EM + NFIX + I*NR + 7)  ! DETA
               Q(LL2EM+26) =  Q(LC2EM + NFIX + I*NR + 8)  ! DPHI
              NBANK = NBANK + 1               ! increment new L2EM bank counter
              GOTO 2                          ! have rebuilt L2EM for this 
            ENDIF                             ! parset and L1 cand. so bug out 
          ENDDO                               ! of DO loop to save time
    2     NC2 = NC2 - 1                       ! decrement C2EM bank index
        ENDDO                                 ! end C2EM DOWHILE
        NC2 = NC2EM                           ! reset C2EM index for each loop
        NPS = NPS + 1                         ! increment parset index
      ENDDO                                   ! end total rebuilt L2EM DOWHILE
C
C L2EM bank order is now exactly opposite original order so invert ZEBRA chain
C
      LL2EM = GZL2EM()
      CALL ZTOPSY(IXCOM,LL2EM)                ! restore original L2EM bank order
C
  999 RETURN 
      END
