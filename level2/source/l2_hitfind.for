      SUBROUTINE L2_HITFIND( IOPT,HITADR,HITLEN,POINT,NPULSE,HITLST )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform full FADC unpacking, and determine
C-                         leading edge for drift time
C-
C-   Inputs  : HITADR,HITLEN unpacked from hitcluster
C-             POINT         pointer to data for channel of interest
C-             NPULSE        number of hits ,so far, this wire
C-             IPED          Pedestal value from PDP banks.  Passed 
C-                           from L2_CDEL since LAYER/SECTOR needed.
C-                                                                  
C-             IOPT          selects thresholds for 1)SW  2)DEL
C-
C-   Outputs : NPULSE        updated with the number of additional
C-                           hits found in this hit cluster
C-             HITLST        array of HIT times (nsec)
C-   Controls: 
C-
C-   Created  23-OCT-1991   Daniel R. Claes
C-                          Based on Srini Rajagopalan's DFSTRK.FOR
C-                                                                      
C-            11-AUG-1992   BIMAP'ing must be applied to the pedestal   
C-                          subtracted signal.  Pedestal is added after.
C-                                                                      
C----------------------------------------------------------------------
C-   Updated  20-JUN-1992   Srini fix BYTE mask ->8 bits
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
C>>
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
C>>
      INCLUDE 'D0$INC:CDCMAP.INC' ! Included explicitly for now
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$INC:L2TRAK.INC/LIST'
      INCLUDE 'D0$INC:SECTLIST.INC/LIST'
C----------------------------------------------------------------------
C
      INTEGER BIN, EVTDAT(0:499), FBIN, HITLEN, HITADR, I
      INTEGER IFIRST, ILAST, INDEX, IOPT, MASK8, NPULSE, POINT
C
      REAL    B(0:500), HITLST(5), INCRE, SUM, SUMX
      REAL THR(2)
C
C     PARAMETER (MASK8 = 'FFFF'X)
C      PARAMETER (MASK8 = 65535)
      PARAMETER (MASK8=255)
C
C----------------------------------------------------------------------
      INTEGER BIMAP(0:255)
      COMMON /TRAK_TABLE/ BIMAP
C----------------------------------------------------------------------
      DATA THR /5.0, 3.0/         ! Thresholds for SWs and DELs
C      DATA  THR1, THR2, IPED /3.0, 10.0, 20/
C----------------------------------------------------------------------
C
        THR2 = 10.0
        THR1 = THR(IOPT)
        DO INDEX = 1,HITLEN/4-1
          POINT = POINT - 1             ! Move into actual FADC data
          BIN = HITADR - (4*INDEX) + 1  ! Compute  timing bin number
C                                                                       
C
          EVTDAT(BIN + 3) = IAND(IQ(POINT), MASK8) - IPED               
          EVTDAT(BIN + 2) = IAND(ISHFT(IQ(POINT),  -8), MASK8) - IPED   
          EVTDAT(BIN + 1) = IAND(ISHFT(IQ(POINT), -16), MASK8) - IPED   
          EVTDAT(BIN)     = IAND(ISHFT(IQ(POINT), -24), MASK8) - IPED   
C
          DO I = 0,3                                                    
            IF (EVTDAT(BIN+I).GT.0) EVTDAT(BIN+I) = BIMAP(EVTDAT(BIN+I))
          ENDDO                                                         
C                                                                       
        ENDDO
C
C       BIMAP scales the integral FADC value according to the bi-linear
C       scheme of the  FADC,  using a map  rather than mapping function
C
C       EVTDAT carries bi-linear corrected raw data for the current hit
C
        FBIN = BIN                      ! First timing bin of zero-supp data
        B(BIN) = 0
C
C       Calculate the first difference of the signal
C
        DO 200 BIN = FBIN+1,HITADR
          B(BIN) = FLOAT(EVTDAT(BIN) - EVTDAT(BIN-1))
  200   CONTINUE
C
        IFIRST = FBIN
        ILAST = FBIN
  201   I = ILAST + 1
  202   IF (I .GT. HITADR) GO TO 999    ! Finished with this hit cluster
        IF (B(I).GT.THR1) THEN
          IF (B(I-1).GT.THR1) THEN
            IF (EVTDAT(I).GE.IPED) THEN
              IF (B(I+1).GE.THR2 .OR. B(I)+B(I-1).GE.THR2) THEN
                IFIRST = I - 2
                IF (B(IFIRST) .LE. 0) IFIRST = I - 1
                GO TO 230
              ELSE
                I = I + 3
              ENDIF
            ELSE
              I = I + 1
            ENDIF
          ELSE
            I = I + 1
          ENDIF
        ELSE
          I = I + 2
        ENDIF
        GO TO 202
  230   CONTINUE                        ! Finds "start" of peak
C
        SUM  = 0
        SUMX = 0
C
        INCRE = 0.
        DO 240 I = IFIRST,HITADR
          IF (B(I) .LE. 0) THEN         ! Finds peak (where 1st diff<0)
            ILAST = I
            GO TO 250
          ENDIF
          INCRE = INCRE + 1.
          SUM = SUM + B(I)
          SUMX = SUMX + INCRE*B(I)      ! Wghted avg of leading edge bins
  240   CONTINUE
        ILAST = HITADR
C
  250   CONTINUE
C
C Compute Drift Time and Peak height
C
        NPULSE = NPULSE + 1             ! count those peaks analyzed for timing
        IF (NPULSE.LE.5) THEN           ! Limited to 5 pulses/wire
C          HITLST(NPULSE)=10.*(SUMX/SUM+IFIRST-0.5) ! Drift time
          HITLST(NPULSE)=(1000./106.)*(SUMX/SUM+IFIRST-0.5) ! Drift time
          IF (NPULSE.LE.4) GO TO 201                     ! Still needs Tzero
        ENDIF                                            ! subtracted
C
  999 RETURN
      END
