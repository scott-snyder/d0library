      SUBROUTINE MUSIM_BOOK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books MUON TRIGGER histograms
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created:           Kamel Bazizi    10-15-92
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IHOFF
      DATA IHOFF/0/
C
C if you want to skip these histos set IHOFF to -1
      IF(IHOFF.EQ.-1) GOTO 999
C
C-- L1 trigger bits
      CALL HBOOK1(IHOFF+1,' L1 MUON TRIGGER BITS - SIMULATOR$',
     &     16,1.,17.,0.)

C
C-- L1.5 trigger bits
      CALL HBOOK1(IHOFF+2,' L1.5 MUON TRIGGER BITS - SIMULATOR$',
     &     16,1.,17.,0.)

C
C-- L1 trigger octant fired 
      CALL HBOOK1(IHOFF+3,' L1 MUON TRIGGER OCTANTS - SIMULATOR$',
     &     39,1.,40.,0.)

C
C-- L1.5 trigger octant fired
      CALL HBOOK1(IHOFF+4,' L1.5 MUON TRIGGER OCTANTS - SIMULATOR$',
     &     39,1.,40.,0.)

C
C-- L1 trigger multiplicity for ETA < 1.0 (CF)
      CALL HBOOK1(IHOFF+5,' L1 TRIG. MULT. FOR |ETA| .LT. 1.0$',
     &     10,0.,10.,0.)

C
C-- L1 trigger multiplicity for ETA < 1.7 (CF+WN+WS)
      CALL HBOOK1(IHOFF+6,' L1 TRIG. MULT. FOR |ETA| .LT. 1.7$',
     &     10,0.,10.,0.)

C
C-- L1 trigger multiplicity for ETA < 2.4 (CF+WN+WS+ON+OS)
      CALL HBOOK1(IHOFF+7,' L1 TRIG. MULT. FOR |ETA| .LT. 2.4$',
     &     10,0.,10.,0.)

C
C-- L1 trigger multiplicity for ETA < 3.4 (CF+WN+WS+ON+OS+SN+SS)
      CALL HBOOK1(IHOFF+8,' L1 TRIG. MULT. FOR |ETA| .LT. 3.4$',
     &     10,0.,10.,0.)

C
C-- L1.5 trigger confirmation for ETA < 1.0 (CF)
      CALL HBOOK1(IHOFF+9,' L1*L1.5 FOR |ETA| .LT. 1.0$',
     &     10,0.,10.,0.)

C
C-- L1 trigger confirmation for ETA < 1.7 (CF+WN+WS)
      CALL HBOOK1(IHOFF+10,' L1*L1.5 FOR |ETA| .LT. 1.7$',
     &     10,0.,10.,0.)

C----------------------------------------------------------------------
  999 RETURN
      END
