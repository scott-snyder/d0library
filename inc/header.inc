C----------------------------------------------------------------------
C
C  Created  14-JUN-1988   Wyatt Merritt
C
C       Contains the information from the event header, unpacked into
C       named variables. A useful local common for use inside the 
C       event processing routines.
C----------------------------------------------------------------------
      INTEGER RUNNO,RECTYP,IEVTNO(2),OEVTNO,EVTTYP,TRGTYP
      INTEGER NODNAM(2)
C
      CHARACTER*24 DATTIM
C
      COMMON /HEADER/ RUNNO,RECTYP,IEVTNO,OEVTNO,EVTTYP,TRGTYP,
     &                NODNAM,DATTIM

