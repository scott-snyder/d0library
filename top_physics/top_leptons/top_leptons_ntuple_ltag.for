      SUBROUTINE TOP_LEPTONS_NTUPLE_LTAG(ELE,MUO,LEP,EVTYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to determine highest Pt leptons from ELE and
C-                         MUO and construct LEP vector
C-
C-   Inputs  : ELE,MUO
C-   Outputs : LEP,EVTYPE
C-   Controls: 
C-
C-   Created  12-AUG-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 17-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,EVTYPE,NUMEL,NUMMU,NUMLP
      REAL  ELE(0:3,14),MUO(0:3,14),LEP(3,14)
C----------------------------------------------------------------------
C
      NUMEL = 0
      NUMMU = 0
      NUMLP = 0
      DO J=1,3
        IF (ELE(J,6) .GT. 0.) NUMEL=NUMEL+1
        IF (MUO(J,6) .GT. 0.) NUMMU=NUMMU+1
      ENDDO
      NUMLP = NUMEL+NUMMU
C
      IF (NUMLP .EQ. 0) THEN           ! no leptons
        EVTYPE = 4
        DO J=1,3
          DO I=1,14
            LEP(J,I)= -7.
          ENDDO
        ENDDO
        GOTO 999
      ENDIF
C
      IF (NUMLP .EQ. 1) THEN           ! single lepton
        EVTYPE = 5 
        IF (NUMEL .EQ. 1 .AND. NUMMU .EQ. 0) THEN
          DO I=1,14
            LEP(1,I)= ELE(1,I)
          ENDDO
        ELSEIF (NUMEL .EQ. 0 .AND. NUMMU .EQ. 1) THEN
          DO I=1,14
            LEP(1,I)= MUO(1,I)
          ENDDO
        ENDIF
        DO I=1,14
          LEP(2,I)= -8.
          LEP(3,I)= -8.
        ENDDO
        GOTO 999
      ENDIF
C
      IF (ELE(2,6) .GE. MUO(1,6)) THEN
        EVTYPE = 1                      ! ee event
        DO I=1,14
          LEP(1,I)=ELE(1,I)
          LEP(2,I)=ELE(2,I)
        ENDDO
        IF (ELE(3,6) .GT. MUO(1,6)) THEN
          DO I=1,14
            LEP(3,I)=ELE(3,I)
          ENDDO
        ELSEIF (ELE(3,6) .LE. MUO(1,6)) THEN
          DO I=1,14
            LEP(3,I)=MUO(1,I)
          ENDDO
        ENDIF
C
      ELSEIF (MUO(2,6) .GE. ELE(1,6)) THEN
        EVTYPE = 2                       ! mumu event
        DO I=1,14
          LEP(1,I)=MUO(1,I)
          LEP(2,I)=MUO(2,I)
        ENDDO
        IF (MUO(3,6) .GT. ELE(1,6)) THEN
          DO I=1,14
            LEP(3,I)=MUO(3,I)
          ENDDO
        ELSEIF (MUO(3,6) .LE. ELE(1,6)) THEN
          DO I=1,14
            LEP(3,I)=ELE(1,I)
          ENDDO
        ENDIF
C
      ELSEIF (ELE(1,6) .GT. MUO(2,6) .AND. MUO(1,6) .GT. 
     &  ELE(2,6)) THEN
        EVTYPE = 3                        ! emu event
        IF (ELE(1,6) .GE. MUO(1,6)) THEN
          DO I=1,14
            LEP(1,I)=ELE(1,I)
            LEP(2,I)=MUO(1,I)
          ENDDO
        ELSEIF (ELE(1,6) .LT. MUO(1,6)) THEN
          DO I=1,14
            LEP(1,I)=MUO(1,I)
            LEP(2,I)=ELE(1,I)
          ENDDO
        ENDIF
C
        IF (ELE(2,6) .GE. MUO(2,6)) THEN
          DO I=1,14
            LEP(3,I)=ELE(2,I)
          ENDDO
        ELSEIF (ELE(2,6) .LT. MUO(2,6)) THEN
          DO I=1,14
            LEP(3,I)=MUO(2,I)
          ENDDO
        ENDIF
C
      ENDIF
  999 RETURN
      END
