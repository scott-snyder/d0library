      SUBROUTINE PRINT_STEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out GEANT control and energy information
C-                         at each step to facilitate GEANT debugging.
C-                         This routine is called from GUSTEP if ISWIT(5)
C-                         .NE. 0.  (FFREAD card SWIT sets the ISWIT array.)
C-                         ISWIT(5) = 1 ==> Fill histograms with information
C-                                  > 1 ==> Also print information in 
C-                                          file STEP_PRT, up to some limit
C-                         The histograms are booked in //PAWC/GEANT during
C-                         the first call. See code for names and contents.
C-                         
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-AUG-1991   K. Wyatt Merritt
C-   Updated  25-NOV-1991   K. Wyatt Merritt  Make self-contained by moving
C-                               the booking of the histograms to this routine. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCKING.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC/LIST'
      INCLUDE 'D0$INC:GCTMED.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
C
      LOGICAL FIRST
      LOGICAL NEW_TRACK
      LOGICAL PRINT_FLAG
C
      INTEGER I,NSVOL
      INTEGER PRU
      INTEGER NTPRT
      INTEGER TYPE
C
      REAL EVOL
C
      DATA PRU / 30 /
      DATA NTPRT / 0 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        PRINT_FLAG = ISWIT(5).GT.1
        IF (PRINT_FLAG) THEN
          OPEN(UNIT=PRU,FORM='FORMATTED',STATUS='NEW',FILE='STEP_PRT',
     &      ERR=100)
        ENDIF
C
        CALL HCDIR('//PAWC',' ')
        CALL HMDIR('GEANT','S')           ! Histograms in //PAWC/GEANT
C
        CALL HBOOK1(444,'Volumes entered',100,0.5,100.5,0.)
        CALL HBOOK1(1,'ID of secondary track',50,0.5,50.5,0.)
        CALL HBOOK1(2,'Energy of secondary track',50,0.,25.,0.)
        CALL HBOOK1(3,'GEANT mechanism for secondary trk',25,0.5,25.5,
     &    0.)
C
        CALL HBOOK1(100,'Energy lost in volume',50,0.,20.,0.)
        CALL HBOOK2(101,'Volume # vs. Energy lost in volume',
     &    50,0.5,50.5,50,0.,20.,0.)
        CALL HBOOK1(102,'# of steps inside volume',20,-0.5,59.5,0.)
        CALL HCDIR('\\',' ')     ! Leave directory set to //PAWC
      ENDIF
C
C   Turn off printing after 150 steps or 5 tracks
C
      IF (NSTEP.GT.150 .OR. NTPRT.GE.5) PRINT_FLAG = .FALSE.
C
      NEW_TRACK = .FALSE.       ! Use the GEANT variable ISTORY to flag
      IF (ISTORY .EQ. 0) THEN   ! a new track (GEANT zeroes this variable
        NTPRT = NTPRT + 1       ! when starting to trace a new track).
        ISTORY = 1
        NEW_TRACK = .TRUE.
      ENDIF
C
      CALL HCDIR('//PAWC/GEANT',' ')  
C
C   Information about each new track
C
      IF (NEW_TRACK) THEN
        IF (PRINT_FLAG) THEN
          WRITE (PRU,1000) NAPART,VECT(7),GETOT,GEKIN
        ENDIF
        CALL HF1(1,FLOAT(IPART),1.)
        CALL HF1(2,GETOT,1.)
        CALL GET_TYPE_INTERACT(KCASE,TYPE)
        CALL HF1(3,FLOAT(TYPE),1.)
      ENDIF
C
C   Information about each new volume
C
      IF (INWVOL .EQ. 1) THEN           ! Entering a new volume
        IF (NLEVEL.GT.0) CALL HF1(444,FLOAT(LVOLUM(NLEVEL)),1.)
        IF (PRINT_FLAG) THEN
          WRITE (PRU,1001) NUMED
          IF (NLEVEL.GT.0) WRITE (PRU,1002) (NAMES(I),I=1,NLEVEL)
          IF (NLEVEL.GT.0) WRITE (PRU,1102) (GONLY(I),I=1,NLEVEL)
        ENDIF
        NSVOL = 0
        EVOL = 0.
C
C   Information when exiting a volume
C
      ELSE IF (INWVOL .EQ. 2) THEN      ! Exiting a volume
        IF (PRINT_FLAG) THEN
          WRITE (PRU,2001) NSVOL,EVOL
        ENDIF
        CALL HF1(100,EVOL,1.)
        IF (NLEVEL.GT.0) CALL HF2(101,FLOAT(LVOLUM(NLEVEL)),EVOL,1.)
        CALL HF1(102,FLOAT(NSVOL),1.)
C
C   Information about each step in a volume
C
      ELSE IF (INWVOL .EQ. 0) THEN      ! Stepping within a volume
        NSVOL = NSVOL + 1
        EVOL = EVOL + DESTEP
        IF (PRINT_FLAG) THEN
          IF (IGNEXT .EQ. 0)
     &      WRITE (PRU,1902) NSTEP,DESTEP,STEP,SAFETY
          IF (IGNEXT .NE.0)
     &      WRITE (PRU,1903) NSTEP,DESTEP,STEP,SAFETY,SNEXT
          IF (NMEC.EQ.0) THEN
            WRITE (PRU,1904)
          ELSE IF (NMEC .LE. 10) THEN
            WRITE (PRU,1905) NMEC,(NAMEC(LMEC(I)),I=1,NMEC)
          ELSE
            WRITE (PRU,1906) NMEC
          ENDIF
          WRITE (PRU,1907) ISTOP
        ENDIF
      ENDIF
C
C   Return to top level directory
C
      CALL HCDIR('//PAWC',' ')
C
      GO TO 999
  100 CALL ERRMSG('GEANT','PRINT_STEP','Could not open STEP_PRT','W')
C
  999 RETURN
C
C       Format statements
C
 1000 FORMAT(/////,3X,'NEW TRACK: ',5A4,' P,E,KE',3(1X,G12.3))
 1001 FORMAT(//,6X,'NEW VOLUME: Medium = ',I5)
 1002 FORMAT(18X,15(1X,A4))
 1102 FORMAT(18X,15(1X,F4.0))
 1902 FORMAT(9X,'Step#',I5,' E=',G12.3,' STEP,SAFETY ',2(G12.3,1X))
 1903 FORMAT(9X,'Step#',I5,' E=',G12.3,' STEP,SAFETY,SNEXT ',
     &  3(G12.3,1X))
 1904 FORMAT(20X,'No mechanisms')
 1905 FORMAT(20X,I2,1X,10(A4,1X))
 1906 FORMAT(20X,'NMEC = ',I3)
 1907 FORMAT(20X,'ISTOP = ',I3)
 2001 FORMAT(6X,'EXIT VOLUME: # steps=',I5,' E sum = ',G15.5)
      END
