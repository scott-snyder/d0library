      SUBROUTINE CDCVEL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine the global CDC velocity for inner
C-                         and outer sense wires and update the velocities
C-                         in DTVA.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-SEP-1989   Domenico Pizzuto
C-   Updated   2-SEP-1994   Stefano Lami      add switch EACRUN to restart
C-                                            iteration for each new run
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:CDPARA.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INCLUDE 'D0$LINKS:IZDTTH.LINK/LIST'
C
      INTEGER LLDRFT,GZDRFT,LLDTRH,GZDTRH,NTRAK,TRK,LLDTRK,GZDTRK
      INTEGER LAYER,SECTOR,STBIT,SEGNUM (0:3),LLDTSG,GZDTSG,SIDE
      INTEGER LAYSEC (0:3),LAYSID (0:3),LLDSEC,GZDSEC,NHIT,NWDSEC
      INTEGER NSW (2),WIRE,LABEL,SEC,SID,MAXLAY
      INTEGER IPDTSG,IPDSEC,IPDTMW,NEVT (2),CDVTRK,NALNEV (2)
      INTEGER MAXSEC,IER,I,LUN,ERR,HMOD,MINAEV,RUNNUM,NVAVG (2)
      INTEGER CDALIT,NUMIT,OLDRUN,LHMOD,HHMOD,GZDTVA,LDTVA,IPDTVA
      INTEGER STATUS,SYS$ASCTIM,TKPASS (2),NSEGS,NWDTSG,J
      INTEGER LDTVH,GZDTVH
C
      REAL PHOFF0,X0,Y0,Y (2),DPHI,VGL (2),DVGL (2),VSC (2)
      REAL YHIT,XHIT,VEL,TIME,DRTIM (2,0:3),V (2),DELV (2)
      REAL X (2,0:3),VV,DVV,VAVG (2),DVAVG (2),SGV (2),SGDELV (2)
      REAL B01,B10,B12,B21,B30,B31,B32,B20,A1,A3,AA,BB,CC,DD,EE,FF,DEN
      REAL ANOCUT,CATCUT,RCELL,YCHI (0:3)
C
      CHARACTER MESG*60,CRUN*7,ASCTIM*23
C
      LOGICAL FIRST,NOVDV (2),EZERROR,LSEGFL,EACRUN
C
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDALNV',
     &      'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('CDVTRK',CDVTRK,ERR)
        CALL EZGET_i('CDALIT',CDALIT,ERR)
        CALL EZGET('ANOCUT',ANOCUT,ERR)
        CALL EZGET('CATCUT',CATCUT,ERR)
        CALL EZGET_l('EACRUN',EACRUN,ERR)
        CALL EZRSET
        LLDRFT = GZDRFT()             !Pointer to DRFT
        PHOFF0 = C(LLDRFT+9)*PI/180.  !Half opening angle of drift cell
        MAXLAY = IC (LLDRFT+1)-1      !# of active layers
        MAXSEC = IC (LLDRFT+2)-1      !# of active sectors per layer
        DO 5 LAYER = 0, MAXLAY
          RCELL = C (LLDRFT+11+2*LAYER)
          YCHI (LAYER) = RCELL*TAN (PHOFF0)-CATCUT
    5   CONTINUE
        NUMIT  = 1
        NALNEV (1) = 0
        NALNEV (2) = 0
        OLDRUN = 0
      END IF
C
      RUNNUM = IQ (LHEAD+6)
      IF ((EACRUN).AND.RUNNUM.NE.OLDRUN) THEN
        OLDRUN = RUNNUM
        IF(NUMIT.GT.CDALIT) THEN
          NUMIT  = 1
          NALNEV (1) = 0
          NALNEV (2) = 0
        ENDIF
      ELSE
        IF (NUMIT.GT.CDALIT) GOTO 999 ! If iteration is > # requested return.
      END IF
C
      IF (NALNEV (1).EQ.0) THEN
        CALL VZERO (V,2)
        CALL VZERO (DELV,2)
        CALL VZERO (SGV,2)
        CALL VZERO (SGDELV,2)
        CALL VZERO_i (NEVT,2)
      END IF
C
      LLDTRH = GZDTRH()           !Pointer to DTRH
      IF (LLDTRH.EQ.0) GOTO 999
      NTRAK  = IQ (LLDTRH+2)      !# of tracks in events
      IF (NTRAK.EQ.0) GOTO 999
      LLDRFT = GZDRFT()           !Pointer to DRFT bank
C
      TKPASS (1) = 0
      TKPASS (2) = 0
      DO 10 TRK = 1, NTRAK
        LLDTRK = GZDTRK(TRK)       !Pointer to TRKth track in DTRK
        LSEGFL = .FALSE.           !Flag signaling at least 127 segments
C- Select only 4 segment tracks
        DO 15 LAYER = 0, MAXLAY
          STBIT = 4+7*LAYER
          SEGNUM (LAYER)= IBITS (IQ (LLDTRK+1),STBIT,7)   !Segment # in LAYER
          IF (SEGNUM (LAYER).EQ.0) GOTO 10                !for track # TRK
          IF (SEGNUM (LAYER).EQ.127) LSEGFL = .TRUE.
   15   CONTINUE
C
C- Check if segment number >= 127 (7 bit maximum) and update SEGNUM
        IF (LSEGFL) THEN
          DO 16 LAYER = 0, MAXLAY
            IF (SEGNUM (LAYER).EQ.127) THEN
              LLDTSG = GZDTSG (LAYER)                    !Pointer to DTSG
              NSEGS  = IQ (LLDTSG+1)
              NWDTSG = IQ (LLDTSG+2)
              LLDTSG = LLDTSG+2+(SEGNUM(LAYER)-1)*IQ (LLDTSG+2)
              IF (IQ (LLDTSG+1).NE.TRK) THEN
                DO 17 J = 128, NSEGS
                  LLDTSG = LLDTSG+NWDTSG
                  SEGNUM (LAYER) = SEGNUM (LAYER)+1
                  IF (IQ (LLDTSG+1).EQ.TRK) GOTO 16
   17           CONTINUE
              END IF
            END IF
   16     CONTINUE
        END IF
C
        DO 20 LAYER = 0, MAXLAY
          LLDTSG = GZDTSG (LAYER)                    !Pointer to DTSG
C- Pointer to SEGNUM(I)th segment in DTSG
          LLDTSG = LLDTSG+2+(SEGNUM(LAYER)-1)*IQ (LLDTSG+2)
          X0 = Q (LLDTSG+3)             !X coord of segment NC point
          Y0 = Q (LLDTSG+4)             !Y coord of segment NC point
C- Return sector and side of SW plane of segment NC point.
          CALL DSGSEC (LAYER,X0,Y0,SECTOR,SIDE)
          LAYSEC (LAYER) = SECTOR
          LAYSID (LAYER) = SIDE
   20   CONTINUE
C
C- Return if segments are not contained within a half module of the CDC
        DO 25 LAYER = 0, 1
          IF (LAYSEC (LAYER).NE.LAYSEC (LAYER+2)) GOTO 10
          IF (LAYSID (LAYER).NE.LAYSID (LAYER+2)) GOTO 10
   25   CONTINUE
        IF (ABS (LAYSID (0)+LAYSID (2)-
     +        LAYSID (1)-LAYSID (3)).NE.2) GOTO 10
C
        NOVDV (1) = .FALSE.
        NOVDV (2) = .FALSE.
        CALL VZERO (X,8)
        CALL VZERO (DRTIM,8)
C- Initial Track selection criteria passed
        DO 30 LAYER = 0, MAXLAY
          LLDTSG = GZDTSG (LAYER)                    !Pointer to DTSG
          IF (LLDTSG.EQ.0) GOTO 10
          LLDSEC = GZDSEC (LAYSEC (LAYER),LAYER)     !Pointer to DSEC
          IF (LLDSEC.EQ.0) GOTO 10
C- # of words in hit block for DSEC
          NWDSEC = IQ (LLDSEC+3)
          LDTVA  = GZDTVA (LAYER)
C- Pointer to SEGNUM(I)th segment in DTSG
          IPDTSG  = LLDTSG+2+(SEGNUM(LAYER)-1)*IQ (LLDTSG+2)
C- Pointer start of wire label block
          IPDTSG = IPDTSG+9
C
          NSW (1) = 0
          NSW (2) = 0
          Y (1)   = 0.
          Y (2)   = 0.
          DO 35 WIRE = 0, MXSENS
            LABEL = IQ (IPDTSG+WIRE)
            IF (LABEL.EQ.0) GOTO 35
            SEC   = IBITS (LABEL,11,5)         !Sector of SW hit
            SID   = IBITS (LABEL,0,1)          !Side of SW hit (0 = +, 1 = -)
C- Cut if all SW hits not contained in same half cell
            IF (SEC.NE.LAYSEC (LAYER)) GOTO 10
            IF (SID.NE.LAYSID (LAYER)) GOTO 10
            NHIT  = IBITS (LABEL,1,7)          !Hit number in DSEC
C- Pointer to 1st hit for WIRE in DSEC
            IPDSEC = LLDSEC+IQ (LLDSEC+4+IQ (LLDSEC+2)+WIRE)
C- Pointer to +/- side dependant drift velocity bank DTVA for WIRE
            IPDTVA  = LDTVA+IC (LDTVA+2)*(IC (LDTVA+1)*SEC+WIRE)+2
C- Pointer to NHITth hit for WIRE in DSEC
            IPDSEC = IPDSEC+NWDSEC*(NHIT-1)
C
            YHIT   = Q (IPDSEC+2+SID)          !Drift distance in cell frame
C- Cut track if a hit is too close to anode or cathode
            IF (ANOCUT.GT.0..AND.ABS(YHIT).LT.ANOCUT) GOTO 10
            IF (CATCUT.GT.0..AND.ABS(YHIT).GT.YCHI (LAYER)) GOTO 10
C
            XHIT = C (LLDRFT+19+WIRE)          !X position of hit in cell frame
            VEL  = C (IPDTVA+1+SID)            !+/- dependent drift velocity
            TIME   = ABS (YHIT)/VEL            !Drift time
C- Sum drift times and positions for inner and outer wires
            I = 1
            IF (WIRE.EQ.0.OR.WIRE.EQ.MXSENS) I = 2
            NSW (I)  = NSW (I)+1
            X (I,LAYER) = X (I,LAYER)+XHIT
            Y (I)  = Y (I)+YHIT
            DRTIM (I,LAYER) = DRTIM (I,LAYER)+TIME
   35     CONTINUE
C
          DO 36 I = 1, 2                       ! 1 = inner / 2 = outer
            IF (NSW (I).NE.0) THEN
C- Avg hit X,Y and drift time in sector 0 LAYER cell
              X (I,LAYER) = C (LLDRFT+11+2*LAYER)+X (I,LAYER)/
     +                    FLOAT (NSW (I))
              Y (I) = Y (I)/FLOAT (NSW (I))
              DRTIM (I,LAYER) = DRTIM (I,LAYER)/FLOAT (NSW (I))
            ELSE
              NOVDV (I) = .TRUE.
            END IF
   36     CONTINUE
C
C- Transform X coordinates from layer 1, layer 3 frame to layer 0, layer 2 frame
          IF (LAYER.EQ.1.OR.LAYER.EQ.3) THEN
            DPHI = PHOFF0
            IF (LAYSID (LAYER).EQ.1) DPHI = -DPHI   !If minus side hits
            X (1,LAYER) = X (1,LAYER)*COS (DPHI)+Y (1)*SIN (DPHI)
            X (2,LAYER) = X (2,LAYER)*COS (DPHI)+Y (2)*SIN (DPHI)
          END IF
C
   30   CONTINUE
C
C- Skip if no inner wire track
        IF (NOVDV (1)) GOTO 10
C
        DO 37 I = 1, 2
C
          IF (NOVDV (I)) GOTO 37
          TKPASS (I) = TKPASS (I)+1       !Count # of tracks for ISW and OSW
C- Begin V,delta V calculations for inner sense wires
          B01 = X (I,0)-X (I,1)
          B10 = -B01
          B12 = X (I,1)-X (I,2)
          B20 = X (I,2)-X (I,0)
          B21 = -B12
          B30 = X (I,3)-X (I,0)
          B31 = X (I,3)-X (I,1)
          B32 = X (I,3)-X (I,2)
          A1  = X (I,1)*TAN (PHOFF0)
          A3  = X (I,3)*TAN (PHOFF0)
C
          AA = DRTIM (I,0)*B31+(DRTIM (I,1)*B30+DRTIM (I,3)*B01)/
     +       COS (PHOFF0)
          BB = 1.5*DRTIM (I,0)*B31+(.5*DRTIM (I,1)*B30+1.5*DRTIM (I,3)
     &      *B10)/
     +      COS (PHOFF0)
          CC = DRTIM (I,2)*B31+(DRTIM (I,1)*B32+DRTIM (I,3)*B21)/
     +       COS (PHOFF0)
          DD = -.5*DRTIM (I,2)*B31+(.5*DRTIM (I,1)*B32+1.5*DRTIM (I,3)
     &      *B12)/
     +      COS (PHOFF0)
          EE = A1*B30+A3*B01
          FF = A1*B32+A3*B21
C
C- Find <V>, Delta V for event for inner sense wires and store values
C- and statistics.
          DEN = DD*AA-CC*BB
          IF (DEN.NE.0.) THEN
            NEVT (I)        = NEVT (I)+1
            VV              = (EE*DD-BB*FF)/DEN
            DVV             = (FF*AA-CC*EE)/DEN
            V (I)           = V (I)+VV
            SGV (I)         = SGV (I)+VV**2
            DELV (I)        = DELV (I)+DVV
            SGDELV (I)      = SGDELV (I)+DVV**2
          END IF
C
   37   CONTINUE
C
   10 CONTINUE
C
C- Total # of events counter for ISW and OSW tracks
      NALNEV (1) = NALNEV (1)+TKPASS (1)
      NALNEV (2) = NALNEV (2)+TKPASS (2)
C
C- If requested # of events then update STP
      IF (NALNEV (2).GE.CDVTRK) THEN
        NALNEV (1) = 0
        NALNEV (2) = 0
C
        WRITE (MESG,2000) NUMIT
 2000   FORMAT (' Completed CDC Velocity Alignment Iteration ',I2)
        CALL INTMSG (MESG)
C
        NUMIT = NUMIT+1           !next iteration number
C
C- Determine global velocity averages and sigmas
        DO 40 I = 1, 2
          IF (NEVT (I).GT.0) THEN
            V (I)    = V (I)/FLOAT (NEVT(I))
            DELV (I) = DELV (I)/FLOAT (NEVT(I))
            SGV (I)  = SGV (I)/FLOAT (NEVT(I))-V (I)**2
            IF (SGV (I).GE.0) THEN
              SGV (I) = SQRT (SGV (I))
            ELSE
              SGV (I) = 0.
            END IF
            SGDELV (I) = SGDELV (I)/FLOAT (NEVT(I))-DELV (I)**2
            IF (SGDELV (I).GE.0) THEN
              SGDELV (I) = SQRT (SGDELV (I))
            ELSE
              SGDELV (I) = 0.
            END IF
          END IF
   40   CONTINUE
C
        WRITE (MESG,2001) V(1)*10000.,V(2)*10000.
 2001   FORMAT (' CDC Global Velocity ISW and OSW     ',2(F5.2,2X))
        CALL INTMSG (MESG)
        WRITE (MESG,2002) DELV(1)*10000.,DELV(2)*10000.
 2002   FORMAT (' CDC Global delta Velocity ISW and OSW ',2(F5.3,2X))
        CALL INTMSG (MESG)
C
C- Extract current STP global velocity and delta velocity
        LDTVH = GZDTVH ()
        VGL (1)  = C (LDTVH+1)
        VGL (2)  = C (LDTVH+2)
        DVGL (1) = C (LDTVH+3)
        DVGL (2) = C (LDTVH+4)
C
C- Substitute with current global velocity and delta velocity
        C (LDTVH+1) = V (1)
        C (LDTVH+2) = V (2)
        C (LDTVH+3) = DELV (1)
        C (LDTVH+4) = DELV (2)
C
C- Now update STP drift velocities
        DO 60 LAYER = 0, MAXLAY
C- Pointer to bank DTVA containing +,- SW drift velocities
          LDTVA = GZDTVA (LAYER)
C- Scale factor multiplying the existing STP DTVA velocities to account
C- for changes in global drift velocity
          VSC (1) = (V (1)+(1.5-FLOAT (LAYER))*DELV (1))/
     +           (VGL (1)+(1.5-FLOAT (LAYER))*DVGL (1))
          VSC (2) = (V (2)+(1.5-FLOAT (LAYER))*DELV (2))/
     +           (VGL (2)+(1.5-FLOAT (LAYER))*DVGL (2))
C
          DO 65 SECTOR = 0, MAXSEC
            LHMOD = 2*SECTOR                           !Low 1/2 module in sector
            IF (LAYER.EQ.0.OR.LAYER.EQ.2) LHMOD = LHMOD-1
            IF (LHMOD.LT.0) LHMOD = 2*MAXSEC+1
            HHMOD = LHMOD+1                            !High 1/2 module in sector
            IF (HHMOD.GT.2*MAXSEC+1) HHMOD = 0
C
            IPDTVA = LDTVA+IC (LDTVA+1)*IC (LDTVA+2)*SECTOR+2
C
            DO 75 WIRE = 0, MXSENS
              I = 1
              IF (WIRE.EQ.0.OR.WIRE.EQ.MXSENS) I = 2
C- Insert side dependant velocities into DTVA
C- + side velocity
              C (IPDTVA+1) = C (IPDTVA+1)*VSC (I)
              C (IPDTVA+2) = C (IPDTVA+2)*VSC (I)
C- - side velocity
              IPDTVA = IPDTVA+IC (LDTVA+2)
   75       CONTINUE
C
   65     CONTINUE
   60   CONTINUE
C
      END IF
C
  999 RETURN
      END
