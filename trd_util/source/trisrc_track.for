      SUBROUTINE TRISRC_TRACK(ITR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Finds the number of reconstructed tracks
C-                        in a cylinder defined by RIN,ZIN,
C-                        ROUT,ZOUT. Adapted from TRISTR.
C-
C-   Inputs  : ITR      =     0    : All tracks (in the sense of
C-                                     option OPT)
C-                      >     0    : Pointer to selected track
C-   Outputs :
C-
C-   Created   4-OCT-1989   J.Fr. Glicenstein
C-   Updated   6-SEP-1990   A. Zylberstejn  Simplify and flag the selected
C-                                          track
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:izzfit.LINK'
      INCLUDE 'D0$LINKS:izztrk.LINK'
      INCLUDE 'D0$LINKS:izztmp.LINK'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER CAS,IER,NERR,NEVOLD,OFFS,IFOIS
      INTEGER I,IDIN,IVERT,ITR
      INTEGER IQTRAK(8),INDICE,IQVTRK(21),LZFIT,LZTRH
      INTEGER IKK1,IKK2,IUCOMP,IZ,LOUT,TRUNIT
      INTEGER LOC,NGOOD,IGTRAC,IDENT,ICDI,IVTI,IFDI
      INTEGER GZZTRH,IVT(NTOTTR),ICD(NTOTTR),IFD(NTOTTR)
      INTEGER NVT,NDT,KG,IDUM,NFT
      REAL CT,VIN(6),VOUT(6),PHI,ST,TETA,THETA,QVTRK(21)
      REAL PHILO,PHIHI,TOLPHI
      REAL QTRAK(8)
      EQUIVALENCE (QVTRK(1),IQVTRK(1)), (QTRAK(1),IQTRAK(1))
      REAL PHII(100),TETAI(100),RGI,ZGI
      INTEGER IDI
      LOGICAL DOPRINT,FIRST,TRD_DO_PRINT
      DATA PHILO,PHIHI,TOLPHI/0.,6.2832,0.0736/
      DATA NERR/0/,IFOIS/0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        NEVOLD=0
        CALL EZGET('THMIN',THMIN,IER)
        CALL EZGET('THMAX',THMAX,IER)
        CALL EZRSET
      END IF
      IFOIS=IFOIS+1
      DOPRINT=TRD_DO_PRINT()
      IF(IQ(LHEAD+9).NE.NEVOLD)THEN ! do the following only once per event
        NEVOLD=IQ(LHEAD+9)
        KG=0
        NVT=0
        NDT=0
        NFT=0
      END IF
      ICDI=0
      IVTI=0
      IFDI=0
      IKK1      =  0
      IKK2      =  0
      NGOOD     =  0
      LZFIT=LQ(ITR-IZZFIT)
      IF(DOPRINT)
     +  WRITE(LOUT,2041)ITR,LZFIT
 2041 FORMAT(' enter TRISRC_TRACK with track',I8,
     &  ' lzfit',I8,/,' ----- ------------')
      IF(LQ(ITR-6).LE.0 .AND. LQ(ITR-7).LE.0 .AND. LQ(ITR-8).LE.0)
     &  GO TO 999
      TETA     =  Q(LZFIT+13)
      THETA=TETA*RADDEG
      IF(THETA.LT.THMIN .OR. THETA.GT.THMAX)GO TO 999
      CALL GTZTRK_LINK(ITR,QTRAK)
      IVTI=IQTRAK(2) ! id of VTX track
      ICDI=IQTRAK(3)! id of CDC track
C      QVTRK(6)=Q(LZFIT+10)
C      PHILO=AMAX1(QVTRK(6)-TOLPHI,0.)
C      PHIHI=AMIN1(QVTRK(6)+TOLPHI,6.2832)
C      IF (DOPRINT) WRITE(LOUT,1001)IVTI,ICDI, PHILO,PHIHI
C      IF(IVTI+ICDI.LE.0)RETURN
      LZTRH=GZZTRH()
      IF(LZTRH.LE.0)THEN
        CALL ERRMSG('Cant find bank ZTRH ','in TRISRC_TRACK',' ','W')
        GO TO 999
      END IF
      VIN(1)=Q(LZFIT+11)
      VIN(2)=Q(LZFIT+12)
      ZGI=Q(LZFIT+15)
      VIN(3)=ZGI
      VIN(4)=Q(LZFIT+20)
      VIN(5)=Q(LZFIT+22)
      VIN(6)=Q(LZFIT+24)
C  Check if track crosses the TRD
C ****  Look at the entrance point
      CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
      IF ( IGTRAC .NE. 0 .OR. ABS(VOUT(3) ) .GT. ZMIN )THEN
        IF(DOPRINT)WRITE(LOUT,*)' track does not cross TRD'
        GO TO 999
      END IF
      OFFS=0
      DO 82 CAS=1,2
        IF(CAS.NE.1 .AND. NGOOD.LE.0)GO TO 999
        IZ=IZZTRK
        IF(CAS.EQ.2)IZ=IZZTMP
        LOC=LQ(LZTRH-IZ)
        IF(LOC.LE.0 )THEN
          NERR=NERR+1
          IF(NERR.LT.20 .AND. DOPRINT)WRITE(LOUT,*)
     &      ' PROBLEM_TRD IN TRISRC_TRACK: NO TRACK FOR CASE',  CAS
          GO TO 80
        END IF
        DO 40 INDICE=1,1000
          IDIN=0
          LZFIT=LQ(LOC-IZZFIT)
          IF(LZFIT.LE.0)THEN
            IF(DOPRINT)
     +        WRITE(LOUT,*)' LZFIT=0 POUR CAS=',CAS
            GO TO 38
          END IF
          CALL GTZTRK_LINK(LOC,QTRAK)
          CALL SBIT1(IDIN,7)
          TETA     =  Q(LZFIT+13)
          THETA=TETA*RADDEG
          PHI      = Q(LZFIT+10)
C          PRINT1004,CAS,INDICE,IQTRAK(2),IQTRAK(3),TETA,PHI
C          WRITE(LOUT,1004)CAS,INDICE,IQTRAK(2),IQTRAK(3),TETA,PHI
          IF(DOPRINT)
     +      WRITE(LOUT,1004)CAS,INDICE,IQTRAK(2+OFFS),IQTRAK(3+OFFS),
     +      IQTRAK(4+OFFS),TETA,PHI
 1004     FORMAT(' CAS',I2,' INDICE',I2, ' IQTRAK',3I3,
     &      ' THETA,PHI',2G10.4)
          IF( THETA.LT.THMIN .OR. THETA.GT.THMAX
C     +        PHI  .LT.PHILO .OR.   PHI.GT.PHIHI
     +        )THEN
            GO TO 38
          END IF
C Reject IF VTX track already used
          IF(LOC.EQ.ITR)GO TO 8
          IF(NVT.GE.1 .AND. IQTRAK(2+OFFS).NE.0)THEN
            IF(IUCOMP(IQTRAK(2+OFFS),IVT,NVT).NE.0)THEN
              IF(DOPRINT)
     +          WRITE(LOUT,*)' TRACK REJECTED BECAUSE VERTEX TRACK',
     &          IQTRAK(2+OFFS),' ALREADY USED USED '
              GO TO 38
            END IF
          END IF
C Reject if CDC track already used
          IF(NDT.GE.1 .AND. IQTRAK(3+OFFS).NE.0)THEN
            IF(IUCOMP(IQTRAK(OFFS+3),ICD,NDT).NE.0)THEN
              IF(DOPRINT)
     +          WRITE(LOUT,*)' TRACK REJECTED BECAUSE CDC TRACK',
     &          IQTRAK(OFFS+3),' ALREADY USED '
              GO TO 38
            END IF
          END IF
C Reject if track with same phi already used
          IF(KG.GE.1)THEN
            DO 6 I=1,KG
              IF(ABS(PHI-PHII(I)).GT.0.001) GO TO 6
              IF (DOPRINT)THEN
                WRITE(LOUT,2044)CAS,INDICE, ABS(PHI-PHII(I)),I
 2044           FORMAT( ' CAS,INDICE',I2,I4, ' TRACK REJECTED,DPHI',
     &            G10.4, ' for previous track',I3)
              END IF
              GO TO 38
    6       CONTINUE
          ENDIF
    8     CONTINUE
          VIN(1)=Q(LZFIT+11)
          VIN(2)=Q(LZFIT+12)
          RGI=Q(LZFIT+14)
          ZGI=Q(LZFIT+15)
          IDI=IDIN+INDICE*128
C  Set bit 4 to one if track corresponds to "selected" one
          IF(LOC.EQ.ITR)THEN
            CALL SBIT1(IDI,4)
            IF(DOPRINT)WRITE(LOUT,*)' CANDIDATE TRACK: SET BIT 4 TO ONE'
          END IF
          VIN(3)=ZGI
          VIN(4)=Q(LZFIT+20)
          VIN(5)=Q(LZFIT+22)
          VIN(6)=Q(LZFIT+24)
C  Check if track crosses the TRD
C ****  Look at the entrance point
          CALL EXTCYL ( VIN, VOUT, RMIN, IGTRAC )
          IF ( IGTRAC .NE. 0 .OR. ABS(VOUT(3) ) .GT. ZMIN )THEN
            GO TO 38
          END IF
          IF(IQTRAK(OFFS+2).NE.0)THEN
            NVT=NVT+1
            IVT(NVT)=IQTRAK(OFFS+2)
          END IF
          IF(IQTRAK(OFFS+3).NE.0)THEN
            NDT=NDT+1
            ICD(NDT)=IQTRAK(OFFS+3)
          END IF
          IF(IQTRAK(OFFS+4).NE.0)THEN
            NFT=NFT+1
            IFD(NFT)=IQTRAK(OFFS+3)
          END IF
          IVERT    =  1
C
C ****  This is a good track : stores its characteristics
C
          KG=KG+1
          PHII(KG)      =  PHI
          IF(DOPRINT)WRITE(LOUT,3062)
     +      CAS,INDICE,KG, IQTRAK(OFFS+2),IQTRAK(OFFS+3),IQTRAK(OFFS+4)
 3062     FORMAT( ' CAS',I2,'INDICE',I4,' GOOD TRACK ',I3,
     &      'v track',I4,'d track',I4,' f track',I4)
          NGOOD           =  NGOOD + 1
          IF (NGOOD.GT.NTOTTR) THEN
            WRITE (LOUT,*) '*******   WARNING  ************'
            WRITE (LOUT,*) '   TOO MANY TRACKS IN TRD !!!!'
            WRITE (LOUT,*) '   SET TO',NTOTTR,'TRACKS'
            NGOOD = NTOTTR
            GOTO 100
          ENDIF
          IDENT  = IDI
          CALL TRDFIL(VIN,PHI,IDENT)
C      IF(IDENT.EQ.-1000)GO TO 100
   38     LOC=LQ(LOC)
          IF(LOC.LE.0)GO TO 80
   40   CONTINUE
   80   CONTINUE
        OFFS=1
   82 CONTINUE
  100 CONTINUE
  999 CONTINUE
C      IF(NVT.GT.0)CALL VZERO(IVT,NVT)
C      IF(NDT.GT.0)CALL VZERO(ICD,NDT)
      RETURN
 1001 FORMAT (' In TRISRC,ivti,icdi',2I3, ' philo,phihi',2G10.4)
      END
