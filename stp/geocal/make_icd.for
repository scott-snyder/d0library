      SUBROUTINE MAKE_ICD(LUNICD,LUNSET)
C
C***************************************************************
C
C     PURPOSE:
C      Create SRCP data file to describe
C      Geometry for the scintillator rings to correct for
C      energy lost in the cryostat walls.
C      The rings cover 0.1 units in eta and 2*pi/64 radians
C      in phi.  The name of the ring contains 10 times the
C      eta value of the lower eta edge of the tower.
C
C     INPUT: LUNICD OUTPUT UNIT NUMBER FOR ICD GEOMETRY FILE
C
C     OUTPUT: None
C
C     CREATED BY Chip Stewart 9 Jan 1889
C     Based on GESCIN routine WRITTEN by Z. Wolf,  Nov. 2, 1988
C
C****************************************************************
C
      IMPLICIT NONE
      REAL INCH_CM
      INTEGER IZ,IETA,I
C  MIXTURE STUFF
      INTEGER MIX(11),NMIX,NMED,NPART,P1,P2
      REAL MIX1,MIX2,DENS
      CHARACTER*4 MIXNAM(3)
      EQUIVALENCE (MIX(1),NMIX),(MIX(2),MIXNAM(1)),(MIX(5),NMED)
      EQUIVALENCE (MIX(6),NPART),(MIX(7),P1),(MIX(8),P2)
      EQUIVALENCE (MIX(9),MIX1),(MIX(10),MIX2),(MIX(11),DENS)
C ETA RANGE
      INTEGER NETA(2)
C DETECTOR STUFF
      INTEGER CDET(84)
      INTEGER           NWHI, NWDI, ITRS, NHMAX, NSET, IDTYPE, NV
      INTEGER           NBITSV, NH, NBITSH, ND, NBITSD, IPOINT
      CHARACTER*4       NAMESV, NAMESH, NAMESD,IUSET, ADET(84)
      REAL              ORIG, FACT,RDET(84)
C
      EQUIVALENCE (CDET(1),IUSET),(CDET(2),NWHI),(CDET(3),NWDI)
      EQUIVALENCE (CDET(4),ITRS),(CDET(5),NHMAX),(CDET(6),NSET)
C
      EQUIVALENCE (CDET(1),RDET(1),ADET(1))
C
C CONE POSITIONING
      INTEGER CDAT(16),NROT,NCOPY,NPAR
      REAL POS(3),SHAPE(5)
      CHARACTER*4 NAME,TYPE,MOTH,PTYP
      EQUIVALENCE (CDAT(1),NAME),(CDAT(2),TYPE),(CDAT(4),MOTH)
      EQUIVALENCE (CDAT(5),PTYP),(CDAT(6),NROT),(CDAT(7),NCOPY)
      EQUIVALENCE (CDAT(8),POS(1)),(CDAT(11),NPAR),(CDAT(12),SHAPE(1))
C
C CONE DIVISIONS
C
      INTEGER CDIV(10),NDIV,NAXIS,NSTEP,NMAX
      REAL PHI0
      CHARACTER*4 DIVNAM,DNAME,DTYPE
      EQUIVALENCE (CDIV(1),DIVNAM),(CDIV(4),DNAME),(CDIV(5),DTYPE)
      EQUIVALENCE (CDIV(6),NDIV),(CDIV(7),NAXIS),(CDIV(8),PHI0)
      EQUIVALENCE (CDIV(9),NSTEP),(CDIV(10),NMAX)
C
C ****  VARIABLE NAMES
C
      INTEGER LUNICD,LUNSET
      CHARACTER*4 MV
      CHARACTER SCNARR*32,SCNNAM*4,DETNAME*16
      CHARACTER DIVARR*32,ZEE(2)*1
      DATA ZEE/'+','-'/
C
C-------------------------------------------------------------
C
C GET DATA FROM SRCP
      CALL GTSRCP_i('ICD_MIXTURES',MIX,1)
      CALL GTSRCP_i('ETA_RANGE',NETA,1)
      CALL GTSRCP('INCH_TO_CM',INCH_CM,1)
C
C ****    DEFINE SCINTILLATOR MATERIAL
C
      WRITE(LUNICD,3)'ICD_MIXTURES',NMIX,
     &      MIXNAM,NMED,NPART,P1,P2,MIX1,MIX2,DENS
C
C ****    DEFINE DETECTOR SET FOR ETA CONES
C
      DO 100 IZ = 1, 2
        WRITE(DETNAME,210)ZEE(IZ)
        CALL GTSRCP_i(DETNAME,CDET,1)
        WRITE(LUNSET,5)DETNAME,
     &  IUSET,NWHI, NWDI, ITRS, NHMAX, NSET
        DO 90 IETA = NETA(1),NETA(2)
          IPOINT = 13*( IETA - NETA(1) ) 
          NAMESV = ADET ( 7 + IPOINT )
          IDTYPE = CDET ( 8 + IPOINT )
          NV     = CDET ( 9 + IPOINT )
          NAMESV = ADET (10 + IPOINT )
          NBITSV = CDET (11 + IPOINT )
          NH     = CDET (12 + IPOINT )
          NAMESH = ADET (13 + IPOINT )
          NBITSH = CDET (14 + IPOINT )
          ORIG   = RDET (15 + IPOINT )
          FACT   = RDET (16 + IPOINT )
          ND     = CDET (17 + IPOINT )
          NAMESD = ADET (18 + IPOINT )
          NBITSD = CDET (19 + IPOINT )
          WRITE(LUNSET,6)
     &      NAMESV,IDTYPE,NV,NAMESV,
     &      NBITSV, NH, NAMESH, NBITSH,
     &      ORIG, FACT, ND, NAMESD, NBITSD
   90   CONTINUE
        WRITE(LUNSET,7)
  100 CONTINUE
C
C ****    DEFINE LOW ETA CONES, POSITION, DEVIDE IN PHI
C
      DO IZ = 1, 2
        DO IETA = NETA(1),NETA(2)
          WRITE(SCNARR,110)IETA,ZEE(IZ)
          WRITE(SCNNAM,111)IETA,ZEE(IZ)
          WRITE(DIVARR,112)IETA,ZEE(IZ)
          WRITE(DIVNAM,113)IETA,ZEE(IZ)
C
C ****  GET SRCP RAW NUMBERS
C
          CALL GTSRCP_i(SCNARR,CDAT,1)
          CALL GTSRCP_i(DIVARR,CDIV,1)
C
C ****  CONVERSION TO CM FROM INCHES
C
          DO I = 1, NPAR
            SHAPE(I) = SHAPE(I)*INCH_CM
          END DO
          DO I = 1, 3
            POS(I) = POS(I)*INCH_CM
          END DO
          WRITE(LUNICD,1)SCNARR,
     &      SCNNAM,TYPE,NMED,MOTH,PTYP,
     &      NROT, NCOPY, POS, NPAR, SHAPE

          WRITE(LUNICD,2)DIVARR,
     &      DIVNAM,TYPE,NMED,DNAME,DTYPE,
     &      NDIV,NAXIS,PHI0,NSTEP,NMAX
        END DO
      END DO
    1 FORMAT(1X,'\ARRAY  ',A32,
     & /2X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,'''  ''',A4,'''',
     & /2X,2I8,3F12.4,I8,
     & /2X,5F12.4,
     & /1X,'\END')
    2 FORMAT(1X,'\ARRAY  ',A32,
     & /2X,'''',A4,'''  ''',A4,'''',5X,I3,5X,'''',A4,'''  ''',A4,'''',
     & /2X,2I10,F12.4,2I10,
     & /1X,'\END')
    3 FORMAT(1X,'\ARRAY  ',A16,
     & /2X,I10,3(2X,'''',A4,''''),5X,2I10,
     & /2X,2I10,3F12.4,
     & /1X,'\END')
    5 FORMAT(1X,'\ARRAY  ',A16,
     & /2X,'''',A4,'''',5X,5I5)
    6 FORMAT(2X,'''',A4,''' ',2I4,' ''',A4,''' ',2I4,
     & ' ''',A4,'''',I4,2F6.0,I4,' ''',A4,'''',I4)
    7 FORMAT(1X,'\END')
  110 FORMAT('ICD_ETA_',I2.2,A1)
  111 FORMAT('S',I2.2,A1)
  112 FORMAT('ICD_DIV_',I2.2,A1)
  113 FORMAT('D',I2.2,A1)
  210 FORMAT('IUSET_ICD',A1,'Z')
      RETURN
      END
