C----------------------------------------------------------------------
      SUBROUTINE ESUM_COMPRESS(IQESUM,IQESUMC,LOK)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Pack and unpack the ESUM and compress ESUM
C-     data.  The compression is done by limited occurances of each 
C-     object type to NOBJECT_MAX and by mapping the ET, PHI and ETA
C-     values onto a fixed set of possibilities.  The resolution
C-     of the compressed values is in RESOLUTION in 'natural' units
C-     (eg, GeV for the ET value)
C-
C-   Inputs  : IQESUM  - Array of ESUM bank (usually IQ(LESUM+1))
C-   Outputs : IQESUMC - Array of compressed ESUM bank data (all ints)
C-   Controls:
C-
C-   Created  13-Oct-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IQESUM(*),IQESUMC(*)
      LOGICAL LOK
C
      INTEGER IESUM_OBJ(6)
      REAL    RESUM_OBJ(6)
      EQUIVALENCE(IESUM_OBJ,RESUM_OBJ)
C
      INTEGER NOBJECT_MAX,NTYPES_MAX,NFIXC,NRC
      CHARACTER*4 TYPE_NAMES(3),MYNAME*20
      REAL    XMIN(4),XMAX(4),RESOLUTION(4)      ! All objects except verts
      REAL    VXMIN(3),VXMAX(3),VRESOLUTION(3)   ! Vertices
      INTEGER NBINS(4),VNBINS(3)
C
      INTEGER I,J,NOBJECTS,ITYPE,IC,IOBJ,K,IET,IETAP,IETAD,IPHI,SIZE
      INTEGER ESUM_SIZE,NESUM_OBJS,VERSION,NFIX,NR,NFOUND,NTOTAL,ISTAT
      CHARACTER*4 BANK_NAME,ERRTXT*80
C
      INTEGER JBIT
C
C        NB: nbins=int(xmax-xmin)/resolution
C        scale_factor=nbins/(xmax-xmin) = 1/resolution
C        NFIXC=size of fixed length portion of compressed bank.
C        NRC=size of each object in compressed bank.
C
      SAVE NOBJECT_MAX,NTYPES_MAX,XMIN,XMAX,RESOLUTION,NBINS,NFIXC,NRC
      SAVE VXMIN,VXMAX,VRESOLUTION,VNBINS
      DATA NOBJECT_MAX/15/,NFIXC/4/,NRC/3/
      DATA NTYPES_MAX/3/,TYPE_NAMES/'TRGR','TR15','FILT'/
C
C                      et      eta   eta   phi
      DATA XMIN      /   0.0, -5.0, -5.0,  0.0   /
      DATA XMAX      /2000.0,  5.0,  5.0,  6.3   /  ! Leave a little extra
      DATA RESOLUTION/   0.1,  0.01, 0.01, 0.0063/
      DATA NBINS     /20000,   1000, 1000, 1000  /
C                           X     Y      Z
      DATA VXMIN      /   -2.5,  -2.5,  -100.0   /
      DATA VXMAX      /    2.5,   2.5,   100.0   /
      DATA VRESOLUTION/    0.005, 0.005,   0.5   /
      DATA VNBINS     /   1000,   1000,  1000    /
C
      ESUM_SIZE(NOBJECTS)=NRC*NOBJECTS+NFIXC
C-----------------------------------------------------------------------
      LOK=.FALSE.
      MYNAME='ESUM_COMPRESS'
C
C  Check TYPE
C
      ITYPE=0
      CALL UHTOC(IQESUM(30),4,BANK_NAME,4)
      DO I=1,NTYPES_MAX
        IF( BANK_NAME.EQ.TYPE_NAMES(I) ) ITYPE=I
      ENDDO
      IF( ITYPE.EQ.0 ) GOTO 999
C
C  Check version number of ESUM bank
C
      VERSION=IQESUM(1)
      WRITE(ERRTXT,*) 'ESUM Version = ',VERSION
      IF( VERSION.GT.15 ) CALL ERRMSG('BADVERSION',MYNAME,ERRTXT,'F')
C
C  Check size assumption for ESUM NFIX and NR
C
      NFIX=IQESUM(2)
      NR=IQESUM(3)
      IF( NFIX.NE.30 ) THEN
        WRITE(ERRTXT,*) 'Input ESUM has NFIX=',NFIX,' expect 30'
        CALL ERRMSG('UNKNOWN_NFIX',MYNAME,ERRTXT,'F')
      ENDIF
      IF( NR.NE.6 ) THEN
        WRITE(ERRTXT,*) 'Input ESUm has NR=',NR,' expect 6'
        CALL ERRMSG('UNKNOWN_NR',MYNAME,ERRTXT,'F')
       ENDIF
C
C  We can process this bank, so zero out space and proceed
C
      NOBJECTS=IQESUM(4)

C  Pack version number and bank type

      IQESUMC(1)=IAND(VERSION,15)+ISHFT(ITYPE,4)

C  Loop over each possible object type (except 24) and do compression/
C  storage...

      NTOTAL=IQESUM(4)
      IC=4
      DO I=0,23

C     Store object count for this type...

        NOBJECTS=IQESUM(5+I)
        IF( NOBJECTS.GT.NOBJECT_MAX ) THEN
          WRITE(ERRTXT,*) 'More than 15 objects of type ',I
          CALL ERRMSG('TRUNCATE_OBJECT',MYNAME,ERRTXT,'I')
          CALL SBIT1(IQESUMC(1),8+I)
          NOBJECTS=NOBJECT_MAX
        ENDIF

        K=I/8 + 2                                  ! Store eight counts/word
        J=4*MOD(I,8)                               ! Get position within word
        IQESUMC(K)=IQESUMC(K)+ISHFT(NOBJECTS,J)    ! Save the object count

C     Store each occurance of this object up to 15 occurances

        NFOUND=0
        J=0
        DO WHILE( J.LT.NTOTAL .AND. NFOUND.LT.NOBJECTS )
          J=J+1
          IOBJ=30 + 6*(J-1)
          IF( IQESUM(IOBJ+1).NE.I ) GOTO 10 ! Check object type

C       Found an object of appropriate type. Process it...
C       Use IESUM_OBJ,RESUM_OBJ to get around strong typing

          CALL UCOPY(IQESUM(IOBJ+1),IESUM_OBJ,6)
          NFOUND=NFOUND+1
          ISTAT=IESUM_OBJ(2)
          IF( I.EQ.0 ) THEN    !  Treat vertex differently.
            DO K=1,3  ! Fix range
              RESUM_OBJ(K+3)=MIN(MAX(RESUM_OBJ(K+3),VXMIN(K)),vXMAX(K))
            ENDDO
            IET=0
            IETAP=NINT((RESUM_OBJ(4)-VXMIN(1))/VRESOLUTION(1))
            IETAD=NINT((RESUM_OBJ(5)-VXMIN(2))/VRESOLUTION(2))
            IPHI=NINT((RESUM_OBJ(6)-VXMIN(3))/VRESOLUTION(3))
          ELSE
            DO K=1,4  ! Fix range
              RESUM_OBJ(K+2)=MIN(MAX(RESUM_OBJ(K+2),XMIN(K)),XMAX(K))
            ENDDO
            IET=NINT((RESUM_OBJ(3)-XMIN(1))/RESOLUTION(1))
            IETAP=NINT((RESUM_OBJ(4)-XMIN(2))/RESOLUTION(2))
            IETAD=NINT((RESUM_OBJ(5)-XMIN(3))/RESOLUTION(3))
            IPHI=NINT((RESUM_OBJ(6)-XMIN(4))/RESOLUTION(4))
          ENDIF
*
          IQESUMC(IC+1)=ISTAT
          IQESUMC(IC+2)=IET    ! We have 16 spare bits here. 
          IQESUMC(IC+3)=IETAP+ISHFT(IETAD,10)+ISHFT(IPHI,20)
          IC=IC+NRC 
 10     ENDDO
      ENDDO
C
      LOK=.TRUE.
C
      GOTO 999
C
C-----------------------------------------------------------------------
      ENTRY ESUM_UNCOMPRESS(IQESUMC,IQESUM,LOK)
C-----------------------------------------------------------------------
      LOK=.FALSE.
      MYNAME='ESUM_UNCOMPRESS'
C
      ITYPE=IAND(ISHFT(IQESUMC(1),-4),7)
      IF( ITYPE.LT.0 .OR. ITYPE.GT.NTYPES_MAX ) THEN
        WRITE(ERRTXT,*) 'Unknown compressed ESUM type: ',ITYPE
        CALL ERRMSG('UNKNOWN_COMPTYPE',MYNAME,ERRTXT,'E')
        GOTO 999
      ENDIF
C
      IQESUM(4)=0
      DO I=0,23
C     Check for overflow/underflow in storage and notify
        IF( JBIT(IQESUMC(1),8+I) ) THEN
          WRITE(ERRTXT,1001) I
 1001     FORMAT('Lost objects of type ',I2)
          CALL ERRMSG('OBJECT_OVERFLOW',MYNAME,ERRTXT,'I')
        ENDIF
        K=I/8 + 2                                  ! Store eight counts/word
        J=4*MOD(I,8)                               ! Get position within word
        IQESUM(5+I)=IAND(ISHFT(IQESUMC(K),-J),NOBJECT_MAX)
        IQESUM(4)=IQESUM(4)+IQESUM(5+I)
      ENDDO
C
C  Get version, structure numbers and type string
C
      IQESUM(1)=IAND(IQESUMC(1),15)
      IQESUM(2)=30
      IQESUM(3)=6
      CALL UCTOH(TYPE_NAMES(ITYPE),IQESUM(30),4,4)
C
C  Recover the objects for all types
C
      IOBJ=30
      IC=4
      DO I=0,23
      DO J=1,IQESUM(5+I)
        IET=IQESUMC(IC+2)
        IETAP=IAND(IQESUMC(IC+3),1023)
        IETAD=IAND(ISHFT(IQESUMC(IC+3),-10),1023)
        IPHI=IAND(ISHFT(IQESUMC(IC+3),-20),1023)
        IESUM_OBJ(1)=I                                   ! Object type
        IESUM_OBJ(2)=IQESUMC(IC+1)                       ! Status
        IF( I.EQ.0 ) THEN    !  Treat vertex differently.
          RESUM_OBJ(3)=0.0
          RESUM_OBJ(4)=FLOAT(IETAP)*VRESOLUTION(1)+VXMIN(1)
          RESUM_OBJ(5)=FLOAT(IETAD)*VRESOLUTION(2)+VXMIN(2)
          RESUM_OBJ(6)=FLOAT(IPHI)*VRESOLUTION(3)+VXMIN(3)
        ELSE
          RESUM_OBJ(3)=FLOAT(IET)*RESOLUTION(1)+XMIN(1)    ! ET
          RESUM_OBJ(4)=FLOAT(IETAP)*RESOLUTION(2)+XMIN(2)  ! ETA(PHYSICS)
          RESUM_OBJ(5)=FLOAT(IETAD)*RESOLUTION(3)+XMIN(3)  ! ETA(DETECTOR)
          RESUM_OBJ(6)=FLOAT(IPHI)*RESOLUTION(4)+XMIN(4)   ! PHI
        ENDIF
        CALL UCOPY(IESUM_OBJ,IQESUM(IOBJ+1),6)
        IC=IC+NRC
        IOBJ=IOBJ+6
      ENDDO
      ENDDO
C
      LOK=.TRUE.
      GOTO 999
C
C-----------------------------------------------------------------------
      ENTRY ESUM_COMPRESSED_SIZE(IQESUM,SIZE)
C-----------------------------------------------------------------------
      SIZE = NFIXC
      DO I=0,23
        SIZE=SIZE+NRC*IQESUM(5+I)
      ENDDO
C
  999 CONTINUE
      RETURN
      END
