      SUBROUTINE VTX_MARK_HITS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VCTH bank to hold links between tracks in
C-                 VTXT and hits in VCHT.  Set status bits in VCHT for all 
C-                 hits used in VTXT and/or ZTRK tracks. Only works for 
C-                 Version 1 or higher of VCHT! (Vers 0 does not have 
C-                 space for the status bits.)
C-   Inputs  : VTXT, VTTH, ZTRK and VCHT banks.
C-   Outputs : Filled VCTH bank; Status bits in VCHT bank.
C-   Controls:
C-
C-   Created  15-FEB-1994   Al Clark
C-   Updated  25-FEB-1994   Al Clark  Change status format; 
C-                                  include VCTH link bank.
C-   Updated  20-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LVTRH, GZVTRH, LVTXT, LVTTH, LZTRK
      INTEGER LVCHT, GZVCHT
      INTEGER LVCTH, GZVCTH
      INTEGER NH, IH, OLDSECID, OLDLAY, OLDWIRE
      INTEGER IADD, LAY, SEC, SECID, WIRE, HITNUM, WIRLAY
      INTEGER MAXSEC(0:2)
      INTEGER WIREX, WORD, NSKIP, III
      INTEGER POINTER(0:3*32-1)
      INTEGER VCHT_LENGTH, NHEAD, NWDSHT, OFFSET, OFFMAX, INCR
      INTEGER VCTH_LEN, N_VTXT
      INTEGER INDX, INDXN, INDXX, MASK
      INTEGER NSEC, SECTOR_HEADER
      INTEGER TSTAT1, TSTAT2
      INTEGER WORDCOUNT
      INTEGER VCHT_SEC(0:2), VCHT_MXWIR(0:2)
      INTEGER LLVCTH, LPVCTH, NHTSWD, MAX_VTXT, HITNMP
      INTEGER VTXT_NUM, TEMP, N_PACKED, PAK_POS
      INTEGER R_ZTK, L_ZTK, R_XY, L_XY, R_RZ, L_RZ
      LOGICAL MATCH, L_R, ON_ZTK, ON_XY, ON_RZ, FILL_VCTH, FIRST
      INTEGER VERS,MAX_VERS
      PARAMETER (MAX_VERS=1)
      INTEGER MATCH_BIT(0:MAX_VERS) , ADDR_OFFSET(0:MAX_VERS)
      INTEGER STAT_LENGTH(0:MAX_VERS) , STAT_OFFSET(0:MAX_VERS)
C
C ****  VCHT packing parameters: SECTOR HEAD WORD
C
      INTEGER COUNT_OFFSET, COUNT_LENGTH, NHVSEC_OFFSET, NHVSEC_LENGTH
      INTEGER SECID_OFFSET
      PARAMETER ( COUNT_OFFSET = 0 )
      PARAMETER ( COUNT_LENGTH = 13 )
      PARAMETER ( NHVSEC_OFFSET = 13 )
      PARAMETER ( NHVSEC_LENGTH = 11 )
      PARAMETER ( SECID_OFFSET = 24 )
C
C ****  VCHT packing parameters: HIT
C
      DATA MATCH_BIT    /31 , 31/
      DATA ADDR_OFFSET  /12 , 15/
      DATA STAT_LENGTH  / 2 ,  5/
      DATA STAT_OFFSET  /10 , 10/
C
C ****  VCHT Status parameters (all are relative in STATUS field)
C
      DATA R_ZTK /'0002'X/    ! For unmatched or 1st wd of matched
      DATA L_ZTK /'0004'X/
      DATA R_XY  /'0008'X/
      DATA L_XY  /'0010'X/

      DATA R_RZ  /'0008'X/    ! For 2nd wd of matched hits
      DATA L_RZ  /'0010'X/

      DATA MAXSEC/ 15, 31, 31/
      DATA FIRST/.TRUE./

C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C**  Need a mask for clearing all status bits except for the saturation bit.
C**  Make a mask with all bits "1", except for the 4 status bits we want to
C**  clear.
        VERS = 1
        CALL MVBITS(65535, 0, STAT_LENGTH(VERS)-1,
     &    MASK, STAT_OFFSET(VERS)+1)
        MASK = NOT( MASK)

C**  Move the mask bits to the proper position

        R_ZTK = ISHFT( R_ZTK, STAT_OFFSET(VERS))
        L_ZTK = ISHFT( L_ZTK, STAT_OFFSET(VERS))
        R_XY  = ISHFT( R_XY,  STAT_OFFSET(VERS))
        L_XY  = ISHFT( L_XY,  STAT_OFFSET(VERS))
        R_RZ  = ISHFT( R_RZ,  STAT_OFFSET(VERS))
        L_RZ  = ISHFT( L_RZ,  STAT_OFFSET(VERS))
      ENDIF
C
      LVTRH = GZVTRH()
      IF ( LVTRH .LE. 0) GO TO 999    ! No tracks

      LVCHT = GZVCHT()
      IF (LVCHT .LE. 0) GO TO 999     ! No VCHT bank to fill
      VERS = IQ(LVCHT+1)
      IF (VERS .LE. 0) GO TO 999   ! Only work with Vers 1 or higher

C** First, spin through VCHT and build pointer tables

      CALL VZERO(POINTER,3*32)
      VCHT_LENGTH = IQ(LVCHT-1)
      NHEAD = IQ(LVCHT+2)
      NWDSHT = IQ(LVCHT+4)        ! Number of words per hit
      OFFSET = NHEAD + 1
      NSEC = 0
      DO WHILE ( OFFSET .LE. VCHT_LENGTH .AND. NSEC .LT. 80 )
        SECTOR_HEADER = IQ(LVCHT+OFFSET)
        SECID = IBITS(SECTOR_HEADER,SECID_OFFSET,7)
        WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
        POINTER(SECID) = OFFSET
        OFFSET = OFFSET + WORDCOUNT
        NSEC = NSEC + 1
      ENDDO

c**  Check VCTH bank; if exists, drop it and clear status bits in VCHT;
c**  Book VCTH

      LVCTH = GZVCTH()
      IF ( LVCTH .GT. 0) THEN
        CALL MZDROP(IXCOM, LVCTH, ' ')
        DO LAY = 0, 2
          DO SEC = 0, MAXSEC(LAY)
            SECID = LAY*32 + SEC
            OFFSET = POINTER( SECID)
            IF (OFFSET .GT. 0) THEN
              SECTOR_HEADER = IQ(LVCHT+OFFSET)
              WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
              INDXN = LVCHT + OFFSET + 1
              INDXX = INDXN + WORDCOUNT - 2
              DO INDX = INDXN, INDXX
                IQ(INDX) = IAND( IQ(INDX), MASK)
              ENDDO
            ENDIF
          ENDDO     ! SEC
        ENDDO       ! LAY
      ENDIF         ! LVCTH .GT. 0

C**  Now find the size of VCTH and book it, and refresh all pointers.

      CALL VGET_VCTH_LEN( VCTH_LEN , N_VTXT )
      CALL BKVCTH( VCTH_LEN , N_VTXT , LVCTH )
      LVTRH = GZVTRH()
      LVCHT = GZVCHT()
      LVCTH = GZVCTH()
      LLVCTH = LVCTH + IQ(LVCTH+2)+1  ! RUNNING PNTR INTO VCTH; wd 1 of 1st blk
      NHTSWD = IQ(LVCTH+4)            ! # of packed hits per wd
      MAX_VTXT = IQ(LVCTH+5)          ! max # of vtxt allowed in VCTH

C**  Next, loop through VTXT

      LVTXT = LQ(LVTRH-1)
      DO WHILE ( LVTXT .GT. 0 )
        VTXT_NUM  = IQ(LVTXT-5)
        FILL_VCTH = VTXT_NUM .LE. MAX_VTXT   ! Do we pack this track?
        N_PACKED = 0
        LPVCTH = LLVCTH + 1             ! PTR to next wd to be packed
        CALL VZERO(VCHT_SEC, 3)
        CALL VZERO(VCHT_MXWIR, 3)

C**  First, find out if this track is part of a ZTRAK.

        ON_ZTK = .FALSE.
        LZTRK = LQ(LVTXT - 2)
        IF ( LZTRK .GT. 0) ON_ZTK = .NOT.BTEST(IQ(LZTRK), 0)

C**  Now trace VTTH >> VCHT.  Get LAY, SEC, WIRE, END, HITNUM from VTTH
C**  Also start packing VCTH if FILL_VCTH = TRUE

        LVTTH = LQ(LVTXT-1)
        IF ( LVTTH .GT. 0 ) THEN
          NH = IQ(LVTXT+2)
          OLDSECID = 99
          OLDLAY = -1
          DO IH = 1, NH
            IADD = IQ(LVTTH+6+4*(IH-1))
            LAY  = IBITS(IADD,9,2)
            VCHT_SEC(LAY) = IBITS(IADD, 4, 5)
            SECID  = IBITS(IADD,4,7)    ! SECID = LAY*32 + SEC
            OLDWIRE = WIRE              ! Save previous value
            WIRE = IBITS(IADD,1,3)
            HITNUM = IQ(LVTTH+7+4*(IH-1))
            L_R = BTEST(IADD, 0)
            WIRLAY = LAY*8 + WIRE
            ON_XY = BTEST( IQ(LVTXT+3), WIRLAY)
            ON_RZ = BTEST( IQ(LVTXT+4), WIRLAY)

C** Build up the status fields

            TSTAT1 = 0
            TSTAT2 = 0
            IF ( L_R) THEN
              IF ( ON_XY)  TSTAT1 = IOR( TSTAT1, L_XY  )
              IF ( ON_ZTK) TSTAT1 = IOR( TSTAT1, L_ZTK )
              IF ( ON_RZ)  TSTAT2 = IOR( TSTAT2, L_RZ  )
            ELSE
              IF ( ON_XY)  TSTAT1 = IOR( TSTAT1, R_XY  )
              IF ( ON_ZTK) TSTAT1 = IOR( TSTAT1, R_ZTK )
              IF ( ON_RZ)  TSTAT2 = IOR( TSTAT2, R_RZ  )
            ENDIF

C** Pack the hit info into VCTH 

            IF (FILL_VCTH) THEN
              HITNMP = MAX0( MIN0(HITNUM, 127), 1)
              IF (HITNUM .GT. 127) HITNMP = 0
              IF ( L_R) HITNMP = IBSET(HITNMP, 7)
              PAK_POS = MOD( N_PACKED, NHTSWD)
              CALL MVBITS(HITNMP, 0, 8, IQ(LPVCTH), PAK_POS*8)
              N_PACKED = N_PACKED + 1
              IF ( PAK_POS .GE. NHTSWD-1) THEN
                LPVCTH = LPVCTH + 1
              ENDIF
            ENDIF

C** Now find our way into VCHT
c**  The following assumes that hits are ordered by WIRLAY in VTTH,
c**  and by WIRE within a sector in VCHT

            IF ( SECID .NE. OLDSECID ) THEN
              VCHT_MXWIR(LAY) = 7              ! Init to seg in 1 sector.
              IF ( LAY .EQ. OLDLAY ) THEN        ! We have changed sectors 
                VCHT_MXWIR(OLDLAY) = OLDWIRE     ! within a layer
              ENDIF
              OFFSET = POINTER( SECID)
              IF (OFFSET .EQ. 0) THEN
                CALL ERRMSG('No sector header',
     &            'VTX_MARK_HITS',' ','W')
                GO TO 800
              ENDIF
              SECTOR_HEADER = IQ(LVCHT+OFFSET)
              WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
              IF (WORDCOUNT .LE. 1)  THEN
                CALL ERRMSG('No data in sector',
     &            'VTX_MARK_HITS',' ','W')
                GO TO 800
              ENDIF
              OFFMAX = OFFSET + WORDCOUNT - 1
              OFFSET = OFFSET + 1 - NWDSHT
              OLDSECID = SECID
              OLDLAY = LAY
            ENDIF

C** Find the first (or next) hit with a wire match

            WIREX = 8
            INCR = NWDSHT
            DO WHILE ( OFFSET .LT. OFFMAX .AND. WIRE .NE. WIREX)
              OFFSET = OFFSET + INCR
              WORD = IQ(LVCHT+OFFSET)
              WIREX = IBITS(WORD,ADDR_OFFSET(VERS)+1,3)
              MATCH = BTEST(WORD, MATCH_BIT(VERS))
              IF ( MATCH ) THEN   ! HAVE TO SKIP 2 HITS
                INCR = NWDSHT + NWDSHT
              ELSE
                INCR = NWDSHT
              ENDIF
            ENDDO
            IF (WIRE .NE. WIREX ) GO TO 700  ! NO WIRE MATCH IN THIS SECID****

C** At this point, WORD is the first word on the desired wire, and we know if
C**  it's matched

            NSKIP = HITNUM - 1
            DO III = 1,NSKIP
              OFFSET = OFFSET + INCR
              WORD = IQ(LVCHT+OFFSET)
              MATCH = BTEST(WORD, MATCH_BIT(VERS))
              IF ( MATCH ) THEN   ! HAVE TO SKIP 2 HITS
                INCR = NWDSHT + NWDSHT
              ELSE
                INCR = NWDSHT
              ENDIF
            ENDDO

            IF (OFFSET .GT. OFFMAX ) GO TO 700  ! PROBLEM*****************
            WIREX = IBITS(WORD,ADDR_OFFSET(VERS)+1,3)
            IF (WIRE .NE. WIREX) GO TO 700   ! PROBLEM*****************

C**  Found it; WORD is the desired word, MATCH is set. 
C**  Mark as required (don't touch the "satureated" bit).

            IQ(LVCHT+OFFSET) = IOR( IQ(LVCHT+OFFSET), TSTAT1)

            IF (MATCH ) THEN     ! HAVE TO MARK THE NEXT HIT ALSO
              OFFSET = OFFSET + NWDSHT 
              IF (OFFSET .GT. OFFMAX) GO TO 700  ! PROBLEM*****************
              WORD = IQ(LVCHT+OFFSET)
              WIREX = IBITS(WORD,ADDR_OFFSET(VERS)+1,3)
              IF (WIRE .NE. WIREX) GO TO 700     ! PROBLEM*****************
              IQ(LVCHT+OFFSET) = IOR( IQ(LVCHT+OFFSET), TSTAT2)
            ENDIF
            GO TO 800

  700       CONTINUE
            CALL ERRMSG('Hit not found',
     &            'VTX_MARK_HITS',' ','W')

  800       CONTINUE
          ENDDO   !   IH = 1,NH
        ENDIF ! LVTTH .GT. 0

C**  Set 1st wd of VCHT track block, and update pntrs

        IF (FILL_VCTH) THEN
          TEMP = IAND( VTXT_NUM, '1FF'X)
          CALL MVBITS( VCHT_SEC(0), 0, 4, TEMP, 9)
          CALL MVBITS( VCHT_SEC(1), 0, 5, TEMP, 13)
          CALL MVBITS( VCHT_SEC(2), 0, 5, TEMP, 18)
          CALL MVBITS( VCHT_MXWIR(0), 0, 3, TEMP, 23)
          CALL MVBITS( VCHT_MXWIR(1), 0, 3, TEMP, 26)
          CALL MVBITS( VCHT_MXWIR(2), 0, 3, TEMP, 29)
          IQ(LLVCTH) = TEMP

          LLVCTH = LLVCTH + 2 + (N_PACKED-1)/NHTSWD
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO   ! WHILE LVTXT .GT. 0

  999 RETURN
      END
