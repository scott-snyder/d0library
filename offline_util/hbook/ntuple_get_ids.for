      SUBROUTINE NTUPLE_GET_IDS(MAXID,NID,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return ntuple ids in current RZ directory.
C-   Uses some code from HBOOK4.
C-
C-   Inputs  : MAXID  [I] Maximum number of IDs to return
C-   Outputs : NID    [I] Number of IDs returned
C-             ID(*)  [I] Ids
C-   Controls:
C-
C-   Created  11-JAN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER MAXID, NID, ID(*)
C----------------------------------------------------------------------
C
C-         ============> N-TUPLE  DATA STRUCTURE <============
C-
C-                   I
C-                   I
C-                   I LCID
C-    -3   -2   -1   V
C-  ************************************************************************
C-  *    *    *    ****** Bits,NDIM,NOENT,etc  (See A)                     *
C-  ************************************************************************
C-    .     I     I   LNLAST  pointer to last linear bank
C-    ......I.....I.......................................
C-          I     I                                      .
C-          I     I LCONT                                .
C-          I     I                                      V
C-          I     I       ****************************   ********************
C-          I     I------>* (x1,y1,z1,,),(x2,y2,z2,,)*-->*    next if no RZ *
C-          I             ****************************   ********************
C-          I LLIMS
C-          I
C-          I        *****************************************
C-          I------->*  xlow,xup,ylow,yup,zlow,zup,etc       *
C-                   *****************************************
C-
C-
C-    **********************************************************************
C-    * word *  tag    *               content                             *
C-    **********************************************************************
C-    *      *         *                                                   *
C-    *   1  *  BITS   *     status word                                   *
C-    *   2  *  NDIM   *     Number of variables in the N-Tuple            *
C-    *   3  *  NOENT  *     Total number of entries                       *
C-    *   4  *  NPRIME *     Number of words for primary allocation        *
C-    *   5  *  NMEM   *     Number of memory blocks (only 1 with RZ)      *
C-    *   6  *  NRZB   *     Number of RZ records (If RZ is used)          *
C-    *   7  *  IFIRST *     pointer to 1st free word in LCONT             *
C-    *   8  *  NWTITL *     Number of words in the title                  *
C- (A)*   9  *  ITIT1  *     Internal pointer to the first word of title   *
C-    *  10  *  ITAG1  *     pointer to the first tag                      *
C-    *  11  *  NCHRZ  *     Number of characters in RZ dir (may be 0)     *
C-    *  12  *  CHRZ1  *     1st word of RZ directory                      *
C-    *  13  *  CHRZ2  *     2nd word,etc                                  *
C-    *      *   ...   *                                                   *
C-    * ITAG1*  TAG1   *     1st tag (8 characters on 2 words)             *
C-    *      *   ...   *                                                   *
C-    * ITIT1*  TITLE1 *     1st word of title                             *
C-    *      *   ...   *                                                   *
C-    **********************************************************************
C-
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PAWC.INC'
C----------------------------------------------------------------------
      INTEGER LCDIR, LCIDN, GZHDIR
      INTEGER ICYCLE,IOFFSET
C----------------------------------------------------------------------
      NID = 0
      LCDIR = GZHDIR()
      IF ( LCDIR .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
C ****  Load ntuples if not yet in memory
C
      LCIDN = LP(LCDIR-2)
      IF ( LCIDN .LE. 0 ) THEN
        ICYCLE = 0
        IOFFSET= 0
        CALL HRIN(0,ICYCLE,IOFFSET)
      ENDIF
C
C ****  Loop over HIDN banks; ntuple-ID = bank-ID
C
      LCIDN = LP(LCDIR-2)
      DO WHILE ( LCIDN .GT. 0 )
C
C ****  Check if it's an ntuple
C
        IF ( JBIT(IP(LCIDN+1),4) .NE. 0 ) THEN
          NID = NID + 1
          ID(NID) = IP(LCIDN-5)
        ENDIF
        LCIDN = LP(LCIDN)
      ENDDO
  999 RETURN
      END
