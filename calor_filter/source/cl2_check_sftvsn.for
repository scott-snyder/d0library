      SUBROUTINE CL2_CHECK_SFTVSN(CRATID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check whether CAGS/CADT banks match data being
C-        analyzed
C-
C-   Inputs  : CRATID   [I]   Level 2 ID of crate being decoded
C-             CAGS and CAD banks
C-             CRATE_POINT and CRATE_CAD from /ADC_LOCATION/
C-             L2CAD1 and L2CAD2 from /CL2_LINKS/
C-             L2CAGS from /CL2_STP_LINKS/
C-   Outputs : warning message if appropriate.
C-   Controls: 
C-
C-   Created  22-SEP-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER CRATID
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$INC:ADC_LOCATION.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INTEGER SFTVSN,DATA_SFTVSN  ! version numbers
      LOGICAL D0_DATA, DATA_D0_DATA, MONTE_CARLO, DATA_MONTE_CARLO
      LOGICAL TYPE_MATCH  !whether D0_DATA and MONTE_CARLO match
      INTEGER POINT,L2CAD
      CHARACTER*128 VARMSG
      CHARACTER*32 IDMSG
      CHARACTER*1 SEVERITY
      INTEGER VERSION_WORD
C&IF VAXVMS,VAXELN
      BYTE VERSION_BYTE(4)
      EQUIVALENCE (VERSION_BYTE(1),VERSION_WORD)
C&ENDIF
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL BTEST
C&ENDIF
      DATA IDMSG/'CL2HITS.RCP Doesn''t Match Data'/
C----------------------------------------------------------------------
C
C...unpack version number from CAGS
C&IF VAXVMS,VAXELN
      VERSION_WORD = IC(L2CAGS+9)
      SFTVSN = VERSION_BYTE(1)
      D0_DATA = .NOT.BTEST(VERSION_WORD,30)     !Bit on for NWA data  
      MONTE_CARLO = BTEST(VERSION_WORD,29)      !Bit on for MC data
C&ENDIF      
C
C...find out data version to see if it matches CAGS bank
      POINT = CRATE_POINT(CRATID)
      L2CAD = L2CAD1
      IF (CRATE_CAD(CRATID).EQ.2) L2CAD = L2CAD2
C&IF VAXVMS,VAXELN
      VERSION_WORD = IQ(L2CAD+4+POINT)
      DATA_SFTVSN = VERSION_BYTE(1)
      DATA_D0_DATA = .NOT.BTEST(VERSION_WORD,30)
      DATA_MONTE_CARLO = BTEST(VERSION_WORD,29)
C&ENDIF
      TYPE_MATCH = (D0_DATA.EQV.DATA_D0_DATA).AND.
     &     (MONTE_CARLO.EQV.DATA_MONTE_CARLO)
      IF (.NOT.TYPE_MATCH.OR.(SFTVSN.NE.DATA_SFTVSN)) THEN
C...CAHITS.RCP parameters don't match the data read--generate a message
        IF (TYPE_MATCH.AND.(SFTVSN.EQ.0).AND.(.NOT.MONTE_CARLO)) THEN
C...allow the special case of real data and SFTVSN = 0 (current type)
          SEVERITY = 'W'
        ELSE
          SEVERITY = 'F'
        ENDIF
        WRITE (VARMSG,'(''Need D0_DATA '',L1,'' MONTE_CARLO '',L1,
     &    '' SFTVSN '',I2,''in CL2HITS_RCP for VMS_FILTER_STP'')')
     &      DATA_D0_DATA,DATA_MONTE_CARLO,DATA_SFTVSN
        CALL ERRMSG(IDMSG,'CL2_CHECK_SFTVSN',VARMSG,SEVERITY)
      ENDIF
  999 RETURN
      END
