      SUBROUTINE DBCLB_LIST_OUT ( UNIT, PATH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the keys in a format we like.  This
C-                         replaces a routine in the DBL3 libraray.
C-     ENTRY DBCLB_LIST_SET sets parameters for what is listed.
C-
C-   Inputs  : UNIT     Unit number to print to
C-             PATH     current directory path
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-FEB-1990   J.Green
C-   Updated  29-MAR-1991   S. Abachi   Saved ISV,ILV,... into a DATA area
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
C      INCLUDE      'D0$INC:DBSTP.INC'
      CHARACTER*(*) PATH
      INTEGER       NKYS, MAXKEY, NKEY, IKEY
      PARAMETER     (NKYS = 15)
      PARAMETER     (MAXKEY = 10000)
      INTEGER       KEYS(NKYS,MAXKEY)
      INTEGER       CDATE, CTIME, IDATE, ITIME
      INTEGER       CYR, CMO, CDA, CHR, CMI, CSE
      INTEGER       IYR, IMO, IDA, IHR, IMI
      CHARACTER*80  MSGSTR
      CHARACTER*2   CHOPT
      INTEGER       ITEM, UNIT
      LOGICAL       LSV, LLV, LSC, LLC, LSI, LLI, LCR
      DATA          LSV, LLV, LSC, LLC, LSI, LLI, LCR / 7*.FALSE. /
      INTEGER       ISV, ILV, ISC, ILC, ISI, ILI, ICR
      DATA          ISV, ILV, ISC, ILC, ISI, ILI, ICR /7*0/
C----------------------------------------------------------------------
C                               Write Header
      IF ( UNIT .EQ. 6 ) THEN
        WRITE ( MSGSTR, 1001 )
        CALL INTMSG ( MSGSTR )
        WRITE ( MSGSTR, 1002 )
        CALL INTMSG ( MSGSTR )
        CALL INTMSG ( ' ' )
      ELSE
        WRITE ( UNIT, 1001 )
        WRITE ( UNIT, 1002 )
        WRITE ( UNIT, 1004 )
      ENDIF
 1001   FORMAT ( '   SERIAL#    START_RUN  END_RUN    CALIB_RUN  MODULE'
     &         '     CREATED    INSERTED ' )
        WRITE ( MSGSTR, 1002 )
 1002   FORMAT (  46X, ' OR CRATE   DATE/TIME  DATE/TIME' )
 1004   FORMAT ( ' ' )
C
C                        retrieve all the keys
      CALL RZCDIR ( PATH, ' ')
      CALL RZKEYS ( NKYS, MAXKEY, KEYS, NKEY )
C
C                      check the keys and options
C
      DO  IKEY = 1,NKEY
        IF ( LSV .AND. KEYS(3,IKEY) .LT. ISV ) GO TO 100
        IF ( LLV .AND. KEYS(4,IKEY) .GT. ILV ) GO TO 100
        IF ( LSC .AND. KEYS(9,IKEY) .LT. ISC ) GO TO 100
        IF ( LLC .AND. KEYS(9,IKEY) .GT. ILC ) GO TO 100
        IF ( LSI .AND. KEYS(7,IKEY) .LT. ISI ) GO TO 100
        IF ( LLI .AND. KEYS(7,IKEY) .GT. ILI ) GO TO 100
        IF ( LCR .AND. KEYS(8,IKEY) .NE. ICR ) GO TO 100
C
        CALL DBUPTS ( CDATE, CTIME, KEYS(9,IKEY) )
        CYR = MOD ( CDATE/10000, 100 )
        CMO = MOD ( CDATE/100, 100 )
        CDA = MOD ( CDATE, 100 )
        CHR = MOD ( CTIME/10000, 100 )
        CMI = MOD ( CTIME/100, 100 )
        CALL DBUPTM ( IDATE, ITIME, KEYS(7,IKEY) )
        IYR = MOD ( IDATE/10000, 100 )
        IMO = MOD ( IDATE/100, 100 )
        IDA = MOD ( IDATE, 100 )
        IHR = MOD ( ITIME/100, 100 )
        IMI = MOD ( ITIME, 100 )
        IF ( UNIT .EQ. 6 ) THEN
          WRITE (MSGSTR, 1000) KEYS(1,IKEY), KEYS(3,IKEY), KEYS(4,IKEY),
     &                         KEYS(11,IKEY), KEYS (8,IKEY), CYR,
     &                         CMO, CDA, IYR, IMO, IDA
          CALL INTMSG ( MSGSTR )
          WRITE ( MSGSTR, 1003 ) CHR, CMI, IHR, IMI
          CALL INTMSG ( MSGSTR )
        ELSE
          WRITE ( UNIT, 1000 ) KEYS(1,IKEY), KEYS(3,IKEY), KEYS(4,IKEY),
     &                         KEYS(11,IKEY), KEYS(8,IKEY), CYR, 
     &                         CMO, CDA, IYR, IMO, IDA
          WRITE ( UNIT, 1003 ) CHR, CMI, IHR, IMI
        ENDIF
  100 CONTINUE
      ENDDO
 1000 FORMAT ( 5I11, 2( I5,'/',I2,'/',I2 ) )
 1003 FORMAT ( 55X, 2( I8,':',I2 ) )
      GO TO 999
C
C---------------------------------------------------------------------
C
      ENTRY DBCLB_LIST_SET ( ITEM, CHOPT )
C
      IF     (CHOPT .EQ. 'SV' ) THEN
        LSV = .TRUE.
        ISV = ITEM
      ELSEIF (CHOPT .EQ. 'LV' ) THEN
        LLV = .TRUE.
        ILV = ITEM
      ELSEIF (CHOPT .EQ. 'SC' ) THEN
        LSC = .TRUE.
        ISC = ITEM
      ELSEIF (CHOPT .EQ. 'LC' ) THEN
        LLC = .TRUE.
        ILC = ITEM
      ELSEIF (CHOPT .EQ. 'SI' ) THEN
        LSI = .TRUE.
        ISI = ITEM
      ELSEIF (CHOPT .EQ. 'LI' ) THEN
        LLI = .TRUE.
        ILI = ITEM
      ELSEIF (CHOPT .EQ. 'CR' ) THEN
        LCR = .TRUE.
        ICR = ITEM
      ELSEIF ( CHOPT .EQ. 'CL' ) THEN
        LSV = .FALSE.
        LLV = .FALSE.
        LSC = .FALSE.
        LLC = .FALSE.
        LSI = .FALSE.
        LLI = .FALSE.
        LCR = .FALSE.
      ELSE
        CALL INTMSG ( 'DBCLB_LIST_SET: invalid character option ')
      ENDIF
C      
  999 RETURN
      END
