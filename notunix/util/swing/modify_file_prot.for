      INTEGER*4 FUNCTION MODIFY_FILE_PROT ( FILE, PROT, CODE )

C     Modifies  the protection  on a specified  file.  The file's access
C     control list, if it  has one, is not modified.  The status of  the
C     operation is returned as a function value.

C     This routine will fail if the protection on the file (prior to the
C     modification) is such that we do not have read and write access to
C     it.  It will also fail if the file has already been opened without
C     write-shareability.

C     Greg Janee, 19-MAR-1986

C-----------------------------------------------------------------------

C     Arguments:
C
C     FILE      type:      character string
C               access:    read only
C               mechanism: by descriptor, fixed-length descriptor
C
C     The filename of  the file whose  protection is to be modified.  If
C     the string is larger than 255 bytes, only the first 255  bytes are
C     used.
C
C     PROT      type:      unsigned word
C               access:    read only
C               mechanism: by reference
C
C     The bit mask  that is to replace  or modify the  file's protection
C     bits.  The mask  should be  specified in  the format  described by
C     section 12.13 of the VAX Record Management Services Reference Man-
C     ual.
C
C     CODE      type:      signed longword integer
C               access:    read only
C               mechanism: by reference
C
C     The type of modification to be performed on  the file's protection
C     bits.  A  value of 0 indicates the  bits are to be replaced by the
C     PROT argument;  values  1, 2, and 3 indicate  the bits  are  to be
C     ANDed, inclusive-ORed, or  exclusive-ORed with the  PROT argument,
C     respectively.  The  protection  bits  are left  unchanged  for all
C     other values of this argument.

C=======================================================================

      IMPLICIT  NONE

      INCLUDE   '($FABDEF)'
      INCLUDE   '($XABDEF)'
      INCLUDE   '($XABPRODEF)'

C     We have to define our own structure to access a XABPRO because DEC
C     is too stupid to define theirs correctly.

      STRUCTURE /XABPRO/
         UNION
            MAP
               RECORD /XABDEF/     A
            END MAP
            MAP
               RECORD /XABPRODEF1/ B
            END MAP
         END UNION
      END STRUCTURE

      CHARACTER FILE*(*)
      INTEGER*2 PROT
      INTEGER*4 CODE

      RECORD    /FABDEF/ FAB
      RECORD    /XABPRO/ XAB

      INTRINSIC JMIN0
      INTRINSIC LEN
      EXTERNAL  LIB$INSV
      EXTERNAL  LIB$MOVC5
      EXTERNAL  SYS$CLOSE
      INTEGER*4 SYS$CLOSE
      EXTERNAL  SYS$OPEN
      INTEGER*4 SYS$OPEN

C-----------------------------------------------------------------------

C     First initialize and  link a FAB and XAB.  Note that  if we do not
C     open the  file with some sort  of write access the protection will
C     not be changed.

      CALL LIB$MOVC5 ( 0, 0, 0, FAB$C_BLN, FAB )

      FAB.FAB$B_BID = FAB$C_BID
      FAB.FAB$B_BLN = FAB$C_BLN
      FAB.FAB$B_FAC = FAB$M_PUT
      FAB.FAB$L_FNA = %LOC( FILE )
      CALL LIB$INSV ( JMIN0( LEN(FILE), 255 ), 0, 8, FAB.FAB$B_FNS )

C     RMS will balk if the  file has been opened by  someone else.  With
C     the following SHR options we'll at least get through the case when
C     the file has been opened write-shared.

      FAB.FAB$B_SHR = FAB$M_SHRPUT .OR. FAB$M_SHRGET .OR.
     .                FAB$M_SHRDEL .OR. FAB$M_SHRUPD .OR. FAB$M_UPI

      FAB.FAB$L_XAB = %LOC( XAB )

      CALL LIB$MOVC5 ( 0, 0, 0, XAB$C_PROLEN, XAB )

      XAB.A.XAB$B_BLN = XAB$C_PROLEN
      XAB.A.XAB$B_COD = XAB$C_PRO

C-----------------------------------------------------------------------

C     There is  no RMS service to  change file protections.  To do so we
C     open the file with write access and then close  it with a new pro-
C     tection mask.

      MODIFY_FILE_PROT = SYS$OPEN( FAB )
      IF ( .NOT.MODIFY_FILE_PROT ) RETURN

      IF     ( CODE .EQ. 0 ) THEN
         XAB.B.XAB$W_PRO =                       PROT
      ELSEIF ( CODE .EQ. 1 ) THEN
         XAB.B.XAB$W_PRO = XAB.B.XAB$W_PRO .AND. PROT
      ELSEIF ( CODE .EQ. 2 ) THEN
         XAB.B.XAB$W_PRO = XAB.B.XAB$W_PRO .OR.  PROT
      ELSEIF ( CODE .EQ. 3 ) THEN
         XAB.B.XAB$W_PRO = XAB.B.XAB$W_PRO .XOR. PROT
      END IF

      MODIFY_FILE_PROT = SYS$CLOSE( FAB )
      RETURN

C=======================================================================

      END
