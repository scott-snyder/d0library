C
C
C
C
C       PROCNS.FOR:   DEC VAX/VMS PS 300 Procedural Interface (GSR)
C                       (FORTRAN-77) Common Declarations file.
C
C       Copyright (C)  1983, Evans & Sutherland Computer Corporation.
C
C       This software is the property of Evans & Sutherland Computer
C       Corporation and shall not be reproduced or copied or used in
C       whole or in part as the basis for the manufacture or sale of
C       items, nor shall such copy be sold or constitute part of a
C       sale without written permission.
C
C                    RESTRICTED RIGHTS LEGEND
C
C       Use, duplication, or disclosure is subject to restrictions
C       stated in contract No.              with the Evans & Sutherland
C       Computer Corporation.
C
C       This software is furnished under a license and may only be
C       used or copied in accordance with the terms of such license.
C
C       E&S PART     xxxxxx-nnn
C
C
C
C       VERSION   DATE        MODIFIED BY        REASON FOR MODIFICATIO
C       P5.V03    10-OCT-83   Brent Kingsbury    Release version.
C	P6.V01    16-DEC-83   Brent Kingsbury    Add PSEIPA error code.
C	V1.V02    17-OCT-85   Jim Buckmiller     Add PSEFLN error code.
C	V3.V00    12-DEC-87   Dan Klass		 Add PSFHTN error code.
C
C
C     The following PARAMETER declarations define the possible error
C     codes used to identify warning or error conditions that may arise
C     while using the Procedural Interface package.
C
C     The set of possible error codes is divided into several regions
C     reserved for specific severity and machine dependency levels:
C
C        1..255  = Machine INDEPENDENT warning conditions.
C      256..511  = Machine   DEPENDENT warning conditions.
C      512..767  = Machine INDEPENDENT error conditions.
C      768..1023 = Machine   DEPENDENT error conditions.
C     1024..1279 = Machine INDEPENDENT fatal error conditions.
C     1280..1535 = Machine   DEPENDENT fatal error conditions.
C
C     Error/Completion codes:
C
C       Mnemonic/Severity/Meaning:
C
C       PSWBNC: Warning:        Bad name character.
C       PSWNTL: Warning:        Name too long.
C       PSWSTL: Warning:        String too long.
C
C     The following (4) warning codes are reserved for the P6.V01
C     version of the Procedural Interface (GSR).
C
C       PSWPCG: Warning:        Pixel Count greater than screen size in
C                               call to PRASWP.
C       PSWPCL: Warning:        Pixel count < 1 in call to PRASWP.
C       PSWRCG: Warning:        Repetition count > 255 in PRASLU call.
C       PSWRCL: Warning:        Repetition count < 1 in PRASLU call.
C
C
C       PSWAAD: Warning:        Attach already done.
C       PSWAKS: Warning:        Attention key seen.  This warning code is
C				ONLY applicable to the IBM version of the
C				Procedural Interface.
C       PSWBGC: Warning:        Bad Generic channel character.  This
C				warning code is ONLY applicable to the IBM
C				version of the Procedural Interface.
C	PSWBSC: Warning:	Bad string character.  This warning code
C				is ONLY applicable to the IBM version of
C				the Procedural Interface.
C	PSWBPC:	Warning:	Bad Parse channel character.  This warning
C				code is ONLY applicable to the IBM version
C				of the Procedural Interface.
C       PSEIMC: Error:          Invalid Multiplexing channel specified.
C       PSEIVC: Error:          Invalid vector list class.
C       PSEIVD: Error:          Invalid vector list dimension.
C       PSEPOE: Error:          Prefix operate node call expected.
C       PSEFOE: Error:          Follow operate node call expected.
C       PSELBE: Error:          Label block call expected.
C       PSEVLE: Error:          Vector List call expected.
C       PSEAMV: Error:          Attempted multiple PVcLis call sequence
C                               for block normalized vectors prohibited
C       PSEMLB: Error:          Missing label block begin call.
C       PSEMVB: Error:          Missing vector list begin call.
C       PSENUN: Error:          A null name is not permitted in this
C                               call context.
C       PSEBCT: Error:          Bad Comparison type operator specified.
C       PSEIFN: Error:          Attempted PS 300 function instance call
C                               failed because the named function canno
C                               possibly exist.  The function name
C                               identifying the function type to
C                               instance was longer than 256 characters
C       PSENNR: Error:          Null name was required for parameter
C                               in operate node call following a
C                               PPref or PFoll procedure call.
C       PSETME: Error:		Too many end_structure calls for
C                               begin/end_structure depth.
C       PSENOA: Error:          The PS 300 communications link has not
C                               been established (NOt Attached)
C       PSEODR: Error:          An overrun occurred during a read
C                               operation.  The input buffer was too
C                               small and truncation has occurred.
C
C     The following (15) error codes are reserved for the P6.V01
C     version of the Procedural Interface (GSR).
C
C	PSEMPB: Error:		Missing Polygon begin call.
C	PSEALE:	Error:		Polygon attribute, list, or end call was
C				expected.
C	PSELEX:	Error:		Polygon list or end call expected.
C	PSEALX:	Error:		Polygon attribute or list call expected.
C	PSELX:	Error:		Polygon list call expected.
C	PSEMPX:	Error:		Max allowable number of polygon vertices
C				exceeded.
C	PSELMP:	Error:		Number of vertices specified for
C				polygon < 3.
C	PSEIPA: Error:		Illegal polygon attribute(s) specified in
C				call to PPlygA.  Attribute(s) ignored.
C       PSEICP: Error:		Illegal Current Pixel specification in
C                               call to PRASCP.
C       PSEIOR: Error:		Index out of range 0..255 in call to
C                               PRASLU.
C       PSELDC: Error:		Illegal LDC specification in call to
C                               PRASLD.
C       PSELNL: Error:		Num less than 1 in call to PRASLU.
C       PSEMGM: Error:		Min > Max in call to PRASLR.
C       PSEMNO: Error:		Min out of range 0..255 in PRASLR.
C       PSEMXO: Error:		Max out of range 0..255 in PRASLR.
C       PSEPNL: Error:		Num less than 1 in call to PRASWP.
C
C
C     Operating System/Device Specific Procedural Interface Error
C     Messages.
C
C       PSEPDT: Error:          Missing or invalid Physical Device Type
C                               specifier in PAttch call.  This error
C                               code is presently ONLY applicable to
C                               the DEC VAX VMS version of the
C                               Procedural Interface.
C       PSELDN: Error:          Missing or invalid Logical Device Name
C                               specifier in PAttch call.  This error
C                               code is presently ONLY applicable to
C                               the DEC VAX VMS version of the
C                               Procedural Interface.
C       PSEADE: Error:          Attach parameter string delimiter
C                               expected.  This error code is currently
C                               only applicable to the DEC VAX VMS
C                               version of the Procedural Interface.
C	PSEFLN: Error:		File output open failed, invalid VMS
C				file specification.
C       PSFIFC: Fatal Error:	Illegal Frame command value in call to
C                               PSURMO.  This error code indicates a
C				Procedural Interface (GSR) Internal
C				validity check error.  This error code
C				is RESERVED for the P6.V01 version
C				of the Procedural Interface (GSR).
C       PSFPAF: Fatal Error:    Physical Attach operation failed.
C       PSFPDF: Fatal Error:    Physical Detach operation failed.
C       PSFPGF: Fatal Error:    Physical Get operation failed.
C       PSFPPF: Fatal Error:    Physical Put operation failed.
C       PSFBTL: Fatal Error:    Buffer too large in call to PSPUT. This
C                               error code is currently only applicable
C                               to the DEC VAX VMS version of the
C                               Procedural Interface.  This error code
C                               indicates an internal Procedural
C                               Interface validity error.
C       PSFWNA: Fatal Error:    Wrong number of arguments to low-level
C                               I/O procedure in PROIOLIB.MAR.  This
C                               error code is DEC VAX VMS Procedural
C                               Interface version specific.  This error
C                               code indicates an internal Procedural
C                               Interface validity error.
C       PSFPTL: Fatal Error:    Prompt too large in call to PSFRCV.
C                               This error code is currently only appl-
C                               icable to the DEC VAX VMS version of
C                               the Procedural Interface.  This error
C                               code indicates an internal Procedural
C                               Interface validity error.
C	PSFHTN: FATAL Error:	Bad host name argument for the 
C				Gethostbyname call.
C
C

      INTEGER*4 PSWBNC, PSWNTL, PSWSTL, PSWPCG, PSWPCL, PSWRCG, PSWRCL,
     &	        PSWAAD, PSWAKS, PSWBGC, PSWBSC, PSWBPC, PSEIMC, PSEIVC,
     &          PSEIVD, PSEPOE, PSEFOE, PSELBE, PSEVLE, PSEAMV, PSEMLB,
     &          PSEMVB, PSENUN, PSEBCT, PSEIFN, PSENNR, PSETME, PSENOA,
     &          PSEODR, PSEMPB, PSEALE, PSELEX, PSEALX, PSELX,  PSEMPX,
     &		PSELMP, PSEIPA, PSEICP, PSEIOR, PSELDC, PSELNL, PSEMGM,
     &		PSEMNO, PSEMXO, PSEPNL, PSEPDT, PSELDN, PSEADE, PSFIFC,
     &          PSFPAF, PSFPDF, PSFPGF, PSFPPF, PSFBTL, PSFWNA, PSFPTL, 
     &          PSEFLN, PSFHTN

      PARAMETER(PSWBNC=   1, PSWNTL=   2, PSWSTL=   3, PSWPCG=  30,
     &		PSWPCL=  31, PSWRCG=  32, PSWRCL=  33, PSWAAD= 256,
     &		PSWAKS= 257, PSWBGC= 258, PSWBSC= 259, PSWBPC= 260,
     &		PSEIMC= 512, PSEIVC= 513, PSEIVD= 514, PSEPOE= 515,
     &		PSEFOE= 516, PSELBE= 517, PSEVLE= 518, PSEAMV= 519,
     &		PSEMLB= 520, PSEMVB= 521, PSENUN= 522, PSEBCT= 523,
     &		PSEIFN= 524, PSENNR= 525, PSETME= 526, PSENOA= 527,
     &		PSEODR= 528, PSEMPB= 529, PSEALE= 530, PSELEX= 531,
     &		PSEALX= 532, PSELX = 533, PSEMPX= 534, PSELMP= 535,
     &		PSEIPA= 536, PSEICP= 550, PSEIOR= 552, PSELDC= 553,
     &		PSELNL= 554, PSEMGM= 555, PSEMNO= 556, PSEMXO= 557,
     &		PSEPNL= 558, PSEPDT= 780, PSELDN= 781, PSEADE= 782,
     &          PSEFLN= 783,
     &		PSFIFC=1024, PSFPAF=1280, PSFPDF=1281, PSFPGF=1282,
     &		PSFPPF=1283, PSFBTL=1290, PSFWNA=1291, PSFPTL=1292,
     &		PSFHTN=1293)

C
C     Procedural Interface (GSR) Input/Level_of_detail tests/
C     Vector List class definitions.
C
C       Name/Meaning:
C
C       PIAPP:   <Append> input number.
C       PIDEL:   <Delete> input number.
C       PICLR:   <Clear>  input number.
C       PISTEP:  <Step>   input number.
C       PIPOS:   <Position> input number.
C       PILAST:  <Last>   input number.
C       PISUBS:  <Substitute> input number.
C       PCLES:   "Less" level of detail comparison operator.
C       PCEQL:   "Equal" level of detail comparison operator.
C       PCLEQL:  "Less-equal" level of detail comparison operator.
C       PCGTR:   "Greater" level of detail comparison operator.
C       PCNEQL:  "Not-equal" level of detail comparison operator.
C       PCGEQL:  "Greater-equal" level of detail comparison operator.
C       PVCONN:  Vector list "Connected" class type.
C       PVDOTS:  Vector List "Dots" class type.
C       PVITEM:  Vector List "Itemized" class type.
C       PVSEPA:  Vector List "Separate" class type.
C

      INTEGER*4 PIAPP,  PIDEL,  PICLR,  PISTEP, PIPOS,  PILAST, PISUBS,
     &		PCLES,  PCEQL,  PCLEQL, PCGTR,  PCNEQL, PCGEQL, PVCONN,
     &		PVDOTS, PVITEM, PVSEPA

      PARAMETER(PIAPP =   0,
     &		PIDEL =  -1, PICLR =  -2, PISTEP=  -3, PIPOS =  -4,
     &		PILAST=  -5, PISUBS=  -6, PCLES =   0, PCEQL =   1,
     &		PCLEQL=   2, PCGTR =   3, PCNEQL=   4, PCGEQL=   5,
     &		PVCONN=   0, PVDOTS=   1, PVITEM=   2, PVSEPA=   3)


