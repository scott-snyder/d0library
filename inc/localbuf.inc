C	==========================================
C	Process Local DAQ Common Block Definitions
C	==========================================
C
C	Modulename:-
C	============
C	DAQ$CMS:LOCALBUF.INC
C
C	Description:-
C	=============
C	This Include File contains the DAQ System Process Local
C	Common Block Declarations.
C
C	Creation Date :  8 Mar 1983
C	===========================
C
      Common/DAQ_LOCAL/CT_INDEX,CT_NAMEA,CT_NAMEB,CT_PID,
     &                 CT_PARTID,CT_EVFLAG,CT_FLAGS,
     &                 CT_LEVEL,CT_UNIT,CT_DUMP
      Integer*4        CT_INDEX,CT_NAMEA,CT_NAMEB,CT_PID,
     &                 CT_PARTID,CT_EVFLAG,CT_FLAGS,
     &                 CT_LEVEL,CT_UNIT,CT_DUMP
C
C	CT_INDEX   = CON_CT Index
C	CT_NAMEA   = Process Logical Name (first 4 characters)
C	CT_NAMEB   = Process Logical Name (last 4 characters)
C	CT_PID     = Process PID (VAX/VMS)
C	CT_PARTID  = Process Partition ID
C	CT_EVFLAG  = Event Flag No.
C	CT_FLAGS   = New Event etc. Flags
C	CT_LEVEL   = Error Message Reporting Level (Default 1)
C	CT_UNIT    = Error Message Logical Unit No. (Default 6)
C	CT_DUMP	   = Dump Type Mode
C
      Parameter CT_DUMP_NONE                = 0
      Parameter CT_DUMP_HEX16               = 1
      Parameter CT_DUMP_HEX32               = 2
      Parameter CT_DUMP_DEC16_SIGNED        = 3
      Parameter CT_DUMP_DEC16_UNSIGNED      = 4
      Parameter CT_DUMP_DEC32               = 5
      Parameter CT_DUMP_BANK_HEADER         = 6
      Parameter CT_DUMP_BANK_HEX16          = 7
      Parameter CT_DUMP_BANK_HEX32          = 8
      Parameter CT_DUMP_BANK_DEC16_SIGNED   = 9
      Parameter CT_DUMP_BANK_DEC16_UNSIGNED = 10
      Parameter CT_DUMP_BANK_DEC32          = 11
C
