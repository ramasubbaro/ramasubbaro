      ******************************************************************00000100
      ******        WEB ENABLEMENT COPYBOOK FOR DE20050           ******00000200
      *****                WEB SERVICE ACCOUNT PARAMETERS          *****00000300
      *****         DECOMM + DEHAPI + DEACTHLD + DEACTSTM +        *****00050000
      *****         (256)  + (1250) + (198)    +  (199)            *****00050000
      *****         DESTMCOD + DERETURN                            *****00050000
      *****          (150)   +  (300)                              *****00050000
      ****          COPYBOOK LENGTH = 4096                          ****00000600
      ******************************************************************00000700
      ******************************************************************00000800
      **                       COPYBOOK UPDATES                       **00000900
      ******************************************************************00001000
      * PRASAD GAJULA                                                  *00020000
      * 06/28/2019 HOST?????? WR?????                                  *00030000
      *          NEW WEB COPYBOOK FOR IIB                              *00040000
      *          ACCOUNT PARAMETERS WEB SERVICE (DE20050)              *00041000
      ******************************************************************00060000
WEB    01  WEB-DE20050-PARAMETERS.                                      00070000
           04  DE-COMM-AREA.
               05  DE-FIRM               PIC 9(3).
               05  DE-SUB                PIC 9(3).
               05  DE-REQ.
                   10  DE-APPLICATION    PIC X(4).
                       88  APPL-ACCT     VALUE 'ACCT'.
                       88  APPL-FUND     VALUE 'FUND'.
                       88  APPL-SPHD     VALUE 'SPHD'.
100897                 88  APPL-SHIP     VALUE 'SHIP'.
                       88  APPL-IRAS     VALUE 'IRAS'.
                       88  APPL-VALU     VALUE 'VALU'.
                       88  APPL-DESC     VALUE 'DESC'.
060998                 88  APPL-VICE     VALUE 'VICE'.
                       88  APPL-SACH     VALUE 'SACH'.
                       88  APPL-PIPS     VALUE 'PIPS'.
                       88  APPL-DOCA     VALUE 'DOCA'.
012099                 88  APPL-HOLD     VALUE 'HOLD'.
122100                 88  APPL-WRAP     VALUE 'WRAP'.
021501                 88  APPL-BORD     VALUE 'BORD'.
080201                 88  APPL-MADD     VALUE 'MADD'.
032102                 88  APPL-SPAD     VALUE 'SPAD'.
041802                 88  APPL-CUST     VALUE 'CUST'.
071802                 88  APPL-AIRE     VALUE 'AIRE'.
020803                 88  APPL-HAPI     VALUE 'HAPI'.
                   10  DE-FUNCTION       PIC X(4).
                       88  FUNC-ADD      VALUE 'ADD '.
                       88  FUNC-EDIT     VALUE 'EDIT'.
                       88  FUNC-CORR     VALUE 'CORR'.
                       88  FUNC-VOID     VALUE 'VOID'.
                       88  FUNC-INQ      VALUE 'INQ '.
041412                 88  FUNC-FIRST    VALUE 'IFUN'.
                       88  FUNC-FRWD     VALUE 'FRWD'.
                       88  FUNC-LAST     VALUE 'LAST'.
041412                 88  FUNC-CLOSE    VALUE 'CFUN'.
111012                 88  FUNC-WASH-ADD VALUE 'WASH'.
111012                 88  FUNC-WASH-REM VALUE 'WREM'.
               05  DE-USER-INFO.
                   10  DE-SECURITY-LEVEL     PIC XX.
                   10  DE-ERROR-CODE         PIC 9(4).
                   10  DE-USER.
                       15  DE-TERMID         PIC X(10).
                       15  DE-USERID         PIC X(10).
WEB   *                15  FILLER REDEFINES DE-USERID.
WEB   *                    20  DE-EMPLOYEE-NO PIC 9(5).
WEB   *                    20  FILLER        PIC X(5).
                   10  DE-DEPT               PIC X(4).
                   10  DE-WIRE-CODE          PIC X(4).
               05  DE-MESSAGE                PIC X(25).
               05  DE-ERROR-TO-TERM-SW       PIC X(1).
                   88  DE-SEND-ERROR-TO-TERM VALUE 'Y' SPACES.
                   88  DE-DONT-SEND-ERROR-TO-TERM
                                             VALUE 'N'.
               05  DE-ERROR-OPTION           PIC X(1).
                   88  DE-RETURN-ERROR-MSG-TO-CALLER
                                            VALUE 'R' SPACES.
                   88  DE-HANDLE-ERROR-IN-SUBROUTINE
                                            VALUE 'S'.
110499         05  DE-TRANS-RID              PIC X(12).
110499         05  DE-TRANS-ORIGIN           PIC X(5).
               05  DE-ERROR-RETURN-SUB       PIC 9(3).
062002         05  DE-ORIGINATOR             PIC X(8).
050604         05  DE-REQUEST-SOURCE-IND     PIC X(1).
050604             88  DE-BETALINK-REQ      VALUE 'A'.
050604             88  DE-BLSERVER-REQ      VALUE 'B'.
050604             88  DE-THIN-CLIENT-REQ   VALUE 'C'.
050604             88  DE-PUBLIC-REQ        VALUE 'D'.
081410             88  DE-CONVERSION-REQ    VALUE 'V'.
060304         05  DE-MAX-BUFFER-IND         PIC X.
081410         05  DE-FILE-OR-DB-MODE        PIC X.
081410             88  DE-FILE-MODE         VALUE 'F'.
081410             88  DE-DB-MODE           VALUE 'D' ' '.
040816         05  DE-USER-ID-TYPE           PIC X(2).
010617         05  DE-INTERNAL-REGION-SW     PIC X(1).
010617             88  DE-BETA-INTERNAL      VALUE 'Y'.
010617             88  DE-EXTERNAL-COMPANY   VALUE 'N'.
101318         05  DE-RECORD-WAS-RETURNED-IND PIC X(1).
101318         05  FILLER                    PIC X(60).
               05  DE-LAST-SET-ID.
                   10  DE-LAST-SET-RID   PIC X(22).
                   10  DE-LAST-SET-URI   PIC X(11).
                   10  DE-LAST-SET-FILL  PIC X(12).
               05  DE-KEY-AREA.
                   10  DE-SEARCH-FORMAT  PIC X.
                   10  DE-KEY            PIC X(40).

           04  DE-HAPI-COMMON-AREA.
               05  DE-HAPI-ACCT-INFO.
                   07  DE-HAPI-ACCT-FND             PIC X(1).
                       88  DE-HAPI-ACCT-NIF    VALUE ALL '*'.
                   07  DE-HAPI-NASUB                PIC 9(3).
                   07  DE-HAPI-REP                  PIC X(04).
                   07  DE-HAPI-REP-B4-ACATS         PIC X(04).
                   07  DE-HAPI-LST-STM-MMYY         PIC 9(5).
                   07  DE-HAPI-NASPHDLG             PIC X(1).
                   07  DE-HAPI-PROCESSING-5B-DEFA   PIC X(1).
                   07  DE-HAPI-NA-LINES             PIC X(180).
                   07  FILLER REDEFINES DE-HAPI-NA-LINES.
                       09  DE-HAPI-NA-LINES-TBL  OCCURS 6 TIMES.
                           11  DE-HAPI-NA-LINE          PIC X(30).
                   07  FILLER REDEFINES DE-HAPI-NA-LINES.
                       09  DE-HAPI-NA-LINE1             PIC X(30).
                       09  DE-HAPI-NA-LINE2             PIC X(30).
                       09  DE-HAPI-NA-LINE3             PIC X(30).
                       09  DE-HAPI-NA-LINE4             PIC X(30).
                       09  DE-HAPI-NA-LINE5             PIC X(30).
                       09  DE-HAPI-NA-LINE6             PIC X(30).
082103             07  DE-HAPI-BRANCH               PIC X(04).
082103             07  FILLER                       PIC X(95).

               05  DE-HAPI-MISC-INFO.
                   07  DE-HAPI-PARENT-ACCT          PIC 9(9).
                   07  DE-HAPI-MAD-ADR-BEG-CYMD     PIC 9(8).
                   07  DE-HAPI-MAD-ADR-END-CYMD     PIC 9(8).
                   07  DE-HAPI-PYDWN-TRCK-CYMD      PIC 9(8).
033004             07  DE-HAPI-FIRM-ON-TAXL-OR-HOLD PIC X(4).
033004                 88 DE-FIRM-IS-ON-TAXL VALUE 'TAXL'.
033004                 88 DE-FIRM-IS-ON-HOLD VALUE 'HOLD'.
033004             07  FILLER                       PIC X(15).

               05  DE-HAPI-MESS-INFO.
                   07  DE-HAPI-FIRMMESS             PIC X(01).
                   07  DE-HAPI-FIRM-FROM-STAT       PIC X(10).
                   07  DE-HAPI-FIRM-MSG-BLANK       PIC X(25).
                   07  DE-HAPI-FIRMMESS1            PIC X(60).
                   07  DE-HAPI-FIRMMESS2            PIC X(60).
                   07  DE-HAPI-FIRMMESS3            PIC X(60).
                   07  DE-HAPI-FIRMMESS4            PIC X(60).
                   07  DE-HAPI-FIRMMESS5            PIC X(60).
                   07  DE-HAPI-ACTMESS              PIC X(01).
                   07  DE-HAPI-ACT-FROM-STAT        PIC X(10).
                   07  DE-HAPI-ACT-MSG-BLANK        PIC X(25).
                   07  DE-HAPI-ACTMESS1             PIC X(60).
                   07  DE-HAPI-ACTMESS2             PIC X(60).
                   07  DE-HAPI-ACTMESS3             PIC X(60).
                   07  DE-HAPI-ACTMESS4             PIC X(60).
                   07  DE-HAPI-ACTMESS5             PIC X(60).
                   07  DE-HAPI-INTCODE              PIC X(20).
                   07  FILLER                       PIC X(208).

WEB   *    04  DE-ACT-HLD-KEY-AREA                     PIC X(11).
WEB   *    04  FILLER REDEFINES DE-ACT-HLD-KEY-AREA.
WEB        04  DE-ACT-HLD-KEY-AREA.
082103         05  DE-ACT-HLD-FIRM-NO                  PIC 9(3).
082103         05  DE-ACT-HLD-ACCT-NO                  PIC 9(8).
WEB   *    04  FILLER REDEFINES DE-ACT-HLD-KEY-AREA.
WEB   *        05  DE-ACT-HLD-FIRM-NO-X                PIC X(3).
WEB   *        05  DE-ACT-HLD-ACCT-NO-X                PIC X(8).
           04  DE-ACT-HLD-LAYOUT.
               05  DE-ACT-HLD-CHANGE-TIMESTAMP         PIC X(26).
               05  DE-ACT-HLD-ACCT-METHOD-CODE         PIC X(1).
020803         05  DE-ACT-HLD-PROCESSING-5B-IND        PIC X(1).
               05  DE-ACT-HLD-APLY-PYDWN-OPN-IND       PIC X(1).
               05  DE-ACT-HLD-APLY-PYDWN-CLS-IND       PIC X(1).
               05  DE-ACT-HLD-CHANGE-WHO-CODE          PIC X(3).
               05  DE-ACT-HLD-CHANGE-USER-ID           PIC X(10).
               05  FILLER                              PIC X(144).

WEB   *    04  DE-ACT-HLD-LAYOUT-X REDEFINES DE-ACT-HLD-LAYOUT.
WEB   *        05  DE-ACT-HLD-CHANGE-TIMESTAMP-X       PIC X(26).
WEB   *        05  DE-ACT-HLD-ACCT-METHOD-CODE-X       PIC X(1).
WEB   *        05  DE-ACT-HLD-PROCESSING-5B-IND-X      PIC X(1).
WEB   *        05  DE-ACT-HLD-APLY-PYDWN-OPN-INDX      PIC X(1).
WEB   *        05  DE-ACT-HLD-APLY-PYDWN-CLS-INDX      PIC X(1).
WEB   *        05  DE-ACT-HLD-CHANGE-WHO-CODE-X        PIC X(3).
WEB   *        05  DE-ACT-HLD-CHANGE-USER-ID-X         PIC X(10).
WEB   *        05  FILLER                              PIC X(144).
WEB   *    04  DE-ACT-STM-KEY-AREA                     PIC X(11).
WEB   *    04  FILLER REDEFINES DE-ACT-STM-KEY-AREA.
WEB        04  DE-ACT-STM-KEY-AREA.
082103         05  DE-ACT-STM-FIRM-NO                  PIC 9(3).
082103         05  DE-ACT-STM-ACCT-NO                  PIC 9(8).
WEB   *    04  FILLER REDEFINES DE-ACT-STM-KEY-AREA.
WEB   *        05  DE-ACT-STM-FIRM-NO-X                PIC X(3).
WEB   *        05  DE-ACT-STM-ACCT-NO-X                PIC X(8).
           04  DE-ACT-STM-LAYOUT.
               05  DE-ACT-STM-CHANGE-TIMESTAMP         PIC X(26).
               05  DE-ACT-STM-STMT-CODE                PIC X(1).
               05  DE-ACT-STM-STAT                     PIC 9(1).
               05  DE-ACT-STM-MMF-SUPPRESS-IND         PIC X(1).
               05  DE-ACT-STM-TAX-LOTS-IND             PIC X(1).
               05  DE-ACT-STM-OFF-PREMISE-IND          PIC X(1).
               05  DE-ACT-STM-REALIZED-IND             PIC X(1).
               05  DE-ACT-STM-APLY-PYDWN-OPN-IND       PIC X(1).
               05  DE-ACT-STM-APLY-PYDWN-CLS-IND       PIC X(1).
               05  DE-ACT-STM-GORL-OVERRIDE-IND        PIC X(1).
               05  DE-ACT-STM-REP-COPY-IND             PIC X(1).
020803         05  DE-ACT-STM-DIST-METHOD-IND          PIC X(1).
               05  DE-ACT-STM-CHANGE-WHO-CODE          PIC X(3).
               05  DE-ACT-STM-CHANGE-USER-ID           PIC X(10).
021904         05  DE-ACT-STM-MSG-SUPPRESS-IND         PIC X(1).
011208         05  DE-ACT-STM-DVP-IND                  PIC X(1).
011208         05  FILLER                              PIC X(26).
               05  DE-ACT-STM-DEFAULTS.
                   07  DE-ACT-STM-TAX-LOTS-DEFA            PIC X.
                   07  DE-ACT-STM-OFF-PREMISE-DEFA         PIC X.
                   07  DE-ACT-STM-REALIZED-DEFA            PIC X.
                   07  DE-ACT-STM-APLY-PYDWN-OPN-DEFA      PIC X.
                   07  DE-ACT-STM-APLY-PYDWN-CLS-DEFA      PIC X.
                   07  DE-ACT-STM-GORL-OVERRIDE-DEFA       PIC X.
                   07  DE-ACT-STM-REP-COPY-DEFA            PIC X.
                   07  FILLER                              PIC X(3).
020803         05  FILLER                              PIC X(100).

WEB   *    04  DE-ACT-STM-LAYOUT-X REDEFINES DE-ACT-STM-LAYOUT.
WEB   *        05  DE-ACT-STM-CHANGE-TIMESTAMP-X       PIC X(26).
WEB   *        05  DE-ACT-STM-STMT-CODE-X              PIC X(1).
WEB   *        05  DE-ACT-STM-STAT-X                   PIC X(1).
WEB   *        05  DE-ACT-STM-MMF-SUPPRESS-IND-X       PIC X(1).
WEB   *        05  DE-ACT-STM-TAX-LOTS-IND-X           PIC X(1).
WEB   *        05  DE-ACT-STM-OFF-PREMISE-IND-X        PIC X(1).
WEB   *        05  DE-ACT-STM-REALIZED-IND-X           PIC X(1).
WEB   *        05  DE-ACT-STM-APLY-PYDWN-OPN-INDX      PIC X(1).
WEB   *        05  DE-ACT-STM-APLY-PYDWN-CLS-INDX      PIC X(1).
WEB   *        05  DE-ACT-STM-GORL-OVERRIDE-IND-X      PIC X(1).
WEB   *        05  DE-ACT-STM-REP-COPY-IND-X           PIC X(1).
WEB   *        05  DE-ACT-STM-DIST-METHOD-IND-X        PIC X(1).
WEB   *        05  DE-ACT-STM-CHANGE-WHO-CODE-X        PIC X(3).
WEB   *        05  DE-ACT-STM-CHANGE-USER-ID-X         PIC X(10).
WEB   *        05  DE-ACT-STM-MSG-SUPPRESS-IND-X       PIC X(1).
WEB   *        05  DE-ACT-STM-DVP-IND-X                PIC X(1).
WEB   *        05  FILLER                              PIC X(26).
WEB   *        05  DE-ACT-STM-DEFAULTS-X               PIC X(10).
WEB   *        05  FILLER                              PIC X(100).
WEB   *    04  DE-STM-COD-KEY-AREA             PIC X(12).
WEB   *    04  FILLER REDEFINES DE-STM-COD-KEY-AREA.

           04  DE-STM-COD-KEY-AREA.
               05  DE-STM-COD-FIRM-NO                  PIC S9(3).
               05  DE-STM-COD-SUB-NO                   PIC S9(3).
           04  DE-STM-COD-LAYOUT.
               05  DE-STM-COD-REP                      PIC X(4).
               05  DE-STM-COD-STMT-CODE-IND            PIC X(1).
               05  DE-STM-COD-FILE-TYPE-IND            PIC X(1).
               05  DE-STM-COD-COST-BASIS-SW            PIC X(1).
               05  DE-STM-COD-TAX-LOTS-SW              PIC X(1).
               05  DE-STM-COD-REALIZED-IND             PIC X(1).
               05  DE-STM-COD-OFF-PREMISE-SW           PIC X(1).
               05  DE-STM-COD-PAYDOWN-OPEN-SW          PIC X(1).
               05  DE-STM-COD-PAYDOWN-CLOS-SW          PIC X(1).
               05  DE-STM-COD-DESCRIPTION              PIC X(30).
               05  DE-STM-COD-GORL-OVERRIDE-SW         PIC X(1).
               05  DE-STM-COD-CHANGE-DATE              PIC X(10).
               05  DE-STM-COD-CHANGE-WHO-CODE          PIC X(3).
               05  DE-STM-COD-TAX-LOTS-IND             PIC X(1).
WEB   *        05  FILLER                              PIC X(81).
WEB        04  FILLER                                  PIC X(1830).

*******  DATA ENGINE RETURN CODES
           04  DE-RETURN-CODE-TABLE.
               05  DE-RETURN-CODES                    OCCURS 50 TIMES.
                   10  DE-RETURN-MSG-IND              PIC X(1).
                       88  DE-RETURN-WARNING          VALUE 'W'.
                       88  DE-RETURN-EDIT-ERROR       VALUE 'E'.
                       88  DE-RETURN-SYSTEM-ERROR     VALUE 'S'.
                   10  DE-RETURN-CODE                 PIC 9(4).
               05  FILLER                              PIC X(50).
