       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL00.
       AUTHOR. BRANDON PAXTON.
       DATE-COMPILED.
      ***************************************************************
      * PURPOSE:  THIS PROGRAM WILL CREATE A DATASET THAT WILL
      * CONTAIN WEEKLY PAROLL INFORMATION.  THE WEEKLY WAGE IS
      * CALCULATED AND THE TOTAL NUMBER OF RECORDS PROCESSED IS
      * ACCUMULATED.
      ***************************************************************
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT EMPLOYEE-DATA
                   ASSIGN TO EMPDATI
                    ORGANIZATION IS INDEXED
                    ACCESS IS SEQUENTIAL
                    RECORD KEY ER-EMP-NUM
                    FILE STATUS IS WS-MAST-STATUS.

            SELECT YOUR-LISTING
                     ASSIGN REPORT1.

       DATA DIVISION.
       FILE SECTION.

       FD  EMPLOYEE-DATA
           RECORD IS VARYING
             FROM 03 TO 96 CHARACTERS.

         01  EMPLOYEE-RECORD.
           05  ER-ROOT-SEGMENT.
               10  ER-EMP-NUM          PIC 99.
               10  ER-EMP-SEGS         PIC 9.
           05 ER-PROJ-SEG         OCCURS 0 TO 3 TIMES
                                  DEPENDING ON ER-EMP-SEGS
                                   INDEXED BY PROJ-X.
               10 ER-PROJ-NUM           PIC X(6).
               10 ER-PROJ-ACTNO         PIC S9(4) USAGE COMP.
               10 ER-PROJ-TIME          PIC S9(3)V99 USAGE COMP-3.
               10 ER-PROJ-START         PIC X(10).
               10 ER-PROJ-END           PIC X(10).

       FD  YOUR-LISTING
            RECORD CONTAINS 132 CHARACTERS.

          01  PRINT-REC       PIC X(132).


       WORKING-STORAGE SECTION.

       01  WORK-FIELDS.
           05 MORE-RECORDS      PIC X(3) VALUE 'YES'.
           05 WS-EMP-CNT        PIC 9(3).
           05 WS-WAGES-EARNED   PIC 9(3)V99.
           05 WS-OT-TOTAL       PIC 999V99.
           05 WS-MAST-STATUS    PIC XX.
           05 WF-EMPNO          PIC X(6).
           05 DISPLAY-SQLCODE     PIC 999-.
           05 EOJ                 PIC X VALUE 'F'.
               88 EOJ-T                 VALUE 'T'.
           05 F-REC               PIC X VALUE 'T'.
               88 F-REC-F               VALUE 'F'.

           05 TEMP-EMPNO          PIC X(6).
           05 WF-AVG              PIC S9(7)V99 COMP-3.
           05 WF-MIN              PIC S9(7)V99 COMP-3.
           05 WF-MAX              PIC S9(7)V99 COMP-3.
           05 WF-COUNT            PIC S9(3) COMP.
           05 SPACE-CONT          PIC 99 VALUE 02.
           05 TEMP-ACTNO          PIC S9(4) COMP.

        01 TITLE-LINE.
              05                    PIC X(5) VALUE SPACES.
              05                    PIC X(7) VALUE 'KC03AF5'.
              05                    PIC X(23) VALUE SPACES.
              05                    PIC X(21) VALUE
                'PROGRAM 4 SQL PROGRAM'.
              05                    PIC X(77) VALUE SPACES.

        01 HEADING-LINE.
           05                    PIC X(06) VALUE 'EMPNUM'.
           05                    PIC X(03) VALUE SPACES.
           05                    PIC X(04) VALUE 'NAME'.
           05                    PIC X(22) VALUE SPACES.
           05                    PIC X(08) VALUE 'DEPT NUM'.
           05                    PIC X(03) VALUE SPACES.
           05                    PIC X(06) VALUE 'SALARY'.
           05                    PIC X(10) VALUE SPACES.
           05                    PIC X(08) VALUE 'HIREDATE'.
           05                    PIC X(05) VALUE SPACES.
           05                    PIC X(08) VALUE 'PROJ NUM'.
           05                    PIC X(03) VALUE SPACES.
           05                    PIC X(11) VALUE 'DESCRIPTION'.
           05                    PIC X(12) VALUE SPACES.
           05                    PIC X(10) VALUE 'START DATE'.

       01 DETAIL-LINE.
          05 DL-EMPNO           PIC X(06).
          05                    PIC X(03) VALUE SPACES.
          05 DL-NAME            PIC X(23).
          05                    PIC X(03) VALUE SPACES.
          05 DL-WORKDEPT        PIC X(03).
          05                    PIC X(08) VALUE SPACES.
          05 DL-SALARY          PIC $$,$$$,999.99.
          05                    PIC X(03) VALUE SPACES.
          05 DL-HIREDT          PIC X(10).
          05                    PIC X(03) VALUE SPACES.
          05 DL-PROJNO          PIC X(06).
          05                    PIC X(05) VALUE SPACES.
          05 DL-ACTDESC         PIC X(20).
          05                    PIC X(03) VALUE SPACES.
          05 DL-EMSTDATE        PIC X(10).

       01 TOTAL-HEADING.
          05                    PIC X(09) VALUE 'TOTAL EMP'.
          05                    PIC X(03) VALUE SPACES.
          05                    PIC X(06) VALUE '   AVG'.
          05                    PIC X(11) VALUE SPACES.
          05                    PIC X(10) VALUE '   MIN SAL'.
          05                    PIC X(07) VALUE SPACES.
          05                    PIC X(10) VALUE '   MAX SAL'.

       01 TOTAL-DETAIL.
          05 TD-COUNT           PIC 999.
          05                    PIC X(09) VALUE SPACES.
          05 TD-AVG             PIC $$,$$$,999.99.
          05                    PIC X(03) VALUE SPACES.
          05 TD-MIN             PIC $$,$$$,999.99.
          05                    PIC X(03) VALUE SPACES.
          05 TD-MAX             PIC $$,$$$,999.99.

           EXEC SQL
             INCLUDE EMPROACT
           END-EXEC.

           EXEC SQL
             INCLUDE ACT
           END-EXEC.

           EXEC SQL
             INCLUDE EMP
           END-EXEC.

           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
               DECLARE EMPCURS CURSOR FOR
            SELECT E.EMPNO, LASTNAME || ', ' || SUBSTR(FIRSTNME,1,1) ||
                     '.' ||
                 CASE
                 WHEN TRIM(MIDINIT) IS NULL THEN '  '
                 ELSE MIDINIT
                 END   || '.' , WORKDEPT, SALARY, HIREDATE,
                 VALUE(PROJNO, 'NO PROJ'), VALUE(ACTDESC, 'CURRENTLY'),
                 COALESCE(CHAR (EMSTDATE) , 'ASSIGNED')
             FROM SCM.EMP E FULL JOIN SCM.EMPPROJACT EP
                     ON E.EMPNO =EP.EMPNO
             FULL JOIN SCM.ACT A ON A.ACTNO = EP.ACTNO
             WHERE HIREDATE IS NOT NULL
             AND HIREDATE >= (CURRENT DATE - 37 YEARS)
             ORDER BY EMPNO
            END-EXEC.

       PROCEDURE DIVISION.

      ***************************************************************
      * 000-MAIN.
      *  - PERFORM 100-HSK
      *  - PERFORM 110-WAGE-ROUTINE UNTIL ALL RECORDS HAVE BEEN READ
      *  - PERFORM 120-EOJ
      *  - RETURN CONTROL BACK TO THE OS
      ***************************************************************
       000-MAIN.

           PERFORM 100-HSK.

           PERFORM UNTIL MORE-RECORDS = 'NO'
               READ EMPLOYEE-DATA
                   AT END
                     MOVE 'NO' TO MORE-RECORDS
                   NOT AT END
                     PERFORM 320-ADD-EMP
                     PERFORM 321-ADD-PROJ
                           VARYING PROJ-X FROM 1 BY 1
                           UNTIL PROJ-X > ER-EMP-SEGS
               END-READ
           END-PERFORM.

           PERFORM 120-PRINT-HEADINGS.
           PERFORM 110-WRITE-DATA WITH TEST BEFORE
                   UNTIL EOJ-T.
           PERFORM 150-EOJ.

      ***************************************************************
      * 100-HSK.
      *  - FILES ARE OPENED TO BEGIN PROCESSING
      ***************************************************************
       100-HSK.

           OPEN INPUT EMPLOYEE-DATA
           OPEN  OUTPUT YOUR-LISTING.
           EXEC SQL
               OPEN  EMPCURS
           END-EXEC.

             IF WS-MAST-STATUS NOT = '00'
               MOVE 'NO' TO MORE-RECORDS
               DISPLAY '******************************'
               DISPLAY ' 100-INITIALIZATION-RTN'
               DISPLAY ' ERROR IN OPENING THE MASTER FILE'
               DISPLAY ' FILE STATUS IS ', WS-MAST-STATUS
               DISPLAY '*******************************'
            END-IF.


      ***************************************************************
      * 320-ADD-EMP.
      *  - ADD ROWS TO TABLES
      ***************************************************************
        320-ADD-EMP.
           INITIALIZE WF-EMPNO.
           MOVE ER-EMP-NUM (1:2) TO WF-EMPNO (1:2).
           MOVE '8031' TO WF-EMPNO (3:4).

           EXEC SQL
               DELETE FROM SCM.EMPPROJACT
                  WHERE EMPNO = :WF-EMPNO
           END-EXEC.


            EXEC SQL
                DELETE FROM SCM.EMP
                   WHERE EMPNO = :WF-EMPNO
            END-EXEC.

           IF SQLCODE  = 0 OR SQLCODE = 100
             EXEC SQL
                  COMMIT
             END-EXEC
           ELSE
           MOVE SQLCODE TO DISPLAY-SQLCODE
           DISPLAY '******************************'
           DISPLAY ' 320-ADD-EMP'
           DISPLAY ' ERROR IN DELETING OLD RECORDS'
           DISPLAY ' SQL STATUS IS ', DISPLAY-SQLCODE
           DISPLAY '*******************************'

           MOVE 'NO' TO MORE-RECORDS
             EXEC SQL
                  ROLLBACK
             END-EXEC
           END-IF.

           EXEC SQL
               INSERT INTO SCM.EMP
                   VALUES( :WF-EMPNO,'BRANDON',
                            'J', 'PAXTON', 'D11', '9999',
                      (CURRENT DATE - 37 YEARS),
                          'CLERK', 18, 'M', '1987-3-21', 50000.00,
                          1600.00, 4220.00)
           END-EXEC.

           IF SQLCODE  = 0
             EXEC SQL
                  COMMIT
             END-EXEC
           ELSE
           MOVE SQLCODE TO DISPLAY-SQLCODE
           MOVE SQLCODE TO DISPLAY-SQLCODE
           DISPLAY '******************************'
           DISPLAY ' 320-ADD-EMP'
           DISPLAY ' ERROR IN INSERTING EMP REC'
           DISPLAY ' SQL STATUS IS ', DISPLAY-SQLCODE
           DISPLAY '*******************************'
           MOVE 'NO' TO MORE-RECORDS
             EXEC SQL
                  ROLLBACK
             END-EXEC
           END-IF.

      ***************************************************************
      * 321-ADD-PROJ.
      *  - ADD ROWS TO TABLES
      ***************************************************************
        321-ADD-PROJ.
           INITIALIZE TEMP-ACTNO.

           MOVE ER-PROJ-NUM (PROJ-X) TO PROJNO.
           MOVE ER-PROJ-ACTNO (PROJ-X) TO TEMP-ACTNO.
           MOVE ER-PROJ-TIME (PROJ-X) TO EMPTIME.
           MOVE ER-PROJ-START (PROJ-X) TO EMSTDATE.
           MOVE ER-PROJ-END (PROJ-X) TO EMENDATE.

           IF ER-EMP-SEGS > 0
              EXEC SQL
                 INSERT INTO SCM.EMPPROJACT
                    VALUES (:WF-EMPNO, :PROJNO,
                :TEMP-ACTNO, :EMPTIME,
                :EMSTDATE, :EMENDATE)
              END-EXEC

             IF SQLCODE  = 0
               EXEC SQL
                    COMMIT
               END-EXEC
             ELSE
             MOVE SQLCODE TO DISPLAY-SQLCODE
             DISPLAY '******************************'
             DISPLAY ' 321-ADD-PROJ'
             DISPLAY ' ERROR IN INSERTING INTO EMPPROJACT'
             DISPLAY ' SQL STATUS IS ', DISPLAY-SQLCODE
             DISPLAY '*******************************'
             MOVE 'NO' TO MORE-RECORDS
               EXEC SQL
                    ROLLBACK
               END-EXEC
             END-IF
           END-IF.
      *********************************
      *  110-WRITE-DATA.
      **********************************
       110-WRITE-DATA.


             INITIALIZE DETAIL-LINE.
             MOVE SPACES TO ACTDESC-TEXT.
             EXEC SQL
                 OPEN  EMPCURS
             END-EXEC.

             EXEC SQL
                 FETCH EMPCURS
                     INTO  :DL-EMPNO, :DL-NAME, :DL-WORKDEPT,
                           :SALARY, :DL-HIREDT, :DL-PROJNO, :ACTDESC,
                           :DL-EMSTDATE
             END-EXEC.


             IF SQLCODE = 100
                 SET EOJ-T TO TRUE
             END-IF.
             IF SQLCODE NOT = 100 AND SQLCODE NOT = 0
                MOVE SQLCODE TO DISPLAY-SQLCODE
                DISPLAY '******************************'
                DISPLAY ' 110-WRITE-DATA'
                DISPLAY ' ERROR IN WRITING DATA TO OUTPUT'
                DISPLAY ' SQL STATUS IS ', DISPLAY-SQLCODE
                DISPLAY '*******************************'
                 SET EOJ-T TO TRUE
             END-IF.

             IF SQLCODE NOT = 100
              MOVE SALARY TO DL-SALARY

              IF DL-EMPNO NOT = TEMP-EMPNO AND F-REC-F
                   PERFORM 120-PRINT-HEADINGS
              END-IF


              MOVE ACTDESC-TEXT TO DL-ACTDESC
              MOVE DL-EMPNO TO  TEMP-EMPNO


              MOVE DETAIL-LINE TO PRINT-REC
              WRITE PRINT-REC AFTER ADVANCING 1 LINE
             END-IF.

             SET F-REC-F TO TRUE.
      *********************************
      *  120-PRINT-HEADINGS.
      **********************************
       120-PRINT-HEADINGS.

            MOVE HEADING-LINE TO PRINT-REC.
            WRITE PRINT-REC AFTER SPACE-CONT.
            MOVE 14 TO SPACE-CONT.
      *********************************
      *  150-EOJ.
      **********************************
        150-EOJ.

            EXEC SQL
              SELECT COUNT(*), AVG(SALARY), MIN(SALARY), MAX(SALARY)
              INTO :WF-COUNT, :WF-AVG, :WF-MIN, :WF-MAX
              FROM SCM.EMP
              WHERE HIREDATE >= CURRENT DATE - 37 YEARS
            END-EXEC.

            IF SQLCODE NOT = 0
              MOVE SQLCODE TO DISPLAY-SQLCODE
              DISPLAY '******************************'
              DISPLAY ' 150-EOJ'
              DISPLAY ' ERROR IN CALCULATING TOTALS'
              DISPLAY ' SQL STATUS IS ', DISPLAY-SQLCODE
              DISPLAY '*******************************'
            END-IF.

            MOVE WF-COUNT TO TD-COUNT.
            MOVE WF-AVG TO TD-AVG.
            MOVE WF-MIN TO TD-MIN.
            MOVE WF-MAX TO TD-MAX.

            MOVE TOTAL-HEADING TO PRINT-REC
            WRITE PRINT-REC AFTER ADVANCING 4 LINES.

            MOVE TOTAL-DETAIL TO PRINT-REC
            WRITE PRINT-REC AFTER ADVANCING 1 LINE.

            EXEC SQL
              CLOSE EMPCURS
            END-EXEC.

               CLOSE YOUR-LISTING.
               STOP RUN.

