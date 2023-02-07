       IDENTIFICATION DIVISION.
       PROGRAM-ID. ROCK-PAPER-SCISSORS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUTFILE ASSIGN TO "./input.txt"
           FILE STATUS IS FILE-CHECK-KEY
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUTFILE.
      * Structure of the input file
       01 FILE-DETAILS.
           88 END-OF-FILE VALUE HIGH-VALUES.
           05 OPPONENT PIC A.
           05 FILLER   PIC A.
           05 OWN      PIC A.

       WORKING-STORAGE SECTION.
      * Variables
       01  FILE-CHECK-KEY   PIC X(2).
       01  WS-TOTAL-PRINT   PIC Z(7)9.
       01  WS-TOTAL-SCORE-1 PIC 9(8) VALUE ZERO.
       01  WS-TOTAL-SCORE-2 PIC 9(8) VALUE ZERO.
       01  WS-OPPONENT      PIC 9(2).
       01  WS-OWN           PIC 9(2).

       PROCEDURE DIVISION.
      * The program starts here
           OPEN INPUT INPUTFILE.
           IF FILE-CHECK-KEY NOT = "00"
               DISPLAY "Unable to open file. Status: ", FILE-CHECK-KEY
               END-DISPLAY
               STOP RUN RETURNING 1
           END-IF.

           READ INPUTFILE
               AT END SET END-OF-FILE TO TRUE
           END-READ.
           PERFORM UNTIL END-OF-FILE
               COMPUTE WS-OPPONENT = FUNCTION ORD(OPPONENT)
                   - FUNCTION ORD("A")
               END-COMPUTE
               COMPUTE WS-OWN = FUNCTION ORD(OWN)
                   - FUNCTION ORD("X")
               END-COMPUTE

               COMPUTE WS-TOTAL-SCORE-1 = WS-TOTAL-SCORE-1 +
                   WS-OWN + 1 +
                   3 * FUNCTION MOD(1 + WS-OWN - WS-OPPONENT, 3)
               END-COMPUTE

               COMPUTE WS-TOTAL-SCORE-2 = WS-TOTAL-SCORE-2 +
                   FUNCTION MOD(WS-OWN + WS-OPPONENT - 1, 3) + 1 +
                   3 * WS-OWN
               END-COMPUTE

               READ INPUTFILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-PERFORM.
           MOVE WS-TOTAL-SCORE-1 TO WS-TOTAL-PRINT
           DISPLAY "Total score for Part 1: ", WS-TOTAL-PRINT
           END-DISPLAY.
           MOVE WS-TOTAL-SCORE-2 TO WS-TOTAL-PRINT
           DISPLAY "Total score for Part 2: ", WS-TOTAL-PRINT
           END-DISPLAY.

           CLOSE INPUTFILE.
           STOP RUN.
       END PROGRAM ROCK-PAPER-SCISSORS.
