      *=======================================================================*
       IDENTIFICATION DIVISION.
      *=======================================================================*
       PROGRAM-ID. PROD0V0.
      *************************************************************************
      *************************************************************************
      *************************************************************************
      *                           IEFP SOLUTIONS                              *
      *DATA:            02/09/2019                                            *
      *PROGRAMADOR:     Marco Silva                                           *
      *OBJECTIVO:                                                             *
      *               ESTE PROGRAMA SUPOSTAMENTE SERÁ UTILIZADO               *
      *              PARA AVALIAR O FORMANDO NA SUA " HABILIDADE"             *
      *                       DE COMPREENSÃO EM COBOL                         *
      *                                                                       *
      *                                                                       *
      *************************************************************************
      ***************************FIM DOCUMENTAÇÃO******************************

      *=======================================================================*
                             ENVIRONMENT DIVISION.

      *=======================================================================*
                             CONFIGURATION SECTION.
      *=======================================================================*
                               SPECIAL-NAMES.
      *=======================================================================*
                             INPUT-OUTPUT SECTION.
      *=======================================================================*
                               FILE-CONTROL.
      *=======================================================================*
                               DATA DIVISION.
      *=======================================================================*
                               FILE SECTION.
      *=======================================================================*
                           WORKING-STORAGE SECTION.

       77  input_grade         PIC z9.
       77  student_name        PIC A(20) VALUE SPACES.
       77  first_grade         PIC 9(2) VALUE 0.
       77  second_grade        PIC 9(2) VALUE 0.
       77  third_grade         PIC 9(2) VALUE 0.
       77  fourth_grade        PIC 9(2) VALUE 0.
       77  sum_grades          PIC 9(2) value zero.
       77  average             PIC S9(2)V99.
       77  avg_masked          PIC z9.99.
       77  exit_cmd            pic x(1) value null.
       77  exam_grade          pic 9(2) value zero.

       01  actual_date.
           03 ac_year   PIC  9(2) values zero.
           03 ac_month  PIC  9(2) values zero.
           03 ac_day    PIC  9(2) values zero.

      *=======================================================================*
                             PROCEDURE DIVISION.

           ACCEPT actual_date FROM DATE.
           DISPLAY "data: " AT 0125 ac_year "/" ac_month "/" ac_day.

           getName.
           DISPLAY "Insert the student name: " AT 0315.
           ACCEPT student_name AT 0340.

           IF (student_name IS NOT ALPHABETIC) OR
      -         (student_name IS EQUAL TO SPACES)
               GO TO getName
           END-IF.

           DISPLAY SPACE AT 0315 SPACE AT 0340.

           DISPLAY "_________________________________________" AT 0215.
           DISPLAY "Evaluating student: " AT 0315 student_name AT 0345.

      *      ======== ACCEPTING GRADES AND CHECKING THE VALUES =====          *
           firstGrade.
           DISPLAY "Insert first grade : " AT 0515.
           ACCEPT input_grade AT 0536.
           MOVE input_grade TO first_grade.

           IF (first_grade GREATER THAN 20) OR (first_grade LESS THAN 0)
              GO TO firstGrade
           END-IF.

           secondGrade.
           DISPLAY "Insert second grade : " AT 0815.
           ACCEPT input_grade AT 0836.
           MOVE input_grade TO second_grade.

           IF (second_grade > 20) OR (second_grade LESS THAN 0)
              GO TO secondGrade
           END-IF.

           thirdGrade.
           DISPLAY "Insert third grade : " AT 1015.
           ACCEPT input_grade AT 1036.
           MOVE input_grade TO third_grade.

           IF (third_grade GREATER THAN 20) OR (third_grade LESS THAN 0)
              GO TO thirdGrade
           END-IF.

           fourthGrade.
           DISPLAY "Insert fourth grade : " AT 1215.
           ACCEPT input_grade AT 1236.
           MOVE input_grade TO fourth_grade.

           IF (fourth_grade > 20) OR (fourth_grade LESS THAN 0)
              GO TO fourthGrade
           END-IF.


      *        ============== AVERAGE CALCULATION ==================          *
           COMPUTE sum_grades  = first_grade + second_grade
      -        + third_grade + fourth_grade.

           COMPUTE average = (sum_grades / 4).
           MOVE average to avg_masked.

           DISPLAY "Average of the student: " AT 1415 avg_masked AT 1440

           DISPLAY "_________________________________________" AT 1515.

           IF (average < 10)
               DISPLAY "FAILED THE YEAR" AT 1615
               GO TO exam
           ELSE
               DISPLAY "CONGRATULATIONS YOU PASSED" AT 1615
           END-IF.

           ACCEPT exit_cmd at 1900.
           STOP RUN.

           exam.
               DISPLAY "Exam to recover the year grade: " AT 1715.
               ACCEPT exam_grade AT 1748.

           IF (exam_grade GREATER THAN 20) OR (exam_grade LESS THAN 0)
               GO TO exam
           END-IF.

           COMPUTE exam_grade = ((exam_grade + average) / 2).
           MOVE exam_grade TO avg_masked.

           IF exam_grade GREATER or EQUAL to 10
               DISPLAY "CONGRATULATIONS YOU PASSED" AT 1815
      -        "with the average of " avg_masked AT 1865
           END-IF.

           ACCEPT exit_cmd at 0101.

       STOP RUN.

       END PROGRAM PROD0V0.
