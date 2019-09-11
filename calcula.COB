      ******************************************************************
      * Author: Marco
      * Date: 05-09-2019
      * Purpose: calculos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       77  primeiroValor   pic 9(2).
       77  segundoValor    pic 9(2).
       77  soma            pic 9(2).
       77  subtrai         pic 9(2).
       77  divisao         pic 9(2).
       77  multiplicar     pic 9(2).

       77  escolha         pic 9(1).

       77  maskValor1      pic zzzz9.
       77  maskValor2      pic zzzz9.

       77  m_soma          pic zzzz9.
       77  m_subtrai       PIC zzzz9.
       77  m_divisao       pic zzzz9.
       77  m_multiplica    PIC zzzz9.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.

           primeiraF.
           DISPLAY "Introduza valor1: ".
           ACCEPT primeiroValor.

           perform until primeiroValor > 0 and primeiroValor < 10
             DISPLAY "Introduza valor1: "
             ACCEPT primeiroValor

           END-PERFORM.

           segundaF.
           DISPLAY "Introduza valor2: ".
           ACCEPT segundoValor.

           perform until segundoValor > 0 and segundoValor < 10
             DISPLAY "Introduza valor2: "
             ACCEPT segundoValor

           END-PERFORM.

           move primeiroValor to maskValor1.
           move segundoValor to maskValor2.

           DISPLAY "operacoes possiveis:".
           DISPLAY "1 - soma".
           DISPLAY "2 - subtrai".
           DISPLAY "3 - divide".
           DISPLAY "4 - multiplica".

           ACCEPT escolha.

           IF escolha = 1
               move multiplicar to m_multiplica
               COMPUTE soma = primeiroValor + segundoValor
               move soma to m_soma
           END-IF.

           IF escolha = 2
                COMPUTE subtrai = primeiroValor - segundoValor
                move subtrai to m_subtrai
           END-IF.

           IF escolha = 3
               COMPUTE divisao = primeiroValor / segundoValor
               move divisao to m_divisao
           END-IF.

           IF escolha = 4
               COMPUTE multiplicar = primeiroValor * segundoValor
                move multiplicar TO m_multiplica
           END-IF.

           DISPLAY "RESULTADOS: ".
           DISPLAY "SOMA           " m_soma.
           DISPLAY "SUBTRAI        " m_subtrai.
           DISPLAY "DIVISAO        " m_divisao.
           DISPLAY "MULTIPLICAR    " m_multiplica.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
