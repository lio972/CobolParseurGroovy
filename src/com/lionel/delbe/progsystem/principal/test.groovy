package com.lionel.delbe.progsystem.principal

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 04/02/14
 * Time: 18:48
 * To change this template use File | Settings | File Templates.
 */

def COBOL_INCLUDE = /INCLUDE\s+([\w\d]+)*([^\s*])*/
def COBOL_LINK = /LINK\s+PROGRAM\s*\(.+\)/
def COBOL_CALLS = /CALL\s+\(.+\)/

def montexte = "01486 *                                                                 EFUTSTA4\n" +
        "01487      INITIALIZE  MATERCOM.                                        EFUTSTA4\n" +
        "01849 *                                                                 MA30T00I\n" +
        "01488      MOVE  IDENT-TS-APP  TO  TERCOM-TS.                           EFUTSTA4\n" +
        "01489      EXEC  CICS  LINK  PROGRAM  ('MA90T07')                       EFUTSTA4\n" +
        "01490                        COMMAREA (MATERCOM)                        EFUTSTA4\n" +
        "01491                        LENGTH   (100)                             EFUTSTA4\n" +
        "01492      END-EXEC.                                                    EFUTSTA4\n" +
        "01493      IF  EIBRCODE NOT = LOW-VALUE                                 EFUTSTA4\n" +
        "01494          MOVE  'MA02 PB LINK MA90T07'  TO  MESS                   EFUTSTA4\n" +
        "01495          GO  TO  ABANDON-TACHE                                    EFUTSTA4\n" +
        "01496      END-IF.                                                      EFUTSTA4\n" +
        "01497      IF  TERCOM-MESS-RETOUR  NOT  =  SPACE                        EFUTSTA4\n" +
        "01498          MOVE  TERCOM-MESS-RETOUR  TO  COM-GENE-MESINF            EFUTSTA4\n" +
        "01499                                        COM-CODERR                 EFUTSTA4\n" +
        "01500      END-IF.                                                      EFUTSTA4\n" +
        "01501 *               "
montexte.eachLine {if(it =~ COBOL_LINK) {
        //  selection = it =~COBOL_LINK
    resultat = (it =~ COBOL_LINK)
    toto = resultat[0].toString().tokenize()
    println(resultat[0] + "==>")
    println(toto)
    println(it)
        //resultat = selection.matches()
        //println(resultat[0][1])
    }
}

//def monregex =