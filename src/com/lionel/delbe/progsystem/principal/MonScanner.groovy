package com.lionel.delbe.progsystem.principal

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 06/01/14
 * Time: 22:52
 * To change this template use File | Settings | File Templates.
 */
class MonScanner {
    File fichier = null
    Analyseur analyseur = null
    def COBOL_INCLUDE = /INCLUDE\s+([\w\d]+)*([^\s*])*/
    def COBOL_LINK = /LINK\s+PROGRAM\s*\(.+\)/
    def COBOL_CALLS = /CALL\s+\(.+\)/


    MonScanner(File fichier) {
        this.fichier = fichier
    }

    def private displayFichierEntree() {
        println(fichier)
    }


    def private afficherContenuFichier() {
        println "debut de lecture"
        fichier.eachLine { ligne ->
            //println(ligne)
            //rechercherLesCalls()
            if (ligne =~ COBOL_CALLS) {
                println ligne
            }
        }

    }

    def private rechercherLesCalls() {
        /*regular expression for  include\s+([\w\d]+)*([^\s*])*/
        if (ligne =~ COBOL_INCLUDE) {
            println ligne
        }
        if (ligne =~ COBOL_CALLS) {
            println ligne
        }
        if (ligne =~ COBOL_LINK) {
            println ligne
        }

        /** #regular expression for link  link\s+program\s*\(.+\)
         * EXEC  CICS  LINK  PROGRAM  ('MA90T07')
         *    COMMAREA (MATERCOM)
         */

        /**
         *
         */


    }


}
