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
    def private COBOL_INCLUDE = /INCLUDE\s+([\w\d]+)*([^\s*])*/
    def private COBOL_LINK = /LINK\s+PROGRAM\s*\(.+\)/
    def private COBOL_CALLS = /CALL\s+\(.+\)/
    def ArrayList includes = new ArrayList()
    def ArrayList links = new ArrayList()
    def ArrayList calls = new ArrayList()



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
//            if (ligne =~ COBOL_INCLUDE && (ligne.charAt(6) != "*")) {
//                println ligne
//            }
//        }
//        if (ligne =~ COBOL_CALLS) {
//            println ligne
//        }
//        if (ligne =~ COBOL_LINK) {
//            println ligne
            rechercherLesCalls(ligne)
        }
    }


    def rechercherLesCalls(UneLigne) {
        /*regular expression for  include\s+([\w\d]+)*([^\s*])*/
        if (UneLigne =~ COBOL_INCLUDE && (UneLigne.charAt(6) != "*")) {
            println UneLigne
            def toto = UneLigne =~ COBOL_INCLUDE
            println(toto)
        }
        if (UneLigne =~ COBOL_CALLS && (UneLigne.charAt(6) != "*")) {
            println UneLigne
        }
        if (UneLigne =~ COBOL_LINK && (UneLigne.charAt(6) != "*")) {
            println UneLigne
        }


    }
}

