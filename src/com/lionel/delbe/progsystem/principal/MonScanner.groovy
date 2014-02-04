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
    def private COBOL_INCLUDE = /(INCLUDE)(\s+)(([\w\d]+)*)(([^\s*])*)/
    def private COBOL_LINK = /(LINK)(\s+)(PROGRAM)(\s*)(\((.+)\))/
    def private COBOL_CALLS = /(CALL)(\s+)(\((.+)\))/
    def ArrayList includes = new ArrayList()
    def ArrayList links = new ArrayList()
    def ArrayList calls = new ArrayList()

    MonScanner(File fichier) {
        this.fichier = fichier
        println fichier.name
    }

    def private displayFichierEntree() {
        //println(fichier)
    }

    def private afficherContenuFichier() {
        println("debut de lecture " + fichier.getName())
        fichier.eachLine { ligne ->
            rechercherLesCalls(ligne)
        }
        println "includes==>" + includes
        println "calls   ==>" + calls
        println "links   ==>" + links

        println(fichier.absolutePath)
        println(fichier.absoluteFile)
        println(fichier.parent)
        println(fichier.parent + "\\" + fichier.name + ".dot")
        def output = new File(fichier.parent + "\\" + fichier.name + ".dot")

    }


    def rechercherLesCalls(UneLigne) {
        //*regular expression for  include\s+([\w\d]+)*([^\s*])*/
        if (UneLigne =~ COBOL_INCLUDE && (UneLigne.charAt(6) != "*")) {
            //println UneLigne
            def toto = (UneLigne =~ COBOL_INCLUDE)
            //println(toto[0].toString().tokenize("" ,))
            //println(toto[0].toString().tokenize().size())
            //def mark = toto[0]
            //mark.each {println(it)
            if (!includes.contains(toto[0][3])) {
                includes.add(toto[0][3])
            }
            includes.sort()
        }


        if (UneLigne =~ COBOL_CALLS && (UneLigne.charAt(6) != "*")) {
            println UneLigne
            def toto = (UneLigne =~ COBOL_CALLS)
            println(toto[0])
            if (!calls.contains(toto[0][3])) {
                calls.add(toto[0][3])
            }
            calls.sort()
        }


        if (UneLigne =~ COBOL_LINK && (UneLigne.charAt(6) != "*")) {
            println UneLigne
            def toto = (UneLigne =~ COBOL_LINK)
            println(toto[0])
            println(toto[0][6])
            if (!links.contains(toto[0][6])) {
                links.add(toto[0][6])
            }
            links.sort()
        }


    }
}

