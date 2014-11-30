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
    //def private COBOL_CALLS = /(CALL)(\s+)(\((.+)\))/  //TODO :Corriger  la detection des call
    def private COBOL_CALLS = /(CALL)(\s+)(\S+)(\s*)/  //TODO :Corriger  la detection des call
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
        println(fichier.name.split("\\.")[0])

        //creation du fichier de sortie ssi includes ou links ou call est > 0

        if (includes.size() > 0 || links.size() > 0 || calls.size() > 0) {

            def nomfichierout = fichier.parent + "\\" + fichier.name.split("\\.")[0] + ".dot"
            println(nomfichierout)
            if (new File(nomfichierout).exists()) {
                new File(nomfichierout).delete()
                println("suppression $nomfichierout:OK")
                //TODO : corriger la gestion de la suppression des fichiers existant avant regeneration
            }
            def output = new File(fichier.parent + "\\" + fichier.name.split("\\.")[0] + ".dot")
            output << "digraph g{\r\n"
            output << "node [shape=box, color=lightblue2, style=filled];\r\n"
            output << fichier.name.split("\\.")[0] + " [shape=circle, color=blue, fontcolor=white];\r\n"

            // gestion des link
            if (links.size() > 0) {
                links.each { it ->
                    output << "   " + fichier.name.split("\\.")[0] + " -> " + existeCoteAlorsSuppressionString(it).toString() + ";\r\n"
                    output << "   " + existeCoteAlorsSuppressionString(it) + " [shape=circle, color=thistle1, fontcolor=purple];\r\n"
                }
            }

            // gestion des includes
            if (includes.size() > 0) {
                includes.each { it -> output << "   " + fichier.name.split("\\.")[0] + " -> " + it.toString() + ";\r\n"
                }
            }

            // gestion des calls
            if (calls.size() > 0) {
                calls.each { it -> output << "   " + fichier.name.split("\\.")[0] + " -> " + it.toString() + ";\r\n"
                output << "    " + it + " [color=yellow, style=filled, shape=polygon, sides=6];\r\n"
                 //TODO ajouter un mise ne forme pour la representation des call sous DOT
                }
            }

            // fin du fichier dot
            output << "}\r\n"
        }

    }


    // suppression de cote sur des string possedant ces elements
    def existeCoteAlorsSuppressionString(String texte){
        if(texte[0]== '\'' && texte[texte.length() - 1]== '\''){
            return texte[1..texte.length()-2]
        }
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

