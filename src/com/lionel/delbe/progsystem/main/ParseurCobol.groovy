package com.lionel.delbe.progsystem.*

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 06/01/14
 * Time: 21:10
 * To change this template use File | Settings | File Templates.
 */
class ParseurCobol {

    static File dossierSrc = null
    /**
     * Methode principale du programme
     */
    public static void main(args){

        MonScanner scanner = new Mon
        try {
            def cheminDossierSrc = args[0];
            println(args[0]);
            dossierSrc = new File(args[0])

            //iteration sur tous les fichier present dans  le dossier
            dossierSrc.eachFile(){un_fichier ->
                println(un_fichier)
                // appeler scanner et lui passer un objet de type fichier
                scanner =  new MonScanner(un_fichier)
                scanner.
            }

        }catch (Exception ex){
            println ex
        }

    }

}
